
/* A trivial filter that logs all email to a file. */

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sysexits.h>
#include <unistd.h>

#include "libmilter/mfapi.h"
#include "strl.h"   /*RAN*/

typedef int bool;

#ifndef FALSE
# define FALSE	0
#endif /* ! FALSE*/
#ifndef TRUE
# define TRUE	1
#endif /* ! TRUE*/

struct mlfiPriv
{
	char	*mlfi_fname;
	FILE	*mlfi_fp;
};

#define MLFIPRIV	((struct mlfiPriv *) smfi_getpriv(ctx))

extern sfsistat	 mlfi_cleanup(SMFICTX *, bool);

sfsistat
mlfi_envfrom(ctx, envfrom)
	SMFICTX *ctx;
	char **envfrom;
{
	struct mlfiPriv *priv;
	int fd;

	/* allocate some private memory */
	priv = malloc(sizeof *priv);
	if (priv == NULL)
	{
		/* can't accept this message right now */
		return SMFIS_TEMPFAIL;
	}
	memset(priv, '\0', sizeof *priv);

	/* open a file to store this message */
	priv->mlfi_fname = strdup("/tmp/msg.XXXXXXXX");
	if (priv->mlfi_fname == NULL)
	{
		free(priv);
		return SMFIS_TEMPFAIL;
	}
	if ((fd = mkstemp(priv->mlfi_fname)) < 0 ||
	    (priv->mlfi_fp = fdopen(fd, "w+")) == NULL)
	{
		free(priv->mlfi_fname);
		free(priv);
		return SMFIS_TEMPFAIL;
	}

	/* save the private data */
	smfi_setpriv(ctx, priv);

	/* continue processing */
	return SMFIS_CONTINUE;
}

sfsistat
mlfi_header(ctx, headerf, headerv)
	SMFICTX *ctx;
	char *headerf;
	char *headerv;
{
	/* write the header to the log file */
	fprintf(MLFIPRIV->mlfi_fp, "%s: %s\r\n", headerf, headerv);

	/* continue processing */
	return SMFIS_CONTINUE;
}

sfsistat
mlfi_eoh(ctx)
	SMFICTX *ctx;
{
	/* output the blank line between the header and the body */
	fprintf(MLFIPRIV->mlfi_fp, "\r\n");

	/* continue processing */
	return SMFIS_CONTINUE;
}

sfsistat
mlfi_body(ctx, bodyp, bodylen)
	SMFICTX *ctx;
	u_char *bodyp;
	size_t bodylen;
{
	/* output body block to log file */
	if (fwrite(bodyp, bodylen, 1, MLFIPRIV->mlfi_fp) <= 0)
	{
		/* write failed */
		(void) mlfi_cleanup(ctx, FALSE);
		return SMFIS_TEMPFAIL;
	}

	/* continue processing */
	return SMFIS_CONTINUE;
}

sfsistat
mlfi_eom(ctx)
	SMFICTX *ctx;
{
	return mlfi_cleanup(ctx, TRUE);
}

sfsistat
mlfi_close(ctx)
	SMFICTX *ctx;
{
	return SMFIS_ACCEPT;
}

sfsistat
mlfi_abort(ctx)
	SMFICTX *ctx;
{
	return mlfi_cleanup(ctx, FALSE);
}

sfsistat
mlfi_cleanup(ctx, ok)
	SMFICTX *ctx;
	bool ok;
{
	sfsistat rstat = SMFIS_CONTINUE;
	struct mlfiPriv *priv = MLFIPRIV;
	char *p;
	char host[512];
	char hbuf[1024];

	if (priv == NULL)
		return rstat;

	/* close the archive file */
	if (priv->mlfi_fp != NULL && fclose(priv->mlfi_fp) == EOF)
	{
		/* failed; we have to wait until later */
		rstat = SMFIS_TEMPFAIL;
		(void) unlink(priv->mlfi_fname);
	}
	else if (ok)
	{
		/* add a header to the message announcing our presence */
		if (gethostname(host, sizeof host) < 0)
			strlcpy(host, "localhost", sizeof host);
		p = strrchr(priv->mlfi_fname, '/');
		if (p == NULL)
			p = priv->mlfi_fname;
		else
			p++;
		snprintf(hbuf, sizeof hbuf, "%s@%s", p, host);
		smfi_addheader(ctx, "X-Archived", hbuf);
	}
	else
	{
		/* message was aborted -- delete the archive file */
		(void) unlink(priv->mlfi_fname);
	}

	/* release private memory */
	free(priv->mlfi_fname);
	free(priv);
	smfi_setpriv(ctx, NULL);

	/* return status */
	return rstat;
}

struct smfiDesc smfilter =
{
	"SampleFilter",	/* filter name */
	SMFI_VERSION,	/* version code -- do not change */
	SMFIF_ADDHDRS,	/* flags */
	NULL,		/* connection info filter */
	NULL,		/* SMTP HELO command filter */
	mlfi_envfrom,	/* envelope sender filter */
	NULL,		/* envelope recipient filter */
	mlfi_header,	/* header filter */
	mlfi_eoh,	/* end of header */
	mlfi_body,	/* body block filter */
	mlfi_eom,	/* end of message */
	mlfi_abort,	/* message aborted */
	mlfi_close	/* connection cleanup */
};


int
main(argc, argv)
	int argc;
	char *argv[];
{
	int c;
	const char *args = "p:";

	/* Process command line options */
	while ((c = getopt(argc, argv, args)) != -1)
	{
		switch (c)
		{
		  case 'p':
			if (optarg == NULL || *optarg == '\0')
			{
				(void) fprintf(stderr, "Illegal conn: %s\n",
					       optarg);
				exit(EX_USAGE);
			}
			(void) smfi_setconn(optarg);
			break;

		}
	}
	if (smfi_register(smfilter) == MI_FAILURE)
	{
		fprintf(stderr, "smfi_register failed\n");
		exit(EX_UNAVAILABLE);
	}
	return smfi_main();
}

/* eof */

