#include "udpkg.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <search.h>
#include <debian-installer.h>

/* Status file handling routines
 * 
 * This is a fairly minimalistic implementation. there are two main functions 
 * that are supported:
 * 
 * 1) reading the entire status file:
 *    the status file is read into memory as a binary-tree, with just the 
 *    package and status info preserved
 *
 * 2) merging the status file
 *    control info from (new) packages is merged into the status file, 
 *    replacing any pre-existing entries. when a merge happens, status info 
 *    read using the status_read function is written back to the status file
 */

static const char *statuswords[][10] = {
	{ (char *)STATUS_WANTSTART, "unknown", "install", "hold", 
		"deinstall", "purge", 0 },
	{ (char *)STATUS_FLAGSTART, "ok", "reinstreq", "hold", 
		"hold-reinstreq", 0 },
	{ (char *)STATUS_STATUSSTART, "not-installed", "unpacked", "half-configured",
		"installed", "half-installed",
		"config-files", "post-inst-failed", 
		"removal-failed", 0 }
};

int package_compare(const void *p1, const void *p2)
{
	return strcmp(((struct package_t *)p1)->package, 
		((struct package_t *)p2)->package);
}

static unsigned long status_parse(const char *line)
{
	char *p;
	int i, j;
	unsigned long l = 0;
	for (i = 0; i < 3; i++)
	{
		p = strchr(line, ' ');
		if (p) *p = 0; 
		j = 1;
		while (statuswords[i][j] != 0) 
		{
			if (strcmp(line, statuswords[i][j]) == 0) 
			{
				l |= (1 << ((long)statuswords[i][0] + j - 1));
				break;
			}
			j++;
		}
		if (statuswords[i][j] == 0) return 0; /* parse error */
		line = p+1;
	}
	return l;
}

static const char *status_print(unsigned long flags)
{
	/* this function returns a static buffer... */
	static char buf[256];
	int i, j;

	buf[0] = 0;
	for (i = 0; i < 3; i++)
	{
		j = 1;
		while (statuswords[i][j] != 0)
		{
			if ((flags & (1 << ((long)statuswords[i][0] + j - 1))) != 0)
			{
				strcat(buf, statuswords[i][j]);
				if (i < 2) strcat(buf, " ");
				break;
			}
			j++;
		}
		if (statuswords[i][j] == 0)
		{
			fprintf(stderr, "corrupted status flag!!: %lx\n",flags);
			return NULL;
		}
	}
	return buf;
}

static char *read_block(FILE *f)
{
	char ch;
	char *multiple_lines = strdup("");
	size_t len = 0;
	char buf[BUFSIZE];

	while (((ch = fgetc(f)) == ' ') && !feof(f)) {
		size_t buflen;

		fgets(buf, BUFSIZE, f);
		buflen = strlen(buf);
		multiple_lines = (char *) di_realloc(multiple_lines, len + buflen + 2);
		memset(multiple_lines + len, '\0', buflen + 2);
		strcat(multiple_lines, " ");
		strcat(multiple_lines, buf);
		len += buflen + 1;
	}
	ungetc(ch, f);
	return multiple_lines;
}

/*
 * Read a control file (or a stanza of a status file) and parse it,
 * filling parsed fields into the package structure
 */
void control_read(FILE *f, struct package_t *p)
{
	char buf[BUFSIZE];
	while (fgets(buf, BUFSIZE, f) && !feof(f))
	{
		buf[strlen(buf)-1] = 0;
		if (*buf == 0)
			return;
		/* these are common to both installed and uninstalled packages */
		else if (strstr(buf, "Package: ") == buf)
		{
			p->package = strdup(buf+9);
		}
		else if (strstr(buf, "Status: ") == buf)
		{
			p->status = status_parse(buf+8);
		}
		else if (strstr(buf, "Depends: ") == buf)
		{
			p->depends = strdup(buf+9);
		}
		else if (strstr(buf, "Provides: ") == buf)
		{
			p->provides = strdup(buf+10);
		}
		else if (strstr(buf, "Description: ") == buf)
		{
			p->description = strdup(buf+13);
			p->long_description = read_block(f);
		}
#ifdef SUPPORTL10N
		else if (strstr(buf, "description-") == buf)
		{
			/* Localized description */
			struct language_description_t *l;
			l = di_malloc(sizeof(struct language_description_t));
			memset(l,'\0',sizeof (struct language_description_t));
			l->next = p->localized_descriptions;
			p->localized_descriptions = l;
			buf[14] = '\0';
			l->language = strdup(buf+12);
			l->description = strdup(buf+16);
			l->long_description = read_block(f);
		}
#endif
		/* This is specific to the Debian Installer. Ifdef? */
		else if (strcasestr(buf, "Installer-Menu-Item: ") == buf)
		{
			p->installer_menu_item = atoi(buf+21);
		}
		else if (strcasestr(buf, "Priority: ") == buf)
		{
			p->priority = strdup(buf + 10);
		}
		else if (strcasestr(buf, "Section: ") == buf)
		{
			p->section = strdup(buf + 9);
		}
		else if (strcasestr(buf, "Installed-Size: ") == buf)
		{
			p->installed_size = strdup(buf + 16);
		}
		else if (strcasestr(buf, "Maintainer: ") == buf)
		{
			p->maintainer = strdup(buf + 12);
		}
		else if (strcasestr(buf, "Version: ") == buf)
		{
			p->version = strdup(buf + 9);
		}
		else if (strcasestr(buf, "Suggests: ") == buf)
		{
			p->suggests = strdup(buf + 10);
		}
		else if (strcasestr(buf, "Recommends: ") == buf)
		{
			p->recommends = strdup(buf + 12);
		}
		else if (strcasestr(buf, "Conffiles: ") == buf)
		{
			p->conffiles = read_block(f);
		}

	}
}

void *status_read(void)
{
	FILE *f;
	void *status = 0;
	struct package_t *m = 0, *p = 0, *t = 0;

	if ((f = fopen(STATUSFILE, "r")) == NULL)
	{
		perror(STATUSFILE);
		return 0;
	}
	if (getenv(UDPKG_QUIET) == NULL)
		printf("(Reading database...)\n");
	while (!feof(f))
	{
		m = (struct package_t *)di_malloc(sizeof(struct package_t));
		memset(m, 0, sizeof(struct package_t));
		control_read(f, m);
		if (m->package)
		{
			/*
			 * If there is an item in the tree by this name,
			 * it must be a virtual package; insert real
			 * package in preference.
			 */
			tdelete(m, &status, package_compare);
			tsearch(m, &status, package_compare);
			if (m->provides)
			{
				/* 
				 * A "Provides" triggers the insertion
				 * of a pseudo package into the status
				 * binary-tree.
				 */
				p = (struct package_t *)di_malloc(sizeof(struct package_t));
				memset(p, 0, sizeof(struct package_t));
				p->package = strdup(m->provides);

				t = *(struct package_t **)tsearch(p, &status, package_compare);
				if (!(t == p))
				{
					di_free(p->package);
					di_free(p);
				}
				else {
					/*
					 * Pseudo package status is the
					 * same as the status of the
					 * package providing it 
					 * FIXME: (not quite right, if 2
					 * packages of different statuses
					 * provide it).
					 */
					t->status = m->status;
				}
			}
		}
		else
		{
			di_free(m);
		}
	}
	fclose(f);
	return status;
}

int status_merge(void *status, struct package_t *pkgs)
{
	FILE *fin, *fout;
	char buf[BUFSIZE];
	struct package_t *pkg = 0, *statpkg = 0;
	struct package_t locpkg;
	int r = 0;

	if ((fin = fopen(STATUSFILE, "r")) == NULL)
	{
		perror(STATUSFILE);
		return 0;
	}
	if ((fout = fopen(STATUSFILE ".new", "w")) == NULL)
	{
		perror(STATUSFILE ".new");
		return 0;
	}
	if (getenv(UDPKG_QUIET) == NULL)
		printf("(Updating database...)\n");
	while (fgets(buf, BUFSIZE, fin) && !feof(fin))
	{
		buf[strlen(buf)-1] = 0; /* trim newline */
		/* If we see a package header, find out if it's a package
		 * that we have processed. if so, we skip that block for
		 * now (write it at the end).
		 *
		 * we also look at packages in the status cache and update
		 * their status fields
		 */
		if (strstr(buf, "Package: ") == buf)
		{
			for (pkg = pkgs; pkg != 0 && strcmp(buf+9,
				pkg->package)!=0; pkg = pkg->next) ;

			locpkg.package = buf+9;
			statpkg = tfind(&locpkg, &status, package_compare);
			
			/* note: statpkg should be non-zero, unless the status
			 * file was changed while we are processing (no locking
			 * is currently done...
			 */
			if (statpkg != 0) statpkg = *(struct package_t **)statpkg;
		}
		if (pkg != 0) continue;

		if (strstr(buf, "Status: ") == buf && statpkg != 0)
		{
			  snprintf(buf, sizeof(buf), "Status: %s",
				 status_print(statpkg->status));
		}
		fputs(buf, fout);
		fputc('\n', fout);
	}

	// Print out packages we processed.
	for (pkg = pkgs; pkg != 0; pkg = pkg->next) {
		fprintf(fout, "Package: %s\nStatus: %s\n", 
			pkg->package, status_print(pkg->status));
		if (pkg->priority)
			fprintf(fout, "Priority: %s\n", pkg->priority);
		if (pkg->section)
			fprintf(fout, "Section: %s\n", pkg->section);
		if (pkg->priority)
			fprintf(fout, "Installed-Size: %s\n", pkg->installed_size);
		if (pkg->maintainer)
			fprintf(fout, "Maintainer: %s\n", pkg->maintainer);
		if (pkg->source)
			fprintf(fout, "Source: %s\n", pkg->source);
		if (pkg->version)
			fprintf(fout, "Version: %s\n", pkg->version);
		if (pkg->pre_depends)
			fprintf(fout, "Pre-Depends: %s\n", pkg->pre_depends);
		if (pkg->depends)
			fprintf(fout, "Depends: %s\n", pkg->depends);
		if (pkg->replaces)
			fprintf(fout, "Replaces: %s\n", pkg->replaces);
		if (pkg->recommends)
			fprintf(fout, "Recommends: %s\n", pkg->recommends);
		if (pkg->suggests)
			fprintf(fout, "Suggests: %s\n", pkg->suggests);
		if (pkg->provides)
			fprintf(fout, "Provides: %s\n", pkg->provides);
		if (pkg->conflicts)
			fprintf(fout, "Conflicts: %s\n", pkg->conflicts);
		if (pkg->conffiles)
			fprintf(fout, "Conffiles:\n %s\n", pkg->conffiles);
		if (pkg->description)
			fprintf(fout, "Description: %s\n%s", pkg->description, pkg->long_description);
#ifdef SUPPORTL10N
		if (pkg->localized_descriptions) {
			struct language_description_t *ld;
			ld = pkg->localized_descriptions;
			while (ld) {
				if (ld->language && ld->description && 
				    ld->long_description) {
					fprintf(fout, "description-%s: %s\n%s", 
						ld->language,
						ld->description, 
						ld->long_description);
				}
				ld = ld->next;
			}
		}
#endif
		if (pkg->installer_menu_item)
			fprintf(fout, "Installer-Menu-Item: %i\n", pkg->installer_menu_item);
		fputc('\n', fout);
	}
	
	fclose(fin);
	fclose(fout);

	r = rename(STATUSFILE, STATUSFILE ".bak");
	if (r == 0) r = rename(STATUSFILE ".new", STATUSFILE);
	return 0;
}
