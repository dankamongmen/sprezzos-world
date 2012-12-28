#include "debconfclient.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <stdarg.h>

#define DBC_DELETE(x) do { free(x); x = NULL; } while (0)

static int debconfclient_handle_rsp(struct debconfclient *client)
{
	char buf[2048];
	char *v;

	fgets(buf, sizeof(buf), stdin);
	if (strlen(buf) > 0)
	{
		if (buf[strlen(buf)-1] == '\n')
			buf[strlen(buf)-1] = '\0';
		/* strip off the return code */
		strtok(buf, " \t\n");
		DBC_DELETE(client->value);
		v = strtok(NULL, "\n");
		if (v == NULL)
			client->value = strdup("");
		else
			client->value = strdup(v);
		return atoi(buf);
	}
	else
	{
		/*
		 * Nothing was entered; never really happens except during
		 * debugging.
		 */
		DBC_DELETE(client->value);
		client->value = strdup("");
		return 0;
	}
}

static int debconfclient_command(struct debconfclient *client,
	const char *command, ...)
{
	va_list ap;
	char *c;

	fputs(command, client->out);
	va_start(ap, command);
	while ((c = va_arg(ap, char *)) != NULL)
	{
		fputs(" ", client->out);
		fputs(c, client->out);
	}
	va_end(ap);
	fputs("\n", client->out);
	fflush(client->out); /* make sure debconf sees it to prevent deadlock */

	return debconfclient_handle_rsp(client);
}

static int debconfclient_commandf(struct debconfclient *client,
	const char *cmd, ...)
{
	va_list ap;

	va_start(ap, cmd);
	vfprintf(client->out, cmd, ap);
	va_end(ap);
	fprintf(client->out, "\n");
	fflush(client->out);

	return debconfclient_handle_rsp(client);
}

static char *debconfclient_ret(struct debconfclient *client)
{
	return client->value;
}

void debconfclient_init(void) __attribute__ ((constructor));
void debconfclient_init(void)
{
}

struct debconfclient *debconfclient_new(void)
{
	struct debconfclient *client = (struct debconfclient *)
		malloc(sizeof(struct debconfclient));
	memset(client, 0, sizeof(struct debconfclient));

	if (getenv("DEBCONF_REDIR") == NULL)
	{
		dup2(DEBCONF_OLD_STDOUT_FD, 1);
		setenv("DEBCONF_REDIR", "1", 1);
	}

	client->command = debconfclient_command;
	client->commandf = debconfclient_commandf;
	client->ret = debconfclient_ret;
	client->out = fdopen(3, "a");
	/* If we aren't connected to cdebconf, try talking to stdout. It
	 * might be (Perl) debconf, or a developer might just want to talk
	 * debconf protocol to us by hand. Either way, it's better than
	 * segfaulting.
	 */
	if (!client->out)
		client->out = stdout;

	return client;
}

void debconfclient_delete(struct debconfclient *client)
{
	DBC_DELETE(client->value);
	DBC_DELETE(client);
}
