#ifndef NODEBUG
#include "common.h"
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#ifdef SYSLOG_LOGGING
#include <syslog.h>
#endif

void debug_printf(int level, const char *fmt, ...)
{
	static int loglevel = -1;
	static FILE *fp = NULL;
	va_list ap;

	if (loglevel < 0)
	{
		const char *loglevel_env = getenv("DEBCONF_DEBUG");
		if (loglevel_env) {
			/* Allow DEBCONF_DEBUG=. to match everything, to
			 * make it easier to debug cdebconf and debconf
			 * simultaneously.
			 */
			if (strcmp(loglevel_env, ".") == 0)
				loglevel = INFO_VERBOSE;
			else if (strcmp(loglevel_env, "developer") == 0)
				loglevel = INFO_DEBUG;
			else
				loglevel = atoi(loglevel_env);
		} else
			loglevel = 0;

		if (getenv("DEBCONF_DEBUGFILE") == NULL ||
		    (fp = fopen(getenv("DEBCONF_DEBUGFILE"), "a")) == NULL)
			fp = stderr;
	}

	if (level <= loglevel)
	{
#ifndef SYSLOG_LOGGING
		va_start(ap, fmt);
		vfprintf(fp, fmt, ap);
		fputc('\n', fp);
		va_end(ap);

		fflush(fp);
#else
		va_start(ap, fmt);
		vsyslog(LOG_USER | LOG_DEBUG, fmt, ap);
		va_end(ap);
#endif
	}
}
#endif /* DEBUG */
