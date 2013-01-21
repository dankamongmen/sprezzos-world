#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include <string.h>
#include <libintl.h>
#include "config.h"

static double
constant(char *name, int arg)
{
	errno = 0;
	if (strEQ(name, "LC_CTYPE")) return LC_CTYPE;
	if (strEQ(name, "LC_NUMERIC")) return LC_NUMERIC;
	if (strEQ(name, "LC_COLLATE")) return LC_COLLATE;
	if (strEQ(name, "LC_MONETARY")) return LC_MONETARY;
	if (strEQ(name, "LC_MESSAGES")) return LC_MESSAGES;
	if (strEQ(name, "LC_ALL")) return LC_ALL;
	errno = EINVAL;
	return 0;
}

#define ANY_MISSING 0

#ifndef HAVE_DGETTEXT
/* if dgettext is not there, neither will
   dcgettext, dngettext and dcngettext be.
   But only deal with dngettext and
   dcngettext if they will not be dealt
   with later because of ngettext */
#define dgettext(a,b) not_here("dgettext")
#define dcgettext(a,b,c) not_here("dcgettext")
#ifdef HAVE_NGETTEXT
#define dngettext(a,b,c,d) not_here("dngettext")
#define dcngettext(a,b,c,d,e) not_here("dcngettext")
#endif
#undef ANY_MISSING
#define ANY_MISSING 1
#endif

#ifndef HAVE_NGETTEXT
#define ngettext(a,b,c) not_here("ngettext")
#define dngettext(a,b,c,d) not_here("dngettext")
#define dncgettext(a,b,c,d,e) not_here("dcngettext")
#undef ANY_MISSING
#define ANY_MISSING 1
#endif

#ifndef HAVE_BIND_TEXTDOMAIN_CODESET
#define bind_textdomain_codeset(a,b) not_here("bind_textdomain_codeset")
#undef ANY_MISSING
#define ANY_MISSING 1
#endif

#if ANY_MISSING
static int
not_here(char *s)
{
	croak("Locale::gettext::%s not implemented on this architecture", s);
	return -1;
}
#endif

MODULE = Locale::gettext	PACKAGE = Locale::gettext

double
constant(name,arg)
	char *		name
	int		arg

char *
gettext(msgid)
	char *		msgid

char *
dcgettext(domainname, msgid, category)
	char *		domainname
	char *		msgid
	int		category

char *
dgettext(domainname, msgid)
	char *		domainname
	char *		msgid

char *
ngettext(msgid, msgid_plural, n)
	char *		msgid
	char *		msgid_plural
	unsigned long	n

char *
dcngettext(domainname, msgid, msgid_plural, n, category)
	char *		domainname
	char *		msgid
	char *		msgid_plural
	unsigned long	n
	int		category

char *
dngettext(domainname, msgid, msgid_plural, n)
	char *		domainname
	char *		msgid
	char *		msgid_plural
	unsigned long	n

char *
textdomain(domain)
	char *		domain

char *
bindtextdomain(domain, dirname = NULL)
	char *		domain
	char *		dirname

char *
bind_textdomain_codeset(domain, codeset = NULL)
	char *		domain
	char *		codeset
