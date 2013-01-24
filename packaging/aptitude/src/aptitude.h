// aptitude.h
//
//  Copyright 2000 Masato Taruishi <taru@debian.org>,
//                 Daniel Burrows <dburrows@debian.org>
//

/** \brief common header
 *
 * 
 *  \file aptitude.h
 */

#ifndef APTITUDE_H
#define APTITUDE_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/* Take care of NLS matters.  */

#ifdef HAVE_LOCALE_H
# include <locale.h>
#endif
#ifndef HAVE_SETLOCALE
inline void setlocale(int, const char *)
{
}
#endif

# include <string.h>

#if ENABLE_NLS
# include <libintl.h>
# include <cwidget/generic/util/transcode.h>
# define _(Text) gettext (Text)
# define W_(Text) cwidget::util::transcode (gettext (Text) )
# define N_(Text) Text

/** Strips everything up to and including the first pipe character
 *  from the translated string.  Translations without a pipe character are unchanged.
 */
#ifdef __GNUG__
__attribute__ ((format_arg(1)))
#endif
inline const char *P_(const char *Text)
{
  const char * const translation = gettext(Text);
  const char * const stripto = strchr(translation, '|');

  if(stripto == NULL)
    return translation;
  else
    return stripto+1;
}

#else
# undef bindtextdomain
# define bindtextdomain(Domain, Directory) /* empty */
# undef textdomain
# define textdomain(Domain) /* empty */
# define _(Text) Text
# define N_(Text) Text
inline const char *P_(const char *Text)
{
  const char * const stripto = strchr(Text, '|');
  return stripto+1;
}
# define gettext(Text) Text
# define dgettext(Domain, Text) Text
#endif


// This is used to store the location of the binary as determined by
// argv[0].
extern const char *argv0;

#endif
