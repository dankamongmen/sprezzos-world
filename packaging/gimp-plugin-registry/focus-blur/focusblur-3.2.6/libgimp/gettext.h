#ifndef __FOCUSBLUR_GETTEXT_H__
#define __FOCUSBLUR_GETTEXT_H__

#ifdef ENABLE_NLS

#include <glib/gi18n.h>

#define INIT_I18N()     G_STMT_START{                           \
    bindtextdomain (GETTEXT_PACKAGE, gimp_locale_directory ()); \
    bind_textdomain_codeset (GETTEXT_PACKAGE, "UTF-8");         \
    textdomain (GETTEXT_PACKAGE);                               \
}G_STMT_END

#else

#define _(String) (String)
#define N_(String) (String)
#define INIT_I18N() {}

#endif /* ENABLE_NLS */

#endif /* __FOCUSBLUR_GETTEXT_H__ */
