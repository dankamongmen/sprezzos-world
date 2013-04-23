/* Focus Blur -- blur with focus plug-in.
 * Copyright (C) 2002-2007 Kyoichiro Suda
 *
 * GIMP - The GNU Image Manipulation Program
 * Copyright (C) 1995 Spencer Kimball and Peter Mattis
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#include "config.h"

#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <glib/gstdio.h>
#include <glib/gprintf.h>
#include <libgimp/gimp.h>

#include "libgimp/stdplugins-intl.h"

#include "focusblur.h"
#include "focusblurrc.h"
#include "focusblurparam.h"


/*---- Variables ----*/

const FblurPreferences fblur_init_preferences = {
  FBLUR_QUALITY_NORMAL, /* rendering quality at result */
  FBLUR_QUALITY_LOW,    /* rendering quality in preview */
  FALSE                 /* disable multi-processors */
};


/****************/
/* read gtkrc.c */
/****************/

static const GScannerConfig fblur_rc_scanner_config =
{
  " \t\r\n",            /* cset_skip_characters */

  G_CSET_a_2_z
  "_"
  G_CSET_A_2_Z,         /* cset_identifier_first */

  G_CSET_a_2_z
  "_-"
  G_CSET_DIGITS
  G_CSET_A_2_Z,         /* cset_identifier_nth */

  ";\n",                /* cpair_comment_single */

  FALSE,                /* case_sensitive */

  FALSE,                /* skip_comment_multi */
  TRUE,                 /* skip_comment_single */
  FALSE,                /* scan_comment_multi */

  TRUE,                 /* scan_identifier */
  FALSE,                /* scan_identifier_1char */
  FALSE,                /* scan_identifier_NULL */

  TRUE,                 /* scan_symbols */
  FALSE,                /* scan_binary */
  FALSE,                /* scan_octal */
  TRUE,                 /* scan_float */
  FALSE,                /* scan_hex */
  FALSE,                /* scan_hex_dollar */
  FALSE,                /* scan_string_sq */
  FALSE,                /* scan_string_dq */

  FALSE,                /* numbers_2_int */
  TRUE,                 /* int_2_float */
  FALSE,                /* identifier_2_string */
  TRUE,                 /* char_2_token */
  TRUE,                 /* symbol_2_token */
  FALSE,                /* scope_0_fallback */
  FALSE,                /* store_int64 */
};


typedef enum {
  FBLUR_RC_TOKEN_BOOLEAN_NO = G_TOKEN_LAST,
  FBLUR_RC_TOKEN_BOOLEAN_YES,

  FBLUR_RC_TOKEN_QUALITY_TYPE_BEST,
  FBLUR_RC_TOKEN_QUALITY_TYPE_NORMAL,
  FBLUR_RC_TOKEN_QUALITY_TYPE_LOW,
  FBLUR_RC_TOKEN_QUALITY_TYPE_DEFECTIVE,

  FBLUR_RC_TOKEN_QUALITY_RENDER,
  FBLUR_RC_TOKEN_QUALITY_PREVIEW,
  FBLUR_RC_TOKEN_DISABLE_MP,
} FblurRcTokenType;


static const struct
{
  gchar *name;
  gint   token;
} symbols[] = {
  /* values */
  { "no",                       FBLUR_RC_TOKEN_BOOLEAN_NO },
  { "yes",                      FBLUR_RC_TOKEN_BOOLEAN_YES },

  { "best",                     FBLUR_RC_TOKEN_QUALITY_TYPE_BEST },
  { "normal",                   FBLUR_RC_TOKEN_QUALITY_TYPE_NORMAL },
  { "low",                      FBLUR_RC_TOKEN_QUALITY_TYPE_LOW },
  { "defective",                FBLUR_RC_TOKEN_QUALITY_TYPE_DEFECTIVE },

  /* accepted parameters */
  { "rendering-quality",        FBLUR_RC_TOKEN_QUALITY_RENDER },
  { "preview-quality",          FBLUR_RC_TOKEN_QUALITY_PREVIEW },
  { "disable-mp",               FBLUR_RC_TOKEN_DISABLE_MP },
};


/*---- Prototypes ----*/

static gint     focusblur_rc_ignore             (GScanner         *scanner);
static gint     focusblur_rc_parse_statement    (GScanner         *scanner,
                                                 FblurPreferences *pref);
static gint     focusblur_rc_get_quality_type   (GScanner         *scanner,
                                                 FblurQualityType *quality);
static gint     focusblur_rc_get_boolean        (GScanner         *scanner,
                                                 gboolean         *bool);

/*---- Parser ----*/

static gint
focusblur_rc_ignore (GScanner *scanner)
{
  gint errors = 0;

  /* jump to end of statement */

  while (g_scanner_get_next_token (scanner) != G_TOKEN_RIGHT_PAREN)
    {
      switch (scanner->token)
        {
        case G_TOKEN_EOF:
          g_scanner_error
            (scanner, "reached to end of file with not closed parentheses.\n");
          return -1;

        default:
          errors ++;
        }
    }

  return errors;
}


static gint
focusblur_rc_get_quality_type (GScanner         *scanner,
                               FblurQualityType *quality)
{
  GTokenType cur, next;
  gint errors = 0;
  gint ret;

  cur = g_scanner_get_next_token (scanner);
  if (cur != FBLUR_RC_TOKEN_QUALITY_TYPE_BEST &&
      cur != FBLUR_RC_TOKEN_QUALITY_TYPE_NORMAL &&
      cur != FBLUR_RC_TOKEN_QUALITY_TYPE_LOW &&
      cur != FBLUR_RC_TOKEN_QUALITY_TYPE_DEFECTIVE)
    {
      g_scanner_error
        (scanner, "chose one of the best, normal, low or defective.");
      errors ++;
    }

  next = g_scanner_get_next_token (scanner);
  if (next != G_TOKEN_RIGHT_PAREN)
    {
      g_scanner_error (scanner, "tow tokens must be in this parentheses.");
      errors ++;
    }

  if (errors)
    {
      ret = focusblur_rc_ignore (scanner);
      if (ret == -1)
        return -1;
      return errors + ret;
    }

  switch (cur)
    {
    case FBLUR_RC_TOKEN_QUALITY_TYPE_BEST:
      *quality = FBLUR_QUALITY_BEST;
      break;
    case FBLUR_RC_TOKEN_QUALITY_TYPE_NORMAL:
      *quality = FBLUR_QUALITY_NORMAL;
      break;
    case FBLUR_RC_TOKEN_QUALITY_TYPE_LOW:
      *quality = FBLUR_QUALITY_LOW;
      break;
    case FBLUR_RC_TOKEN_QUALITY_TYPE_DEFECTIVE:
      *quality = FBLUR_QUALITY_DEFECTIVE;
      break;
    default:
      g_assert_not_reached ();
    }

  return 0;
}


static gint
focusblur_rc_get_boolean (GScanner      *scanner,
                          gboolean      *bool)
{
  GTokenType cur, next;
  gint errors = 0;
  gint ret;

  cur = g_scanner_get_next_token (scanner);
  if (cur != FBLUR_RC_TOKEN_BOOLEAN_YES &&
      cur != FBLUR_RC_TOKEN_BOOLEAN_NO)
    {
      g_scanner_error
        (scanner, "chose yes or no.");
      errors ++;
    }

  next = g_scanner_get_next_token (scanner);
  if (next != G_TOKEN_RIGHT_PAREN)
    {
      g_scanner_error (scanner, "tow tokens must be in this parentheses.");
      errors ++;
    }

  if (errors)
    {
      ret = focusblur_rc_ignore (scanner);
      if (ret == -1)
        return -1;
      return errors + ret;
    }

  switch (cur)
    {
    case FBLUR_RC_TOKEN_BOOLEAN_YES:
      *bool = TRUE;
      break;
    case FBLUR_RC_TOKEN_BOOLEAN_NO:
      *bool = FALSE;
      break;
    default:
      g_assert_not_reached ();
    }

  return 0;
}


static gint
focusblur_rc_parse_statement (GScanner         *scanner,
                              FblurPreferences *pref)
{
  gint errors = 0;
  gint ret = 0;

  while (g_scanner_get_next_token (scanner) != G_TOKEN_EOF)
    {
      switch (scanner->token)
        {
        case G_TOKEN_LEFT_PAREN:
          ret = focusblur_rc_parse_statement (scanner, pref);
          if (ret == -1)
            return -1;
          errors += ret;
          break;
        case G_TOKEN_RIGHT_PAREN:
          return errors;

        case FBLUR_RC_TOKEN_QUALITY_RENDER:
          ret = focusblur_rc_get_quality_type (scanner, &(pref->quality));
          goto fin;

        case FBLUR_RC_TOKEN_QUALITY_PREVIEW:
          ret = focusblur_rc_get_quality_type (scanner,&(pref->quality_preview));
          goto fin;

        case FBLUR_RC_TOKEN_DISABLE_MP:
          ret = focusblur_rc_get_boolean (scanner, &(pref->disable_mp));
          goto fin;

        default:
          g_scanner_unexp_token (scanner, G_TOKEN_IDENTIFIER,
                                 "variable", NULL, NULL, NULL, FALSE);
          errors ++;
          ret = focusblur_rc_ignore (scanner);
          goto fin;
        }
    }

 fin:
  if (ret == -1)
    return -1;
  errors += ret;

  return errors;
}


/*---- Functions ----*/

void
focusblur_rc_load_preferences (FblurPreferences *pref)
{
  GScanner              *scanner = NULL;
  gchar                 *rc_file;
  gint                   fd = -1;
  gint                   errors = 0;
  gint                   i, ret;

  *pref = fblur_init_preferences;

  rc_file = gimp_personal_rc_file ("focusblurrc");
  if (! rc_file)
    goto fin;

  if (! g_file_test (rc_file, G_FILE_TEST_EXISTS) ||
      g_file_test (rc_file, G_FILE_TEST_IS_DIR))
    goto fin;

  fd = g_open (rc_file, O_RDONLY, S_IRUSR);
  if (fd == -1)
    goto fin;

  scanner = g_scanner_new (&fblur_rc_scanner_config);
  g_scanner_input_file (scanner, fd);
  scanner->input_name = rc_file;

  for (i = 0; i < G_N_ELEMENTS (symbols); i ++)
    g_scanner_scope_add_symbol
      (scanner, 0, symbols[i].name, GINT_TO_POINTER (symbols[i].token));

  while (g_scanner_get_next_token (scanner) != G_TOKEN_EOF)
    {
      switch (scanner->token)
        {
        case G_TOKEN_LEFT_PAREN:
          ret = focusblur_rc_parse_statement (scanner, pref);
          break;
        default:
          g_scanner_unexp_token (scanner, G_TOKEN_LEFT_PAREN,
                                 NULL, NULL, NULL, NULL, TRUE);
          ret = -1;
        }
      if (ret == -1)
        break;
      errors += ret;
    }

  if (errors)
    {
      gimp_message (_("Resource file is invalid."));

      *pref = fblur_init_preferences;
    }

 fin:

  if (scanner)
    g_scanner_destroy (scanner);

  if (fd != -1)
    close (fd);

  if (rc_file)
    g_free (rc_file);
}


void
focusblur_rc_save_preferences (FblurPreferences *pref)
{
  const gchar const *fblur_quality_type_names[] = {
    "best", "normal", "low", "defective"
  };

  FILE          *fp = NULL;
  gchar         *rc_file;

  rc_file = gimp_personal_rc_file ("focusblurrc");
  if (! rc_file)
    goto fin;

  if (g_file_test (rc_file, G_FILE_TEST_IS_DIR))
    goto fin;

  fp = g_fopen (rc_file, "w");
  if (! fp)
    goto fin;

  g_fprintf (fp, "; Focus Blur plug-in resource file\n");

  /* store only different values */

  if (pref->quality != fblur_init_preferences.quality)
    g_fprintf (fp, "(rendering-quality %s)\n",
               fblur_quality_type_names[pref->quality]);

  if (pref->quality_preview != fblur_init_preferences.quality_preview)
    g_fprintf (fp, "(preview-quality %s)\n",
               fblur_quality_type_names[pref->quality_preview]);

  if (pref->disable_mp != fblur_init_preferences.disable_mp)
    g_fprintf (fp, "(disable-mp %s)\n", pref->disable_mp ? "yes" : "no");

 fin:

  if (fp)
    fclose (fp);

  if (rc_file)
    g_free (rc_file);
}
