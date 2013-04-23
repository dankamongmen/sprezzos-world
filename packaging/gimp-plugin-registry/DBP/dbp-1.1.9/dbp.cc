/* DBP (Dave's Batch Processor)
 * A simple batch processor for the GIMP
 * Copyright (C) 2001 - 2008 David Hodson
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

/*
 * Revision history
 * 0.1 - original Dec 2001
 * 0.2 - removed some C++(C99?)-isms, to compile under gcc 2.95 (I hope)
 * 0.3 - fixed crash on OK, close after processing
 *       rearranged processing controls
 *       many minor tidyups
 *       (unreleased)
 * 0.4 - converted to C++, I'm sick of working in C
 * 0.5 - added test mode, seems to be working OK
 * 0.5.1 - minor fix(?) to getting directory from multiple file selector.
 * 0.5.2 - fix some show/hide problems in output page.
 * 0.5.3 - add flatten, add cineon output format, minor gui tweaks.
 * 0.5.4 - add tiff output
 * 0.5.5 - made output formats generic, added option menu gui
 * 0.5.6 - fixes for gimp-1.3
 * 0.5.7 - fixes for gimp-1.3 past version 20...
 * 1.0 - tidied up, release for Gimp 2.x
 * 1.0.1 - changed gui of output formats to closer match Gimp versions
 *       - pre-test for output files
 *       - warning if no file to test
 * 1.0.2 - added auto levels to colour page
 * 1.0.3 - added b/w conversion
 *       - reuse same display
 * 1.0.4 - removed unused thread code, to compile OK under Windows
 *       - added Gimp 2.2 specific code to hide progress popup
 * 1.0.5 - added image rotation, indexed gif output
 * 1.1   - removed all Gimp and Gtk deprecated features
 * 1.1.1 - fixed Windows path bug
 * 1.1.2 - moved code from dbp.cc to op and gui
 *       - fixed flatten on resize with padding
 *       - fixed sharpen parameter type error in Gimp > 2.2.9 or 2.3.5
 *       - changed sharpen radius range to match Gimp
 *       - changed threshold control to match Gimp
 *       - added saturation to colour controls
 *       - changed file selector Cancel to Close
 *       - (probably) fixed "missing sentinel" warning on gcc 4.0
 * 1.1.3 - fixed another missing sentinel
 *       - removed -DGIMP_DISABLE_DEPRECATED from Makefile, to compile under 2.3.8(?)
 * 1.1.4 - added PAT format by request
 * 1.1.5 - fixed loading bug that halted DBP after processing a few images
 * 1.1.6 - added convert to greyscale / convert to indexed
 * 1.1.7 - fixed bug in colour controls
 *       - use new progress api for 2.3 and above
 * 1.1.8 - started to add gettext stuff
 *       - removed another gtk_file_selector, fixed breakage on gtk-2.6
 *       - added invert to recolour functions
 * 1.1.9 - fix longstanding deprecated Gtk problems
 */

#include "gui.h"

#include <libintl.h>
#define _(String) gettext(String)
#define gettext_noop(String) String
#define N_(String) gettext_noop(String)
#define GETTEXT_PACKAGE "gimp-dbp"
#define LOCALEDIR "."

#define PLUGIN_NAME "extension_dbp"

extern "C" {
static void query();
static void run(const gchar*, gint, const GimpParam*, gint*, GimpParam**);
static void dbp();

GimpPlugInInfo PLUG_IN_INFO = { NULL, NULL, query, run };
}

MAIN()

static void
query() {

  // ORLY?
  static gchar* mode = g_strdup("run_mode");
  static gchar* desc = g_strdup("Interactive or non-interactive");
  static GimpParamDef args[] = {
    { GIMP_PDB_INT32, mode, desc }
  };
  static GimpParamDef* return_vals = 0;
  static int n_args = sizeof(args) / sizeof(args[0]);
  static int n_return_vals = 0;

  gimp_install_procedure(PLUGIN_NAME,
      "Batch processes multiple images",
      "DBP performs simple batch processing of images.",
      "David Hodson <hodsond@acm.org>",
      "2001 - 2008 David Hodson",
      "16 Dec 2008 (Version 1.1.9)",
      "<Toolbox>/Xtns/Batch Process...",
      NULL, GIMP_EXTENSION,
      n_args, n_return_vals, args, return_vals);
}

static void
run (const gchar* name,
     gint nparams,
     const GimpParam* param,
     gint* nreturn_vals,
     GimpParam** return_vals)
{
  GimpParam values;

  *return_vals = &values;
  *nreturn_vals = 1;
  values.type = GIMP_PDB_STATUS;
  values.data.d_status = GIMP_PDB_SUCCESS;

  GimpRunMode run_mode = (GimpRunMode)param[0].data.d_int32;
  switch (run_mode) {
  case GIMP_RUN_INTERACTIVE:
  case GIMP_RUN_WITH_LAST_VALS:
    dbp();
    break;

  case GIMP_RUN_NONINTERACTIVE:
  default:
    values.data.d_status = GIMP_PDB_CALLING_ERROR;
    break;
  }
}

using namespace Dbp;

void
dbp() {

  /* g_magic_incantations() */
  int argc = 1;
  gchar** argv = g_new(gchar*, 1);
  argv[0] = g_strdup("dbp");
  gtk_init(&argc, &argv);
  gtk_rc_parse(gimp_gtkrc());

  setlocale(LC_ALL, "");
  bindtextdomain(GETTEXT_PACKAGE, LOCALEDIR);
  textdomain(GETTEXT_PACKAGE);

#if 0
  /*  Initialize i18n support  */
  bindtextdomain(GETTEXT_PACKAGE, LOCALEDIR);
#ifdef HAVE_BIND_TEXTDOMAIN_CODESET
  bind_textdomain_codeset(GETTEXT_PACKAGE, "UTF-8");
#endif
  textdomain(GETTEXT_PACKAGE);
#endif

  DbpData dbpData;
  DbpGui dbpGui(dbpData, PLUGIN_NAME);
  dbpGui.build();
  gimp_extension_ack();
  dbpGui.run();
}
