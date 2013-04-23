/* Focus Blur -- blur with focus plug-in.
 * Copyright (C) 2002-2008 Kyoichiro Suda
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

#include <libgimp/gimp.h>

#include "libgimp/stdplugins-intl.h"

#include "gimpplugin.h"
#include "focusblurparam.h"
#include "interface.h"
#include "render.h"


/*---- Prototypes ----*/

static void query       (void);

static void run         (const gchar             *name,
                         gint                     nparams,
                         const GimpParam         *param,
                         gint                    *nreturn_vals,
                         GimpParam              **return_vals);


/*---- Variables ----*/

const GimpPlugInInfo PLUG_IN_INFO =
{
  NULL,  /* init_proc  */
  NULL,  /* quit_proc  */
  query, /* query_proc */
  run,   /* run_proc   */
};


/*---- Functions ----*/

MAIN ()


static void
query (void)
{
  static const GimpParamDef args[] =
  {
    {GIMP_PDB_INT32,    "run_mode",   "Interactive, non-interactive"},
    {GIMP_PDB_IMAGE,    "image",      "Input image"},
    {GIMP_PDB_DRAWABLE, "drawable",   "Input drawable"},
    {GIMP_PDB_INT32,    "model",      "Model of blurring: (0:Flat, 1:Spherical, 2:Gaussian, 3:Ring, 4:Concave, 5:Brush)"},
    {GIMP_PDB_FLOAT,    "radius",     "Radius for blurring: (0.0 < radius))"},
    {GIMP_PDB_FLOAT,    "focus",      "Focal depth: (0.0 <= focus <= 100.0)"},
    {GIMP_PDB_INT32,    "map_id",     "Depth map drawable id: (drawable_ID or -1 unused)"},
    {GIMP_PDB_FLOAT,    "s_radius",   "Radius for shining: (0.0 <= shine, 0 unused, -1 full shined)"},
    {GIMP_PDB_FLOAT,    "threshold",  "Threshold for shining: (0.0 <= threshold <= 100.0 in %, 0 unused)"},
  };

  gimp_install_procedure (PLUG_IN_PROC,
                          N_("Make a out of focus with luminosity and depth, "
                             "like a sight or lenses"),
                          "This plug-in makes a out of focus with luminosity "
                          "and depth, like a sight or lenses. It can be used "
                          "with depth map, depth fakes and shining effect. "
                          "Also it works as a simple and applicable blur.",
                          "Kyoichiro Suda <das atmark dream dot japan>",
                          "Kyoichiro Suda",
                          "2002-2008",
                          N_("_Focus Blur..."),
                          "RGB*, GRAY*",
                          GIMP_PLUGIN,
                          G_N_ELEMENTS (args), 0,
                          args, NULL);

  gimp_plugin_menu_register (PLUG_IN_PROC, "<Image>/Filters/Blur");
#ifdef ENABLE_NLS
  gimp_plugin_domain_register (GETTEXT_PACKAGE, NULL);
#endif
}

static void
run (const gchar         *name,
     gint                 nparams,
     const GimpParam     *param,
     gint                *nreturn_vals,
     GimpParam          **return_vals)
{
  static GimpParam   values[1];
  GimpRunMode        run_mode;
  GimpPDBStatusType  status;
  //gint32           image_ID;
  gint32             drawable_ID;
  FblurParam        *fblur_param;

  /* Get the specified values */
  run_mode    = param[0].data.d_int32;
  //image_ID    = param[1].data.d_image;
  drawable_ID = param[2].data.d_drawable;

#ifdef ENABLE_MP
  if (! g_thread_supported ())
    g_thread_init (NULL);
#endif

  INIT_I18N ();

  /* Check image type */
  if (! gimp_drawable_is_rgb (drawable_ID) &&
      ! gimp_drawable_is_gray (drawable_ID))
    {
      status = GIMP_PDB_EXECUTION_ERROR;
      goto focusblur_exit;
    }

  /* Initialize parameters */
  fblur_param = focusblur_param_new (drawable_ID);
  if (! fblur_param)
    {
      status = GIMP_PDB_EXECUTION_ERROR;
      goto focusblur_exit;
    }

  switch (run_mode)
    {
    case GIMP_RUN_INTERACTIVE:
      /* Possibly retrieve data */
      focusblur_param_restore (fblur_param);

      /* Get information from the dialog */
      if (! focusblur_dialog (fblur_param))
        {
          status = GIMP_PDB_SUCCESS;
          goto focusblur_exit;
        }
      break;

    case GIMP_RUN_NONINTERACTIVE:
      /* Make sure all the arguments are present */
      if (! focusblur_param_set (fblur_param, nparams, param))
        {
          status = GIMP_PDB_CALLING_ERROR;
          goto focusblur_exit;
        }
      break;

    case GIMP_RUN_WITH_LAST_VALS:
      /* Possibly retrieve data */
      if (! focusblur_param_restore (fblur_param))
        {
          status = GIMP_PDB_CALLING_ERROR;
          goto focusblur_exit;
        }
      break;
    }

  if (focusblur_execute (fblur_param, NULL))
    status = GIMP_PDB_SUCCESS;
  else
    status = GIMP_PDB_EXECUTION_ERROR;

  /* If run mode is interactive, flush displays */
  if (run_mode != GIMP_RUN_NONINTERACTIVE)
    gimp_displays_flush ();

  /* Store data */
  if (run_mode == GIMP_RUN_INTERACTIVE)
    focusblur_param_store (fblur_param);

 focusblur_exit:

  if (run_mode == GIMP_RUN_INTERACTIVE)
    focusblur_rc_save_preferences (&(fblur_param->pref));

  focusblur_param_destroy (&fblur_param);
  values[0].type          = GIMP_PDB_STATUS;
  values[0].data.d_status = status;
  *nreturn_vals = G_N_ELEMENTS (values);
  *return_vals  = values;
}
