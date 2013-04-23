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

#include "focusblurparam.h"
#include "focusblurrc.h"
#include "gimpplugin.h"
#include "source.h"
#include "diffusion.h"
#include "depthmap.h"
#include "shine.h"
#include "interface.h"
#include "render.h"
#ifdef HAVE_FFTW3
#  include "fftblurbuffer.h"
#endif

/*---- Variable ----*/

const FblurStoreParam fblur_init_param = {
  FBLUR_MODEL_FLAT,     /* model of focus */
  FBLUR_SHINE_SATURATION, /* shine type */
  FALSE,                /* use depth map */
  FALSE,                /* use depth precedence */
  FALSE,                /* use depth anti-anti-alias */
  FALSE,                /* use brush balance */
  -1,                   /* depth map id */
  0.0,                  /* focal depth */
  5.0,                  /* radius for blurring */
  0.0,                  /* rotation degrees */
  0.0,                  /* model filling inside */
  0.0,                  /* model softness */
  0.0,                  /* shine flooding radius */
  2.0,                  /* threshold for shine */
  100.0,                /* shine level */
  1.00                  /* shine curve */
};


/*---- Functions ----*/

FblurParam*
focusblur_param_new (gint32     drawable_ID)
{
  FblurParam        *param;

  param = g_new0 (FblurParam, 1);

  if (! param)
    return NULL;

  param->store          = fblur_init_param;

  focusblur_rc_load_preferences (&(param->pref));

  param->drawable_ID    = drawable_ID;
  param->drawable       = gimp_drawable_get (drawable_ID);

  if (! param->drawable)
    {
      g_free (param);
      return NULL;
    }

  param->brush_name = gimp_context_get_brush ();

  return param;
}


gboolean
focusblur_param_set (FblurParam         *param,
                     gint                gimp_nparams,
                     const GimpParam    *gimp_param)
{
  g_assert (param);
  g_assert (gimp_param);

  if (gimp_nparams == 9)
    {
      param->store.model_type =
        CLAMP (gimp_param[3].data.d_int32,
               FBLUR_MODEL_FLAT, FBLUR_MODEL_BRUSH);
      param->store.model_radius =
        CLAMP (gimp_param[4].data.d_float, 0.0, FBLUR_RADIUS_MAX);
      param->store.focal_depth =
        CLAMP (gimp_param[5].data.d_float, 0.0, 100.0);
      param->store.enable_depth_map =
        (gimp_param[6].data.d_int32 != -1) ? TRUE : FALSE;
      param->store.depth_map_ID = gimp_param[6].data.d_int32;

      param->store.shine_radius =
        CLAMP (gimp_param[7].data.d_float, 0.0, FBLUR_RADIUS_MAX);
      param->store.shine_threshold =
        CLAMP (gimp_param[8].data.d_float, 0.0, 100.0);

      /* These parameter is not used with non-interactive */
      param->store.enable_depth_precedence  = FALSE;
      param->store.model_fill               = 0.0;
      param->store.model_softness           = 0.0;
      param->store.shine_level              = 100.0;
      param->store.shine_curve              = 1.0;

      return TRUE;
    }

  else
    return FALSE;
}


gboolean
focusblur_param_prepare (FblurParam       *param,
                         FblurQualityType  quality)
{
  gint   max_threads;

  g_assert (param);

#ifdef ENABLE_MP
    if (param->pref.disable_mp)
      {
        max_threads = 0;
      }
    else
      {
        gchar *str = gimp_gimprc_query ("num-processors");
        gint   num;

        num = g_ascii_strtoll (str, (gchar **) NULL, 10);
        max_threads = CLAMP (num, 1, FBLUR_MAX_NUM_THREADS) - 1;
        g_free (str);
      }

  if (max_threads != param->max_threads)
    {
      param->max_threads = max_threads;
      if (max_threads)
        {
          if (! param->thread_pool)
            param->thread_pool = g_thread_pool_new
              (focusblur_execute_thread, param, max_threads, FALSE, NULL);
          else
            g_thread_pool_set_max_threads
              (param->thread_pool, max_threads, NULL);
        }
    }
#endif

  if (! param->source &&
      ! focusblur_source_update (&(param->source), param->drawable))
    {
      gimp_message (_("Failed to update source buffer."));

      return FALSE;
    }

  if (! focusblur_diffusion_update (&(param->diffusion),
                                    NULL, &(param->store), param->brush_name))
    return FALSE;

  if (! focusblur_depth_map_update (&(param->depth_map),
                                    NULL, &(param->store), quality))
    {
      gimp_message (_("Failed to update depth info."));

      return FALSE;
    }

  if (! focusblur_shine_update (&(param->shine),
                                param->drawable, &(param->store)))
    {
      gimp_message (_("Failed to update shine data."));

      return FALSE;
    }

  return TRUE;
}


void
focusblur_param_store (FblurParam  *param)
{
  const gchar* identifier = PLUG_IN_PROC;
  const gsize  size       = sizeof (FblurStoreParam);

  g_assert (param);

  gimp_procedural_db_set_data (identifier, &(param->store), size);
}


gboolean
focusblur_param_restore (FblurParam     *param)
{
  const gchar* identifier = PLUG_IN_PROC;
  const gsize  size       = sizeof (FblurStoreParam);

  g_assert (param);

  if (gimp_procedural_db_get_data_size (identifier) == size &&
      gimp_procedural_db_get_data (identifier, &(param->store)))

    return TRUE;

  else

    return FALSE;
}


void
focusblur_param_destroy (FblurParam **param)
{
  g_assert (param);

  if (*param)
    {
      if ((*param)->source)
        focusblur_source_destroy (&((*param)->source));

      if ((*param)->diffusion)
        focusblur_diffusion_destroy (&((*param)->diffusion));

#ifdef HAVE_FFTW3
      if ((*param)->fft)
        focusblur_fft_buffer_destroy (&((*param)->fft));
#endif

#ifdef ENABLE_MP
      if ((*param)->thread_pool)
        g_thread_pool_free ((*param)->thread_pool, TRUE, FALSE);
#endif

      if ((*param)->depth_map)
        focusblur_depth_map_destroy (&((*param)->depth_map));

      if ((*param)->shine)
        focusblur_shine_destroy (&((*param)->shine));

      g_free ((*param)->brush_name);

      if ((*param)->drawable)
        gimp_drawable_detach ((*param)->drawable);

      g_free (*param);
      *param = NULL;
    }
}
