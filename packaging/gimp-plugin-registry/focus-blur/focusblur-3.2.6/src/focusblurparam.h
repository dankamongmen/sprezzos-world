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

#ifndef __FOCUSBLUR_PARAM_H__
#define __FOCUSBLUR_PARAM_H__

#include <glib/gmacros.h>
#include <glib/gtypes.h>
#include <gtk/gtkstyle.h>
#include <libgimp/gimptypes.h>

#include "focusblurtypes.h"
#include "focusblurenums.h"
#include "focusblurrc.h"


G_BEGIN_DECLS


/*---- Structures ----*/

struct _FblurStoreParam
{
  FblurModelType         model_type;
  FblurShineType         shine_type;
  gboolean               enable_depth_map;
  gboolean               enable_depth_precedence;
  gboolean               enable_depth_aaa;
  gboolean               enable_brush_balance;
  gint32                 depth_map_ID;
  gfloat                 focal_depth;
  gfloat                 model_radius;
  gfloat                 model_rotate;
  gfloat                 model_fill;
  gfloat                 model_softness;
  gfloat                 shine_radius;
  gfloat                 shine_threshold;
  gfloat                 shine_level;
  gfloat                 shine_curve;
};

struct _FblurParam
{
  FblurStoreParam        store;
  FblurPreferences       pref;
  GimpDrawable          *drawable;
  FblurSourceImage      *source;
  FblurDiffusionTable   *diffusion;
  FblurFftBuffer        *fft;
  FblurDepthMap         *depth_map;
  FblurShineData        *shine;

#ifdef ENABLE_MP
  GThreadPool           *thread_pool;
  gint                   max_threads;
#endif

  GtkWidget             *widgets[FBLUR_WIDGET_MAX];

  gchar                 *brush_name;
  gint32                 drawable_ID;
};


/*---- Variable ----*/

const FblurStoreParam fblur_init_param;


/*---- Functions ----*/

FblurParam*     focusblur_param_new     (gint32           drawable_ID);
gboolean        focusblur_param_set     (FblurParam      *param,
                                         gint             gimp_nparams,
                                         const GimpParam *gimp_param);
gboolean        focusblur_param_prepare (FblurParam      *param,
                                         FblurQualityType quality);
void            focusblur_param_store   (FblurParam      *param);
gboolean        focusblur_param_restore (FblurParam      *param);
void            focusblur_param_destroy (FblurParam     **param);


G_END_DECLS


#endif /* __FOCUSBLUR_PARAM_H__ */
