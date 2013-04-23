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

#ifndef __FOCUSBLUR_DIFFUSION_H__
#define __FOCUSBLUR_DIFFUSION_H__


#include <glib/gtypes.h>

#include "focusblur.h"
#include "focusblurtypes.h"
#include "focusblurenums.h"

G_BEGIN_DECLS


/*---- Structures ----*/

struct _FblurDiffusionTable
{
  /* specified parameters */
  FblurModelType         model_type;
  gfloat                 model_radius;
  gfloat                 model_rotate;
  gfloat                 model_fill;
  gfloat                 model_softness;
  gfloat                 shine_radius;
  /* sub value */
  FblurBrush            *brush;
  /* pre-computed values */
  gfloat                 model_fill_float;
  gint                   model_radius_int;
  /* main data */
  gsize                  centeroffset;
  gsize                  rowstride;
  gsize                  blocksize;
  gfloat                *distribution[FBLUR_DIFFUSION_NTABLES];
  gfloat                 density[FBLUR_DIFFUSION_NTABLES];
  gfloat                 density_max;
};


/*---- Functions ----*/

gboolean focusblur_diffusion_update     (FblurDiffusionTable    **diffusion,
                                         FblurFftBuffer          *fft,
                                         FblurStoreParam         *store,
                                         gchar                   *brush_name);
void    focusblur_diffusion_destroy     (FblurDiffusionTable    **diffusion);
gfloat  focusblur_diffusion_get         (FblurDiffusionTable     *diffusion,
                                         gint                     level,
                                         gint                     pos_x,
                                         gint                     pos_y,
                                         gint                     x,
                                         gint                     y);
gfloat  focusblur_diffusion_getf        (FblurDiffusionTable     *diffusion,
                                         gint                     level,
                                         gfloat                   fx,
                                         gfloat                   fy);
gfloat  focusblur_diffusion_get_shine   (FblurDiffusionTable     *diffusion,
                                         gint                     depth_level,
                                         gint                     shine_level);

G_END_DECLS

#endif /* __FOCUSBLUR_DIFFUSION_H__ */
