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

#include <libgimp/gimp.h>

#include "focusblurparam.h"
#include "shine.h"


/*---- Structure ----*/

struct _FblurShineData
{
  /* specified parameters */
  FblurShineType         shine_type;
  gfloat                 shine_threshold;
  gfloat                 shine_level;
  gfloat                 shine_curve;
  /* pre-computed value */

  /* main data */
  gint                   x1, x2, y1, y2;
  gint                   width, height;
  guchar                *data;
};


/*---- Prototypes ----*/

static gint     focusblur_shine_filter_luminosity (guchar       *pixel,
                                                   gint          gate,
                                                   gfloat        scale,
                                                   gfloat        level,
                                                   gfloat        curve);
static gint     focusblur_shine_filter_luminosity_alpha (guchar *pixel,
                                                         gint    gate,
                                                         gfloat  scale,
                                                         gfloat  level,
                                                         gfloat  curve);
static gint     focusblur_shine_filter_saturation (guchar       *pixel,
                                                   gint          gate,
                                                   gfloat        scale,
                                                   gfloat        level,
                                                   gfloat        curve);
static gint     focusblur_shine_filter_saturation_alpha (guchar *pixel,
                                                         gint    gate,
                                                         gfloat  scale,
                                                         gfloat  level,
                                                         gfloat  curve);
static gint     focusblur_shine_filter_gray       (guchar       *pixel,
                                                   gint          gate,
                                                   gfloat        scale,
                                                   gfloat        level,
                                                   gfloat        curve);
static gint     focusblur_shine_filter_gray_alpha  (guchar      *pixel,
                                                   gint          gate,
                                                   gfloat        scale,
                                                   gfloat        level,
                                                   gfloat        curve);
static gint     focusblur_shine_filter_sub        (gfloat        fval,
                                                   gfloat        level,
                                                   gfloat        curve);


/*---- Functions ----*/

gboolean
focusblur_shine_update (FblurShineData  **shine,
                        GimpDrawable     *drawable,
                        FblurStoreParam  *store)
{
  GimpPixelRgn   pr;
  gpointer       p;
  gint           light_width;
  gint           shine_threshold_gate;
  gfloat         shine_threshold_scale;
  gfloat         shine_level_float;
  gfloat         shine_curve_float;
  guchar        *slp, *sp;
  guchar        *dlp, *dp;
  gint           x, y;

  g_return_val_if_fail (drawable != NULL, FALSE);

  if (! focusblur_shine_check_enabled (store))
    return TRUE;

  if (*shine)
    {
      if (store->shine_type == (*shine)->shine_type &&
          store->shine_threshold == (*shine)->shine_threshold &&
          store->shine_level == (*shine)->shine_level &&
          store->shine_curve == (*shine)->shine_curve)
        {
          /* not changed */
          return TRUE;
        }
    }

  else
    {
      *shine = g_new0 (FblurShineData, 1);
      gimp_drawable_mask_bounds (drawable->drawable_id,
                                 &((*shine)->x1), &((*shine)->y1),
                                 &((*shine)->x2), &((*shine)->y2));
      (*shine)->width = (*shine)->x2 - (*shine)->x1;
      (*shine)->height = (*shine)->y2 - (*shine)->y1;
      (*shine)->data = g_new (guchar, (*shine)->width * (*shine)->height);
    }

  /* copy running values */
  (*shine)->shine_type = store->shine_type;
  (*shine)->shine_threshold = store->shine_threshold;
  (*shine)->shine_level = store->shine_level;
  (*shine)->shine_curve = store->shine_curve;

  /* pre-computed value */
      /* none */

  light_width = rintf (255 * (*shine)->shine_threshold / 100.0f);
  shine_threshold_gate = 255 - light_width;
  shine_threshold_scale = (light_width > 0) ? (1.0f / light_width) : 0.0f;
  shine_level_float = (*shine)->shine_level / 100.0f;
  shine_curve_float =
    ((*shine)->shine_curve > 0.0001f) ? (1.0f / (*shine)->shine_curve) : 0.0f;

  gimp_pixel_rgn_init (&pr, drawable, (*shine)->x1, (*shine)->y1,
                       (*shine)->width, (*shine)->height, FALSE, FALSE);
  p = gimp_pixel_rgns_register (1, &pr);

  if (gimp_drawable_is_rgb (drawable->drawable_id))
    {
      switch ((*shine)->shine_type)
        {
        case FBLUR_SHINE_LUMINOSITY:
          if (gimp_drawable_has_alpha (drawable->drawable_id))
            for (; p; p = gimp_pixel_rgns_process (p))
              for (y = pr.h, slp = pr.data,
                     dlp = (*shine)->data + (pr.y - (*shine)->y1)
                     * (*shine)->width + (pr.x - (*shine)->x1);
                   y --; slp += pr.rowstride, dlp += (*shine)->width)
                for (x = pr.w, sp = slp, dp = dlp; x --; sp += pr.bpp, dp ++)
                  *dp = focusblur_shine_filter_luminosity_alpha
                    (sp, shine_threshold_gate, shine_threshold_scale,
                     shine_level_float, shine_curve_float);
          else
            for (; p; p = gimp_pixel_rgns_process (p))
              for (y = pr.h, slp = pr.data,
                     dlp = (*shine)->data + (pr.y - (*shine)->y1)
                     * (*shine)->width + (pr.x - (*shine)->x1);
                   y --; slp += pr.rowstride, dlp += (*shine)->width)
                for (x = pr.w, sp = slp, dp = dlp; x --; sp += pr.bpp, dp ++)
                  *dp = focusblur_shine_filter_luminosity
                    (sp, shine_threshold_gate, shine_threshold_scale,
                     shine_level_float, shine_curve_float);
          break;

        case FBLUR_SHINE_SATURATION:
          if (gimp_drawable_has_alpha (drawable->drawable_id))
            for (; p; p = gimp_pixel_rgns_process (p))
              for (y = pr.h, slp = pr.data,
                     dlp = (*shine)->data + (pr.y - (*shine)->y1)
                     * (*shine)->width + (pr.x - (*shine)->x1);
                   y --; slp += pr.rowstride, dlp += (*shine)->width)
                for (x = pr.w, sp = slp, dp = dlp; x --; sp += pr.bpp, dp ++)
                  *dp = focusblur_shine_filter_saturation_alpha
                    (sp, shine_threshold_gate, shine_threshold_scale,
                     shine_level_float, shine_curve_float);
          else
            for (; p; p = gimp_pixel_rgns_process (p))
              for (y = pr.h, slp = pr.data,
                     dlp = (*shine)->data + (pr.y - (*shine)->y1)
                     * (*shine)->width + (pr.x - (*shine)->x1);
                   y --; slp += pr.rowstride, dlp += (*shine)->width)
                for (x = pr.w, sp = slp, dp = dlp; x --; sp += pr.bpp, dp ++)
                  *dp = focusblur_shine_filter_saturation
                    (sp, shine_threshold_gate, shine_threshold_scale,
                     shine_level_float, shine_curve_float);
          break;

        default:
          g_assert_not_reached ();
        }
    }
  else
    {
      if (gimp_drawable_has_alpha (drawable->drawable_id))
        for (; p; p = gimp_pixel_rgns_process (p))
          for (y = pr.h, slp = pr.data,
                 dlp = (*shine)->data + (pr.y - (*shine)->y1)
                 * (*shine)->width + (pr.x - (*shine)->x1);
               y --; slp += pr.rowstride, dlp += (*shine)->width)
            for (x = pr.w, sp = slp, dp = dlp; x --; sp += pr.bpp, dp ++)
              *dp = focusblur_shine_filter_gray_alpha
                (sp, shine_threshold_gate, shine_threshold_scale,
                 shine_level_float, shine_curve_float);
      else
        for (; p; p = gimp_pixel_rgns_process (p))
          for (y = pr.h, slp = pr.data,
                 dlp = (*shine)->data + (pr.y - (*shine)->y1)
                 * (*shine)->width + (pr.x - (*shine)->x1);
               y --; slp += pr.rowstride, dlp += (*shine)->width)
            for (x = pr.w, sp = slp, dp = dlp; x --; sp += pr.bpp, dp ++)
              *dp = focusblur_shine_filter_gray
                (sp, shine_threshold_gate, shine_threshold_scale,
                 shine_level_float, shine_curve_float);
    }

  return TRUE;
}


void
focusblur_shine_destroy (FblurShineData **shine)
{
  if (*shine)
    {
      if ((*shine)->data)
        g_free ((*shine)->data);

      g_free (*shine);
      *shine = NULL;
    }
}


gint
focusblur_shine_get (FblurShineData     *shine,
                     gint                x,
                     gint                y)
{
  gint  val;

  x -= shine->x1;
  y -= shine->y1;

  g_assert (x >= 0);
  g_assert (x < shine->width);
  g_assert (y >= 0);
  g_assert (y < shine->height);

  val = shine->data[y * shine->width + x];

  return val; 
}


static gint
focusblur_shine_filter_luminosity (guchar       *pixel,
                                   gint          gate,
                                   gfloat        scale,
                                   gfloat        level,
                                   gfloat        curve)
{
  gfloat        fval;
  gint          r, g, b;
  gint          val;

  r = pixel[0];
  g = pixel[1];
  b = pixel[2];

  fval = GIMP_RGB_LUMINANCE ((gfloat) r, (gfloat) g, (gfloat) b);
  fval -= gate;
  fval *= scale;
  fval = CLAMP (fval, 0.0f, 1.0f);
  val = focusblur_shine_filter_sub (fval, level, curve);

  return val;
}


static gint
focusblur_shine_filter_luminosity_alpha (guchar *pixel,
                                         gint    gate,
                                         gfloat  scale,
                                         gfloat  level,
                                         gfloat  curve)
{
  const gfloat  color_fnum = 1.0f / 255.0f;
  gfloat        fval;
  gint          r, g, b;
  gint          val;

  r = pixel[0];
  g = pixel[1];
  b = pixel[2];

  fval = GIMP_RGB_LUMINANCE ((gfloat) r, (gfloat) g, (gfloat) b);
  fval -= gate;
  fval *= scale;
  fval = CLAMP (fval, 0.0f, 1.0f);
  fval *= color_fnum * pixel[3];
  val = focusblur_shine_filter_sub (fval, level, curve);

  return val;
}


static gint
focusblur_shine_filter_saturation (guchar       *pixel,
                                   gint          gate,
                                   gfloat        scale,
                                   gfloat        level,
                                   gfloat        curve)
{
  gfloat        fr, fg, fb;
  gfloat        fval;
  gint          r, g, b;
  gint          val;

  r = pixel[0];
  g = pixel[1];
  b = pixel[2];

  r -= gate;
  g -= gate;
  b -= gate;

  fr = CLAMP0255 (r) * scale;
  fg = CLAMP0255 (g) * scale;
  fb = CLAMP0255 (b) * scale;

  fval = GIMP_RGB_LUMINANCE (fr, fg, fb);
  val = focusblur_shine_filter_sub (fval, level, curve);

  return val;
}


static gint
focusblur_shine_filter_saturation_alpha (guchar *pixel,
                                         gint    gate,
                                         gfloat  scale,
                                         gfloat  level,
                                         gfloat  curve)
{
  const gfloat  color_fnum = 1.0f / 255.0f;
  gfloat        fr, fg, fb;
  gfloat        fval;
  gint          r, g, b;
  gint          val;

  r = pixel[0];
  g = pixel[1];
  b = pixel[2];

  r -= gate;
  g -= gate;
  b -= gate;

  fr = CLAMP0255 (r) * scale;
  fg = CLAMP0255 (g) * scale;
  fb = CLAMP0255 (b) * scale;

  fval = GIMP_RGB_LUMINANCE (fr, fg, fb);
  fval *= color_fnum * pixel[3];
  val = focusblur_shine_filter_sub (fval, level, curve);

  return val;
}


static gint
focusblur_shine_filter_gray (guchar     *pixel,
                             gint        gate,
                             gfloat      scale,
                             gfloat      level,
                             gfloat      curve)
{
  gfloat        fval;
  gint          gray;
  gint          val;

  gray = pixel[0];
  gray -= gate;
  fval = CLAMP0255 (gray) * scale;
  val = focusblur_shine_filter_sub (fval, level, curve);

  return val;
}


static gint
focusblur_shine_filter_gray_alpha (guchar       *pixel,
                                   gint          gate,
                                   gfloat        scale,
                                   gfloat        level,
                                   gfloat        curve)
{
  const gfloat  color_fnum = 1.0f / 255.0f;
  gfloat        fval;
  gint          gray;
  gint          val;

  gray = pixel[0];
  gray -= gate;
  fval = CLAMP0255 (gray) * scale;
  fval *= color_fnum * pixel[1];
  val = focusblur_shine_filter_sub (fval, level, curve);

  return val;
}


static gint
focusblur_shine_filter_sub (gfloat      fval,
                            gfloat      level,
                            gfloat      curve)
{
  gint val;

  /* effect level and curve */
  if (curve > 0.0f)
    {
      fval = powf (fval, curve);
      g_assert (finite (fval));
    }
  else if (fval < 0.99999f)
    return 0;

  fval *= level;

  val = rintf (255.0f * fval);

  return val;
}
