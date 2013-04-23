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
#include <math.h>
#include <string.h>

#include "libgimp/stdplugins-intl.h"

#include "brush.h"


/*---- Structures ----*/

struct _FblurBrush
{
  /* parameter */
  gchar                 *name;
  gboolean               balance;

  /* computed */
  gfloat                 original_density;
  gfloat                 radius;
  gfloat                 center_x;
  gfloat                 center_y;

  /* temporary */
  gfloat                 cache_rotate;
  gfloat                 cache_rot_sin;
  gfloat                 cache_rot_cos;

  /* constant */
  gint                   width;
  gint                   height;
  gsize                  length;
  guint8                *data;
};


/*---- Prototypes ----*/

static void     focusblur_brush_init_density    (FblurBrush     *brush);
static void     focusblur_brush_init_balance    (FblurBrush     *brush,
                                                 gboolean        balance);
static void     focusblur_brush_init_radius     (FblurBrush     *brush);
static gfloat   focusblur_brush_render_oversamp (gfloat         *dlp,
                                                 gsize           rowstride,
                                                 gint            width,
                                                 FblurBrush     *brush,
                                                 gfloat          factor,
                                                 gfloat          fx,
                                                 gfloat          fx_incx,
                                                 gfloat          fx_incy,
                                                 gfloat          fy,
                                                 gfloat          fy_incx,
                                                 gfloat          fy_incy);
static gfloat   focusblur_brush_render_biliner  (gfloat         *dlp,
                                                 gsize           rowstride,
                                                 gint            width,
                                                 FblurBrush     *brush,
                                                 gfloat          factor,
                                                 gfloat          fx,
                                                 gfloat          fx_incx,
                                                 gfloat          fx_incy,
                                                 gfloat          fy,
                                                 gfloat          fy_incx,
                                                 gfloat          fy_incy);


static inline guint8
focusblur_brush_get_pixel (FblurBrush   *brush,
                           gint          x,
                           gint          y)
{
  if (x < 0 || x >= brush->width ||
      y < 0 || y >= brush->height)
    return 0;

  return brush->data[brush->width * y + x];
}


/*---- Functions ----*/

FblurBrush*
focusblur_brush_new (gchar      *brush_name,
                     gboolean    balance)
{
  FblurBrush    *brush;
  gboolean       ret;
  gint           width, height;
  gint           mask_bpp, num_mask_bytes;
  gint           color_bpp, num_color_bytes;
  guint8        *mask_bytes, *color_bytes;

  ret = gimp_brush_get_pixels (brush_name, &width, &height,
                               &mask_bpp,  &num_mask_bytes,  &mask_bytes,
                               &color_bpp, &num_color_bytes, &color_bytes);
  if (! ret)
    {
      gimp_message (_("gimp_brush_get_pixels() is failed."));
      g_free (mask_bytes);
      return NULL;
    }

  g_assert (mask_bpp == 1); /* other bpp is not supported. */

  /* color is not supported. */
  if (color_bytes)
    {
      gfloat             tval = 1.0f / 255.0f / color_bpp;
      guint8            *mp = mask_bytes;
      guint8            *cp = color_bytes;
      gint               i, b, cval;

      for (i = num_mask_bytes; i --; mp ++, cp += color_bpp)
        if (*mp)
          {
            cval = 0;
            for (b = 0; b < color_bpp; b ++)
              cval += cp[b];
            *mp = rint (cval * *mp * tval);
          }

      g_free (color_bytes);
    }

  brush                 = g_new0 (FblurBrush, 1);
  brush->cache_rotate   = -1; /* not cached */
  brush->name           = g_strdup (brush_name);
  brush->width          = width;
  brush->height         = height;
  brush->length         = num_mask_bytes;
  brush->data           = mask_bytes;

  focusblur_brush_init_density (brush);

  if (brush->original_density <= 0.0f)
    {
      focusblur_brush_destroy (&brush);
      return NULL;
    }

  focusblur_brush_init_balance (brush, balance);
  focusblur_brush_init_radius (brush);

  return brush;
}


void
focusblur_brush_destroy (FblurBrush **brush)
{
  g_return_if_fail (brush != NULL);

  if (*brush)
    {
      g_free ((*brush)->name);
      g_free ((*brush)->data);
      g_free (*brush);
      *brush = NULL;
    }
}


gboolean
focusblur_brush_name_is (FblurBrush     *brush,
                         gchar          *brush_name)
{
  g_return_val_if_fail (brush != NULL, FALSE);

  return strcmp (brush->name, brush_name) == 0;
}


/* returns whether diffusion table must be update or not */
gboolean
focusblur_brush_update_balance (FblurBrush      *brush,
                                gboolean         balance)
{
  gfloat tmp_x, tmp_y;

  g_return_val_if_fail (brush != NULL, TRUE);

  if (brush->balance == balance)
    return FALSE;

  tmp_x = brush->center_x;
  tmp_y = brush->center_y;

  focusblur_brush_init_balance (brush, balance);

  if (brush->center_x == tmp_x &&
      brush->center_y == tmp_y)
    return FALSE;

  focusblur_brush_init_radius (brush);

  return TRUE;
}


static void
focusblur_brush_init_density (FblurBrush *brush)
{
  const gfloat   color_fnum = 1.0f / 255.0f;
  guint8        *dp = brush->data;
  gint           i = brush->length;
  float          sum = 0.0f;

  while (i --)
    sum += *dp ++;

  brush->original_density = sum * color_fnum;
}


static void
focusblur_brush_init_balance (FblurBrush *brush,
                              gboolean    balance)
{
  const gfloat   color_fnum = 1.0f / 255.0f;
  guint8        *dp, *dlp;
  gint           x, y;
  gfloat         sum, sumb, half;

  brush->balance = balance;

  if (! balance)
    {
      brush->center_x = (gfloat) (brush->width  - 1) / 2.0f;
      brush->center_y = (gfloat) (brush->height - 1) / 2.0f;

      return;
    }

  /* balanced value */
  half = brush->original_density / color_fnum / 2.0f;

  /* search Y */
  sum = sumb = 0.0f;
  for (y = 0, dp = brush->data; y < brush->height; y ++)
    for (x = 0; x < brush->width; x ++, dp ++)
      {
        sum += *dp;
        if (sum >= half)
          {
            brush->center_y = y + (half - sumb) / (sum - sumb) - 0.5f;
            goto break_y;
          }
        sumb = sum;
      }
 break_y:

  /* search X */
  sum = sumb = 0.0f;
  for (x = 0, dlp = brush->data; x < brush->width; x ++, dlp ++)
    for (y = 0, dp = dlp; y < brush->height; y ++, dp += brush->width)
      {
        sum += *dp;
        if (sum >= half)
          {
            brush->center_x = x + (half - sumb) / (sum - sumb) - 0.5f;
            goto break_x;
          }
        sumb = sum;
      }
 break_x:

  return;
}


static void
focusblur_brush_init_radius (FblurBrush *brush)
{
  guint8        *dp;
  gint           x, y;
  gfloat         max, radius;

  max = 0.0f;
  for (y = 0, dp = brush->data; y < brush->height; y ++)
    for (x = 0; x < brush->width; x ++, dp ++)
      {
        if (*dp)
          {
            radius = hypotf (x - brush->center_x, y - brush->center_y);
            //radius -= (gfloat) *dp / 255.0f; /* counter to anti-alias */
            if (radius > max)
              max = radius;
          }
      }

  brush->radius = max;
}


gfloat
focusblur_brush_make_density (FblurBrush        *brush,
                              gfloat             radius)
{
  gfloat         fval;
  gfloat         factor;

  factor = (1.0f + 2 * radius) / (1.0f + 2 * brush->radius);
  fval = brush->original_density * factor * factor;
  /* it will make difference. */

  g_return_val_if_fail (fval > 1.0f, 1.0f);

  return fval;
}


gfloat
focusblur_brush_render (FblurBrush      *brush,
                        gfloat           radius,
                        gfloat           rotate,
                        gsize            rowstride,
                        gfloat          *centerp)
{
  gfloat        *cornerp;
  gint           range, width;
  gfloat         density;
  gfloat         factor, radian;
  gfloat         fx, fx_incx, fx_incy;
  gfloat         fy, fy_incx, fy_incy;
  gfloat         rot_sin, rot_cos;
  gfloat         tran_cos, tran_sin;

  g_assert (brush != NULL);

  range   = ceilf (radius);
  cornerp = centerp - range * rowstride - range;
  factor  = (1.0f + 2 * brush->radius) / (1.0f + 2 * radius);

  /* it is computed? */
  if (brush->cache_rotate == rotate)
    {
      rot_sin = brush->cache_rot_sin;
      rot_cos = brush->cache_rot_cos;
    }
  else
    {
      /* 1.0 for inverting x and y */
      radian = (rotate / 180.0f + 1.0f) * G_PI;

      rot_sin = brush->cache_rot_sin = sinf (radian);
      rot_cos = brush->cache_rot_cos = cosf (radian);
      brush->cache_rotate = rotate;
    }

  /* first coordinates and incremental value */
  tran_sin = factor * rot_sin;
  tran_cos = factor * rot_cos;

  fx = brush->center_x - range * (tran_cos + tran_sin);
  fy = brush->center_y - range * (tran_cos - tran_sin);
  fx_incx = tran_cos;
  fy_incx = - tran_sin;
  width = 1 + 2 * range;
  fx_incy = tran_sin - tran_cos * width;
  fy_incy = tran_cos + tran_sin * width;

  if (factor > 1.0)
    density = focusblur_brush_render_oversamp
      (cornerp, rowstride, width, brush, factor,
       fx, fx_incx, fx_incy, fy, fy_incx, fy_incy);
  else
    density = focusblur_brush_render_biliner
      (cornerp, rowstride, width, brush, factor,
       fx, fx_incx, fx_incy, fy, fy_incx, fy_incy);

  return density;
}


static gfloat
focusblur_brush_render_oversamp (gfloat         *dlp,
                                 gsize           rowstride,
                                 gint            width,
                                 FblurBrush     *brush,
                                 gfloat          factor,
                                 gfloat          fx,
                                 gfloat          fx_incx,
                                 gfloat          fx_incy,
                                 gfloat          fy,
                                 gfloat          fy_incx,
                                 gfloat          fy_incy)
{
  const gfloat   color_fnum = 1.0f / 255.0f;
  gfloat         density = 0.0f;
  gfloat        *dp;

  gint           rx, ry, sx, sy;
  gfloat         tran_sin, tran_cos;

  gint           div;
  gfloat         div_fac;
  gfloat         val, sum;
  gfloat         bit_sin, bit_cos;
  gfloat         bx, by;
  gfloat         bx_incx, by_incx;
  gfloat         bx_incy, by_incy;
  gfloat         bx_d, by_d;

  /* divided coordinates and first offset  */
  div = ((gint) floorf (factor)) * 2 + 1;
  if (div > 15)
    div = 15;

  div_fac = 1.0 / (div * div);

  tran_sin = - fy_incx; 
  tran_cos = fx_incx; 

  bit_sin = tran_sin / div;
  bit_cos = tran_cos / div;

  bx_incx = bit_cos;
  by_incx = - bit_sin;
  bx_incy = bit_sin - tran_cos;
  by_incy = bit_cos + tran_sin;
  bx_d = (bit_cos + bit_sin) * (div / 2);
  by_d = (bit_cos - bit_sin) * (div / 2);

  /* run on distribution */
  for (ry = width; ry --; dlp += rowstride, fx += fx_incy, fy += fy_incy)
    for (rx = width, dp = dlp; rx --; dp ++, fx += fx_incx, fy += fy_incx)
      {
        /* clip */
        if (fx + factor < 0.0f || fx - factor > (brush->width - 1) ||
            fy + factor < 0.0f || fy - factor > (brush->height - 1))
          continue;

        bx = fx - bx_d;
        by = fy - by_d;
        sum = 0.0f;

        /* run more minutely */
        for (sy = div; sy --; bx += bx_incy, by += by_incy)
          for (sx = div; sx --; bx += bx_incx, by += by_incx)
            sum += focusblur_brush_get_pixel (brush, rintf (bx), rintf (by));
        val = sum * div_fac;

        density += *dp = val * color_fnum;
      }

  g_assert (density > 0.0f);
  return density;
}


static gfloat
focusblur_brush_render_biliner (gfloat          *dlp,
                                gsize            rowstride,
                                gint             width,
                                FblurBrush      *brush,
                                gfloat           factor,
                                gfloat           fx,
                                gfloat           fx_incx,
                                gfloat           fx_incy,
                                gfloat           fy,
                                gfloat           fy_incx,
                                gfloat           fy_incy)
{
  const gfloat   color_fnum = 1.0f / 255.0f;
  gfloat         density = 0.0f;
  gfloat        *dp;

  gint           rx, ry, sx, sy;

  gfloat         balx, baly;
  gfloat         val, val1, val2;
  guint8         c[4];

  /* run on distribution */
  for (ry = width; ry --; dlp += rowstride, fx += fx_incy, fy += fy_incy)
    for (rx = width, dp = dlp; rx --; dp ++, fx += fx_incx, fy += fy_incx)
      {
        /* clip it but leaves 1px to interpolate */
        if (fx <= -1.0f || fx >= brush->width ||
            fy <= -1.0f || fy >= brush->height)
          continue;

        sx = floorf (fx);
        sy = floorf (fy);
        balx = fx - sx;
        baly = fy - sy;

        c[0] = focusblur_brush_get_pixel (brush, sx,     sy);
        c[1] = focusblur_brush_get_pixel (brush, sx + 1, sy);
        c[2] = focusblur_brush_get_pixel (brush, sx,     sy + 1);
        c[3] = focusblur_brush_get_pixel (brush, sx + 1, sy + 1);

        val1 = c[0] + balx * (c[1] - c[0]);
        val2 = c[2] + balx * (c[3] - c[2]);
        val  = val1 + baly * (val2 - val1);

        density += *dp = val * color_fnum;
      }

  g_assert (density > 0.0f);
  return density;
}
