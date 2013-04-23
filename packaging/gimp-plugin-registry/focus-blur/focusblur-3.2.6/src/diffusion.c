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

#include <glib.h>
#include <math.h>
#include <stdlib.h>

#include "focusblurparam.h"
#include "diffusion.h"
#include "brush.h"
#ifdef HAVE_FFTW3
#  include "fftblurbuffer.h"
#endif

#ifndef SQR
#  define SQR(a) (a*a)
#endif


/*---- Types ----*/

typedef gfloat
(*FocusblurDiffusionModelFunc)  (FblurDiffusionTable    *diffusion,
                                 gfloat                  radius,
                                 gfloat                  difference);


/*---- Static values ----*/

const int        table_length = FBLUR_RADIUS_MAX + 1;
static gfloat   *difference_table = NULL;


/*---- Prototypes ----*/

static void     focusblur_diffusion_difference_table_init (void);
static void     focusblur_diffusion_make (FblurDiffusionTable   *diffusion,
                                          gint                   level,
                                          gint                   num);
static gfloat   focusblur_diffusion_make_density
                                        (FblurDiffusionTable    *diffusion,
                                         gfloat                  radius);
static void     focusblur_diffusion_blur (FblurDiffusionTable   *diffusion,
                                          gfloat                *data,
                                          gfloat                 model_radius,
                                          gint                   level);
static gfloat   focusblur_diffusion_model_flat
                                        (FblurDiffusionTable    *diffusion,
                                         gfloat                  radius,
                                         gfloat                 difference);
static gfloat   focusblur_diffusion_model_spherical
                                        (FblurDiffusionTable    *diffusion,
                                         gfloat                  radius,
                                         gfloat                 difference);
static gfloat   focusblur_diffusion_model_gaussian
                                        (FblurDiffusionTable    *diffusion,
                                         gfloat                  radius,
                                         gfloat                 difference);
static gfloat   focusblur_diffusion_model_ring
                                        (FblurDiffusionTable    *diffusion,
                                         gfloat                  radius,
                                         gfloat                 difference);
static gfloat   focusblur_diffusion_model_concave
                                        (FblurDiffusionTable    *diffusion,
                                         gfloat                  radius,
                                         gfloat                 difference);

/*---- Functions ----*/

static void
focusblur_diffusion_difference_table_init (void)
{
  gfloat        *tp, *tp2;
  gint           x, y;

  if (difference_table)
    return;

  difference_table = g_new (gfloat, table_length * table_length);

  tp = difference_table;
  for (y = 0; y < table_length; y ++)
    {
      for (x = 0, tp2 = difference_table + y; x < y; x ++, tp2 += table_length)
        *tp ++ = *tp2;
      for (; x < table_length; x ++)
        *tp ++ = hypotf (x, y);
    }
}


gboolean
focusblur_diffusion_update (FblurDiffusionTable **diffusion,
                            FblurFftBuffer       *fft,
                            FblurStoreParam      *store,
                            gchar                *brush_name)
{
  FblurBrush    *brush_tmp = NULL;
  gsize          rowstride, centeroffset;
  gint           radius_int, width;
  gfloat         radius;
  gfloat         density_max_bak = 0.0f;

  if (! difference_table)
    focusblur_diffusion_difference_table_init ();
 
  if (*diffusion)
    {
      gboolean same_source, same_density, same_diffusion;
      if (store->model_type == FBLUR_MODEL_BRUSH)
        {
          same_source = (*diffusion)->brush &&
            focusblur_brush_name_is ((*diffusion)->brush, brush_name);
          same_density = same_source &&
            ! focusblur_brush_update_balance ((*diffusion)->brush,
                                              store->enable_brush_balance);
          same_diffusion = same_density &&
            store->model_rotate == (*diffusion)->model_rotate;
        }
      else
        same_source = same_density = same_diffusion = TRUE; /* ignored */

      if (store->model_type == (*diffusion)->model_type &&
          store->model_radius == (*diffusion)->model_radius &&
          (store->model_type == FBLUR_MODEL_FLAT ||
           store->model_type == FBLUR_MODEL_BRUSH ||
           store->model_fill == (*diffusion)->model_fill) &&
          store->model_softness == (*diffusion)->model_softness &&
          same_diffusion /* model == brush */ )
        {
          if (store->shine_radius != (*diffusion)->shine_radius)
            {
              (*diffusion)->shine_radius = store->shine_radius;
              (*diffusion)->density_max = focusblur_diffusion_make_density
                (*diffusion, (*diffusion)->shine_radius);
            }

          return TRUE;
        }

      if (store->shine_radius == (*diffusion)->shine_radius &&

          store->model_type == (*diffusion)->model_type &&
          same_density /* model == brush */ &&
          (store->model_type == FBLUR_MODEL_BRUSH ||
           store->model_type == FBLUR_MODEL_FLAT ||
           store->model_fill == (*diffusion)->model_fill))
        /* reuse */
        density_max_bak = (*diffusion)->density_max;

      if (same_source)
        {
          /* it doesn't need to update */
          brush_tmp = (*diffusion)->brush;
          (*diffusion)->brush = NULL;
        }

      focusblur_diffusion_destroy (diffusion);
    }

#ifdef HAVE_FFTW3
  if (fft)
    focusblur_fft_buffer_invalidate_diffusion (fft);
#endif

  if (store->model_type == FBLUR_MODEL_BRUSH &&
      ! brush_tmp)
    {
      brush_tmp =
        focusblur_brush_new (brush_name, store->enable_brush_balance);

      if (! brush_tmp)
        return FALSE;
    }

  *diffusion = g_new0 (FblurDiffusionTable, 1);

  (*diffusion)->model_type           = store->model_type;
  (*diffusion)->model_radius         = store->model_radius;
  (*diffusion)->model_rotate         = store->model_rotate;
  (*diffusion)->model_fill           = store->model_fill;
  (*diffusion)->model_softness       = store->model_softness;
  (*diffusion)->shine_radius         = store->shine_radius;

  (*diffusion)->model_fill_float = (*diffusion)->model_fill / 100.0f;

  radius = (*diffusion)->model_radius;
  radius *= 1.0f + (*diffusion)->model_softness / 100.0f;

  radius_int = ceilf (radius);

  /* the figure symmetric on each axes */
  if (store->model_type != FBLUR_MODEL_BRUSH)
    {
      width = 1 + radius_int;
      rowstride = width;
      centeroffset = 0; /* means symmetrical */
    }
  else
    {
      width = radius_int + 1 + radius_int;
      rowstride = width;
      centeroffset = rowstride * radius_int + radius_int;
    }

  (*diffusion)->model_radius_int = radius_int;
  (*diffusion)->centeroffset = centeroffset;
  (*diffusion)->rowstride = rowstride;
  (*diffusion)->blocksize = sizeof (gfloat) * rowstride * width;
  (*diffusion)->brush = brush_tmp;

  if (density_max_bak)
    (*diffusion)->density_max = density_max_bak;
  else
    (*diffusion)->density_max = focusblur_diffusion_make_density
      (*diffusion, (*diffusion)->shine_radius);

  g_assert ((*diffusion)->model_type != FBLUR_MODEL_BRUSH ||
            (*diffusion)->brush      != NULL);

  return TRUE;
}


void
focusblur_diffusion_destroy (FblurDiffusionTable        **diffusion)
{
  gint i;

  if (*diffusion)
    {
      for (i = 0; i < FBLUR_DIFFUSION_NTABLES; i ++)
        if ((*diffusion)->distribution[i])
          g_slice_free1 ((*diffusion)->blocksize,
                         (*diffusion)->distribution[i]);

      focusblur_brush_destroy (&((*diffusion)->brush));

      g_free (*diffusion);
      *diffusion = NULL;
    }
}


gfloat
focusblur_diffusion_get (FblurDiffusionTable    *diffusion,
                         gint                    level,
                         gint                    pos_x,
                         gint                    pos_y,
                         gint                    x,
                         gint                    y)
{
  gfloat        *dp;
  gint           dx, dy;
  gint           num;

  dx = pos_x - x;
  dy = pos_y - y;

  if (dx < - diffusion->model_radius_int ||
      dx > diffusion->model_radius_int ||
      dy < - diffusion->model_radius_int ||
      dy > diffusion->model_radius_int)
    return 0.0f;

  g_assert (level <= FBLUR_DEPTH_MAX);
  g_assert (level >= -FBLUR_DEPTH_MAX);

  if (! level)
    return (dx || dy) ? 0.0f : 1.0f;

  level = abs (level);

  /* Farthest distribution is allocated at index 0. */
  num = FBLUR_DEPTH_MAX - level;

  g_assert (num >= 0);
  g_assert (num < FBLUR_DIFFUSION_NTABLES);

  if (! diffusion->distribution[num])
    focusblur_diffusion_make (diffusion, level, num);

  g_assert (diffusion->distribution[num] != NULL);

  dp = diffusion->distribution[num];
  if (diffusion->centeroffset == 0)
    {
      /* data in quarto */
      dx = abs (dx);
      dy = abs (dy);
    }
  else
    {
      dp += diffusion->centeroffset;
    }
  dp += dy * diffusion->rowstride + dx;

  return *dp;
}


/* it is not enough for shrinked coordinates,
   but it is not bad than to do nothing. */
gfloat
focusblur_diffusion_getf (FblurDiffusionTable   *diffusion,
                          gint                   level,
                          gfloat                 fx,
                          gfloat                 fy)
{
  gint   x0, x1, y0, y1;
  gfloat distribution, d[4];
  gfloat xbal, ybal, r0, r1;

  x0 = floorf (fx);
  x1 = ceilf (fx);
  y0 = floorf (fy);
  y1 = ceilf (fy);

  d[0] =
    focusblur_diffusion_get (diffusion, level, x0, y0, 0, 0);
  d[1] =
    focusblur_diffusion_get (diffusion, level, x1, y0, 0, 0);
  d[2] =
    focusblur_diffusion_get (diffusion, level, x0, y1, 0, 0);
  d[3] =
    focusblur_diffusion_get (diffusion, level, x1, y1, 0, 0);

  /* same as gimp_bilinear (fx, fy, d), but float. */

  xbal = fx - x0;
  ybal = fy - y0;
  r0 = d[0] + (d[1] - d[0]) * xbal;
  r1 = d[2] + (d[3] - d[2]) * xbal;
  distribution = r0 + (r1 - r0) * ybal;

  return distribution;
}


gfloat
focusblur_diffusion_get_shine (FblurDiffusionTable      *diffusion,
                               gint                      depth_level,
                               gint                      shine_level)
{
  const gfloat   color_fnum = 1.0f / 255.0f;
  gfloat         shine_density;
  gfloat         fval;
  gint           num;

  g_assert (depth_level <= FBLUR_DEPTH_MAX);
  g_assert (depth_level >= -FBLUR_DEPTH_MAX);

  if (! depth_level ||
      ! shine_level ||
      ! diffusion->shine_radius)
    return 1.0f;

  depth_level = abs (depth_level);

  /* Farthest distribution is allocated at index 0. */
  num = FBLUR_DEPTH_MAX - depth_level;

  g_assert (num >= 0);
  g_assert (num < FBLUR_DIFFUSION_NTABLES);

  if (! diffusion->distribution[num])
    focusblur_diffusion_make (diffusion, depth_level, num);

  shine_density = diffusion->density[num];

  if (diffusion->density_max > 1.0f &&
      shine_density > diffusion->density_max)
    shine_density = diffusion->density_max;

  shine_density -= 1.0f;

  g_return_val_if_fail (shine_density >= 0.0f, 1.0f);

  fval = 1.0f + shine_level * shine_density * color_fnum;

  return fval;
}


static void
focusblur_diffusion_make (FblurDiffusionTable   *diffusion,
                          gint                   level,
                          gint                   num)
{
  FocusblurDiffusionModelFunc func;

  gfloat        *dlp, *dp;
  gfloat        *tlp, *tp;
  gfloat         radius;
  gfloat         density;
  gfloat         fval;
  gint           range, width;
  gint           x, y;

  g_assert (level > 0);

  radius = diffusion->model_radius;

  switch (diffusion->model_type)
    {
    case FBLUR_MODEL_FLAT:
      func = focusblur_diffusion_model_flat;
      break;

    case FBLUR_MODEL_SPHERICAL:
      func = focusblur_diffusion_model_spherical;
      break;

    case FBLUR_MODEL_GAUSSIAN:
      func = focusblur_diffusion_model_gaussian;
      break;

    case FBLUR_MODEL_RING:
      func = focusblur_diffusion_model_ring;
      break;

    case FBLUR_MODEL_CONCAVE:
      func = focusblur_diffusion_model_concave;
      break;

    case FBLUR_MODEL_BRUSH:
      func = 0;
      break;

    default:
      g_assert_not_reached ();
      func = NULL; /* shut a warning */
    }

  radius *= (gfloat) level / (gfloat) FBLUR_DEPTH_MAX;
  range = ceilf (radius);
  width = 1 + range;
  if (diffusion->centeroffset)
    width += range;

  g_assert (radius >= 0.0f);
  g_assert (range < table_length);

  diffusion->distribution[num] = g_slice_alloc0 (diffusion->blocksize);

  density = 0.0f;
  dlp = diffusion->distribution[num];

  /* rendring and total value */
  if (diffusion->centeroffset == 0)
    {
      g_assert (func != 0);

      tlp = difference_table;
      for (y = width; y --; dlp += diffusion->rowstride, tlp += table_length)
        for (x = width, dp = dlp, tp = tlp; x --; dp ++, tp ++)
          density += (*dp = (*func) (diffusion, radius, *tp));

      /* 4 times density */
      dp = diffusion->distribution[num];
      for (x = width; x --; dp ++)
        density -= *dp;
      density *= 4;
      density += diffusion->distribution[num][0];
    }
  else
    {
      dlp += diffusion->centeroffset;

      switch (diffusion->model_type)
        {
        case FBLUR_MODEL_BRUSH:
          g_assert (diffusion->brush != NULL);

          density = focusblur_brush_render
            (diffusion->brush, radius, diffusion->model_rotate,
             diffusion->rowstride, dlp);
          break;

        default:
          /* currently supports only brush */
          g_assert_not_reached ();
        }
    }
  diffusion->density[num] = MAX (density, 1.0f);

  /* normalize */
  g_assert (density > 0.0f);
  fval = 1.0f / density;
  dlp = diffusion->distribution[num];
  if (diffusion->centeroffset)
    /* move to corner */
    dlp += diffusion->centeroffset - diffusion->rowstride * range - range;

  for (y = width; y --; dlp += diffusion->rowstride)
    for (x = width, dp = dlp; x --; dp ++)
      *dp *= fval;

  focusblur_diffusion_blur
    (diffusion, diffusion->distribution[num], radius, level);
}


/* Pre-making figure and compute density. it limits
   diffuseable maximum density to be shown as shining. */
static gfloat
focusblur_diffusion_make_density (FblurDiffusionTable   *diffusion,
                                  gfloat                 radius)
{
  FocusblurDiffusionModelFunc func;

  gfloat        *tlp, *tp;
  gfloat         density;
  gint           range;
  gint           x, y;

  if (! radius)
    return 1.0f;

  if (radius < 0.0f)
    return 0.0f;

  switch (diffusion->model_type)
    {
    case FBLUR_MODEL_FLAT:
      func = focusblur_diffusion_model_flat;
      break;

    case FBLUR_MODEL_SPHERICAL:
      func = focusblur_diffusion_model_spherical;
      break;

    case FBLUR_MODEL_GAUSSIAN:
      func = focusblur_diffusion_model_gaussian;
      break;

    case FBLUR_MODEL_RING:
      func = focusblur_diffusion_model_ring;
      break;

    case FBLUR_MODEL_CONCAVE:
      func = focusblur_diffusion_model_concave;
      break;

    case FBLUR_MODEL_BRUSH:
      return focusblur_brush_make_density (diffusion->brush, radius);

    default:
      g_assert_not_reached ();
      func = NULL; /* shut a warning */
    }

  range = ceilf (radius);

  g_assert (radius >= 0.0f);
  g_assert (range < table_length);

  density = 0.0f;
  tlp = difference_table;

  if (diffusion->centeroffset == 0)
    {
      /* skip first line, it has been overlapped */
      tlp += table_length;

      for (y = 1; y <= range; y ++, tlp += table_length)
        for (x = 0, tp = tlp; x <= range; x ++, tp ++)
          density += (*func) (diffusion, radius, *tp);

      /* 4 times (like a pinwheel) and fill center */
      density *= 4;
      density += (*func) (diffusion, radius, 0.0f);
    }
  else
    g_assert_not_reached ();

  return density;
}


static void
focusblur_diffusion_blur (FblurDiffusionTable   *diffusion,
                          gfloat                *data,
                          gfloat                 model_radius,
                          gint                   level)
{
  const gint     width = 1 + 2 * FBLUR_RADIUS_MAX;
  gfloat         blur_table[width];
  /* tp[] has minus offset. it reduces calling abs(). */
  gfloat        *tp = &(blur_table[FBLUR_RADIUS_MAX]);
  gfloat        *dlp, *dp, *sp;

  gint           range;
  gint           blur_radius_int;
  gint           x, y, i, j;
  gfloat         softness;
  gfloat         blur_radius;
  gfloat         vr, fval, sval;

  if (! diffusion->model_softness)
    return;

  softness = diffusion->model_softness / 100.0f;
  range = diffusion->model_radius_int;
  blur_radius = model_radius * softness;
  blur_radius_int = ceilf (blur_radius);

  vr = FBLUR_GAUSSIAN_FACTOR * (blur_radius + 1);
  vr = -1.0f / (2.0f * SQR (vr));

  /* make blurring table */
  tp[0] = 1.0f;
  sval = 0.0f;
  for (i = 1; i <= blur_radius_int; i ++)
    {
      fval = expf (SQR (i) * vr);
      tp[i] = tp[-i] = fval;
      sval += fval;
    }
  sval *= 2.0f;
  sval += 1.0f;

  for (i = -blur_radius_int; i <= blur_radius_int; i ++)
    tp[i] /= sval;

  /* for symmetrical figures */
  if (diffusion->centeroffset == 0)
    {
      gfloat    sum[1 + range];

      /* run on X lines */
      for (y = 0, dlp = data; y <= range; y ++, dlp += diffusion->rowstride)
        {
          for (x = 0, sp = sum; x <= range; x ++, sp ++)
            {
              fval = 0.0f;
              for (i = - blur_radius_int; i <= blur_radius_int; i ++)
                {
                  j = abs (x + i); /* abs() works as mirror */
                  if (j <= range) /* limits inside of X */
                    fval += tp[i] * dlp[j];
                }
              *sp = fval;
            }
          for (x = 0, dp = dlp, sp = sum; y <= range; y ++, dp ++, sp ++)
            *dp = *sp;
        }

      /* run on Y lines */
      for (x = 0, dlp = data; x <= range; x ++, dlp ++)
        {
          for (y = 0, sp = sum; y <= range; y ++, sp ++)
            {
              fval = 0.0f;
              for (i = - blur_radius_int; i <= blur_radius_int; i ++)
                {
                  j = abs (y + i);
                  if (j <= range)
                    fval += tp[i] * dlp[j * diffusion->rowstride];
                }
              *sp = fval;
            }
          for (y = 0, dp = dlp, sp = sum; y <= range;
               y ++, dp += diffusion->rowstride, sp ++)
            *dp = *sp;
        }
    }
  else /* has center offset */
    {
      gfloat    sum[range + 1 + range];
      gsize     centerline = diffusion->rowstride * range;

      /* move to corner */
      data += diffusion->centeroffset - centerline - range;

      for (y = -range, dlp = data + range;
           y <= range; y ++, dlp += diffusion->rowstride)
        {
          for (x = -range, sp = sum; x <= range; x ++, sp ++)
            {
              fval = 0.0f;
              for (i = - blur_radius_int; i <= blur_radius_int; i ++)
                {
                  j = x + i;
                  if (j >= -range && j <= range)
                    fval += tp[i] * dlp[j];
                }
              *sp = fval;
            }
          for (x = -range, dp = dlp - range, sp = sum;
               y <= range; y ++, dp ++, sp ++)
            *dp = *sp;
        }

      for (x = -range, dlp = data + centerline;
           x <= range; x ++, dlp ++)
        {
          for (y = -range, sp = sum; y <= range; y ++, sp ++)
            {
              fval = 0.0f;
              for (i = - blur_radius_int; i <= blur_radius_int; i ++)
                {
                  j = y + i;
                  if (j >= -range && j <= range)
                    fval += tp[i] * dlp[j * diffusion->rowstride];
                }
              *sp = fval;
            }
          for (y = -range, dp = dlp - centerline, sp = sum;
               y <= range; y ++, dp += diffusion->rowstride, sp ++)
            *dp = *sp;
        }
    }
}


static gfloat
focusblur_diffusion_model_flat (FblurDiffusionTable     *diffusion,
                                gfloat                   radius,
                                gfloat                   difference)
{
  gfloat        distribution;

  distribution = 1.0f + radius - difference;

  if (distribution <= 0.0f)
    distribution = 0.0f;

  else if (distribution >= 1.0f)
    distribution = 1.0f;

  return distribution;
}


static gfloat
focusblur_diffusion_model_spherical (FblurDiffusionTable *diffusion,
                                     gfloat               radius,
                                     gfloat               difference)
{
  gfloat distribution;
  gfloat base, r, nd, fd, nval, fval;

  if (difference < 0.5f)
    return 1.0f;

  r = radius + 0.5f;
  nd = difference - 0.5f;
  fd = difference + 0.5f;

  if (nd < 0.0f)
    nd = 0.0f;
  if (nd > r)
    return 0.0f;

  if (fd > r)
    {
      fd = r;
      fval = 0.0f;
    }
  else
    fval = sqrtf (1.0f - SQR (fd / r));

  base = fd - nd;

  nval = sqrtf (1.0f - SQR (nd / r));

  distribution = base * (nval + fval) / 2.0f;

  return distribution;
}


static gfloat
focusblur_diffusion_model_gaussian (FblurDiffusionTable *diffusion,
                                    gfloat               radius,
                                    gfloat               difference)
{
  static gfloat cache_radius = 0.0f;
  static gfloat cache_vr = 0.0f;

  gfloat distribution;
  gfloat vr;

  if (difference - radius >= 1.0f)
    return 0.0f;

  if (radius == cache_radius)
    {
      vr = cache_vr;
    }
  else
    {
      vr = FBLUR_GAUSSIAN_FACTOR * (radius + 1);
      vr = -1.0f / (2.0f * vr * vr);

      cache_radius = radius;
      cache_vr = vr;
    }

  distribution = expf (difference * difference * vr);

  return distribution;
}


static gfloat
focusblur_diffusion_model_ring (FblurDiffusionTable     *diffusion,
                                gfloat                   radius,
                                gfloat                   difference)
{
  gfloat        distribution;

  if (difference == 0.0f)
    {
      if (radius <= 5.0f)
        distribution = 1.0f;

      else if (radius < 1.0f)
        {
          distribution = 2.0f * (1.0f - radius);
          distribution += diffusion->model_fill_float * (1.0f - distribution);

          return distribution;
        }
      else
        distribution = diffusion->model_fill_float;

      return distribution;
    }

  distribution = 1.0f + radius - difference;

  if (distribution <= 0.0f)
    distribution = 0.0f;

  else if (distribution >= 2.0f)
    distribution = diffusion->model_fill_float;

  else if (distribution > 1.0f)
    {
      distribution = 2.0f - distribution;

      distribution *= 1.0f - diffusion->model_fill_float;
      distribution += diffusion->model_fill_float;
    }

  return distribution;
}


static gfloat
focusblur_diffusion_model_concave (FblurDiffusionTable  *diffusion,
                                   gfloat                radius,
                                   gfloat                difference)
{
  gfloat        distribution;

  if (difference == 0.0f)
    {
      if (radius <= 5.0f)
        distribution = 1.0f;

      else if (radius < 1.0f)
        {
          distribution = 2.0f * (1.0f - radius);
          distribution += diffusion->model_fill_float * (1.0f - distribution);

          return distribution;
        }
      else
        distribution = diffusion->model_fill_float;

      return distribution;
    }

  distribution = 1.0f + radius - difference;

  if (distribution <= 0.0f)
    distribution = 0.0f;

  else if (distribution > 1.0f)
    {
      distribution = (difference + 0.5f) / (radius + 0.5f);
      distribution *= distribution;

      distribution *= 1.0f - diffusion->model_fill_float;
      distribution += diffusion->model_fill_float;
    }

  return distribution;
}
