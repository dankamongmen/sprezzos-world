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

#include <string.h>
#ifdef HAVE_COMPLEX_H
#  include <complex.h>
#  include <math.h>
#endif
#include <fftw3.h>
#include <libgimp/gimp.h>
#include <libgimp/gimpui.h>

#include "libgimp/stdplugins-intl.h"

#include "focusblurenums.h"
#include "focusblurtypes.h"
#include "focusblurparam.h"
#include "gimpplugin.h"
#include "diffusion.h"
#include "depthmap.h"
#include "shine.h"
#include "fftblurbuffer.h"
#include "fftblurproc.h"


/*---- Prototypes ----*/

static gboolean
focusblur_fft_buffer_update_source      (FblurFftBuffer         *fft,
                                         GimpDrawable           *drawable,
                                         GimpPreview            *preview);
static gboolean
focusblur_fft_buffer_update_work        (FblurFftBuffer         *fft,
                                         gint                    radius);
static void
focusblur_fft_buffer_update_depth_division
                                        (FblurFftBuffer         *fft,
                                         FblurQualityType        quality,
                                         gint                    radius,
                                         gint                    focal_depth);
static void
focusblur_fft_buffer_update_depth_table            (FblurFftBuffer  *fft,
                                                    gint             division,
                                                    gint             slide);
static void
focusblur_fft_buffer_update_depth_count            (FblurFftBuffer  *fft,
                                                    FblurDepthMap   *map);

static void     focusblur_fft_buffer_clear_source  (FblurFftBuffer  *fft);
static void     focusblur_fft_buffer_clear_work    (FblurFftBuffer  *fft);
static void     focusblur_fft_buffer_clear_depth   (FblurFftBuffer  *fft);


/*---- Functions for structures ----*/

gboolean
focusblur_fft_buffer_update (FblurFftBuffer **fft,
                             FblurParam      *param,
                             FblurQualityType quality,
                             GimpPreview     *preview)
{
  gint                   width, height;
  gint                   radius, range;
  gint                   x1, x2, y1, y2;


  /* check condition */
  if (quality == FBLUR_QUALITY_BEST)
    return FALSE;

  if (! focusblur_diffusion_update (&(param->diffusion), *fft,
                                    &(param->store), param->brush_name))
    {
      gimp_message (_("Failed to update diffusion table."));

      focusblur_fft_buffer_destroy (fft);
      return FALSE;
    }

  radius = ceilf (param->diffusion->model_radius); // without softness
  range = param->diffusion->model_radius_int;

  if (radius < 3 ||
      (quality == FBLUR_QUALITY_NORMAL &&
       param->store.enable_depth_map &&
       radius > 63)) // needs more tune
    return FALSE;

  gimp_drawable_mask_bounds (param->drawable_ID, &x1, &y1, &x2, &y2);

  if (preview)
    {
      gint px1, px2, py1, py2;
      gint pw, ph;

      gimp_preview_get_position (GIMP_PREVIEW (preview), &px1, &py1); 
      gimp_preview_get_size (GIMP_PREVIEW (preview), &pw, &ph); 
      px2 = px1 + pw;
      py2 = py1 + ph;

      /* it is not need for GimpDrawablePreview */
      if (x1 >= px2 || x2 <= px1 ||
          y1 >= py2 || y2 <= py1)
        return TRUE;

      x1 = MAX (px1, x1);
      x2 = MIN (px2, x2);
      y1 = MAX (py1, y1);
      y2 = MIN (py2, y2);
    }

  width  = x2 - x1;
  height = y2 - y1;

  if (width  < range ||
      height < range)
    return FALSE;

  if (! *fft)
    {
      *fft = g_new0 (FblurFftBuffer, 1);

      if (! *fft)
        {
          gimp_message (_("Failed to allocate memory."));

          return FALSE;
        }
    }

  //focusblur_source_destroy (&(param->source));

  /* fftsource must be updated before to update work and depth */
  if (! focusblur_fft_buffer_update_source (*fft, param->drawable, preview))
    {
      gimp_message (_("Failed to update source buffer."));

      focusblur_fft_buffer_destroy (fft);
      return FALSE;
    }

  if (param->store.enable_depth_map)
    {
      if (! focusblur_depth_map_update (&(param->depth_map), *fft,
                                        &(param->store), quality))
        {
          gimp_message (_("Failed to update depth info."));

          focusblur_fft_buffer_destroy (fft);
          return FALSE;
        }
      else
        {
          gint d = focusblur_depth_map_focal_depth (param->depth_map);
          focusblur_fft_buffer_update_depth_division (*fft, quality, range, d);

          focusblur_fft_buffer_update_depth_count (*fft, param->depth_map);
        }
    }

  if (! focusblur_fft_buffer_update_work (*fft, range))
    {
      gimp_message (_("Failed to update working buffer."));

      focusblur_fft_buffer_destroy (fft);
      return FALSE;
    }

  if (! focusblur_shine_update (&(param->shine), param->drawable,
                                &(param->store)))
    {
      gimp_message (_("Failed to update shine data."));

      focusblur_fft_buffer_destroy (fft);
      return FALSE;
    }

  return TRUE;
}


void
focusblur_fft_buffer_destroy (FblurFftBuffer **fft)
{
  if (*fft)
    {
      focusblur_fft_buffer_clear_source (*fft);
      focusblur_fft_buffer_clear_work (*fft);
      focusblur_fft_buffer_clear_depth (*fft);

      g_free (*fft);
      *fft = NULL;
    }
}


void
focusblur_fft_buffer_draw (FblurFftBuffer *fft)
{
  GimpDrawablePreview *preview;
  gboolean             dirty;
  guint8              *data;
  GimpPixelRgn         pr;

  if (! fft->source.preview)
    {
      preview = NULL;
      dirty = TRUE;
      data = fft->source.data;
    }
  else
    {
      preview = GIMP_DRAWABLE_PREVIEW (fft->source.preview);
      g_assert (preview != NULL);

      dirty = FALSE;
      data = fft->source.data_preview;
    }

  g_assert (data != NULL);

  gimp_pixel_rgn_init (&pr, fft->source.drawable,
                       fft->source.x1, fft->source.y1,
                       fft->source.width, fft->source.height, dirty, TRUE);
  gimp_pixel_rgn_set_rect (&pr, data,
                           fft->source.x1, fft->source.y1,
                           fft->source.width, fft->source.height);

  if (! preview)
    {
      gimp_drawable_flush (fft->source.drawable);
      gimp_drawable_merge_shadow (fft->source.drawable->drawable_id, TRUE);
      gimp_drawable_update (fft->source.drawable->drawable_id,
                            fft->source.x1, fft->source.y1,
                            fft->source.width, fft->source.height);

      /* this buffer has been dirty */
      focusblur_fft_buffer_clear_source (fft);
    }
  else
    {
      gimp_drawable_preview_draw_region (preview, &pr);

    }
}


void
focusblur_fft_buffer_make_kernel (FblurFftBuffer      *fft,
                                  FblurDiffusionTable *diffusion,
                                  gint                 level)
{
  gfloat        *o0, *o1, *o2, *o3;
  gfloat        *dlp, *dp;
  gint           row, col;
  gint           n, r;
  gint           x, y;
  gfloat         norm;

  if (level == fft->work.level)
    return;

  norm = 1.0f / (fft->work.row * fft->work.col);

  if (level)
    {
      row = fft->work.row;
      col = fft->work.col;
      n = fft->work.col_padded;
      r = MIN (diffusion->model_radius_int, fft->work.space);

      o0 = (gfloat *) fft->work.image;
      o1 = o0 + col - r;
      o2 = o0 + (row - r) * n;
      o3 = o2 + col - r;

      focusblur_fft_work_fill_zero (fft);

      for (x = 0, dlp = o0; x <= r; x ++, dlp += n)
        for (y = 0, dp = dlp; y <= r; y ++, dp ++)
          *dp = norm *
            focusblur_diffusion_get (diffusion, level, 0, 0, x, y);

      for (x = 0, dlp = o1; x <= r; x ++, dlp += n)
        for (y = -r, dp = dlp; y < 0; y ++, dp ++)
          *dp = norm *
            focusblur_diffusion_get (diffusion, level, 0, 0, x, y);

      for (x = -r, dlp = o2; x < 0; x ++, dlp += n)
        for (y = 0, dp = dlp; y <= r; y ++, dp ++)
          *dp = norm *
            focusblur_diffusion_get (diffusion, level, 0, 0, x, y);

      for (x = -r, dlp = o3; x < 0; x ++, dlp += n)
        for (y = -r, dp = dlp; y < 0; y ++, dp ++)
          *dp = norm *
            focusblur_diffusion_get (diffusion, level, 0, 0, x, y);

      fftwf_execute (fft->work.plan_r2c);

      focusblur_fft_work_store_in_kernel (fft);
    }

  fft->work.level = level;
}


void
focusblur_fft_buffer_make_depth_slice (FblurFftBuffer   *fft,
                                       FblurDepthMap    *depth_map,
                                       gint              look)
{
  FblurFftDepthTable    *table;
  gfloat                *dlp, *dp;
  gint                   depth, x, y;

  focusblur_fft_work_fill_zero (fft);

  dlp = (gfloat *) fft->work.image + fft->work.origin;

  if (fft->depth.quality == FBLUR_QUALITY_NORMAL)
    for (y = fft->source.y1; y < fft->source.y2; y ++, dlp ++)
      for (x = fft->source.x1, dp = dlp; x < fft->source.x2;
           x ++, dp += fft->work.col_padded)
        {
          depth = focusblur_depth_map_get_depth (depth_map, x, y);
          table = &(fft->depth.table[depth]);

          if (table->floor == look)
            *dp = 1.0f - table->diff;

          else if (table->ceil == look)
            *dp = table->diff;
        }

  else /* quality < normal */
    for (y = fft->source.y1; y < fft->source.y2; y ++, dlp ++)
      for (x = fft->source.x1, dp = dlp; x < fft->source.x2;
           x ++, dp += fft->work.col_padded)
        {
          depth = focusblur_depth_map_get_depth (depth_map, x, y);
          table = &(fft->depth.table[depth]);

          if (table->round == look)
            *dp = 1.0f;
        }
}


void
focusblur_fft_buffer_make_depth_behind (FblurFftBuffer  *fft,
                                       FblurDepthMap    *depth_map)
{
  FblurFftDepthTable    *table;
  gfloat                *dlp, *dp;
  gint                   depth, look;
  gint                   x, y;

  /* this is imperfect to fill behind,
     because fftblur works in a whole picture,
     it can't work adaptive at each pixels. */

  look = focusblur_depth_map_focal_depth (depth_map);

  focusblur_fft_work_fill_zero (fft);

  dlp = (gfloat *) fft->work.image + fft->work.origin;

  for (y = fft->source.y1; y < fft->source.y2; y ++, dlp ++)
    for (x = fft->source.x1, dp = dlp; x < fft->source.x2;
         x ++, dp += fft->work.col_padded)
      {
        depth = focusblur_depth_map_get_depth (depth_map, x, y);
        table = &(fft->depth.table[depth]);

        /* problem: floods focused field */
        if (table->round >= look)
          *dp = 1.0f;
      }
}


void
focusblur_fft_buffer_invalidate_depth_map (FblurFftBuffer *fft)
{
  if (fft)
    {
      fft->depth.count = 0;
    }
}


void
focusblur_fft_buffer_invalidate_diffusion (FblurFftBuffer *fft)
{
  if (fft)
    {
      fft->work.level = 0;
    }
}


static gboolean
focusblur_fft_buffer_update_source (FblurFftBuffer *fft,
                                    GimpDrawable   *drawable,
                                    GimpPreview    *preview)
{
  GimpPixelRgn  pr;
  gsize         size;
  gint          x1, x2, y1, y2;
  gint          width, height;
  gint          ntiles;

  fft->source.drawable = drawable;
  fft->source.preview  = preview;

  gimp_drawable_mask_bounds (drawable->drawable_id, &x1, &y1, &x2, &y2);

  if (preview)
    {
      gint px1, px2, py1, py2;
      gint pw, ph;

      gimp_preview_get_position (GIMP_PREVIEW (preview), &px1, &py1); 
      gimp_preview_get_size (GIMP_PREVIEW (preview), &pw, &ph); 
      px2 = px1 + pw;
      py2 = py1 + ph;

      x1 = MAX (px1, x1);
      x2 = MIN (px2, x2);
      y1 = MAX (py1, y1);
      y2 = MIN (py2, y2);
    }

  g_assert (x1 < x2);
  g_assert (y1 < y2);

  width  = x2 - x1;
  height = y2 - y1;

  ntiles = 1 + x2 / TILE_WIDTH - x1 / TILE_WIDTH;
  gimp_tile_cache_ntiles (ntiles);

  fft->source.has_alpha = gimp_drawable_has_alpha (drawable->drawable_id);
  fft->source.bpp       = drawable->bpp;
  fft->source.channels  = drawable->bpp - (fft->source.has_alpha ? 1 : 0);
  fft->source.rowstride = drawable->bpp * width;

  size = fft->source.rowstride * height;

  if (fft->source.data_preview)
    {
      if (! preview)
        {
          g_free (fft->source.data_preview);
          fft->source.data_preview = NULL;
        }
      else if (size != fft->source.size)
        {
          g_free (fft->source.data_preview);
          goto allocate2;
        }
    }
  else
    {
    allocate2:
      if (preview)
        {
          fft->source.data_preview = g_malloc (size);
          if (! fft->source.data_preview)
            return FALSE;
        }
    }

  if (fft->source.data)
    {
      if (size != fft->source.size)
        {
          g_free (fft->source.data);
          goto allocate;
        }
    }
  else
    {
    allocate:
      fft->source.size = size;
      fft->source.data = g_malloc (size);
      if (! fft->source.data)
        return FALSE;
      goto reload;
    }

  if (x1 == fft->source.x1 &&
      x2 == fft->source.x2 &&
      y1 == fft->source.y1 &&
      y2 == fft->source.y2)
    return TRUE;

 reload:
  fft->source.x1     = x1;
  fft->source.x2     = x2;
  fft->source.y1     = y1;
  fft->source.y2     = y2;
  fft->source.width  = width;
  fft->source.height = height;

  /* need to recount */
  fft->depth.count = 0;

  /* load */
  gimp_pixel_rgn_init (&pr, drawable, x1, y1, width, height, FALSE, FALSE);
  gimp_pixel_rgn_get_rect (&pr, fft->source.data,
                           fft->source.x1, fft->source.y1,
                           fft->source.width, fft->source.height);

  return TRUE;
}


static gboolean
focusblur_fft_buffer_update_work (FblurFftBuffer *fft,
                                  gint            radius)
{
  gint row, col;

  row = fft->source.width  + 2 * radius;
  col = fft->source.height + 2 * radius;

  if (fft->work.buffers)
    {
      g_warning ("buffer hadn't been cleared.");
      focusblur_fft_work_free_buffers (fft);
    }

  if (fft->work.image &&
      row == fft->work.row &&
      col == fft->work.col)
    {
      if (radius != fft->work.space)
        {
          fft->work.space = radius;
          fft->work.origin = (fft->work.col_padded + 1) * radius;
          fft->work.level = 0;
        }
      return TRUE;
    }

  focusblur_fft_buffer_clear_work (fft);

  fft->work.row = row;
  fft->work.col = col;
  fft->work.col_padded = (col + 2) & ~1;

  fft->work.nelements = row * fft->work.col_padded;
  fft->work.complex_nelements = fft->work.nelements / 2;
  fft->work.size = sizeof (fftwf_complex) * fft->work.complex_nelements;

  /* 32-bytes pair (4x complex or 8x real) processing */
  fft->work.size += 31;
  fft->work.size &= ~31;

  /* fftwf_malloc() (or distributed package) is broken. */
  fft->work.image  = fftwf_malloc (fft->work.size);
  fft->work.kernel = fftwf_malloc (fft->work.size);
  if (! fft->work.image || ! fft->work.kernel)
    {
      focusblur_fft_buffer_clear_work (fft);
      return FALSE;
    }

  fft->work.plan_r2c = fftwf_plan_dft_r2c_2d
    (row, col, (gfloat *) fft->work.image, fft->work.image, FFTW_ESTIMATE);

  fft->work.plan_c2r = fftwf_plan_dft_c2r_2d
    (row, col, fft->work.image, (gfloat *) fft->work.image, FFTW_ESTIMATE);

  if (! fft->work.plan_r2c || ! fft->work.plan_c2r)
    {
      focusblur_fft_buffer_clear_work (fft);
      return FALSE;
    }

  fft->work.space = radius;
  fft->work.origin = (fft->work.col_padded + 1) * radius;
  fft->work.level = 0;

  return TRUE;
}


static void
focusblur_fft_buffer_update_depth_division (FblurFftBuffer   *fft,
                                            FblurQualityType  quality,
                                            gint              radius,
                                            gint              focal_depth)
{
  gint division;
  gint slide;

  fft->depth.quality = quality;

  division = radius;
  switch (quality)
    {
    case FBLUR_QUALITY_NORMAL:
      division = MIN (radius, FBLUR_DEPTH_MAX);
      slide = focal_depth;
      break;

    case FBLUR_QUALITY_LOW:
      division = MIN (radius, 15);
      slide = 0;
      break;

    case FBLUR_QUALITY_DEFECTIVE:
      division = MIN (radius, 7);
      slide = 0;
      break;

    default:
      g_assert_not_reached ();
    }

  focusblur_fft_buffer_update_depth_table (fft, division, slide);
}


static void
focusblur_fft_buffer_update_depth_table (FblurFftBuffer *fft,
                                         gint            division,
                                         gint            slide)
{
  FblurFftDepthTable    *table;
  gfloat                 dfac, fval;
  gfloat                 r, f, c;
  gint                   ri, fi, ci;
  gint                   i;

  g_assert (division > 0);

  if (division == fft->depth.division &&
      slide    == fft->depth.slide)
    return;

  dfac = (gfloat) FBLUR_DEPTH_MAX / division;

  for (i = 0; i <= FBLUR_DEPTH_MAX; i ++)
    {
      table = &(fft->depth.table[i]);
      fval = (gfloat) (i - slide) / dfac;

      r = rintf (fval);

      ri = rintf (r * dfac) + slide;
      ri = CLAMP (ri, 0, FBLUR_DEPTH_MAX);

      table->round = ri;

      if (fabsf (r - fval) < 0.001f)
        {
          table->floor = table->ceil = ri;
          table->diff = 0.0f;
        }
      else
        {
          f = floorf (fval);
          c = ceilf (fval);

          fi = rintf (f * dfac) + slide;
          ci = rintf (c * dfac) + slide;

          fi = CLAMP (fi, 0, FBLUR_DEPTH_MAX);
          ci = CLAMP (ci, 0, FBLUR_DEPTH_MAX);

          table->floor = fi;
          table->ceil  = ci;

          table->diff  = (ci > fi) ? (gfloat) (i - fi) / (ci - fi) : 0.0f;
        }
    }

  fft->depth.division = division;
  fft->depth.slide = slide;
  fft->depth.count = 0;
}


static void
focusblur_fft_buffer_update_depth_count (FblurFftBuffer *fft,
                                         FblurDepthMap  *depth_map)
{
  FblurFftDepthTable    *table;
  gint                   orig, depth;
  gint                   x, y;

  if (fft->depth.count)
    return;

  /* count layer of depth */
  memset (fft->depth.check, 0, sizeof fft->depth.check);

  if (fft->depth.quality == FBLUR_QUALITY_NORMAL)
    {
      for (y = fft->source.y1; y < fft->source.y2; y ++)
        for (x = fft->source.x1; x < fft->source.x2; x ++)
          {
            orig = focusblur_depth_map_get_depth (depth_map, x, y);
            table = &(fft->depth.table[orig]);

            depth = table->floor;

            if (! fft->depth.check[depth])
              {
                fft->depth.check[depth] = TRUE;
                fft->depth.count ++;
              }

            depth = table->ceil;

            if (! fft->depth.check[depth])
              {
                fft->depth.check[depth] = TRUE;
                fft->depth.count ++;
              }
          }
    }
  else
    {
      for (y = fft->source.y1; y < fft->source.y2; y ++)
        for (x = fft->source.x1; x < fft->source.x2; x ++)
          {
            orig = focusblur_depth_map_get_depth (depth_map, x, y);
            table = &(fft->depth.table[orig]);

            depth = table->round;

            if (! fft->depth.check[depth])
              {
                fft->depth.check[depth] = TRUE;
                fft->depth.count ++;
              }
          }
    }
}


static void
focusblur_fft_buffer_clear_source (FblurFftBuffer *fft)
{
  g_assert (fft != NULL);

  if (fft->source.data)
    g_free (fft->source.data);

  if (fft->source.data_preview)
    g_free (fft->source.data_preview);

  memset (&(fft->source), 0, sizeof (FblurFftSource));
}


static void
focusblur_fft_buffer_clear_work (FblurFftBuffer *fft)
{
  g_assert (fft != NULL);

  if (fft->work.buffers)
    focusblur_fft_work_free_buffers (fft);

  if (fft->work.plan_r2c)
    fftwf_destroy_plan (fft->work.plan_r2c);

  if (fft->work.plan_c2r)
    fftwf_destroy_plan (fft->work.plan_c2r);

  if (fft->work.image)
    fftwf_free (fft->work.image);

  if (fft->work.kernel)
    fftwf_free (fft->work.kernel);

  memset (&(fft->work), 0, sizeof (FblurFftWork));
}


static void
focusblur_fft_buffer_clear_depth (FblurFftBuffer *fft)
{
  g_assert (fft != NULL);

  memset (&(fft->depth), 0, sizeof (FblurFftDepth));
}
