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
#include "source.h"
#include "diffusion.h"
#include "depthmap.h"
#include "shine.h"
#include "render.h"
#ifdef HAVE_FFTW3
#  include "fftblur.h"
#endif


/*---- Types ----*/

#ifdef ENABLE_MP
typedef struct _FblurThreadSet FblurThreadSet;
struct _FblurThreadSet
{
  GimpPixelRgn  *region;
  volatile gint *waiting;
  gint           number;
};
#endif


/*---- Prototypes ----*/

static void     focusblur_execute_region        (FblurParam     *param,
                                                 GimpPixelRgn   *region,
                                                 gint            number);
static void     focusblur_render_pixel          (gint             pos_x,
                                                 gint             pos_y,
                                                 guchar          *dest_pixel,
                                                 gint             dest_bpp,
                                                 FblurParam      *param);


static inline void
cutoff_in_bound (gint    bound,
                 gint   *point,
                 gint   *cutoff)
{
  gint tmp = bound - *point;
  if (tmp > 0)
    *point = bound;
  else
    tmp = 0;

  if (cutoff)
    *cutoff = tmp;
}


static inline void
cutoff_out_bound (gint   bound,
                  gint  *point,
                  gint  *cutoff)
{
  gint tmp = *point - bound;
  if (tmp > 0)
    *point = bound;
  else
    tmp = 0;

  if (cutoff)
    *cutoff = tmp;
}


/*---- main function ----*/

gboolean
focusblur_execute (FblurParam   *param,
                   GimpPreview  *preview)
{
  FblurQualityType quality;
  GimpPixelRgn     dest_rgn;
  gpointer         reg_rgn;
  gdouble          progress, full_progress;
  gint             x1, x2, y1, y2;
  gint             width, height;

  /* Nothing to do */
  if (param->store.model_radius <= 0.0f)
    return TRUE;

  quality = preview ? param->pref.quality_preview : param->pref.quality;

#ifdef HAVE_FFTW3
  if (focusblur_fft_execute (param, quality, preview))
    return TRUE;
#endif

  if (! focusblur_param_prepare (param, quality))
    return FALSE;

  if (! preview)
    {
      x1 = param->source->x1;
      x2 = param->source->x2;
      y1 = param->source->y1;
      y2 = param->source->y2;
    }
  else
    {
      gimp_preview_get_position (GIMP_PREVIEW (preview), &x1, &y1);
      gimp_preview_get_size (GIMP_PREVIEW (preview), &width, &height);
      x2 = x1 + width;
      y2 = y1 + height;

      /* it is not need for GimpDrawablePreview */
      if (param->source->x1 >= x2 || param->source->x2 <= x1 ||
          param->source->y1 >= y2 || param->source->y2 <= y1)
        return TRUE;

      x1 = MAX (x1, param->source->x1);
      x2 = MIN (x2, param->source->x2);
      y1 = MAX (y1, param->source->y1);
      y2 = MIN (y2, param->source->y2);
    }
  width  = x2 - x1;
  height = y2 - y1;

  gimp_tile_cache_ntiles (16);

  /* Destined image */
  gimp_pixel_rgn_init (&dest_rgn, param->drawable,
                       x1, y1, width, height, (preview == NULL), TRUE);

  progress = full_progress = 0.0;
  if (! preview)
    {
      full_progress = width * height;
      progress = 0.0;
      gimp_progress_init (_("Focus Blur..."));
      gimp_progress_update (0.0001);
    }

  for (reg_rgn = gimp_pixel_rgns_register (1, &dest_rgn);
       reg_rgn != NULL; reg_rgn = gimp_pixel_rgns_process (reg_rgn))
    {
#ifdef ENABLE_MP
      if (param->max_threads)
        {
          volatile gint  num = param->max_threads;
          FblurThreadSet set[param->max_threads];
          gint           i;

          //param->thread_pool->func = focusblur_execute_thread;

          for (i = 0; i < param->max_threads; i ++)
            {
              set[i].region  = &dest_rgn;
              set[i].waiting = &num;
              set[i].number  =  i;
              g_thread_pool_push (param->thread_pool, &(set[i]), NULL);
            }
          focusblur_execute_region (param, &dest_rgn, param->max_threads);
          while (g_atomic_int_get (&num))
            g_thread_yield ();
        }
      else
#endif /* ENABLE_MP */
        focusblur_execute_region (param, &dest_rgn, 0);

      if (! preview)
        {
          progress += (dest_rgn.w * dest_rgn.h);
          gimp_progress_update (progress / full_progress);
        }
      else
        {
          gimp_drawable_preview_draw_region (GIMP_DRAWABLE_PREVIEW (preview),
                                             &dest_rgn);
        }
    }

  if (! preview)
    {
      /* update the blurred region */
      gimp_drawable_flush (param->drawable);
      gimp_drawable_merge_shadow (param->drawable_ID, TRUE);
      gimp_drawable_update (param->drawable_ID, x1, y1, width, height);
    }

  return TRUE;
}


#ifdef ENABLE_MP
void
focusblur_execute_thread (gpointer      data,
                          gpointer      user_data)
{
  FblurThreadSet        *thread_set = data;
  FblurParam            *param = user_data;

  focusblur_execute_region (param, thread_set->region, thread_set->number);
  g_atomic_int_add (thread_set->waiting, -1);
}
#endif /* ENABLE_MP */


static void
focusblur_execute_region (FblurParam    *param,
                          GimpPixelRgn  *region,
                          gint           number)
{
  guchar        *blp, *bp;
  gint           bpp = param->drawable->bpp;
  gint           rowstride = region->rowstride;
  gint           x1, x2, x;
  gint           y1, y2, y;

  blp = region->data;

  x1 = region->x;
  x2 = x1 + region->w;

  y1 = region->y;
  y2 = y1 + region->h;

#ifdef ENABLE_MP
  if (param->max_threads)
    {
      gint h, h0, y3;
      g_assert (number <= param->max_threads);

      h = (region->h + param->max_threads) / (param->max_threads + 1);
      h0 = number * h;
      y1 += h0;
      blp += h0 * rowstride;
      y3 = y1 + h;
      if (y3 < y2)
        y2 = y3;
    }
#endif

  for (y = y1; y < y2; y ++, blp += rowstride)
    for (x = x1, bp = blp; x < x2; x ++, bp += bpp)
      focusblur_render_pixel (x, y, bp, bpp, param);
}


static void
focusblur_render_pixel (gint             pos_x,
                        gint             pos_y,
                        guchar          *dest_pixel,
                        gint             dest_bpp,
                        FblurParam      *param)
{
  const gfloat   color_fnum = 1.0f / 255.0f;
  const gint     depth_ntables = FBLUR_DEPTH_MAX + 1;
  guchar         src_pixel[4];
  gfloat         distribution, val_shone, val_alpha;
  gfloat         sum_alpha, sum_pixel, sum_color[3];
  gfloat         assume_pixel;
  gfloat         tsum_alpha[depth_ntables], tsum_pixel[depth_ntables];
  gfloat         transit[depth_ntables], tsum_color[depth_ntables][3];
  gfloat         through;
  gfloat         separate_ratio;
  gboolean       enable_depth_precedence;
  gboolean       enable_depth_aaa, depth_aaa_do;
  gboolean       enable_shine;
  gint           shine;
  gint           depth, level;
  gint           depth_aaa_next_depth;
  gint           x1, x2, y1, y2;
  gint           x1c, x2c, y1c, y2c;
  gint           x, y, c, d;

  sum_alpha = 0.0f;
  sum_pixel = 0.0f;
  sum_color[0] = sum_color[1] = sum_color[2] = 0.0f;

  enable_depth_precedence = param->store.enable_depth_map &&
    param->store.enable_depth_precedence;
  enable_depth_aaa = param->store.enable_depth_map &&
    focusblur_depth_map_has_aaa (param->depth_map);

  depth_aaa_do = FALSE;
  separate_ratio = 0.0f;
  depth_aaa_next_depth = 0;

  if (enable_depth_precedence)
    {
      for (d = 0; d < depth_ntables; d ++)
        tsum_alpha[d] = 0.0f;
      for (d = 0; d < depth_ntables; d ++)
        tsum_pixel[d] = 0.0f;
      for (d = 0; d < depth_ntables; d ++)
        transit[d] = 0.0f;
      for (d = 0; d < depth_ntables; d ++)
        tsum_color[d][0] = tsum_color[d][1] = tsum_color[d][2] = 0.0f;
    }

  /* Sets farthest distance when depth map is not specified */
  depth = 0;
  level = FBLUR_DEPTH_MAX;
  enable_shine = focusblur_shine_check_enabled (&(param->store));

  x1 = pos_x - param->diffusion->model_radius_int;
  x2 = pos_x + param->diffusion->model_radius_int + 1;
  y1 = pos_y - param->diffusion->model_radius_int;
  y2 = pos_y + param->diffusion->model_radius_int + 1;

  cutoff_in_bound (param->source->x1, &x1, &x1c);
  cutoff_out_bound (param->source->x2, &x2, &x2c);
  cutoff_in_bound (param->source->y1, &y1, &y1c);
  cutoff_out_bound (param->source->y2, &y2, &y2c);

  for (y = y1; y < y2; y ++)
    for (x = x1; x < x2; x ++)
      {
        if (param->store.enable_depth_map)
          {
            depth = focusblur_depth_map_get_depth (param->depth_map, x, y);
          depth_aaa_next:
            level = focusblur_depth_map_get_level (param->depth_map, depth);
          }

        distribution = focusblur_diffusion_get
          (param->diffusion, level, pos_x, pos_y, x, y);

        if (distribution <= 0.0f)
          continue;

        focusblur_source_get (param->source, x, y, src_pixel);

        val_shone = distribution;
        if (enable_shine)
          {
            shine = focusblur_shine_get (param->shine, x, y);
            if (shine)
              {
                val_shone *= focusblur_diffusion_get_shine
                  (param->diffusion, level, shine);
              }
          }

        val_alpha = val_shone;
        if (param->source->has_alpha)
          {
            gfloat aval = color_fnum * src_pixel[param->source->channels]; 
            val_alpha *= aval;
            distribution *= aval; // for precedence
          }

        if (enable_depth_aaa && ! depth_aaa_do)
          separate_ratio = focusblur_depth_map_get_aaa
            (param->depth_map, x, y, &depth_aaa_next_depth);

        if (! separate_ratio)
          {
            if (! enable_depth_precedence)
              {
                for (c = 0; c < param->source->channels; c ++)
                  sum_color[c] += val_alpha * src_pixel[c];
                sum_alpha += val_alpha;
                sum_pixel += val_shone;
              }
            else
              {
                for (c = 0; c < param->source->channels; c ++)
                  tsum_color[depth][c] += val_alpha * src_pixel[c];
                tsum_alpha[depth] += val_alpha;
                tsum_pixel[depth] += val_shone;
                transit[depth] += distribution;
              }
          }
        /* separates anti-aliased pixel to tow values */
        else
          {
            separate_ratio = 1.0f - separate_ratio;
            g_assert (separate_ratio < 1.0f);
            g_assert (separate_ratio > 0.0f);

            if (! enable_depth_precedence)
              {
                for (c = 0; c < param->source->channels; c ++)
                  sum_color[c] += separate_ratio * val_alpha * src_pixel[c];
                sum_alpha += separate_ratio * val_alpha;
                sum_pixel += separate_ratio * val_shone;
              }
            else
              {
                for (c = 0; c < param->source->channels; c ++)
                  tsum_color[depth][c] += separate_ratio * val_alpha * src_pixel[c];
                tsum_alpha[depth] += separate_ratio * val_alpha;
                tsum_pixel[depth] += separate_ratio * val_shone;
                transit[depth] += separate_ratio * distribution;
              }

            if (! depth_aaa_do)
              {
                depth = depth_aaa_next_depth;
                depth_aaa_do = TRUE;
                goto depth_aaa_next;
              }
            depth_aaa_do = FALSE;
          }

      }

  if (enable_depth_precedence)
    {
      gdouble aval, fval;
      through = 1.0f;
      for (d = 0; d < depth_ntables; d ++)
        {
          if (transit[d] > 0.0f)
            {
              aval = transit[d];
              fval = MIN (aval, through);
              through -= fval;
              fval /= aval;
              sum_alpha += fval * tsum_alpha[d];
              sum_pixel += fval * tsum_pixel[d];
              for (c = 0; c < param->source->channels; c ++)
                sum_color[c] += fval * tsum_color[d][c];

              if (through < color_fnum)
                break;
            }
        }

      /* easy fake for out of frame */
      assume_pixel = sum_pixel;
      if (x1c || x2c || y1c || y2c)
        {
          gint inpix, outpix;
          gfloat fval;

          inpix = (x2 - x1) * (y2 - y1);
          outpix = (x1c + x2c) * (y2 - y1)
            + (y1c + y2c) * ((x2 + x2c) - (x1 - x1c));
          fval = (gfloat) outpix / inpix;

          assume_pixel *= 1.0f + fval;
        }

      /* fill behind */
      if (assume_pixel < 1.0f)
        {
          gint   depth_pos;
          gint   depth_nearest;
          gfloat depth_fuzzy;
          gfloat bsum_alpha, bsum_pixel, bsum_color[3];

          bsum_alpha = 0.0f;
          bsum_pixel = 0.0f;
          bsum_color[0] = bsum_color[1] = bsum_color[2] = 0.0f;

          depth_pos = focusblur_depth_map_get_depth
            (param->depth_map, pos_x, pos_y);
          level = focusblur_depth_map_get_level (param->depth_map, depth_pos);

          /* fuzzy depth */
          depth_fuzzy = 0.0f;
          depth_nearest = depth_pos;
          for (y = y1; y < y2; y ++)
            for (x = x1; x < x2; x ++)
              {
                distribution = focusblur_diffusion_get
                  (param->diffusion, level, pos_x, pos_y, x, y);

                if (distribution <= 0.0f)
                  continue;

                depth = focusblur_depth_map_get_depth (param->depth_map, x, y);
                if (depth < depth_nearest)
                  depth_nearest = depth;
              }
          if (depth_pos > depth_nearest)
            depth_fuzzy = 1.0f / (depth_pos - depth_nearest);

          for (y = y1; y < y2; y ++)
            for (x = x1; x < x2; x ++)
              {
                distribution = focusblur_diffusion_get
                  (param->diffusion, level, pos_x, pos_y, x, y);
                if (distribution <= 0.0f)
                  continue;

                depth = focusblur_depth_map_get_depth (param->depth_map, x, y);
                if (depth <= depth_pos)
                  {
                    if (depth_fuzzy)
                      distribution *= depth_fuzzy * (depth - depth_nearest);
                    else
                      continue;
                  }

                focusblur_source_get (param->source, x, y, src_pixel);

                val_alpha = distribution;
                if (param->source->has_alpha)
                  val_alpha *= color_fnum * src_pixel[param->source->channels];

                for (c = 0; c < param->source->channels; c ++)
                  bsum_color[c] += val_alpha * src_pixel[c];
                bsum_alpha += val_alpha;
                bsum_pixel += distribution;
              }

          if (bsum_pixel > color_fnum)
            {
              through = 1.0f - assume_pixel;
              through /= bsum_pixel;
              sum_alpha += through * bsum_alpha;
              sum_pixel += through * bsum_pixel;
              for (c = 0; c < param->source->channels; c ++)
                sum_color[c] += through * bsum_color[c];
            }
        }
    } /* end of depth precedence */

  if (! sum_pixel ||
      sum_alpha / sum_pixel < color_fnum)
    {
      for (c = 0; c < param->drawable->bpp; c ++)
        dest_pixel[c] = 0;
    }
  else
    {
      gfloat val;
      gint   col;
      for (c = 0; c < param->source->channels; c ++)
        {
          val = sum_color[c] / sum_alpha;
          col = rintf (val);
          dest_pixel[c] = CLAMP0255 (col);
        }
      if (param->source->has_alpha)
        {
          val = 255 * sum_alpha / sum_pixel;
          col = rintf (val);
          dest_pixel[param->source->channels] = CLAMP0255 (col);
        }
    }
}

