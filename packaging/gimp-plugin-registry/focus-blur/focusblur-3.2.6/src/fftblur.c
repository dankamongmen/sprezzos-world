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
#include <libgimp/gimpui.h>

#include "libgimp/stdplugins-intl.h"

#include "fftblur.h"
#include "fftblurbuffer.h"
#include "fftblurproc.h"

#include "focusblurparam.h"
#include "diffusion.h"
#include "depthmap.h"
#include "shine.h"


/*---- Types ----*/


/*---- Prototypes ----*/

static gboolean focusblur_fft_render            (FblurParam     *param,
                                                 gboolean        progress_bar);
static gboolean focusblur_fft_render_depth_map  (FblurParam     *param,
                                                 gboolean        progress_bar);
static gboolean focusblur_fft_render_depth_precedence (FblurParam *param,
                                                 gboolean        progress_bar);


/*---- Temporary structure for iterator ----*/

typedef struct _ConvertShineSet ConvertShineSet;

struct _ConvertShineSet
{
  FblurDiffusionTable   *diffusion;
  FblurShineData        *shine;
  gint                   level;
};


/*---- Functions for iterator ----*/

inline static gfloat
convf_shine_sub (ConvertShineSet *ss,
                 gint             x,
                 gint             y)
{
  gint s1 = focusblur_shine_get (ss->shine, x, y);
  return focusblur_diffusion_get_shine (ss->diffusion, ss->level, s1);
}

static void
convf_shine0 (gfloat    *workp,
              gint       x,
              gint       y,
              gpointer   data)
{
  *workp = convf_shine_sub ((ConvertShineSet *) data, x, y);
}

static void
convf_shinex (gfloat    *workp,
              gint       x,
              gint       y,
              gpointer   data)
{
  *workp *= convf_shine_sub ((ConvertShineSet *) data, x, y);
}

static void
convf_alpha0 (gfloat    *workp,
              guchar    *sourcep)
{
  const gfloat color_fnum = 1.0f / 255.0f;
  *workp = color_fnum * *sourcep;
}

static void
convf_alphax (gfloat    *workp,
              guchar    *sourcep)
{
  const gfloat color_fnum = 1.0f / 255.0f;
  *workp *= color_fnum * *sourcep;
}

static void
convf_color0 (gfloat    *workp,
              guchar    *sourcep)
{
  *workp = *sourcep;
}

static void
convf_colorx (gfloat    *workp,
              guchar    *sourcep)
{
  *workp *= *sourcep;
}

static void
setf_alphax (gfloat     *workp,
             gfloat     *realp,
             guchar     *sourcep)
{
  gint val = rintf (255.0f * *workp / *realp);
  *sourcep = CLAMP0255 (val);
}

static void
setf_alpha (gfloat      *dummy,
            gfloat      *realp,
            guchar      *sourcep)
{
  gint val = rintf (255.0f * *realp);
  *sourcep = CLAMP0255 (val);
}

static void
setf_colorx (gfloat     *workp,
             gfloat     *realp,
             guchar     *sourcep)
{
  gint val = rintf (*workp / *realp);
  *sourcep = CLAMP0255 (val);
}

static void
setf_color (gfloat      *workp,
            gfloat      *dummy,
            guchar      *sourcep)
{
  gint val = rintf (*workp);
  *sourcep = CLAMP0255 (val);
}

static void
convf_radd (gfloat      *workp,
            gfloat      *realp)
{
  *realp += *workp;
}

static void
convf_mul (gfloat       *workp,
           gfloat       *realp)
{
  *workp *= *realp;
}

static void
convf_transit0 (gfloat  *workp,
                gfloat  *realp)
{
  gfloat through;
  if (*workp > 0.0f &&
      *realp < 1.0f)
    {
      through = 1.0f - *realp;
      if (through >= *workp)
        *workp = 1.0f;
      else
        *workp = through / *workp;
    }
  else
    *workp = 0.0f;
}

static void
convf_fill0 (gfloat     *workp,
             gfloat     *realp)
{
  if (*workp > 0.0001f &&
      *realp > 0.0001f &&
      *realp < 1.0f)
    *workp = (1.0f - *realp) / *workp;
  else
    *workp = 0.0f;
}

static void
convf_fill5 (guchar     *sourcep,
             gint        bpp,
             gint        offset,
             gpointer    data)
{
  gfloat **sum = (gfloat **) data;
  gfloat fval;
  gint c;

  if (sum[5][offset] > 0.0f)
    {
      fval = sum[bpp][offset] / sum[5][offset];
      for (c = 0; c < bpp; c ++)
        sum[c][offset] *= fval;
    }
}

static void
setf_suma (guchar       *sourcep,
           gint          bpp,
           gint          offset,
           gpointer      data)
{
  gfloat **sum = (gfloat **) data;
  gint val, ch, c;

  ch = bpp - 1;
  if (sum[bpp][offset] > 0.0001f &&
      (val = rintf (255.0f * sum[ch][offset] / sum[bpp][offset]),
       val = CLAMP0255 (val)))
    {
      sourcep[ch] = val;
      for (c = 0; c < ch; c ++)
        {
          val = rintf (sum[c][offset] / sum[ch][offset]);
          sourcep[c] = CLAMP0255 (val);
        }
    }
  else
    for (c = 0; c < bpp; c ++)
      sourcep[c] = 0;
}

static void
setf_sum (guchar        *sourcep,
          gint           bpp,
          gint           offset,
          gpointer       data)
{
  gfloat **sum = (gfloat **) data;
  gint val, c;

  if (sum[bpp][offset] > 0.0001f)
    for (c = 0; c < bpp; c ++)
      {
        val = rintf (sum[c][offset] / sum[bpp][offset]);
        sourcep[c] = CLAMP0255 (val);
      }
  else
    for (c = 0; c < bpp; c ++)
      sourcep[c] = 0;
}


/*---- Functions ----*/


gboolean
focusblur_fft_execute (FblurParam       *param,
                       FblurQualityType quality,
                       GimpPreview      *preview)
{
  gboolean       progress_bar;
  gboolean       success;

  g_return_val_if_fail (param->store.model_radius > 0.0f, FALSE);

  if (! focusblur_fft_buffer_update (&(param->fft), param, quality, preview))
    return FALSE;

  progress_bar = preview ? FALSE : TRUE;

  if (progress_bar)
    {
      gimp_progress_init (_("Focus Blur..."));
      gimp_progress_update (0.0001);
    }

  /* with depth map */
  if (! param->store.enable_depth_map)
    success = focusblur_fft_render (param, progress_bar);
  else if (quality != FBLUR_QUALITY_DEFECTIVE &&
           param->store.enable_depth_precedence)
    success = focusblur_fft_render_depth_precedence (param, progress_bar);
  else
    success = focusblur_fft_render_depth_map (param, progress_bar);

  if (success)
    focusblur_fft_buffer_draw (param->fft);

  if (progress_bar)
    gimp_progress_update (1.0);

  focusblur_fft_work_free_buffers (param->fft);

  return success;
}


static gboolean
focusblur_fft_render (FblurParam        *param,
                      gboolean           progress_bar)
{
  FblurFftBuffer        *fft = param->fft;
  gint                   bpp = fft->source.bpp;
  gint                   channels = fft->source.channels;
  gboolean               has_alpha = fft->source.has_alpha;
  gboolean               enable_shine;

  gfloat                *alpha1 = NULL;
  gfloat                *shine0 = NULL;
  gfloat                *shine1 = NULL;
  const gint             level = FBLUR_DEPTH_MAX;
  gint                   progress = 0, full_progress = 0;
  gint                   c;

  g_assert (param->store.enable_depth_map == FALSE);

  enable_shine = focusblur_shine_check_enabled (&(param->store));

  if (progress_bar)
    {
      full_progress = 2;
      full_progress += bpp;
      if (enable_shine)
        full_progress ++;
    }

  if (has_alpha)
    {
      alpha1 = focusblur_fft_work_add_buffer (fft);
      if (! alpha1)
        return FALSE;
    }

  if (enable_shine)
    {
      if (has_alpha)
        {
          shine1 = focusblur_fft_work_add_buffer (fft);
          if (! shine1)
            return FALSE;
          /* alpha1 will be used instead of shine0 */
        }
      else
        {
          shine0 = focusblur_fft_work_add_buffer (fft);
          shine1 = focusblur_fft_work_add_buffer (fft);
          if (! shine0 || ! shine1 )
            return FALSE;
        }
    }

  focusblur_fft_buffer_make_kernel (fft, param->diffusion, level);

  if (progress_bar)
    gimp_progress_update ((gdouble) ++ progress / full_progress);

  /* has alpha, with shine */
  if (has_alpha)
    if (enable_shine)
      {
        gfloat *alpha0;
        ConvertShineSet shine_set = { param->diffusion, param->shine, level };

        /* shine */
        focusblur_fft_convert_coords2work (fft, convf_shine0, &shine_set);
        focusblur_fft_work_treat_outside (fft);

        focusblur_fft_work_store (fft, alpha1); /* as shine0 */
        focusblur_fft_work_apply (fft);
        focusblur_fft_work_store (fft, shine1);
        if (progress_bar)
          gimp_progress_update ((gdouble) ++ progress / full_progress);

        /* alpha */
        focusblur_fft_work_restore (fft, alpha1);
        focusblur_fft_convert_source2work (fft, convf_alphax, channels);
        focusblur_fft_work_treat_outside (fft);

        focusblur_fft_work_store (fft, alpha1); /* as alpha0 */
        focusblur_fft_work_apply (fft);
        if (progress_bar)
          gimp_progress_update ((gdouble) ++ progress / full_progress);

        focusblur_fft_convert_work2source (fft, shine1, setf_alphax, channels);

        /* renameing for code reading */
        alpha0 = alpha1;
        alpha1 = shine1;
        //shine1 = alpha0; pointers are in list for freed.
        focusblur_fft_work_store (fft, alpha1);

        /* color */
        for (c = 0; c < channels; c ++)
          {
            focusblur_fft_work_restore (fft, alpha0);
            focusblur_fft_convert_source2work (fft, convf_colorx, c);
            focusblur_fft_work_treat_outside (fft);

            focusblur_fft_work_apply (fft);
            if (progress_bar)
              gimp_progress_update ((gdouble) ++ progress / full_progress);

            focusblur_fft_convert_work2source (fft, alpha1, setf_colorx, c);
          }
      }

  /* has alpha, withwout shine */
    else
      {
        focusblur_fft_convert_source2work (fft, convf_alpha0, channels);
        focusblur_fft_work_treat_outside (fft);

        focusblur_fft_work_apply (fft);
        focusblur_fft_work_store (fft, alpha1);

        if (progress_bar)
          gimp_progress_update ((gdouble) ++ progress / full_progress);

        for (c = 0; c < channels; c ++)
          {
            focusblur_fft_convert_source2work (fft, convf_alpha0, c);
            focusblur_fft_convert_source2work (fft, convf_colorx, channels);
            focusblur_fft_work_treat_outside (fft);

            focusblur_fft_work_apply (fft);
            if (progress_bar)
              gimp_progress_update ((gdouble) ++ progress / full_progress);

            focusblur_fft_convert_work2source (fft, alpha1, setf_colorx, c);
          }

        focusblur_fft_convert_work2source (fft, alpha1, setf_alpha, channels);
      }

  /* no alpha, with shine */
  else
    if (enable_shine)
      {
        ConvertShineSet shine_set = { param->diffusion, param->shine, level };

        focusblur_fft_convert_coords2work (fft, convf_shine0, &shine_set);
        focusblur_fft_work_treat_outside (fft);

        focusblur_fft_work_store (fft, shine0);
        focusblur_fft_work_apply (fft);
        focusblur_fft_work_store (fft, shine1);
        if (progress_bar)
          gimp_progress_update ((gdouble) ++ progress / full_progress);

        for (c = 0; c < channels; c ++)
          {
            focusblur_fft_work_restore (fft, shine0);
            focusblur_fft_convert_source2work (fft, convf_colorx, c);
            focusblur_fft_work_treat_outside (fft);

            focusblur_fft_work_apply (fft);
            if (progress_bar)
              gimp_progress_update ((gdouble) ++ progress / full_progress);

            focusblur_fft_convert_work2source (fft, shine1, setf_colorx, c);
          }
      }

  /* no alpha, without shine */
    else
      {
        for (c = 0; c < channels; c ++)
          {
            focusblur_fft_convert_source2work (fft, convf_color0, c);
            focusblur_fft_work_treat_outside (fft);

            focusblur_fft_work_apply (fft);
            if (progress_bar)
              gimp_progress_update ((gdouble) ++ progress / full_progress);

            focusblur_fft_convert_work2source (fft, NULL, setf_color, c);
          }
      }

  return TRUE;
}


static gboolean
focusblur_fft_render_depth_map (FblurParam      *param,
                                gboolean         progress_bar)
{
  FblurFftBuffer        *fft = param->fft;
  gint                   bpp = fft->source.bpp;
  gint                   channels = fft->source.channels;
  gboolean               has_alpha = fft->source.has_alpha;
  gboolean               enable_shine;

  gfloat                *slice0 = NULL;
  gfloat                *sum[5] = { NULL, NULL, NULL, NULL, NULL };
  gfloat                *alpha0 = NULL;
  gint                   depth, depth_count, level;
  gint                   progress = 0, full_progress = 0;
  gint                   c;

  g_assert (param->store.enable_depth_map == TRUE);

  enable_shine = focusblur_shine_check_enabled (&(param->store));

  if (progress_bar)
    full_progress = 2 + fft->depth.count;

  slice0 = focusblur_fft_work_add_buffer (fft);
  if (! slice0)
    return FALSE;

  for (c = 0; c <= bpp; c ++)
    {
      sum[c] = focusblur_fft_work_add_buffer_zero (fft);
      if (! sum[c])
        return FALSE;
    }

  if (has_alpha)
    {
      alpha0 = focusblur_fft_work_add_buffer (fft);
      if (! alpha0)
        return FALSE;
    }

  for (depth = 0, depth_count = fft->depth.count; depth_count --; depth ++)
    {
      while (! fft->depth.check[depth])
        depth ++;

      level = focusblur_depth_map_get_level (param->depth_map, depth);
      focusblur_fft_buffer_make_kernel (fft, param->diffusion, level);
      focusblur_fft_buffer_make_depth_slice (fft, param->depth_map, depth);

      if (enable_shine)
        {
          ConvertShineSet shine_set =
            { param->diffusion, param->shine, level };

          focusblur_fft_convert_coords2work (fft, convf_shinex, &shine_set);
        }
      focusblur_fft_work_store (fft, slice0);

      if (has_alpha)
        {
          focusblur_fft_convert_source2work (fft, convf_alphax, channels);
          focusblur_fft_work_store (fft, alpha0);

          for (c = 0; c < channels; c ++)
            {
              focusblur_fft_convert_source2work (fft, convf_colorx, c);
              focusblur_fft_work_apply (fft);
              focusblur_fft_convert_work (fft, convf_radd, sum[c]);
              focusblur_fft_work_restore (fft, alpha0);
            }

          focusblur_fft_work_apply (fft);
          focusblur_fft_convert_work (fft, convf_radd, sum[channels]);
          focusblur_fft_work_restore (fft, slice0);
        }
      else
        {
          for (c = 0; c < channels; c ++)
            {
              focusblur_fft_convert_source2work (fft, convf_colorx, c);
              focusblur_fft_work_apply (fft);
              focusblur_fft_convert_work (fft, convf_radd, sum[c]);
              focusblur_fft_work_restore (fft, slice0);
            }
        }

      focusblur_fft_work_apply (fft);
      focusblur_fft_convert_work (fft, convf_radd, sum[bpp]);

      if (progress_bar)
        gimp_progress_update ((gdouble) ++ progress / full_progress);
    }

  /* finish */
  if (has_alpha)
    focusblur_fft_convert_source (fft, setf_suma, sum);
  else
    focusblur_fft_convert_source (fft, setf_sum, sum);

  if (progress_bar)
    gimp_progress_update ((gdouble) ++ progress / full_progress);

  return TRUE;
}


static gboolean
focusblur_fft_render_depth_precedence (FblurParam       *param,
                                       gboolean  progress_bar)
{
  FblurFftBuffer        *fft = param->fft;
  gint                   bpp = fft->source.bpp;
  gint                   channels = fft->source.channels;
  gboolean               has_alpha = fft->source.has_alpha;
  gboolean               enable_shine;

  gfloat                *transit = NULL;
  gfloat                *slice0 = NULL, *slice1 = NULL;
  gfloat                *sum[7] = { NULL, NULL, NULL, NULL, NULL, NULL, NULL };
  gint                   depth, depth_count, level;
  gint                   progress = 0, full_progress = 0;
  gint                   c;

  g_assert (param->store.enable_depth_map == TRUE);
  g_assert (param->store.enable_depth_precedence == TRUE);

  enable_shine = focusblur_shine_check_enabled (&(param->store));

  if (progress_bar)
    full_progress = 2 + fft->depth.count;
  /* fill behind */
    full_progress += 1;

  transit = focusblur_fft_work_add_buffer (fft);
  if (! transit)
    return FALSE;

  slice0 = focusblur_fft_work_add_buffer (fft);
  slice1 = focusblur_fft_work_add_buffer (fft);
  if (! slice0 || ! slice1)
    return FALSE;

  for (c = 0; c <= bpp; c ++)
    {
      sum[c] = focusblur_fft_work_add_buffer_zero (fft);
      if (! sum[c])
        return FALSE;
    }

  if (enable_shine)
    {
      sum[5] = focusblur_fft_work_add_buffer_zero (fft);
      if (! sum[5])
        return FALSE;

      if (has_alpha)
        {
          sum[6] = focusblur_fft_work_add_buffer_zero (fft);
          if (! sum[6])
            return FALSE;
        }
    }

  for (depth = 0, depth_count = fft->depth.count; depth_count --; depth ++)
    {
      while (! fft->depth.check[depth])
        depth ++;

      level = focusblur_depth_map_get_level (param->depth_map, depth);
      focusblur_fft_buffer_make_kernel (fft, param->diffusion, level);
      focusblur_fft_buffer_make_depth_slice (fft, param->depth_map, depth);

      /* make transit value */

      focusblur_fft_work_treat_outside (fft);
      focusblur_fft_work_store (fft, slice0);
      focusblur_fft_work_apply (fft);
      focusblur_fft_work_store (fft, slice1);

      if (has_alpha)
        {
          focusblur_fft_work_restore (fft, slice0);
          focusblur_fft_convert_source2work (fft, convf_alphax, channels);
          focusblur_fft_work_treat_outside (fft);

          if (! enable_shine)
            focusblur_fft_work_store (fft, slice0);

          focusblur_fft_work_apply (fft);
          focusblur_fft_convert_work (fft, convf_transit0, sum[channels]);
        }
      else
        {
          focusblur_fft_convert_work (fft, convf_transit0, sum[bpp]);
        }
      focusblur_fft_work_store (fft, transit);

      focusblur_fft_convert_work (fft, convf_mul, slice1);
      focusblur_fft_convert_work (fft, convf_radd, sum[bpp]);

      if (has_alpha)
        {
          focusblur_fft_work_restore (fft, slice0);
          //focusblur_fft_work_treat_outside (fft);
          focusblur_fft_work_apply (fft);
          focusblur_fft_convert_work (fft, convf_mul, transit);
          focusblur_fft_convert_work (fft, convf_radd, sum[channels]);
        }

      if (enable_shine)
        {
          ConvertShineSet shine_set =
            { param->diffusion, param->shine, level };

          focusblur_fft_work_restore (fft, slice0);
          focusblur_fft_convert_coords2work (fft, convf_shinex, &shine_set);
          focusblur_fft_work_treat_outside (fft);
          focusblur_fft_work_store (fft, slice0);
          focusblur_fft_work_apply (fft);
          focusblur_fft_convert_work (fft, convf_mul, transit);
          focusblur_fft_convert_work (fft, convf_radd, sum[5]);

          if (has_alpha)
            {
              focusblur_fft_work_restore (fft, slice0);
              focusblur_fft_convert_source2work (fft, convf_alphax, channels);
              focusblur_fft_work_treat_outside (fft);
              focusblur_fft_work_store (fft, slice0);
              focusblur_fft_work_apply (fft);
              focusblur_fft_convert_work (fft, convf_mul, transit);
              focusblur_fft_convert_work (fft, convf_radd, sum[6]);
            }
        }

      for (c = 0; c < channels; c ++)
        {
          focusblur_fft_work_restore (fft, slice0);
          focusblur_fft_convert_source2work (fft, convf_colorx, c);
          focusblur_fft_work_treat_outside (fft);
          focusblur_fft_work_apply (fft);
          focusblur_fft_convert_work (fft, convf_mul, transit);
          focusblur_fft_convert_work (fft, convf_radd, sum[c]);
        }

      if (progress_bar)
        gimp_progress_update ((gdouble) ++ progress / full_progress);
    }

  if (enable_shine &&
      has_alpha)
    focusblur_fft_work_copy (fft, sum[channels], sum[6]);

  /* fill behind */
  level = FBLUR_DEPTH_MAX;
  focusblur_fft_buffer_make_kernel (fft, param->diffusion, level);

  /* this is imperfect */
  focusblur_fft_buffer_make_depth_behind (fft, param->depth_map);

  focusblur_fft_work_treat_outside (fft);
  focusblur_fft_work_store (fft, slice0);

  focusblur_fft_work_apply (fft);
  focusblur_fft_work_store (fft, slice1);

  if (has_alpha)
    /* need to more tests */
    focusblur_fft_convert_work (fft, convf_fill0, sum[channels]);
  else
    focusblur_fft_convert_work (fft, convf_fill0, sum[bpp]);

  focusblur_fft_work_store (fft, transit);

  focusblur_fft_work_restore (fft, slice1);
  focusblur_fft_convert_work (fft, convf_mul, transit);
  focusblur_fft_convert_work (fft, convf_radd, sum[bpp]);

  if (enable_shine)
    {
      focusblur_fft_convert_work (fft, convf_radd, sum[5]);
    }

  if (has_alpha)
    {
      focusblur_fft_work_restore (fft, slice0);
      focusblur_fft_convert_source2work (fft, convf_alphax, channels);
      focusblur_fft_work_treat_outside (fft);
      focusblur_fft_work_store (fft, slice0);
      focusblur_fft_work_apply (fft);
      focusblur_fft_convert_work (fft, convf_mul, transit);
      focusblur_fft_convert_work (fft, convf_radd, sum[channels]);
    }

  for (c = 0; c < channels; c ++)
    {
      focusblur_fft_work_restore (fft, slice0);
      focusblur_fft_convert_source2work (fft, convf_colorx, c);
      focusblur_fft_work_treat_outside (fft);
      focusblur_fft_work_apply (fft);
      focusblur_fft_convert_work (fft, convf_mul, transit);
      focusblur_fft_convert_work (fft, convf_radd, sum[c]);
    }

  /* end of fill behind */
  if (progress_bar)
    gimp_progress_update ((gdouble) ++ progress / full_progress);

  if (enable_shine)
    focusblur_fft_convert_source (fft, convf_fill5, sum);

  if (has_alpha)
    focusblur_fft_convert_source (fft, setf_suma, sum);
  else
    focusblur_fft_convert_source (fft, setf_sum, sum);

  if (progress_bar)
    gimp_progress_update ((gdouble) ++ progress / full_progress);

  return TRUE;
}
