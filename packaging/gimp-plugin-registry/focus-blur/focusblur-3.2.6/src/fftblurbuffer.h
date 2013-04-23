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

#ifndef __FOCUSBLUR_FFTBLUR_BUFFER_H__
#define __FOCUSBLUR_FFTBLUR_BUFFER_H__

#ifdef HAVE_COMPLEX_H
#  include <complex.h>
#  include <math.h>
#endif
#include <fftw3.h>

#include <glib/gmacros.h>
#include <glib/gtypes.h>
#include <gtk/gtkstyle.h>
#include <libgimp/gimptypes.h>
#include <libgimpwidgets/gimpwidgetstypes.h>

#include "focusblur.h"
#include "focusblurtypes.h"
#include "focusblurenums.h"


G_BEGIN_DECLS


/*---- Types ----*/

typedef struct _FblurFftSource          FblurFftSource;
typedef struct _FblurFftWork            FblurFftWork;
typedef struct _FblurFftDepth           FblurFftDepth;
typedef struct _FblurFftDepthTable      FblurFftDepthTable;


/*---- Structure ----*/

struct _FblurFftBuffer
{
  /* for source image */
  struct _FblurFftSource
  {
    GimpDrawable        *drawable;
    GimpPreview         *preview;

    gint                 bpp;
    gint                 rowstride;
    gint                 channels;
    gboolean             has_alpha;
    gint                 x1, x2;
    gint                 y1, y2;
    gint                 width, height;

    gsize                size;
    guchar              *data;
    guchar              *data_preview;
  } source;

  /* for fftw works */
  struct _FblurFftWork
  {
    gint                 row, col;
    gint                 col_padded;
    gint                 nelements;
    gint                 complex_nelements;

    gsize                size;
    GSList              *buffers;
    fftwf_complex       *image;
    fftwf_complex       *kernel;
    fftwf_plan           plan_r2c;
    fftwf_plan           plan_c2r;

    gint                 space;
    gint                 origin;
    gint                 level;
  } work;

  /* for depth map */
  struct _FblurFftDepth
  {
    FblurQualityType     quality;

    gint                 division;
    gint                 slide;

    struct _FblurFftDepthTable
    {
      gint               round;
      gint               floor;
      gint               ceil;
      gfloat             diff;
    } table[FBLUR_DEPTH_MAX + 1];

    gint                 count;
    gboolean             check[FBLUR_DEPTH_MAX + 1];
  } depth;
};


/*---- Functions for structure ----*/

gboolean focusblur_fft_buffer_update            (FblurFftBuffer **fft,
                                                 FblurParam      *param,
                                                 FblurQualityType quality,
                                                 GimpPreview     *preview);
void    focusblur_fft_buffer_destroy            (FblurFftBuffer **fft);
void    focusblur_fft_buffer_draw               (FblurFftBuffer  *fft);

void    focusblur_fft_buffer_make_kernel        (FblurFftBuffer  *fft,
                                                 FblurDiffusionTable *diffusio,
                                                 gint             level);
void    focusblur_fft_buffer_make_depth_slice   (FblurFftBuffer  *fft,
                                                 FblurDepthMap   *depth_map,
                                                 gint             look);
void    focusblur_fft_buffer_make_depth_behind  (FblurFftBuffer  *fft,
                                                 FblurDepthMap   *depth_map);

void    focusblur_fft_buffer_invalidate_depth_map (FblurFftBuffer  *fft);
void    focusblur_fft_buffer_invalidate_diffusion (FblurFftBuffer  *fft);


G_END_DECLS


#endif /* __FOCUSBLUR_FFTBLUR_BUFFER_H__ */
