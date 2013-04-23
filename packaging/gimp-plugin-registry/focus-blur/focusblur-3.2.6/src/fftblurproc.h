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

#ifndef __FOCUSBLUR_FFTBLUR_PROC_H__
#define __FOCUSBLUR_FFTBLUR_PROC_H__


#include <glib/gmacros.h>
#include <glib/gtypes.h>

#include "focusblurtypes.h"


G_BEGIN_DECLS


/*---- Types ----*/

typedef void (*FblurFftConvertFunc)             (gfloat         *workp,
                                                 guchar         *sourcep);

typedef void (*FblurFftConvertFuncCoords)       (gfloat         *workp,
                                                 gint            x,
                                                 gint            y,
                                                 gpointer        data);

typedef void (*FblurFftConvertFuncMix)          (gfloat         *workp,
                                                 gfloat         *realp,
                                                 guchar         *sourcep);

typedef void (*FblurFftConvertFuncWork)         (gfloat         *workp,
                                                 gfloat         *realp);

typedef void (*FblurFftConvertFuncSource)       (guchar         *sourcep,
                                                 gint            bpp,
                                                 gint            offset,
                                                 gpointer        data);


/*---- Functions for processing ----*/

gfloat* focusblur_fft_work_add_buffer           (FblurFftBuffer      *fft);
gfloat* focusblur_fft_work_add_buffer_zero      (FblurFftBuffer      *fft);
void    focusblur_fft_work_free_buffers         (FblurFftBuffer      *fft);
void    focusblur_fft_work_store_in_kernel      (FblurFftBuffer      *fft);
void    focusblur_fft_work_fill_zero            (FblurFftBuffer      *fft);
void    focusblur_fft_work_store                (FblurFftBuffer      *fft,
                                                 gfloat              *buf);
void    focusblur_fft_work_restore              (FblurFftBuffer      *fft,
                                                 gfloat              *buf);
void    focusblur_fft_work_copy                 (FblurFftBuffer      *fft,
                                                 gfloat              *dest,
                                                 gfloat              *source);
void    focusblur_fft_work_treat_outside        (FblurFftBuffer      *fft);
void    focusblur_fft_work_apply                (FblurFftBuffer      *fft);

void    focusblur_fft_convert_source2work       (FblurFftBuffer      *fft,
                                                 FblurFftConvertFunc  func,
                                                 gint                 channel);
void    focusblur_fft_convert_coords2work       (FblurFftBuffer      *fft,
                                            FblurFftConvertFuncCoords func,
                                                 gpointer             data);
void    focusblur_fft_convert_work2source       (FblurFftBuffer      *fft,
                                                 gfloat              *real,
                                               FblurFftConvertFuncMix func,
                                                 gint                 channel);
void    focusblur_fft_convert_work              (FblurFftBuffer      *fft,
                                              FblurFftConvertFuncWork func,
                                                 gfloat              *real);
void    focusblur_fft_convert_source            (FblurFftBuffer      *fft,
                                            FblurFftConvertFuncSource func,
                                                 gpointer             data);

G_END_DECLS


#endif /* __FOCUSBLUR_FFTBLUR_PROC_H__ */
