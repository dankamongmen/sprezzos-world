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

#include <string.h>
#include <stdlib.h>
#ifdef HAVE_COMPLEX_H
#  include <complex.h>
#  include <math.h>
#endif
#include <fftw3.h>

#include <glib.h>

#include "fftblurbuffer.h"
#include "fftblurproc.h"


/*---- Prototypes ----*/

static void     focusblur_fft_real_fill         (gfloat         *dest,
                                                 gfloat          value,
                                                 gsize           count);
static void     focusblur_fft_real_copy         (gfloat         *dest,
                                                 gfloat         *source,
                                                 gsize           count);
static void     focusblur_fft_real_treat_outside (gfloat        *workbuf,
                                                  gint           origin,
                                                  gint           space,
                                                  gint           insiderow,
                                                  gint           insidecol,
                                                  gint           rowstride);
static void     focusblur_fft_complex_mul       (fftwf_complex  *dest,
                                                 fftwf_complex  *source,
                                                 gint            count);


/*---- Functions ----*/


gfloat*
focusblur_fft_work_add_buffer (FblurFftBuffer *fft)
{
  gfloat* buf;

  buf = fftwf_malloc (fft->work.size);

  if (buf)
    fft->work.buffers = g_slist_append (fft->work.buffers, buf);

  return buf;
}


gfloat*
focusblur_fft_work_add_buffer_zero (FblurFftBuffer *fft)

{
  gfloat *buf;

  buf = focusblur_fft_work_add_buffer (fft);

  if (buf)
    focusblur_fft_real_fill (buf, 0.0f, fft->work.nelements);

  return buf;
}


void
focusblur_fft_work_free_buffers (FblurFftBuffer *fft)
{
  GSList *list, *next;

  for (list = fft->work.buffers; list; list = next)
    {
      next = list->next;

      if (list->data)
        fftwf_free (list->data);

      g_slist_free_1 (list);
    }

  fft->work.buffers = NULL;
}

void
focusblur_fft_work_store_in_kernel (FblurFftBuffer *fft)
{
  focusblur_fft_real_copy ((gfloat *) fft->work.kernel,
                           (gfloat *) fft->work.image,
                           fft->work.nelements);
}


void
focusblur_fft_work_fill_zero (FblurFftBuffer    *fft)
{
  focusblur_fft_real_fill ((gfloat *) fft->work.image, 0.0f,
                           fft->work.nelements);
}


void
focusblur_fft_work_store (FblurFftBuffer             *fft,
                          gfloat                     *buf)
{
  focusblur_fft_real_copy (buf, (gfloat *) fft->work.image,
                           fft->work.nelements);
}


void
focusblur_fft_work_restore (FblurFftBuffer           *fft,
                            gfloat                   *buf)
{
  focusblur_fft_real_copy ((gfloat *) fft->work.image, buf,
                           fft->work.nelements);
}


void
focusblur_fft_work_copy (FblurFftBuffer      *fft,
                         gfloat              *dest,
                         gfloat              *source)
{
  focusblur_fft_real_copy (dest, source, fft->work.nelements);
}


void
focusblur_fft_work_treat_outside (FblurFftBuffer        *fft)
{
  /* width and height are turned in real buffer */
  focusblur_fft_real_treat_outside ((gfloat *) fft->work.image,
                                    fft->work.origin, fft->work.space,
                                    fft->source.width, fft->source.height,
                                    fft->work.col_padded);
}


void
focusblur_fft_work_apply (FblurFftBuffer        *fft)
{
  if (! fft->work.level)
    return;

  fftwf_execute (fft->work.plan_r2c);
  focusblur_fft_complex_mul (fft->work.image, fft->work.kernel,
                             fft->work.complex_nelements);
  fftwf_execute (fft->work.plan_c2r);
}


static void
focusblur_fft_complex_mul (fftwf_complex        *dest,
                           fftwf_complex        *source,
                           gint                  count)
{
  gint n;

  for (n = 0; n < count; n ++)
    {
#ifdef HAVE_COMPLEX_H
      /* C99 */
      dest[n] *= source[n];
#else
      float r, i;
      r = dest[n][0] * source[n][0] - dest[n][1] * source[n][1];
      i = dest[n][0] * source[n][1] + dest[n][1] * source[n][0];
      dest[n][0] = r;
      dest[n][1] = i;
#endif
    }
}


/*---- Iterator ----*/

void
focusblur_fft_convert_source2work (FblurFftBuffer      *fft,
                                   FblurFftConvertFunc  func,
                                   gint                 channel)
{
  gfloat        *work;
  guchar        *sp, *slp;
  gint           offset_line, offset;
  gint           x, y;

  g_assert (channel < fft->source.bpp);

  work = (gfloat *) fft->work.image;
  slp = fft->source.data + channel;
  offset_line = fft->work.origin;

  for (x = fft->source.x1; x < fft->source.x2; x ++)
    {
      offset = offset_line;
      sp = slp;
      for (y = fft->source.y1; y < fft->source.y2; y ++)
        {
          func (&(work[offset]), sp);

          offset ++;
          sp += fft->source.rowstride;
        }
      offset_line += fft->work.col_padded;
      slp += fft->source.bpp;
    }

}


void
focusblur_fft_convert_coords2work (FblurFftBuffer               *fft,
                                   FblurFftConvertFuncCoords     func,
                                   gpointer                      data)
{
  gfloat        *wlp, *wp;
  gint           x, y;

  wlp = (gfloat *) fft->work.image + fft->work.origin;

  for (y = fft->source.y1; y < fft->source.y2; y ++)
    {
      wp = wlp;
      for (x = fft->source.x1; x < fft->source.x2; x ++)
        {
          func (wp, x, y, data);

          wp += fft->work.col_padded;
        }
      wlp ++;
    }
}


void
focusblur_fft_convert_work2source (FblurFftBuffer               *fft,
                                   gfloat                       *real,
                                   FblurFftConvertFuncMix        func,
                                   gint                          channel)
{
  gfloat        *work;
  guchar        *sp, *slp;
  gint           offset_line, offset;
  gint           x, y;

  g_assert (channel < fft->source.bpp);

  work = (gfloat *) fft->work.image;
  offset_line = fft->work.origin;

  if (fft->source.data_preview)
    slp = fft->source.data_preview + channel;
  else
    slp = fft->source.data + channel;

  for (x = fft->source.x1; x < fft->source.x2; x ++)
    {
      offset = offset_line;
      sp = slp;
      for (y = fft->source.y1; y < fft->source.y2; y ++)
        {
          func (&(work[offset]), &(real[offset]), sp);

          offset ++;
          sp += fft->source.rowstride;
        }
      offset_line += fft->work.col_padded;
      slp += fft->source.bpp;
    }
}


void
focusblur_fft_convert_work (FblurFftBuffer          *fft,
                            FblurFftConvertFuncWork  func,
                            gfloat                  *real)
{
  gfloat        *work;
  gint           offset_line, offset;
  gint           x, y;

  work = (gfloat *) fft->work.image;
  offset_line = fft->work.origin;

  for (x = fft->source.x1; x < fft->source.x2; x ++)
    {
      offset = offset_line;
      for (y = fft->source.y1; y < fft->source.y2; y ++)
        {
          func (&(work[offset]), &(real[offset]));

          offset ++;
        }
      offset_line += fft->work.col_padded;
    }
}


void
focusblur_fft_convert_source (FblurFftBuffer            *fft,
                              FblurFftConvertFuncSource  func,
                              gpointer                   data)
{
  guchar        *sp;
  gint           offset_line, offset;
  gint           x, y;

  offset_line = fft->work.origin;
  if (fft->source.data_preview)
    sp = fft->source.data_preview;
  else
    sp = fft->source.data;

  for (y = fft->source.y1; y < fft->source.y2; y ++)
    {
      offset = offset_line;
      for (x = fft->source.x1; x < fft->source.x2; x ++)
        {
          func (sp, fft->source.bpp, offset, data);

          offset += fft->work.col_padded;
          sp += fft->source.bpp;
        }
      offset_line ++;
    }
}


/*---- memset, memcpy ----*/

static void
focusblur_fft_real_fill (gfloat *dest,
                         gfloat  value,
                         gsize   count)
{
  while (count --)
    *dest ++ = value;
}


static void
focusblur_fft_real_copy (gfloat *dest,
                         gfloat *source,
                         gsize   count)
{
  while (count --)
    *dest ++ = *source ++;
}


static void
focusblur_fft_real_treat_outside (gfloat        *real,
                                  gint           origin,
                                  gint           space,
                                  gint           insiderow,
                                  gint           insidecol,
                                  gint           rowstride)
{
  gfloat        *op, *rp;
  gint           leftrow, leftcol;
  gint           copyrow, copycol;
  gsize          linesize;
  gint           row, col;

  linesize = sizeof (gfloat) * rowstride;

  op = real + origin;
  leftrow = leftcol = space;

  while (leftrow || leftcol)
    {
      copyrow = MIN (leftrow, insiderow);
      copycol = MIN (leftcol, insidecol);

      rp = op;

      for (row = 0; row < insiderow; row ++, rp += rowstride)
        for (col = 0; col < copycol; col ++)
          {
            rp[- (col + 1)] = rp[col];
            rp[insidecol + col] = rp[insidecol - (col + 1)];
          }

      op -= copycol;
      rp = op;

      for (row = 0; row < copyrow; row ++)
        {
          memcpy (&(rp[rowstride * -(row + 1)]),
                  &(rp[rowstride * row]), linesize);
          memcpy (&(rp[rowstride * (insiderow + row)]),
                  &(rp[rowstride * (insiderow - (row + 1))]), linesize);
        }

      op -= copyrow * rowstride;

      leftrow -= copyrow;
      leftcol -= copycol;

      insiderow += copyrow + copyrow;
      insidecol += copycol + copycol;
    }
}
