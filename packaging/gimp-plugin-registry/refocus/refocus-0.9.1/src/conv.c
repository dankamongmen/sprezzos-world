/* Refocus plug-in
 * Copyright (C) 1999-2003 Ernst Lippe
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
 *
 * Version $Id: conv.c,v 1.1.1.1 2003/01/30 21:30:18 ernstl Exp $
 */

#include <stdlib.h>
#include <string.h>
#include <libgimp/gimp.h>
#include <math.h>
#include "refocus.h"
#include "util.h"
#include "tilebuf.h"
#include "conv.h"

#ifndef lint
static char vcid[] GCC_UNUSED = "$Id: conv.c,v 1.1.1.1 2003/01/30 21:30:18 ernstl Exp $";
#endif /* lint */

#if 0
                                /* This is the original version */
void
convolve_tile (GimpTile * tile, const guchar * const buf,
               const gint buf_row_width, const gint ncolors, const gint bpp,
               const MATRIX * const mat, const gint mat_size)
{
  const gint mat_offset = mat_size / 2;
  const gint eheight = tile->eheight;
  const gint ewidth = tile->ewidth;
  const gboolean has_alpha = bpp > ncolors;
  gint x1, y1, x2, y2, color;

  for (y1 = eheight - 1; y1 >= 0; y1--)
    {
      for (x1 = ewidth - 1; x1 >= 0; x1--)
        {
          for (color = ncolors - 1; color >= 0; color--)
            {
              REAL val = 0.0;
              for (y2 = mat_size - 1; y2 >= 0; y2--)
                {
                  for (x2 = mat_size - 1; x2 >= 0; x2--)
                    {
                      val += (*mat)[y2][x2] *
                        buf[buf_row_width * (y1 + y2 - mat_offset) +
                            bpp * (x1 + x2 - mat_offset) + color];
                    };
                };
              /* Write value to tile */
              tile->data[(y1 * ewidth + x1) * bpp + color] =
                (guchar) CLAMP (val, 0, 255);
            };
          if (has_alpha)
            {                   /*  copy alpha */
              tile->data[(y1 * ewidth + x1) * bpp + ncolors] =
                buf[buf_row_width * y1 + bpp * x1 + ncolors];
            };
        };
    };
}
#endif

void
convolve_tile_grey (GimpTile * tile, const guchar * const buf,
                    const gint buf_row_width,
                    const REAL * const mat, const gint mat_size)
{
  const gint bpp = 1;
  const gint mat_offset = mat_size / 2;
  const gint eheight = tile->eheight;
  const gint ewidth = tile->ewidth;
  const gint env_offset = (buf_row_width + bpp) * mat_offset;
  const gint bp_stride = buf_row_width - mat_size * bpp;
  const gint sp_stride = buf_row_width - ewidth * bpp;
  register gint x1, y1, x2, y2;
  register guchar *tp = tile->data;
  register guchar const *sp = buf;

  for (y1 = eheight; y1 > 0; y1--)
    {
      for (x1 = ewidth; x1 > 0; x1--)
        {
          register guchar const *bp = sp - env_offset;
          register REAL const *mp = mat;
          register REAL val = 0.0;

          for (y2 = mat_size; y2 > 0; y2--)
            {
              for (x2 = mat_size; x2 > 0; x2--)
                {
                  val += *mp * *bp;
                  mp++;
                  bp += bpp;
                };
              bp += bp_stride;
            };
          /* Write value to tile */
          *tp = (guchar) CLAMP (val, 0, 255);
          tp++;
          sp++;
        };
      sp += sp_stride;
    };
}

void
convolve_tile_grey_alpha (GimpTile * tile, const guchar * const buf,
                          const gint buf_row_width,
                          const REAL * const mat, const gint mat_size)
{
  const gint bpp = 2;
  const gint mat_offset = mat_size / 2;
  const gint eheight = tile->eheight;
  const gint ewidth = tile->ewidth;
  const gint env_offset = (buf_row_width + bpp) * mat_offset;
  const gint bp_stride = buf_row_width - mat_size * bpp;
  const gint sp_stride = buf_row_width - ewidth * bpp;
  register gint x1, y1, x2, y2;
  register guchar *tp = tile->data;
  register guchar const *sp = buf;

  for (y1 = eheight; y1 > 0; y1--)
    {
      for (x1 = ewidth; x1 > 0; x1--)
        {
          register guchar const *bp = sp - env_offset;
          register REAL const *mp = mat;
          register REAL val = 0.0;

          for (y2 = mat_size; y2 > 0; y2--)
            {
              for (x2 = mat_size; x2 > 0; x2--)
                {
                  val += *mp * *bp;
                  mp++;
                  bp += bpp;
                };
              bp += bp_stride;
            };
          /* Write value to tile */
          *tp = (guchar) CLAMP (val, 0, 255);
          tp++;
          sp++;
          *tp++ = *sp++;
        };
      sp += sp_stride;
    };
}

void
convolve_tile_rgb (GimpTile * tile, const guchar * const buf,
                   const gint buf_row_width,
                   const REAL * const mat, const gint mat_size)
{
  const gint bpp = 3;
  const gint mat_offset = mat_size / 2;
  const gint eheight = tile->eheight;
  const gint ewidth = tile->ewidth;
  const gint env_offset = (buf_row_width + bpp) * mat_offset;
  const gint bp_stride = buf_row_width - mat_size * bpp;
  const gint sp_stride = buf_row_width - ewidth * bpp;
  register gint x1, y1, x2, y2;
  register guchar *tp = tile->data;
  register guchar const *sp = buf;

  for (y1 = eheight; y1 > 0; y1--)
    {
      for (x1 = ewidth; x1 > 0; x1--)
        {
          {
            register guchar const *bp = sp - env_offset;
            register REAL const *mp = mat;
            register REAL val = 0.0;

            for (y2 = mat_size; y2 > 0; y2--)
              {
                for (x2 = mat_size; x2 > 0; x2--)
                  {
                    val += *mp * *bp;
                    mp++;
                    bp += bpp;
                  };
                bp += bp_stride;
              };
            /* Write value to tile */
            *tp = (guchar) CLAMP (val, 0, 255);
            tp++;
            sp++;
          };
          {
            register guchar const *bp = sp - env_offset;
            register REAL const *mp = mat;
            register REAL val = 0.0;

            for (y2 = mat_size; y2 > 0; y2--)
              {
                for (x2 = mat_size; x2 > 0; x2--)
                  {
                    val += *mp * *bp;
                    mp++;
                    bp += bpp;
                  };
                bp += bp_stride;
              };
            /* Write value to tile */
            *tp = (guchar) CLAMP (val, 0, 255);
            tp++;
            sp++;
          };
          {
            register guchar const *bp = sp - env_offset;
            register REAL const *mp = mat;
            register REAL val = 0.0;

            for (y2 = mat_size; y2 > 0; y2--)
              {
                for (x2 = mat_size; x2 > 0; x2--)
                  {
                    val += *mp * *bp;
                    mp++;
                    bp += bpp;
                  };
                bp += bp_stride;
              };
            /* Write value to tile */
            *tp = (guchar) CLAMP (val, 0, 255);
            tp++;
            sp++;
          };
        };
      sp += sp_stride;
    };
}

void
convolve_tile_rgb_alpha (GimpTile * tile, const guchar * const buf,
                         const gint buf_row_width,
                         const REAL * const mat, const gint mat_size)
{
  const gint bpp = 4;
  const gint mat_offset = mat_size / 2;
  const gint eheight = tile->eheight;
  const gint ewidth = tile->ewidth;
  const gint env_offset = (buf_row_width + bpp) * mat_offset;
  const gint bp_stride = buf_row_width - mat_size * bpp;
  const gint sp_stride = buf_row_width - ewidth * bpp;
  register gint x1, y1, x2, y2;
  register guchar *tp = tile->data;
  register guchar const *sp = buf;

  for (y1 = eheight; y1 > 0; y1--)
    {
      for (x1 = ewidth; x1 > 0; x1--)
        {
          {
            register guchar const *bp = sp - env_offset;
            register REAL const *mp = mat;
            register REAL val = 0.0;

            for (y2 = mat_size; y2 > 0; y2--)
              {
                for (x2 = mat_size; x2 > 0; x2--)
                  {
                    val += *mp * *bp;
                    mp++;
                    bp += bpp;
                  };
                bp += bp_stride;
              };
            /* Write value to tile */
            *tp = (guchar) CLAMP (val, 0, 255);
            tp++;
            sp++;
          };
          {
            register guchar const *bp = sp - env_offset;
            register REAL const *mp = mat;
            register REAL val = 0.0;

            for (y2 = mat_size; y2 > 0; y2--)
              {
                for (x2 = mat_size; x2 > 0; x2--)
                  {
                    val += *mp * *bp;
                    mp++;
                    bp += bpp;
                  };
                bp += bp_stride;
              };
            /* Write value to tile */
            *tp = (guchar) CLAMP (val, 0, 255);
            tp++;
            sp++;
          };
          {
            register guchar const *bp = sp - env_offset;
            register REAL const *mp = mat;
            register REAL val = 0.0;

            for (y2 = mat_size; y2 > 0; y2--)
              {
                for (x2 = mat_size; x2 > 0; x2--)
                  {
                    val += *mp * *bp;
                    mp++;
                    bp += bpp;
                  };
                bp += bp_stride;
              };
            /* Write value to tile */
            *tp = (guchar) CLAMP (val, 0, 255);
            tp++;
            sp++;
          };
          *tp++ = *sp++;        /* copy alpha */
        };
      sp += sp_stride;
    };
}

void
convolve_tile_general (GimpTile * tile, const guchar * const buf,
                       const gint buf_row_width, const gint ncolors,
                       const gint bpp, const REAL * const mat,
                       const gint mat_size)
{
  const gint mat_offset = mat_size / 2;
  const gint eheight = tile->eheight;
  const gint ewidth = tile->ewidth;
  const gboolean has_alpha = bpp > ncolors;
  const gint env_offset = (buf_row_width + bpp) * mat_offset;
  const gint bp_stride = buf_row_width - mat_size * bpp;
  const gint sp_stride = buf_row_width - ewidth * bpp;
  register gint x1, y1, x2, y2, color;
  register guchar *tp = tile->data;
  register guchar const *sp = buf;

  for (y1 = eheight; y1 > 0; y1--)
    {
      for (x1 = ewidth; x1 > 0; x1--)
        {
          for (color = ncolors - 1; color >= 0; color--)
            {
              register guchar const *bp = sp - env_offset;
              register REAL const *mp = mat;
              register REAL val = 0.0;

              for (y2 = mat_size; y2 > 0; y2--)
                {
                  for (x2 = mat_size; x2 > 0; x2--)
                    {
                      val += *mp * *bp;
                      mp++;
                      bp += bpp;
                    };
                  bp += bp_stride;
                };
              /* Write value to tile */
              *tp = (guchar) CLAMP (val, 0, 255);
              tp++;
              sp++;
            };
          if (has_alpha)
            {                   /*  copy alpha */
              *tp++ = *sp++;
            };
        };
      sp += sp_stride;
    };
}

void
convolve_tile (GimpTile * tile, const guchar * const buf,
               const gint buf_row_width, const gint ncolors, const gint bpp,
               const REAL * const mat, const gint mat_size)
{
  if ((ncolors == 3) && (bpp == 4))
    {
      convolve_tile_rgb_alpha (tile, buf, buf_row_width, mat, mat_size);
    }
  else if ((ncolors == 3) && (bpp == 3))
    {
      convolve_tile_rgb (tile, buf, buf_row_width, mat, mat_size);
    }
  else if ((ncolors == 1) && (bpp == 2))
    {
      convolve_tile_grey_alpha (tile, buf, buf_row_width, mat, mat_size);
    }
  else if ((ncolors == 1) && (bpp == 1))
    {
      convolve_tile_grey (tile, buf, buf_row_width, mat, mat_size);
    }
  else
    {
      convolve_tile_general (tile, buf, buf_row_width, ncolors, bpp, mat,
                             mat_size);
    };
}

void
convolve_image (TileSource * source, TileSink * sink,
                gint sx, gint sy, gint width, gint height,
                TB_BOUNDARY_TYPE boundary_type,
                const CMat * mat, gint mat_size, BDClosure * progress_update)
{
  TileStripBuffer buf;
  gint x, buf_start_y, total_tiles, tile_nr = 0;
  /* Will be false when progress_update tells us that the data is no longer needed */
  gboolean ok = TRUE;
  /*
    tile_height()                 [util.c]
    tile_width()                  [util.c]
   */
  initialize_buf (&buf, source, mat_size / 2, sx, sy, width, height,
                  boundary_type);
  total_tiles = (1 + floor ((buf.x + buf.width - 1) / tile_width ())
                 - floor ((buf.x) / tile_width ())) *
    (1 + floor ((buf.y_limit - 1) / tile_height ())
     - floor ((buf.y) / tile_height ()));
#ifdef RLXTEST
  printf("convolve_image: total_tiles %d\n",total_tiles);
#endif

  buf_start_y = buf.y;
  while (ok && shift_buf (&buf, source))
    {
      /* Process the tiles in the buffer */
      for (x = buf.x; ok && (x < buf.x + buf.width); x += tile_width ())
        {
          const gint ncolors = source->bpp - source->has_alpha;
          GimpTile *tile = tile_sink_get_tile (sink, x, buf.y);
#ifdef RLXTEST
          printf("convolve_image: x %d, y %d, tile->eheight %d\n",
                 x, buf.y, tile->eheight);
#endif
          convolve_tile (tile, pixpos_in_buf (&buf, x, buf.y), buf.row_stride,
                         ncolors, sink->bpp, mat->data, mat_size);
          tile_sink_tile_unref (sink, tile);
          ok =
            bd_closure_invoke (progress_update,
                               (double) (++tile_nr) / total_tiles);
        };
    };
  finalize_buf (&buf);
}
