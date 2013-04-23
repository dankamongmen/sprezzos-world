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
 * Version $Id: tilebuf.c,v 1.1.1.1 2003/01/30 21:30:19 ernstl Exp $
 */

#include <stdio.h>
#include "tilebuf.h"
#include "util.h"
#include <string.h>

#ifndef lint
static char vcid[] GCC_UNUSED = "$Id: tilebuf.c,v 1.1.1.1 2003/01/30 21:30:19 ernstl Exp $";
#endif /* lint */

/**
 * pixpos_in_buf:
 * @buf: 
 * @x: x coordinate of pixel.
 * @y: y coordinate of pixel.
 * 
 * Return a pointer to the pixel in @buf with coordinates (@x,@y)
 * 
 * Return value: 
 **/
guchar *
pixpos_in_buf (TileStripBuffer * buf, const gint x, const gint y)
{
  /*
    & data = array(? lines x row_stride columns)
    & pixpos_in_buf = &data[(x - real_x)*bytes_per_pixel, y - real_y]
    & (real_x, real_y) = origin of tile.
   */
  return (&buf->data[buf->row_stride * (y - buf->real_y) +
                     buf->bpp * (x - buf->real_x)]);
}

static void
copy_tile_to_buf (TileStripBuffer * buf, TileSource * source, gint x, gint y)
     /* Copy the tile from at position (x,y) to buf */
{
  const gint tile_x = floorm (x, tile_width ());
  const gint tile_y = floorm (y, tile_height ());

  if ((tile_x >= 0) && (tile_x < source->width) &&
      (tile_y >= 0) && (tile_y < source->height))
    {
      GimpTile *tile = tile_source_get_tile (source, tile_x, tile_y);

      copy_rect (buf->data,
                 buf->real_x, buf->real_y, buf->real_width, buf->real_height,
                 tile->data,
                 tile_x, tile_y, tile->ewidth, tile->eheight, buf->bpp);
      tile_source_tile_unref (source, tile);
    }
}

static void
zero_rect_in_buf (TileStripBuffer * buf, gint x, gint y, gint w, gint h)
{
  gint row;
  guchar *p = pixpos_in_buf (buf, x, y);

  for (row = h; row > 0; row--)
    {
      memset (p, 0, w * buf->bpp);
      p += buf->row_stride;
    };
}

void
finalize_buf (TileStripBuffer * buf)
     /* Free the buffers */
{
  if (buf && buf->data)
    {
      g_free (buf->data);
    };
}

static void
copy_col (TileStripBuffer * buf, const gint x, const gint y,
          const gint height, const gint x2)
     /* Copy one collumn of pixels. The collumn starts at coordinates (x,y) */
     /* with height pixels. The destination collumn starts at (x2,y) */
{
  const gint row_stride = buf->row_stride;
  const gint bpp_minus1 = buf->bpp - 1;
  register guchar *src = pixpos_in_buf (buf, x, y);
  guchar *dest = pixpos_in_buf (buf, x2, y);
  register gint row_nr, b;

  for (row_nr = height - 1; row_nr >= 0; row_nr--)
    {
      for (b = bpp_minus1; b >= 0; b--)
        {
          dest[b] = src[b];
        };
      src += row_stride;
      dest += row_stride;
    };
}

static void
copy_row (TileStripBuffer * buf, const gint x, const gint y, const gint width,
          const gint y2)
     /* Copy one row of pixels. The collumn starts at coordinates (x,y) */
     /* with with pixels. The destination collumn starts at (x,y2) */
{
  memmove (pixpos_in_buf (buf, x, y2),
           pixpos_in_buf (buf, x, y), width * buf->bpp);
}

static void
fix_left_boundary (TileStripBuffer * buf, const gint x_lo, const gint y_start,
                   const gint y_end)
{
  if ((x_lo <= buf->real_x) || (y_end <= y_start))
    {
      return;
    };

  switch (buf->boundary_type)
    {
    case TB_BOUNDARY_ZERO:
      zero_rect_in_buf (buf, buf->real_x, y_start,
                        x_lo - buf->real_x, y_end - y_start);
      break;
    case TB_BOUNDARY_MIRROR:
      {
        register gint x;

        for (x = x_lo - 1; x >= buf->real_x; x--)
          {
            copy_col (buf, 2 * x_lo - x, y_start, y_end - y_start, x);
          };
      }
      break;
    default:
      break;
    }
}

static void
fix_right_boundary (TileStripBuffer * buf, const gint x_hi,
                    const gint y_start, const gint y_end)
{
  if ((x_hi >= buf->real_x + buf->real_width) || (y_end <= y_start))
    {
      return;
    };

  switch (buf->boundary_type)
    {
    case TB_BOUNDARY_ZERO:
      zero_rect_in_buf (buf, x_hi, y_start,
                        buf->real_x + buf->real_width - x_hi,
                        y_end - y_start);
      break;
    case TB_BOUNDARY_MIRROR:
      {
        register gint x;

        for (x = x_hi; x < buf->real_x + buf->real_width; x++)
          {
            copy_col (buf, 2 * (x_hi - 1) - x, y_start, y_end - y_start, x);
          };
      }
      break;
    default:
      break;
    }
}

static void
fix_top_boundary (TileStripBuffer * buf, const gint y_lo)
{
  if (y_lo <= buf->real_y)
    {
      return;
    };

  switch (buf->boundary_type)
    {
    case TB_BOUNDARY_ZERO:
      zero_rect_in_buf (buf, buf->real_x, buf->real_y,
                        buf->real_width, y_lo - buf->real_y);
      break;
    case TB_BOUNDARY_MIRROR:
      {
        register gint y;

        for (y = y_lo - 1; y >= buf->real_y; y--)
          {
            copy_row (buf, buf->real_x, 2 * y_lo - y, buf->real_width, y);
          };
      }
      break;
    default:
      break;
    }
}

static void
fix_bottom_boundary (TileStripBuffer * buf, const gint y_hi)
{
  if (y_hi >= buf->real_y + buf->real_height)       /* (1) */
    {
      return;
    };

  switch (buf->boundary_type)
    {
    case TB_BOUNDARY_ZERO:
      zero_rect_in_buf (buf, buf->real_x, y_hi,
                        buf->real_width,
                        buf->real_y + buf->real_height - y_hi);
      break;
    case TB_BOUNDARY_MIRROR:
      {
        register gint y2;

        /*if (y_hi < buf->y_limit)*/
        for (y2 = y_hi; y2 < buf->real_y + buf->real_height; y2++) /*(2)*/
          {
            /*
              &1 y < y_hi              [domain of y, definition of y_hi, (1)]
              &2 y_hi <= y2 < buf->real_y + buf->real_height  [(1),(2)]
              &3 i = 0, 1, ...
              &4 y=y_hi-i-1 -> y2=y_hi+i               [definition of mirror]
              &5 y = y2 - 2*i - 1                      [4]
              &6 y = 2*y2 - 2*i - y2 - 1               [5,algebra]
              &7 y = 2*y_hi - y2 - 1                   [4,6]
            */
            gint y =  2*y_hi - y2 - 1;
            if (y < buf->real_y) break;
#ifdef RLXTEST
           printf("fix_bottom_boundary: Copying row %d to %d, width %d, buf_loc %d %d, real_y %d\n",
                  2*y_hi - y2 - 1, y2, buf->real_width,
                  buf->row_stride * (2*y_hi - y2 - 1 - buf->real_y) +
                   buf->bpp * (buf->real_x - buf->real_x),
                  buf->row_stride * (y2 - buf->real_y) +
                   buf->bpp * (buf->real_x - buf->real_x),
                   buf->real_y);
#endif
            copy_row (buf, buf->real_x, y, buf->real_width,y2);
          };
      }
      break;
    default:
      break;
    }
}

static void
fix_boundaries (TileStripBuffer * buf, TileSource * source,
                gboolean first_time)
     /* Fix the boundary areas in buf that fall outside the source. */
     /* When first_time is FALSE only the area with y > buf->y + tile_height */
     /* will be fixed */
{
  const gint x_lo = MAX (buf->real_x, source->x);
  const gint x_hi = MIN (buf->real_x + buf->real_width,
                         source->x + source->width);
  const gint y_lo = MAX (buf->real_y, source->y);
  const gint y_hi = MIN (buf->real_y + buf->real_height,
                         source->y + source->height);
  /*
    y:  domain of y.
    & 0 <= y.  y is a 0-index.
    & | & y in the source image
        & 0 <=  source->y <= y < source->y + source->height
      | & y in the buffer
        & 0 <= buf->real_y <= y < buf->real_y + buf->real_height
    x:  domain of x = domain of y with mapping y -> x, height -> width.
   */

#ifdef RLXTEST
  printf("buf->real_y %d, buf->real_height %d, source->y %d, source->heigh %d, y_hi %d\n",
         buf->real_y,  buf->real_height,
       source->y, source->height, y_hi);
#endif
  fix_left_boundary (buf, x_lo, first_time ? buf->y : buf->y + tile_height (),
                     y_hi);
  fix_right_boundary (buf, x_hi,
                      first_time ? buf->y : buf->y + tile_height (), y_hi);
  if (first_time)
    {
      fix_top_boundary (buf, y_lo);
    };
#ifdef RLXTEST
  printf("fix_boundaries: Now goto fix_bottom_boundary: y_hi %d, real_y + real_height %d, data[%d]\n",
         y_hi, buf->real_y + buf->real_height,
         buf->real_height * buf->row_stride);
#endif
  fix_bottom_boundary (buf, y_hi);
#ifdef RLXTEST
  printf("bottom boundary fixed\n");
#endif
}

void
initialize_buf (TileStripBuffer * buf, TileSource * source,
                gint border_width, gint sx, gint sy, gint width, gint height,
                TB_BOUNDARY_TYPE boundary_type)
{
  gint x, y;

  buf->bpp = source->bpp;
  buf->border_width = border_width;
  buf->x = floorm (sx, tile_width ());
  buf->width = ceilm (width + sx - buf->x, tile_width ());
  buf->y = floorm (sy, tile_height ());
  buf->y_limit = sy + height;
  buf->height = tile_height ();
  buf->real_x = buf->x - buf->border_width;
  buf->real_y = buf->y - buf->border_width;
  buf->real_width = buf->width + 2 * buf->border_width;
  buf->real_height = buf->height + buf->border_width + tile_height ();
  buf->row_stride = buf->real_width * buf->bpp;
  buf->first_time = TRUE;
  buf->boundary_type = boundary_type;
  buf->data = g_new0 (guchar, buf->real_height * buf->row_stride);
  for (y = buf->y - tile_height ();
       y <= buf->y + tile_height (); y += tile_height ())
    {
      for (x = buf->x - tile_width ();
           x <= buf->x + buf->width; x += tile_width ())
        {
          copy_tile_to_buf (buf, source, x, y);
        };
    };
  fix_boundaries (buf, source, TRUE);
}


gboolean
shift_buf (TileStripBuffer * buf, TileSource * source)
 /* Shift the buffer to its next position.
    Returns FALSE when the entire image has been processed.
  */
{
  gint x;
  gboolean not_finished = TRUE;

  /*
    & y_limit = height of picture, if full picture.
   */
#ifdef RLXTEST
  printf("buf->y %d + tile_height%d () >= buf->y_limit %d\n",
         buf->y,tile_height(),buf->y_limit);
#endif

  if (buf->first_time)
    {
      /* Buf must already have been initialized so there is nothing to do */
      buf->first_time = FALSE;
    }
  else if (buf->y + tile_height () >= buf->y_limit)
    {
      /* We are finished */
      not_finished = FALSE;
    }
  else
    {
#ifdef RLXTEST
      printf("shift_buf: tile_height %d\n", tile_height());
#endif
      memmove (pixpos_in_buf (buf, buf->real_x,
                              buf->real_y),
               pixpos_in_buf (buf, buf->real_x,
                              buf->real_y + tile_height ()),
               buf->row_stride * (tile_height () + buf->border_width));
      buf->y += tile_height ();
#ifdef RLXTEST
      printf("shift_buf: y %d, y_limit %d\n", buf->y,  buf->y_limit);
#endif
      buf->real_y = buf->y - buf->border_width;
      for (x = buf->x - tile_width ();
           x <= buf->x + buf->width; x += tile_width ())
        {
          copy_tile_to_buf (buf, source, x, buf->y + tile_height ());
#ifdef RLXTEST
          printf("shift_buf: x %d\n", x);
#endif
        };
      fix_boundaries (buf, source, FALSE);
    };
#ifdef RLXTEST
  printf("shift_buf: done\n");
#endif
  return (not_finished);
}
