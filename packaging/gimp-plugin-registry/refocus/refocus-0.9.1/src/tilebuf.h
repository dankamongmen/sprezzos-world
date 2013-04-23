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
 * Version $Id: tilebuf.h,v 1.1.1.1 2003/01/30 21:30:19 ernstl Exp $
 */
#ifndef TILEBUF_H_INCLUDED
#define TILEBUF_H_INCLUDED

#include <glib.h>
#include "prevman.h"

G_BEGIN_DECLS
  /* Buffer to hold image data.
     The central part of the buffer contains one row of tiles.
     Around the central part a border is added. The size of the
     border is at least border_width that is equal to the radius
     of the convolution matrix. The size of the border at the top and
     at the left side of the buffer is equal to border_width.
     The top border contain the contents of the previous row of tiles.
     The bottom border has a height equal to the height of one tile and
     acts as a buffer for the next row of tiles.
     The central part of the buffer has a width that is equal to the width
     of the image. The left and the right border have width border_width
     and contain zero's.
     Coordinates of pixels are so arranged that the first point of the
     central image data (that lies at an offset (border_width, border_width)
     from the start) has coordinates (x,y) in the original image.
   */
  typedef enum
{ TB_BOUNDARY_ZERO,
  TB_BOUNDARY_MIRROR,
  TB_BOUNDARY_TORUS,
  TB_BOUNDARY_REPEAT
}
TB_BOUNDARY_TYPE;

typedef struct
{
  guchar *data;
  gint row_stride;
  gint border_width;
  gint bpp;
  gint x, width, y, height;
  gint real_x, real_width, real_y, real_height;
  gint y_limit;
  gboolean first_time;
  TB_BOUNDARY_TYPE boundary_type;
}
TileStripBuffer;

extern guchar *pixpos_in_buf (TileStripBuffer * buf, const gint x,
                              const gint y);

extern void finalize_buf (TileStripBuffer * buf);

extern void
initialize_buf (TileStripBuffer * buf, TileSource * source,
                gint border_width, gint sx, gint sy, gint width, gint height,
                TB_BOUNDARY_TYPE boundary_type);

gboolean shift_buf (TileStripBuffer * buf, TileSource * source);

G_END_DECLS
#endif /* TILEBUF_H_INCLUDED */
