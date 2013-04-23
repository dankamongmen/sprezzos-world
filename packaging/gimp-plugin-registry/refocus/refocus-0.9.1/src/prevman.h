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
 * Version $Id: prevman.h,v 1.1.1.1 2003/01/30 21:30:18 ernstl Exp $
 */

#ifndef PREVMAN_H_INCLUDED
#define PREVMAN_H_INCLUDED

#include <libgimp/gimp.h>

G_BEGIN_DECLS 

typedef enum
{ TP_PREV, TP_DRAWABLE }
PMType;

typedef struct
{
  PMType type;
  gint x;
  gint y;
  gint width;
  gint height;
  gint bpp;
  gboolean has_alpha;
  union
  {
    struct
    {
      GimpDrawable *drawable;
      GimpPixelRgn pixel_rgn;
    }
    drw;
    struct
    {
      gint *dummy;              /* Just to shut up the compiler */
    }
    prv;
  }
  un;
}
TileSource;

typedef struct
{
  PMType type;
  gint x;
  gint y;
  gint width;
  gint height;
  gint bpp;
  gboolean has_alpha;
  union
  {
    struct
    {
      GimpDrawable *drawable;
      GimpPixelRgn pixel_rgn;
    }
    drw;
    struct
    {
      guchar *data;
      gint ntile_rows;
      gint ntile_cols;
      GimpTile *tiles;
    }
    prv;
  }
  un;
}
TileSink;

extern void
tile_source_init_from_drawable (TileSource * tile_source,
                                GimpDrawable * drawable, gint x, gint y,
                                gint width, gint height);
extern GimpTile *tile_source_get_tile (TileSource * tile_source, const gint x,
                                       const gint y);

extern void
tile_source_tile_unref (TileSource * tile_source, GimpTile * tile);

extern void
tile_sink_init_from_drawable (TileSink * tile_sink, GimpDrawable * drawable,
                              gint x, gint y, gint width, gint height);
extern void
tile_sink_init_for_preview (TileSink * tile_sink, GimpDrawable * drawable,
                            gint x, gint y, gint width, gint height);

extern void tile_sink_free_buffers (TileSink * tile_sink);

extern GimpTile *tile_sink_get_tile (TileSink * tile_sink, const gint x,
                                     const gint y);

extern void tile_sink_tile_unref (TileSink * tile_sink, GimpTile * tile);

extern void
tile_sink_get_row (TileSink * tile_sink, guchar * buf, gint x, gint y,
                   gint width);

G_END_DECLS
#endif /*  PREVMAN_H_INCLUDED */
