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
 * Version $Id: prevman.c,v 1.1.1.1 2003/01/30 21:30:18 ernstl Exp $
 */

#include <string.h>
#include "prevman.h"
#include "util.h"

#ifndef lint
static char vcid[] GCC_UNUSED = "$Id: prevman.c,v 1.1.1.1 2003/01/30 21:30:18 ernstl Exp $";
#endif /* lint */

/**
 * tile_source_init_from_drawable:
 * @tile_source: The @TileSource that is initialized.
 * @drawable: The #GimpDrawable that is used as the real source of tiles.
 * @x: Leftmost position in the @drawable that will be used.
 * @y: Topmost position in the @drawable that will be used.
 * @width: Width of the rectangle in the @drawable that will be used.
 * @height: Height of the rectangle in the @drawable that will be used.
 * 
 * Initialize a #TileSource to use a #GimpDrawable.
 * It can then be used to get tiles from the rectangle
 * (@x, @y, @width, @height) in the @drawable.
 * @see tile_source_get_tile() and tile_source_tile_unref()
 **/
void
tile_source_init_from_drawable (TileSource * tile_source,
                                GimpDrawable * drawable, gint x, gint y,
                                gint width, gint height)
{
  tile_source->type = TP_DRAWABLE;
  tile_source->un.drw.drawable = drawable;
  tile_source->x = 0;
  tile_source->y = 0;
  tile_source->width = drawable->width;
  tile_source->height = drawable->height;
  tile_source->bpp = drawable->bpp;
  tile_source->has_alpha = gimp_drawable_has_alpha (drawable->drawable_id);
  tile_source->un.drw.drawable = drawable;
  gimp_pixel_rgn_init (&tile_source->un.drw.pixel_rgn, drawable, x, y, width,
                       height, FALSE, FALSE);
}

/**
 * tile_source_get_tile:
 * @tile_source: The #TileSource.
 * @x: x coordinate of point in tile.
 * @y: y coordinate of point in tile.
 * 
 * Get the tile that contains the point (@x, @y).
 * When this tile is no longer needed it must be released with
 * tile_source_tile_unref().
 * 
 * Return value: The requested tile.
 **/
GimpTile *
tile_source_get_tile (TileSource * tile_source, const gint x, const gint y)
{
  GimpTile *result = NULL;
  if (tile_source->type == TP_DRAWABLE)
    {
      result =
        gimp_drawable_get_tile2 (tile_source->un.drw.drawable, FALSE, x, y);
      gimp_tile_ref (result);
    }
  else
    {
    };
  return (result);
}

/**
 * tile_source_tile_unref:
 * @tile_source: The #TileSource that manages @tile.
 * @tile: The #GimpTile that can be released.
 * 
 * Release the @tile. This function must always be called when
 * the @tile is no longer needed.
 **/
void
tile_source_tile_unref (TileSource * tile_source, GimpTile * tile)
{
  if (tile_source->type == TP_DRAWABLE)
    {
      gimp_tile_unref (tile, FALSE);
    }
  else
    {
    };
}



/**
 * tile_source_get_row:
 * @tile_source: The TileSource.
 * @buf: The buffer in which the result is returned. Its size must
 * be at least @width * @tile_source->bpp bytes.
 * @x: x coordinate of the start of the row.
 * @y: y coordinate of the row.
 * @width: length of the row in pixels.
 * 
 * Get a row of pixels from @tile_source.
 **/
void
tile_source_get_row (TileSource * tile_source, guchar * buf, gint x, gint y,
                     gint width)
{
  if (tile_source->type == TP_DRAWABLE)
    {
      gimp_pixel_rgn_get_row (&tile_source->un.drw.pixel_rgn, buf, x, y,
                              width);
    }
  else
    {
    };
}

void
tile_sink_init_from_drawable (TileSink * tile_sink, GimpDrawable * drawable,
                              gint x, gint y, gint width, gint height)
{
  tile_sink->type = TP_DRAWABLE;
  tile_sink->un.drw.drawable = drawable;
  tile_sink->x = 0;
  tile_sink->y = 0;
  tile_sink->width = drawable->width;
  tile_sink->height = drawable->height;
  tile_sink->bpp = drawable->bpp;
  tile_sink->has_alpha = gimp_drawable_has_alpha (drawable->drawable_id);
  gimp_pixel_rgn_init (&tile_sink->un.drw.pixel_rgn, drawable, x, y, width,
                       height, TRUE, TRUE);
}

void
tile_sink_init_for_preview (TileSink * tile_sink, GimpDrawable * drawable,
                            gint x, gint y, gint width, gint height)
{
  tile_sink->type = TP_PREV;
  tile_sink->x = x;
  tile_sink->y = y;
  tile_sink->width = width;
  tile_sink->height = height;
  tile_sink->bpp = drawable->bpp;
  tile_sink->has_alpha = gimp_drawable_has_alpha (drawable->drawable_id);
  tile_sink->un.prv.data = g_new (guchar,
                                  tile_sink->width * tile_sink->height *
                                  tile_sink->bpp);
  /* This number of rows and cols is only sometimes too high */
  tile_sink->un.prv.ntile_cols = 1 + (tile_sink->width + tile_width () - 1) /
    tile_width ();
  tile_sink->un.prv.ntile_rows =
    1 + (tile_sink->height + tile_height () - 1) / tile_height ();
  tile_sink->un.prv.tiles =
    g_new (GimpTile,
           tile_sink->un.prv.ntile_rows * tile_sink->un.prv.ntile_cols);
}

void
tile_sink_free_buffers (TileSink * tile_sink)
{
  if (tile_sink->type == TP_PREV)
    {
      g_free (tile_sink->un.prv.data);
      g_free (tile_sink->un.prv.tiles);
    };
}

GimpTile *
tile_sink_get_tile (TileSink * tile_sink, const gint x, const gint y)
{
  GimpTile *result;
  if (tile_sink->type == TP_DRAWABLE)
    {
      result =
        gimp_drawable_get_tile2 (tile_sink->un.drw.drawable, TRUE, x, y);
      gimp_tile_ref (result);
    }
  else
    {
      gint tile_row_nr =
        (y - floorm (tile_sink->y, tile_height ())) / tile_height ();
      gint tile_col_nr =
        (x - floorm (tile_sink->x, tile_width ())) / tile_width ();
      gint tile_nr = tile_row_nr * tile_sink->un.prv.ntile_cols + tile_col_nr;

      g_assert (tile_row_nr >= 0);
      g_assert (tile_row_nr < tile_sink->un.prv.ntile_rows);
      g_assert (tile_col_nr >= 0);
      g_assert (tile_col_nr < tile_sink->un.prv.ntile_cols);
      result = tile_sink->un.prv.tiles + tile_nr;
      result->ewidth = tile_width ();   /* Wroong on the right */
      result->eheight = tile_height (); /* Wroong on the bottom */
      result->bpp = tile_sink->bpp;
      result->data = g_new (guchar,
                            result->ewidth * result->eheight * result->bpp);
      result->tile_num = tile_nr;
    };
  return (result);
}

void
tile_sink_tile_unref (TileSink * tile_sink, GimpTile * tile)
{
  if (tile_sink->type == TP_DRAWABLE)
    {
      gimp_tile_unref (tile, TRUE);
    }
  else
    {
      gint tile_row_nr = tile->tile_num / tile_sink->un.prv.ntile_cols;
      gint tile_col_nr = tile->tile_num % tile_sink->un.prv.ntile_cols;
      gint tile_x = floorm (tile_sink->x, tile_width ()) +
        tile_col_nr * tile_width ();
      gint tile_y = floorm (tile_sink->y, tile_height ()) +
        tile_row_nr * tile_height ();

      g_assert (tile_sink->un.prv.tiles + tile->tile_num == tile);
      copy_rect (tile_sink->un.prv.data, tile_sink->x, tile_sink->y,
                 tile_sink->width, tile_sink->height,
                 tile->data, tile_x, tile_y, tile->ewidth, tile->eheight,
                 tile_sink->bpp);
      g_free (tile->data);
    };
}

void
tile_sink_get_row (TileSink * tile_sink, guchar * buf, gint x, gint y,
                   gint width)
{
  if (tile_sink->type == TP_DRAWABLE)
    {

    }
  else
    {
      guchar *start = tile_sink->un.prv.data +
        (tile_sink->bpp *
         ((y - tile_sink->y) * tile_sink->width + (x - tile_sink->x)));
      memmove (buf, start, width * tile_sink->bpp);
    }
}
