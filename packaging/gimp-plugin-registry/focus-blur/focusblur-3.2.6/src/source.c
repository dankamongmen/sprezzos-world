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

#include <libgimp/gimp.h>

#include "focusblurparam.h"
#include "source.h"


/*---- Prototypes ----*/

static guchar*  focusblur_source_getp   (FblurSourceImage       *source,
                                         gint                    x,
                                         gint                    y);


/*---- Functions ----*/

gboolean
focusblur_source_update (FblurSourceImage       **source,
                         GimpDrawable            *drawable)
{
  GimpPixelRgn pr;
  gint x1, x2, y1, y2;
  gint width, height;

  if (*source)
    return TRUE;

  *source = g_new0 (FblurSourceImage, 1);

  /* Get selection rectangle */
  (*source)->has_selection =
    gimp_drawable_mask_bounds (drawable->drawable_id, &x1, &y1, &x2, &y2);

  (*source)->x1 = x1;
  (*source)->x2 = x2;
  (*source)->y1 = y1;
  (*source)->y2 = y2;

  width = x2 - x1;
  height = y2 - y1;

  (*source)->bpp = drawable->bpp;
  (*source)->rowstride = (*source)->bpp * width;
  (*source)->data = g_new (guchar, (*source)->rowstride * height);

  if (! (*source)->data)
    {
      focusblur_source_destroy (source);
      return FALSE;
    }

  gimp_pixel_rgn_init (&pr, drawable, x1, y1, width, height,
                       FALSE, FALSE);
  gimp_pixel_rgn_get_rect (&pr, (*source)->data, x1, y1, width, height);

  (*source)->has_alpha = gimp_drawable_has_alpha (drawable->drawable_id);
  (*source)->is_rgb    = gimp_drawable_is_rgb (drawable->drawable_id);
  (*source)->channels  = drawable->bpp - ((*source)->has_alpha ? 1 : 0);

  return TRUE;
}


void
focusblur_source_destroy (FblurSourceImage **source)
{
  if (*source)
    {
      if ((*source)->data)
        g_free ((*source)->data);

      g_free (*source);
      *source = NULL;
    }
}


void
focusblur_source_get (FblurSourceImage  *source,
                      gint               x,
                      gint               y,
                      guchar            *pixel_ret)
{
  guchar        *pixelp;
  gint           c;

  pixelp = focusblur_source_getp (source, x, y);

  for (c = 0; c < source->bpp; c ++)
    pixel_ret[c] = pixelp[c];
}


static guchar*
focusblur_source_getp (FblurSourceImage *source,
                       gint              x,
                       gint              y)
{
  gsize offset;

  g_assert (x >= source->x1);
  g_assert (x < source->x2);
  g_assert (y >= source->y1);
  g_assert (y < source->y2);

  x -= source->x1;
  y -= source->y1;

  offset = y * source->rowstride + x * source->bpp;

  return source->data + offset;
}
