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

#ifndef __FOCUSBLUR_SOURCE_H__
#define __FOCUSBLUR_SOURCE_H__

#include "config.h"

#include <glib/gtypes.h>
#include <libgimp/gimptypes.h>

#include "focusblurtypes.h"
#include "focusblurenums.h"


G_BEGIN_DECLS


struct _FblurSourceImage
{
  guchar                *data;
  gint                   bpp;
  gsize                  rowstride;
  gint                   channels;
  gint                   x1, x2, y1, y2;
  gboolean               has_selection;
  gboolean               is_rgb;
  gboolean               has_alpha;
};


gboolean        focusblur_source_update         (FblurSourceImage **source,
                                                 GimpDrawable      *drawable);
void            focusblur_source_destroy        (FblurSourceImage **shine);
void            focusblur_source_get            (FblurSourceImage  *source,
                                                 gint               x,
                                                 gint               y,
                                                 guchar            *pixel_ret);


G_END_DECLS


#endif /* __FOCUSBLUR_SOURCE_H__ */
