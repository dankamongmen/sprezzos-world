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

#ifndef __GIMPPLUGIN_H__
#define __GIMPPLUGIN_H__

#include <libgimp/gimp.h>

G_BEGIN_DECLS


#define PLUG_IN_BINARY  "focusblur"
#define PLUG_IN_PROC    "plug-in-focusblur"

#define TILE_WIDTH  gimp_tile_width()
#define TILE_HEIGHT gimp_tile_height()


G_END_DECLS

#endif /* __GIMPPLUGIN_H__ */
