/* Focus Blur -- blur with focus plug-in.
 * Copyright (C) 2002-2008 Kyoichiro Suda
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

#ifndef __FOCUSBLUR_DEPTHMAP_H__
#define __FOCUSBLUR_DEPTHMAP_H__

#include <glib/gtypes.h>

#include "focusblurtypes.h"
#include "focusblurenums.h"


G_BEGIN_DECLS


gboolean        focusblur_depth_map_update      (FblurDepthMap  **depth_map,
                                                 FblurFftBuffer  *fft,
                                                 FblurStoreParam *store,
                                                 FblurQualityType quality);
void            focusblur_depth_map_destroy     (FblurDepthMap  **depth_map);
gint            focusblur_depth_map_get_depth   (FblurDepthMap   *depth_map,
                                                 gint             x,
                                                 gint             y);
gint            focusblur_depth_map_get_level   (FblurDepthMap   *depth_map,
                                                 gint             depth);
gint            focusblur_depth_map_focal_depth (FblurDepthMap   *depth_map);
gboolean        focusblur_depth_map_has_aaa     (FblurDepthMap   *depth_map);
gfloat          focusblur_depth_map_get_aaa     (FblurDepthMap   *depth_map,
                                                 gint             x,
                                                 gint             y,
                                                 gint            *depth_ret);


G_END_DECLS


#endif /* __FOCUSBLUR_DEPTHMAP_H__ */
