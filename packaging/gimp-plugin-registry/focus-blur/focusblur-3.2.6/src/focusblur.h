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

#ifndef __FOCUSBLUR_H__
#define __FOCUSBLUR_H__

#include <glib/gmacros.h>

G_BEGIN_DECLS


#define FBLUR_MAX_NUM_THREADS   16
#define FBLUR_RADIUS_MAX        127
/* do not set over the precision of 1 byte (255),
   and it is hard-coded in [012]depthmap.c, fftblur*.c */
#define FBLUR_DEPTH_MAX         127
#define FBLUR_DIFFUSION_NTABLES (1 + FBLUR_DEPTH_MAX)
#define FBLUR_GAUSSIAN_FACTOR   0.3003866304f


G_END_DECLS

#endif /* __FOCUSBLUR_H__ */
