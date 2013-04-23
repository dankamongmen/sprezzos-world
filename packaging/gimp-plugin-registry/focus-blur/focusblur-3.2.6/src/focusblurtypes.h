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

#ifndef __FOCUSBLUR_TYPES_H__
#define __FOCUSBLUR_TYPES_H__

#include <glib/gmacros.h>


G_BEGIN_DECLS


typedef struct _FblurStoreParam         FblurStoreParam;
typedef struct _FblurPreferences        FblurPreferences;
typedef struct _FblurParam              FblurParam;

typedef struct _FblurSourceImage        FblurSourceImage;
typedef struct _FblurDiffusionTable     FblurDiffusionTable;
typedef struct _FblurBrush              FblurBrush;
typedef struct _FblurFftBuffer          FblurFftBuffer;
typedef struct _FblurDepthMap           FblurDepthMap;
typedef struct _FblurShineData          FblurShineData;


G_END_DECLS

#endif /* __FOCUSBLUR_TYPES_H__ */
