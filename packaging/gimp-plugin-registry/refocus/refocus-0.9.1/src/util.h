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
 * Version $Id: util.h,v 1.1.1.1 2003/01/30 21:30:19 ernstl Exp $
 */

#ifndef UTIL_H_INCLUDED
#define UTIL_H_INCLUDED
#include <glib.h>

G_BEGIN_DECLS 


/* This macro can be used to silence gcc's warnings about unused variables. */
#ifdef __GNUC__
#define GCC_UNUSED __attribute__ ((__unused__)) 
#else
#define GCC_UNUSED 
#endif


extern gint floorm (gint a, gint b);

extern gint ceilm (gint a, gint b);

extern void
copy_rect (guchar * dest_buf, gint dest_x, gint dest_y,
           gint dest_width, gint dest_height,
           guchar * src_buf, gint src_x, gint src_y,
           gint src_width, gint src_height, gint bpp);

extern gint tile_width (void);

extern gint tile_height (void);

G_END_DECLS
#endif /* UTIL_H_INCLUDED */
