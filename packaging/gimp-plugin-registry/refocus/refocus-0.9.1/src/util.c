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
 * Version $Id: util.c,v 1.1.1.1 2003/01/30 21:30:19 ernstl Exp $
 */

#include <stdio.h>
#include "util.h"
#include <string.h>
#include <math.h>
#include <libgimp/gimp.h>

#ifndef lint
static char vcid[] GCC_UNUSED = "$Id: util.c,v 1.1.1.1 2003/01/30 21:30:19 ernstl Exp $";
#endif /* lint */

gint
floorm (gint a, gint b)
 /* return largest multiple of b that is <= a */
 /*
  & & m = floorm(a,b)
    & a = b*m + r
    &  0 <= r < b
  */
{
#ifdef RLXTEST
  printf("floorm: a/b %d, fl %g\n", a/b, floor ((gdouble) a / b));
#endif
  return (b * floor ((gdouble) a / b));
}

gint
ceilm (gint a, gint b)
 /* return least multiple of b that is >= a */
 /*
    & m = ceilm(a,b)
    & a = b*m - r;
    & m = a/b
    % r = a%b
    & -a = -b*m + r

    & ceilm = (r == 0 ? b*m : (b+1)*m)
  */
{
#ifdef RLXTEST
  printf("ceil: a %d, b %d, -(-a/b) %d,a/b+(a%b != 0 ? 1:0) %d,  fl %g\n",
         a,b,
         -((-a)/b),
          a/b+(a%b != 0 ? 1:0),
         ceil ((gdouble) a / b) );
#endif
  return (b * ceil ((gdouble) a / b));
}


void
copy_rect (guchar * dest_buf, gint dest_x, gint dest_y,
           gint dest_width, gint dest_height,
           guchar * src_buf, gint src_x, gint src_y,
           gint src_width, gint src_height, gint bpp)
{
  gint x_lo, x_hi, y_lo, y_hi, y;

  x_lo = MAX (src_x, dest_x);
  x_hi = MIN (src_x + src_width, dest_x + dest_width);
  y_lo = MAX (src_y, dest_y);
  y_hi = MIN (src_y + src_height, dest_y + dest_height);
  if (x_hi > x_lo)
    {
      for (y = y_lo; y < y_hi; y++)
        {
          memmove (dest_buf +
                   ((y - dest_y) * dest_width + x_lo - dest_x) * bpp,
                   src_buf + ((y - src_y) * src_width + x_lo - src_x) * bpp,
                   (x_hi - x_lo) * bpp);
        };
    };
}

/**
 * tile_width:
 * 
 * This function converts the guint that is returned by
 * gimp_tile_width into a gint. This helps prevent
 * all kind of signed/unsigned problems in the code.
 * 
 * Return value: The width of a tile.
 **/
gint
tile_width (void)
{
  return (gimp_tile_width ());
}

/**
 * tile_height:
 * 
 * This function converts the guint that is returned by
 * gimp_tile_height into a gint. This helps prevent
 * all kind of signed/unsigned problems in the code.
 * 
 * Return value: The height of a tile.
 **/
gint
tile_height (void)
{
  return (gimp_tile_height ());
}
