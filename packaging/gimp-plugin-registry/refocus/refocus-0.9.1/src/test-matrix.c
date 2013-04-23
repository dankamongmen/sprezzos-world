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
 * Version $Id: test-matrix.c,v 1.1.1.1 2003/01/30 21:30:19 ernstl Exp $
 */

#include <stdlib.h>
#include "matrix.h"
#include "util.h"

#ifndef lint
static char vcid[] GCC_UNUSED = "$Id: test-matrix.c,v 1.1.1.1 2003/01/30 21:30:19 ernstl Exp $";
#endif /* lint */


int
main (int argc, const char *argv[])
{
  CMat *g;
  gint m = strtol (argv[1], 0, 0);
  REAL gamma = strtod (argv[2], 0);
  REAL alpha = strtod (argv[3], 0);
  REAL noise_factor = strtod (argv[4], 0);
  CMat convolution;

  make_circle_convolution (alpha, &convolution, m);
  fprintf (stderr, "********* Convolution matrix ***********\n");
  print_c_mat (stderr, &convolution);

  fprintf (stderr, "********* Not Symmetric ***********\n");
  g = compute_g_matrix (&convolution, m, gamma, noise_factor, 0.0, FALSE);
  fprintf (stderr,
           "********* Not Symmetric Deconvolution matrix ***********\n");
  print_c_mat (stderr, g);
  fprintf (stderr, "********* Symmetric ***********\n");
  g = compute_g_matrix (&convolution, m, gamma, noise_factor, 0.0, TRUE);
  fprintf (stderr, "********* Symmetric Deconvolution matrix ***********\n");
  print_c_mat (stderr, g);

  return (0);
}
