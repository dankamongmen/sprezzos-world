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
 * Version $Id: matrix.h,v 1.1.1.1 2003/01/30 21:30:18 ernstl Exp $
 */

#ifndef MATRIX_H_INCLUDED
#define MATRIX_H_INCLUDED

#include <stdio.h>
#include <glib.h>
#include "refocus.h"

G_BEGIN_DECLS

/**
 * CMat:
 * @radius: Radius of the matrix.
 *
 * Centered matrix. This is a square matrix where
 * the indices range from [-radius, radius].
 * The matrix contains (2 * radius + 1) ** 2 elements.
 *
 **/
  typedef struct
{
  gint radius;                  /* Radius of the matrix */
  gint row_stride;              /* Size of one row = 2 * radius + 1 */
  REAL *data;                   /* Contents of matrix */
  REAL *center;                 /* Points to element with index (0, 0) */
}
CMat;


/**
 * Mat:
 * @rows: Number of rows in the matrix.
 *
 * Normal matrix type. Indices range from
 * [0, rows -1 ] and [0, cols - 1].
 *
 **/
typedef struct
{
  gint rows;                    /* Number of rows in the matrix */
  gint cols;                    /* Number of columns in the matrix */
  REAL *data;                   /* Content of the matrix */
}
Mat;


extern void
make_circle_convolution (const REAL radius, CMat * convolution, const gint m);

extern void
make_gaussian_convolution (const REAL alpha, CMat * convolution,
                           const gint m);

extern void
convolve_star_mat (CMat * result, const CMat * const mata,
                   const CMat * const matb);

extern CMat *compute_g_matrix (const CMat * const convolution, const gint m,
                               const REAL gamma, const REAL noise_factor,
                               const REAL musq, const gboolean symmetric);

extern void finish_matrix (Mat * mat);
extern void finish_and_free_matrix (Mat * mat);
extern void init_c_mat (CMat * mat, const gint radius);
extern void finish_c_mat (CMat * mat);
extern void print_c_mat (FILE * file, const CMat * const mat);
extern void print_convolution_matrix (FILE * file, REAL mat[], gint m);

G_END_DECLS
#endif /* MATRIX_H_INCLUDED */
