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
 * Version $Id: matrix.c,v 1.1.1.1 2003/01/30 21:30:18 ernstl Exp $
 */

#include <libgimp/gimp.h>
#include <math.h>
#include "fwlapack.h"

#include "matrix.h"
#include "util.h"

#ifndef lint
static char vcid[] GCC_UNUSED = "$Id: matrix.c,v 1.1.1.1 2003/01/30 21:30:18 ernstl Exp $";
#endif /* lint */

static void
fill_matrix (CMat * matrix, const gint m, REAL f (gint, gint, REAL),
             const REAL fun_arg);

static void
fill_matrix2 (CMat * matrix, const gint m,
              REAL f (const gint, const gint, const REAL, const REAL),
              const REAL fun_arg1, const REAL fun_arg2);

Mat *
allocate_matrix (gint nrows, gint ncols)
{
  Mat *result = g_malloc0 (sizeof (Mat));

  result->cols = ncols;
  result->rows = nrows;
  result->data = g_new0 (REAL, nrows * ncols);
  return (result);
}

void
finish_matrix (Mat * mat)
{
  g_free (mat->data);
}

void
finish_and_free_matrix (Mat * mat)
{
  g_free (mat->data);
  g_free (mat);
}

REAL *
mat_eltptr (Mat * mat, const gint r, const gint c)
{
#ifdef RF_DEBUG
  g_assert ((r >= 0) && (r < mat->rows));
  g_assert ((c >= 0) && (c < mat->rows));
#endif
  return (&(mat->data[mat->rows * c + r]));
}

REAL
mat_elt (const Mat * mat, const gint r, const gint c)
{
#ifdef RF_DEBUG
  g_assert ((r >= 0) && (r < mat->rows));
  g_assert ((c >= 0) && (c < mat->rows));
#endif
  return (mat->data[mat->rows * c + r]);
}


void
init_c_mat (CMat * mat, const gint radius)
{
  mat->radius = radius;
  mat->row_stride = 2 * radius + 1;
  mat->data = g_new0 (REAL, SQR (mat->row_stride));
  mat->center = mat->data + mat->row_stride * mat->radius + mat->radius;
}

CMat *
allocate_c_mat (const gint radius)
{
  CMat *result = g_malloc0 (sizeof (CMat));

  init_c_mat (result, radius);
  return (result);
}

void
finish_c_mat (CMat * mat)
{
  g_free (mat->data);
  mat->data = NULL;
}

inline REAL *
c_mat_eltptr (CMat * mat, const gint col, const gint row)
{
#ifdef RF_DEBUG
  g_assert ((ABS (row) <= mat->radius) && (ABS (col) <= mat->radius));
#endif
  return (mat->center + mat->row_stride * row + col);
}

inline REAL
c_mat_elt (const CMat * const mat, const gint col, const gint row)
{
#ifdef RF_DEBUG
  g_assert ((ABS (row) <= mat->radius) && (ABS (col) <= mat->radius));
#endif
  return (mat->center[mat->row_stride * row + col]);
}

void
convolve_mat (CMat * result, const CMat * const mata, const CMat * const matb)
{
  register gint xr, yr, xa, ya;

  for (yr = -result->radius; yr <= result->radius; yr++)
    {
      for (xr = -result->radius; xr <= result->radius; xr++)
        {
          const gint ya_low = MAX (-mata->radius, yr - matb->radius);
          const gint ya_high = MIN (mata->radius, yr + matb->radius);
          const gint xa_low = MAX (-mata->radius, xr - matb->radius);
          const gint xa_high = MIN (mata->radius, xr + matb->radius);
          register REAL val = 0.0;

          for (ya = ya_low; ya <= ya_high; ya++)
            {
              for (xa = xa_low; xa <= xa_high; xa++)
                {
                  val += c_mat_elt (mata, xa, ya) *
                    c_mat_elt (matb, xr - xa, yr - ya);
                };
            };
          *c_mat_eltptr (result, xr, yr) = val;
        };
    };
}

void
convolve_star_mat (CMat * result, const CMat * const mata,
                   const CMat * const matb)
{
  register gint xr, yr, xa, ya;

  for (yr = -result->radius; yr <= result->radius; yr++)
    {
      for (xr = -result->radius; xr <= result->radius; xr++)
        {
          const gint ya_low = MAX (-mata->radius, -matb->radius - yr);
          const gint ya_high = MIN (mata->radius, matb->radius - yr);
          const gint xa_low = MAX (-mata->radius, -matb->radius - xr);
          const gint xa_high = MIN (mata->radius, matb->radius - xr);
          register REAL val = 0.0;

          for (ya = ya_low; ya <= ya_high; ya++)
            {
              for (xa = xa_low; xa <= xa_high; xa++)
                {
                  val += c_mat_elt (mata, xa, ya) *
                    c_mat_elt (matb, xr + xa, yr + ya);
                };
            };
          *c_mat_eltptr (result, xr, yr) = val;
        };
    };
}

void
convolve_mat_fun (CMat * result, const CMat * const mata,
                  REAL (f) (gint, gint))
{
  register gint xr, yr, xa, ya;

  for (yr = -result->radius; yr <= result->radius; yr++)
    {
      for (xr = -result->radius; xr <= result->radius; xr++)
        {
          register REAL val = 0.0;

          for (ya = -mata->radius; ya <= mata->radius; ya++)
            {
              for (xa = -mata->radius; xa <= mata->radius; xa++)
                {
                  val += c_mat_elt (mata, xa, ya) * f (xr - xa, yr - ya);
                };
            };
          *c_mat_eltptr (result, xr, yr) = val;
        };
    };
}

gint
as_idx (const gint k, const gint l, const gint m)
{
  return ((k + m) * (2 * m + 1) + (l + m));
}

gint
as_cidx (const gint k, const gint l)
{
  const gint a = MAX (ABS (k), ABS (l));
  const gint b = MIN (ABS (k), ABS (l));
  return ((a * (a + 1)) / 2 + b);
}

void
print_c_mat (FILE * file, const CMat * const mat)
{
  register gint x, y;

  for (y = -mat->radius; y <= mat->radius; y++)
    {
      for (x = -mat->radius; x <= mat->radius; x++)
        {
          fprintf (file, "%f ", c_mat_elt (mat, x, y));
        };
      fprintf (file, "\n");
    };
}

void
print_matrix (FILE * file, Mat * matrix)
{
  gint col_idx, row_idx;

  for (row_idx = 0; row_idx < matrix->rows; row_idx++)
    {
      for (col_idx = 0; col_idx < matrix->cols; col_idx++)
        {
          fprintf (file, "%f ", mat_elt (matrix, row_idx, col_idx));
        };
      fprintf (file, "\n");
    };
}

Mat *
make_s_matrix (CMat * mat, gint m, REAL noise_factor)
{
  const gint mat_size = SQR (2 * m + 1);
  Mat *result = allocate_matrix (mat_size, mat_size);
  register gint yr, yc, xr, xc;

  for (yr = -m; yr <= m; yr++)
    {
      for (xr = -m; xr <= m; xr++)
        {
          for (yc = -m; yc <= m; yc++)
            {
              for (xc = -m; xc <= m; xc++)
                {
                  *mat_eltptr (result, as_idx (xr, yr, m),
                               as_idx (xc, yc, m)) =
                    c_mat_elt (mat, xr - xc, yr - yc);
                  if ((xr == xc) && (yr == yc))
                    {
                      *mat_eltptr (result, as_idx (xr, yr, m),
                                   as_idx (xc, yc, m)) += noise_factor;
                    };
                };
            };
        };
    };
  return (result);
}

Mat *
make_s_cmatrix (CMat * mat, gint m, REAL noise_factor)
{
  const gint mat_size = as_cidx (m + 1, 0);
  Mat *result = allocate_matrix (mat_size, mat_size);
  register gint yr, yc, xr, xc;

  for (yr = 0; yr <= m; yr++)
    {
      for (xr = 0; xr <= yr; xr++)
        {
          for (yc = -m; yc <= m; yc++)
            {
              for (xc = -m; xc <= m; xc++)
                {
                  *mat_eltptr (result, as_cidx (xr, yr), as_cidx (xc, yc)) +=
                    c_mat_elt (mat, xr - xc, yr - yc);
                  if ((xr == xc) && (yr == yc))
                    {
                      *mat_eltptr (result, as_cidx (xr, yr),
                                   as_cidx (xc, yc)) += noise_factor;
                    };
                };
            };
        };
    };
  return (result);
}


REAL
correlation (const gint x, const gint y, const REAL gamma, const REAL musq)
{
  return (musq + pow (gamma, sqrt (SQR (x) + SQR (y))));
}

Mat *
copy_vec (const CMat * const mat, const gint m)
{
  Mat *result = allocate_matrix (SQR (2 * m + 1), 1);
  register gint x, y, index = 0;

  for (y = -m; y <= m; y++)
    {
      for (x = -m; x <= m; x++)
        {
          *mat_eltptr (result, index, 0) = c_mat_elt (mat, x, y);
          index++;
        };
    };
  g_assert (index == SQR (2 * m + 1));
  return (result);
}

Mat *
copy_cvec (const CMat * const mat, const gint m)
{
  Mat *result = allocate_matrix (as_cidx (m + 1, 0), 1);
  register gint x, y, index = 0;

  for (y = 0; y <= m; y++)
    {
      for (x = 0; x <= y; x++)
        {
          *mat_eltptr (result, index, 0) = c_mat_elt (mat, x, y);
          index++;
        };
    };
  g_assert (index == as_cidx (m + 1, 0));
  return (result);
}

CMat *
copy_cvec2mat (const Mat * const cvec, const gint m)
{
  CMat *result = allocate_c_mat (m);
  register gint x, y;

  for (y = -m; y <= m; y++)
    {
      for (x = -m; x <= m; x++)
        {
          *c_mat_eltptr (result, x, y) = mat_elt (cvec, as_cidx (x, y), 0);
        };
    };
  return (result);
}

CMat *
copy_vec2mat (const Mat * const cvec, const gint m)
{
  CMat *result = allocate_c_mat (m);
  register gint x, y;

  for (y = -m; y <= m; y++)
    {
      for (x = -m; x <= m; x++)
        {
          *c_mat_eltptr (result, x, y) = mat_elt (cvec, as_idx (x, y, m), 0);
        };
    };
  return (result);
}


CMat *
compute_g (const CMat * const convolution, const gint m, const REAL gamma,
           const REAL noise_factor, const REAL musq, const gboolean symmetric)
{
  CMat h_conv_ruv, a, corr;
  CMat *result;
  Mat *b;
  Mat *s;
  int status;

  init_c_mat (&h_conv_ruv, 3 * m);
  fill_matrix2 (&corr, 4 * m, correlation, gamma, musq);
  convolve_mat (&h_conv_ruv, convolution, &corr);
  init_c_mat (&a, 2 * m);
  convolve_star_mat (&a, convolution, &h_conv_ruv);
  if (symmetric)
    {
      s = make_s_cmatrix (&a, m, noise_factor);
      b = copy_cvec (&h_conv_ruv, m);
    }
  else
    {
      s = make_s_matrix (&a, m, noise_factor);
      b = copy_vec (&h_conv_ruv, m);
    };
#ifdef RF_DEBUG
  fprintf (stderr, "Convolution:\n");
  print_c_mat (stderr, convolution);
  fprintf (stderr, "h_conv_ruv:\n");
  print_c_mat (stderr, &h_conv_ruv);
  fprintf (stderr, "Value of s:\n");
  print_matrix (stderr, s);
  fprintf (stderr, "\n");
#endif

  g_assert (s->cols == s->rows);
  g_assert (s->rows == b->rows);
  status = dgesv (s->rows, 1, s->data, s->rows, b->data, b->rows);

  if (symmetric)
    {
      result = copy_cvec2mat (b, m);
    }
  else
    {
      result = copy_vec2mat (b, m);
    };
#ifdef RF_DEBUG
  fprintf (stderr, "Deconvolution:\n");
  print_c_mat (stderr, result);
  fprintf (stderr, "\n");
#endif

  finish_c_mat (&a);
  finish_c_mat (&h_conv_ruv);
  finish_c_mat (&corr);
  finish_and_free_matrix (s);
  finish_and_free_matrix (b);
  return (result);
}

CMat *
compute_g_matrix (const CMat * const convolution, const gint m,
                  const REAL gamma, const REAL noise_factor,
                  const REAL musq, const gboolean symmetric)
{
  CMat *g = compute_g (convolution, m, gamma, noise_factor, musq, symmetric);
  gint r, c;
  REAL sum = 0.0;

  /* Determine sum of array */
  for (r = -g->radius; r <= g->radius; r++)
    {
      for (c = -g->radius; c <= g->radius; c++)
        {
          sum += c_mat_elt (g, r, c);
        }
    };
  for (r = -g->radius; r <= g->radius; r++)
    {
      for (c = -g->radius; c <= g->radius; c++)
        {
          *c_mat_eltptr (g, r, c) /= sum;
        }
    };
  return (g);
}

static void
fill_matrix (CMat * matrix, const gint m,
             REAL f (const gint, const gint, const REAL), const REAL fun_arg)
{
  register gint x, y;
  init_c_mat (matrix, m);

  for (y = -m; y <= m; y++)
    {
      for (x = -m; x <= m; x++)
        {
          *c_mat_eltptr (matrix, x, y) = f (x, y, fun_arg);
        }
    }
}

static void
fill_matrix2 (CMat * matrix, const gint m,
              REAL f (const gint, const gint, const REAL, const REAL),
              const REAL fun_arg1, const REAL fun_arg2)
{
  register gint x, y;
  init_c_mat (matrix, m);

  for (y = -m; y <= m; y++)
    {
      for (x = -m; x <= m; x++)
        {
          *c_mat_eltptr (matrix, x, y) = f (x, y, fun_arg1, fun_arg2);
        }
    }
}

void
make_gaussian_convolution (const REAL gradius, CMat * convolution,
                           const gint m)
{
  register gint x, y;

  init_c_mat (convolution, m);
  if (SQR (gradius) <= 1 / G_MAXFLOAT)
    {
      for (y = -m; y <= m; y++)
        {
          for (x = -m; x <= m; x++)
            {
              *c_mat_eltptr (convolution, x, y) = 0;
            }
        }
      *c_mat_eltptr (convolution, 0, 0) = 1;
    }
  else
    {
      const REAL alpha = log (2.0) / SQR (gradius);
      for (y = -m; y <= m; y++)
        {
          for (x = -m; x <= m; x++)
            {
              *c_mat_eltptr (convolution, x, y) =
                exp (-alpha * (SQR (x) + SQR (y)));
            }
        }
    }
}

REAL
circle_integral (const REAL x, const REAL radius)
     /* Return the integral of sqrt(radius^2 - z^2) for z = 0 to x */
{
  if (radius == 0)
    {                           /* Perhaps some epsilon must be added here */
      return (0);
    }
  else
    {
      const REAL sin = x / radius;
      const REAL sq_diff = SQR (radius) - SQR (x);
      /* From a mathematical point of view the following is redundant.
         Numerically they are not equivalent!
       */
      if ((sq_diff < 0.0) || (sin < -1.0) || (sin > 1.0))
        {
          if (sin < 0)
            {
              return (-0.25 * SQR (radius) * G_PI);
            }
          else
            {
              return (0.25 * SQR (radius) * G_PI);
            }
        }
      else
        {
          return (0.5 * x * sqrt (sq_diff) + 0.5 * SQR (radius) * asin (sin));
        }
    }
}

REAL
circle_intensity (const gint x, const gint y, const REAL radius)
{
  if (radius == 0)
    {
      return (((x == 0) && (y == 0)) ? 1 : 0);
    }
  else
    {
      register REAL xlo = ABS (x) - 0.5, xhi = ABS (x) + 0.5,
        ylo = ABS (y) - 0.5, yhi = ABS (y) + 0.5;
      register REAL symmetry_factor = 1, xc1, xc2;

      if (xlo < 0)
        {
          xlo = 0;
          symmetry_factor *= 2;
        };
      if (ylo < 0)
        {
          ylo = 0;
          symmetry_factor *= 2;
        };
      if (SQR (xlo) + SQR (yhi) > SQR (radius))
        {
          xc1 = xlo;
        }
      else if (SQR (xhi) + SQR (yhi) > SQR (radius))
        {
          xc1 = sqrt (SQR (radius) - SQR (yhi));
        }
      else
        {
          xc1 = xhi;
        };
      if (SQR (xlo) + SQR (ylo) > SQR (radius))
        {
          xc2 = xlo;
        }
      else if (SQR (xhi) + SQR (ylo) > SQR (radius))
        {
          xc2 = sqrt (SQR (radius) - SQR (ylo));
        }
      else
        {
          xc2 = xhi;
        };
      return (((yhi - ylo) * (xc1 - xlo) +
               circle_integral (xc2, radius) - circle_integral (xc1, radius) -
               (xc2 - xc1) * ylo) * symmetry_factor / (G_PI * SQR (radius)));
    }
}

void
make_circle_convolution (const REAL radius, CMat * convolution, const gint m)
{
  fill_matrix (convolution, m, circle_intensity, radius);
}
