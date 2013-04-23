/* Wrapper to dgesv from either CLAPACK or ATLAS
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
 * Version $Id: fwlapack.c,v 1.1.1.1 2003/01/30 21:30:18 ernstl Exp $
 */


#include <glib.h>

#include "fwlapack.h"
#include "util.h"

#ifndef lint
static char vcid[] GCC_UNUSED = "$Id: fwlapack.c,v 1.1.1.1 2003/01/30 21:30:18 ernstl Exp $";
#endif /* lint */

#ifdef HAVE_ATLAS
/* Fix stupid bugs in the atlas header file that use ATLAS_* instead of CBLAS_ */
#define ATLAS_ORDER CBLAS_ORDER
#define ATLAS_UPLO CBLAS_UPLO
#define ATLAS_DIAG CBLAS_DIAG

#include "clapack.h"

#else /* Not HAVE_ATLAS */

#include "f2c.h"
#include "clapack.h"
#endif

int
dgesv (const int N, const int NRHS, double *A, const int lda,
       double *B, const int ldb)
{
  int result = 0;
#ifdef HAVE_ATLAS
  int *ipiv = g_new (int, N);
  result = clapack_dgesv (CblasColMajor, N, NRHS, A, lda, ipiv, B, ldb);
  g_free (ipiv);
#else
  integer i_N = N, i_NHRS = NRHS, i_lda = lda, i_ldb = ldb, info;
  integer *ipiv = g_new (integer, N);

  dgesv_ (&i_N, &i_NHRS, A, &i_lda, ipiv, B, &i_ldb, &info);
  g_free (ipiv);
  result = info;
#endif
  return (result);
}
