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
 * Version $Id: fwlapack.h,v 1.1.1.1 2003/01/30 21:30:18 ernstl Exp $
 */
#ifndef FWLAPACK_H_INCLUDED
#include <glib.h>

G_BEGIN_DECLS

/* Wrapper function for lapack dgesv function */
int dgesv (const int N, const int NRHS, double *A, const int lda,
           double *B, const int ldb);

G_END_DECLS
#define FWLAPACK_H_INCLUDED (1)
#endif
