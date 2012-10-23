/*
 * mem.c
 *
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *               2003 Bastian Blank <waldi@debian.org>
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
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#include <debian-installer/mem.h>

#include <debian-installer/macros.h>
#include <debian-installer/log.h>

#include <stdlib.h>
#include <string.h>

void *di_malloc (size_t n_bytes)
{
  void *mem;

  mem = malloc (n_bytes);

  if (!mem)
    di_error ("%s: failed to allocate %zu bytes", DI_STRLOC, n_bytes);
  return mem;
}

void *di_malloc0 (size_t n_bytes)
{
  void *mem;

  mem = calloc (1, n_bytes);

  if (!mem)
    di_error ("%s: failed to allocate %zu bytes", DI_STRLOC, n_bytes);
  return mem;
}

void *di_realloc (void *mem, size_t n_bytes)
{
  mem = realloc (mem, n_bytes);

  if (!mem)
    di_error ("%s: failed to allocate %zu bytes", DI_STRLOC, n_bytes);
  return mem;
}

void di_free (void *mem)
{
  free (mem);
}

