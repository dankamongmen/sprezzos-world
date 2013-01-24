// dirent_safe.h     -*-c++-*-
//
//   Copyright (C) 2007, 2011 Daniel Burrows
//
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.

#ifndef DIRENT_SAFE_H
#define DIRENT_SAFE_H

#include <dirent.h>
#include <stddef.h>

/** \file dirent_safe.h
 */

/** \brief A version of dirent that's safe to use with readdir_r
 *  across platforms.
 *
 *  See the GNU C library documentation of readdir_r for an
 *  explanation of the raison d'etry of this struct; basically, some
 *  platforms define dirent to be too small.
 */
union dirent_safe
{
  /** \brief The actual dirent. */
  struct dirent d;
#if NAME_MAX < 255
  char b[offsetof(struct dirent, d_name) + 255 + 1];
#else
  char b[offsetof(struct dirent, d_name) + NAME_MAX + 1];
#endif
};

#endif // DIRENT_SAFE_H
