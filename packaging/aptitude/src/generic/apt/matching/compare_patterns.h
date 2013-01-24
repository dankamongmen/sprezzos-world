// compare_patterns.h       -*-c++-*-
//
//   Copyright (C) 2008 Daniel Burrows
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

#ifndef EQUAL_H
#define EQUAL_H

/** \file compare_patterns.h */

#include <cwidget/generic/util/ref_ptr.h>

namespace aptitude
{
  namespace matching
  {
    class pattern;

    /** \brief Compare two patterns.
     *
     *  The comparison is strictly syntactic; no effort is made to
     *  test patterns for equivalence (which is good, since I don't
     *  believe that it would be possible, even in principle, to do
     *  so).
     *
     *  The motivation behind this function is a desire to write test
     *  cases for the pattern parser.
     *
     *  \return a number less than zero if p1 is less than p2, a
     *  number greater than zero if p1 is greater than p2, and zero if
     *  p1 equals p2.
     */
    int compare_patterns(const cwidget::util::ref_ptr<pattern> &p1,
			 const cwidget::util::ref_ptr<pattern> &p2);
  }
}

#endif // EQUAL_H
