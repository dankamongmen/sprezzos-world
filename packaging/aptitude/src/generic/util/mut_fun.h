// mut_fun.h                                    -*-c++-*-
//
//   Copyright (C) 2005 Daniel Burrows
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

#ifndef MUT_FUN_H
#define MUT_FUN_H

/** \file mut_fun.h
 */

/** A functor that assigns a value to a variable when it is
 *  triggered.
 */
template<class T>
class _mut_fun_slot
{
  T& target;
public:
  _mut_fun_slot(T& _target):target(_target) {}

  void operator()(const T& source) const {target=source;}
};

template<class T>
inline _mut_fun_slot<T> mut_fun(T& target)
{
  return _mut_fun_slot<T>(target);
}

#endif // MUT_FUN_H
