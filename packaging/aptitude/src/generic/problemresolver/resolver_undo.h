// resolver_undo.h                            -*-c++-*-
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
//

#ifndef RESOLVER_UNDO_H
#define RESOLVER_UNDO_H

#include <generic/util/undo.h>

/** \brief Undo items related to the resolver
 *
 * 
 * Undo items related to the resolver.  (Q: these could be unified
 * into a single item based on slots or somesuch?)
 * 
 *  \file resolver_undo.h
 */

template<typename PackageUniverse>
class generic_problem_resolver;

/** An undoable that executes the given action on the given object. */
template<typename PackageUniverse, typename T>
class undo_resolver_manipulation : public undoable
{
public:
  typedef void (generic_problem_resolver<PackageUniverse>::*undo_method)(const T &, undo_group *);

  generic_problem_resolver<PackageUniverse> *resolver;
  T t;
  undo_method reverse_action;

public:
  undo_resolver_manipulation(generic_problem_resolver<PackageUniverse> *_resolver,
			     const T &_t,
			     undo_method _reverse_action)
    : resolver(_resolver), t(_t), reverse_action(_reverse_action)
  {
  }

  void undo()
  {
    (resolver->*reverse_action)(t, NULL);
  }
};

#endif // RESOLVER_UNDO_H
