/** \file cost_limits.h */    // -*-c++-*-


//   Copyright (C) 2009-2010 Daniel Burrows
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

#ifndef COST_LIMITS_H
#define COST_LIMITS_H

#include "cost.h"

class cost_limits
{
public:
  static const int maximum_level = INT_MAX;

  /** \brief The maximum structural level; reserved for solutions that
   *  contain a logical conflict and thus are dead-ends.
   *
   *  Search nodes at this level are discarded without being visited.
   */
  static const int conflict_structural_level = maximum_level;

  /** \brief The second highest structural level; reserved for
   *  solutions that were already generated (to prevent them from
   *  being generated again).
   *
   *  Search nodes at this level are discarded without being visited.
   */
  static const int already_generated_structural_level = conflict_structural_level - 1;

  /** \brief The third highest structural level; reserved for solutions
   *  that violate a user constraint and will be deferred until the
   *  constraints are changed.
   */
  static const int defer_structural_level = conflict_structural_level - 2;

  /** \brief The minimum level; this is the initial level of the empty
   *  solution.
   */
  static const int minimum_level = INT_MIN;

  /** \brief Costs in which only a structural level is set. */
  // @{
  static const cost maximum_structural_level_cost;
  static const cost conflict_cost;
  static const cost already_generated_cost;
  static const cost defer_cost;
  static const cost minimum_cost;
  // @}
};

#endif
