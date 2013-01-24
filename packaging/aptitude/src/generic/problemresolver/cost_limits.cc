/** \file cost_limits.cc */


// Copyright (C) 2010 Daniel Burrows
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

#include "cost_limits.h"

const int cost_limits::maximum_level;
const int cost_limits::conflict_structural_level;
const int cost_limits::defer_structural_level;
const int cost_limits::already_generated_structural_level;
const int cost_limits::minimum_level;

const cost cost_limits::maximum_structural_level_cost(cost::make_advance_structural_level(maximum_level));
const cost cost_limits::conflict_cost(cost::make_advance_structural_level(conflict_structural_level));
const cost cost_limits::already_generated_cost(cost::make_advance_structural_level(already_generated_structural_level));
const cost cost_limits::defer_cost(cost::make_advance_structural_level(defer_structural_level));
const cost cost_limits::minimum_cost;
