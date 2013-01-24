/** \file aptitude_resolver_cost_types.cc */


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



#include "aptitude_resolver_cost_types.h"

#include <iostream>

bool cost_component_structure::operator==(const cost_component_structure &other) const
{
  return combining_op == other.combining_op && *entries == *other.entries;
}

std::ostream &operator<<(std::ostream &out, const cost_component_structure &costs)
{
  const std::vector<cost_component_structure::entry> &entries =
    *costs.get_entries();

  if(costs.get_combining_op() == cost_component_structure::combine_max)
    out << "max(";

  bool first_entry = true;
  for(std::vector<cost_component_structure::entry>::const_iterator
        entries_it = entries.begin(); entries_it != entries.end();
      ++entries_it)
    {
      if(entries_it->get_scaling_factor() == 0)
        continue;

      if(first_entry)
        first_entry = false;
      else switch(costs.get_combining_op())
             {
             case cost_component_structure::combine_add:
               out << " + ";
               break;

             case cost_component_structure::combine_max:
               out << ", ";
               break;

             default:
               // Should never happen.
               out << " ";
               break;
             }

      if(entries_it->get_scaling_factor() != 1)
        out << entries_it->get_scaling_factor() << "*";
      out << entries_it->get_name();
    }

  if(costs.get_combining_op() == cost_component_structure::combine_max)
    out << ")";

  return out;
}
