/** \file cost.cc */


// Copyright (C) 2010 Daniel Burrows
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

#include "cost.h"
#include "cost_limits.h"

#include <ostream>

std::ostream &operator<<(std::ostream &out, const level &l)
{
  if(l.get_value() == cost_limits::minimum_level)
    out << "minimum";
  else if(l.get_value() == cost_limits::maximum_level)
    out << "maximum";
  else
    out << l.get_value();

  return out;
}

cost::cost_impl::cost_impl(const cost_impl &cost1, const cost_impl &cost2, combine_tag)
  : structural_level(std::max<int>(cost1.structural_level,
				   cost2.structural_level))
{
  // Straightforward merge (why doesn't STL have a merge that lets
  // you combine equivalent elements instead of just copying one
  // of them to the output?
  std::vector<std::pair<level_index, level> >::const_iterator
    it1 = cost1.actions.begin(), it2 = cost2.actions.begin();

  while(it1 != cost1.actions.end() && it2 != cost2.actions.end())
    {
      if(it1->first < it2->first)
	{
	  actions.push_back(*it1);
	  ++it1;
	}
      else if(it2->first < it1->first)
	{
	  actions.push_back(*it2);
	  ++it2;
	}
      else
	{
	  actions.push_back(std::make_pair(it1->first,
					   level::combine(it1->second, it2->second)));
	  ++it1;
	  ++it2;
	}
    }

  if(it1 != cost1.actions.end())
    actions.insert(actions.end(), it1, cost1.actions.end());
  else if(it2 != cost2.actions.end())
    actions.insert(actions.end(), it2, cost2.actions.end());
}

bool cost::cost_impl::is_above_or_equal(const cost_impl &other) const
{
  std::vector<std::pair<level_index, level> >::const_iterator
    it_this = actions.begin(), it_other = other.actions.begin();

  bool is_greater_or_equal = true;

  if(structural_level < other.structural_level)
    is_greater_or_equal = false;

  while(is_greater_or_equal && it_this != actions.end() && it_other != other.actions.end())
    {
      if(it_this->first < it_other->first)
	++it_this;
      else if(it_other->first < it_this->first)
	{
	  if(it_other->second.get_state() != level::unmodified)
	    is_greater_or_equal = false;
	  ++it_other;
	}
      else
	{
	  if( !(it_this->second == it_other->second) )
	    {
	      if(!it_this->second.is_above_or_equal(it_other->second))
		is_greater_or_equal = false;
	    }

	  ++it_this;
	  ++it_other;
	}
    }

  if(is_greater_or_equal && it_other != other.actions.end())
    is_greater_or_equal = false;

  return is_greater_or_equal;
}

int cost::cost_impl::compare(const cost_impl &other) const
{
  const int structural_level_cmp = aptitude::util::compare3(structural_level, other.structural_level);
  if(structural_level_cmp != 0)
    return structural_level_cmp;
  else
    return aptitude::util::compare3(actions, other.actions);
}

void cost::cost_impl::dump(std::ostream &out) const
{
  out << "(";
  if(structural_level != INT_MIN)
    out << "advance: " << structural_level;
  else
    out << "nop";

  std::size_t column = 0;
  for(std::vector<std::pair<level_index, level> >::const_iterator
	it = actions.begin(); it != actions.end(); ++it)
    {
      out << ", ";

      while(column < it->first)
	{
	  out << "nop, ";
	  ++column;
	}

      switch(it->second.get_state())
	{
	case level::added:
	  out << "add: ";
	  break;
	case level::lower_bounded:
	  out << "advance: ";
	  break;
	case level::unmodified:
	  // Shouldn't happen:
	  out << "(unmodified): ";
	  break;
	}
      out << it->second.get_value();
      ++column;
    }

  out << ")";
}

cost::cost_impl::cost_impl(const cost_impl &cost1, const cost_impl &cost2, upper_bound_tag)
  : structural_level(std::max<int>(cost1.structural_level,
				   cost2.structural_level))
{
  actions.reserve(std::max<std::size_t>(cost1.actions.size(), cost2.actions.size()));

  std::vector<std::pair<level_index, level> >::const_iterator
    it1 = cost1.actions.begin(),
    it2 = cost2.actions.begin();

  const std::vector<std::pair<level_index, level> >::const_iterator
    end1 = cost1.actions.end(),
    end2 = cost2.actions.end();

  while(it1 != end1 && it2 != end2)
    {
      const std::pair<level_index, level> &p1 = *it1, &p2 = *it2;

      if(p1.first < p2.first)
	{
	  actions.push_back(p1);
	  ++it1;
	}
      else if(p2.first < p1.first)
	{
	  actions.push_back(p2);
	  ++it2;
	}
      else
	{
	  actions.push_back(std::make_pair(p1.first,
					   level::upper_bound(p1.second, p2.second)));
	  ++it1;
	  ++it2;
	}
    }

  if(it1 != end1)
    actions.insert(actions.end(),
		   it1, end1);
  else if(it2 != end2)
    actions.insert(actions.end(),
		   it2, end2);
}

cost::cost_impl::cost_impl(const cost_impl &cost1, const cost_impl &cost2, lower_bound_tag)
  : structural_level(std::min<int>(cost1.structural_level,
				   cost2.structural_level))
{
  std::vector<std::pair<level_index, level> >::const_iterator
    it1 = cost1.actions.begin(),
    it2 = cost2.actions.begin();

  const std::vector<std::pair<level_index, level> >::const_iterator
    end1 = cost1.actions.end(),
    end2 = cost2.actions.end();

  while(it1 != end1 && it2 != end2)
    {
      const std::pair<level_index, level> &p1 = *it1, &p2 = *it2;

      if(p1.first < p2.first)
	++it1;
      else if(p2.first < p1.first)
	++it2;
      else
	{
	  actions.push_back(std::make_pair(p1.first,
					   level::lower_bound(p1.second, p2.second)));
	  ++it1;
	  ++it2;
	}
    }
}

level cost::cost_impl::get_user_level(level_index idx) const
{

  // "Slow" implementation right now because this isn't used much
  // (only for display).
  for(std::vector<std::pair<level_index, level> >::const_iterator it =
	actions.begin(); it != actions.end(); ++it)
    if(it->first == idx)
      return it->second;

  return level();
}

cost cost::least_upper_bound(const cost &cost1,
                             const cost &cost2)
{
  return cost(cost1, cost2, upper_bound_tag());
}

cost cost::greatest_lower_bound(const cost &cost1,
                                const cost &cost2)
{
  return cost(cost1, cost2, lower_bound_tag());
}

std::size_t hash_value(const cost &cost)
{
  return cost.get_hash_value();
}

std::ostream &operator<<(std::ostream &out, const cost &t)
{
  t.dump(out);

  return out;
}
