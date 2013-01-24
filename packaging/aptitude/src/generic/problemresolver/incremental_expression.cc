/** \file incremental_expression.cc */   // -*-c++-*-


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

#include "incremental_expression.h"

void counting_bool_e::init_num_true()
{
  const std::vector<cwidget::util::ref_ptr<expression<bool> > > &children(get_children());

  num_true = 0;
  for(std::vector<cwidget::util::ref_ptr<expression<bool> > >::const_iterator
	it = children.begin(); it != children.end(); ++it)
    if(!it->valid() || (*it)->get_value())
      ++num_true;
}

void counting_bool_e::child_modified(const cwidget::util::ref_ptr<expression<bool> > &child,
				     bool old_value,
				     bool new_value)
{
  if(old_value != new_value) // Should always be true, but it's cheap
			     // to double-check.
    {
      bool my_old_value = get_value();

      if(new_value)
	++num_true;
      else
	--num_true;

      bool my_new_value = get_value();
      if(my_new_value != my_old_value)
	signal_value_changed(my_old_value, my_new_value);
    }
}

void counting_bool_e::add_child(const cwidget::util::ref_ptr<expression<bool> > &child)
{
  bool old_value = get_value();

  expression_container_base<bool>::add_child(child);
  if(!child.valid() || child->get_value())
    ++num_true;

  bool new_value = get_value();

  if(old_value != new_value)
    signal_value_changed(old_value, new_value);
}

void counting_bool_e::remove_child(const cwidget::util::ref_ptr<expression<bool> > &child)
{
  bool old_value = get_value();

  expression_container_base<bool>::remove_child(child);
  if(!child.valid() || child->get_value())
    --num_true;

  bool new_value = get_value();

  if(old_value != new_value)
    signal_value_changed(old_value, new_value);
}

bool and_e::get_value()
{
  return get_num_true() == get_children().size();
}

std::string and_e::get_name()
{
  return "and";
}

bool or_e::get_value()
{
  return get_num_true() > 0;
}

std::string or_e::get_name()
{
  return "or";
}

void not_e::child_modified(const cwidget::util::ref_ptr<expression<bool> > &child,
			   bool old_value,
			   bool new_value)
{
  signal_value_changed(!old_value, !new_value);
}

bool not_e::get_value()
{
  if(get_child().valid())
    return !get_child()->get_value();
  else
    // NULL pointers count as "true".
    return false;
}

void not_e::dump(std::ostream &out)
{
  out << "~" << get_child();
}
