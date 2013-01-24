/** \file choice_indexed_set.h */    // -*-c++-*-


//   Copyright (C) 2009 Daniel Burrows
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

#ifndef CHOICE_INDEXED_SET_H
#define CHOICE_INDEXED_SET_H

#include "choice.h"

#include <generic/util/compare3.h>
#include <generic/util/immset.h>
#include <generic/util/maybe.h>

#include <functional>
#include <iostream>
#include <utility>

/** \brief A map from choices to objects, with support for iterating
 *         over the objects associated with choices contained in a
 *         particular choice.
 *
 *  \todo Various things could be made more efficient if this object
 *  (and also imm::map) supported an update-in-place variant of
 *  for_each.
 *
 *  \tparam ValueType   The value type of the map.
 */
template<typename PackageUniverse, typename ValueType >
class generic_choice_indexed_map
{
public:
  typedef unsigned int size_type;

private:
  typedef typename PackageUniverse::package package;
  typedef typename PackageUniverse::version version;
  typedef typename PackageUniverse::dep dep;

  typedef generic_choice<PackageUniverse> choice;

  /** \brief Represents information about all the objects whose
   *  choices install a version.
   */
  struct version_info
  {
    maybe<ValueType> not_from_dep_source;
    imm::map<dep, ValueType> from_dep_source;
  };

  // Objects stored for the choice (Install(v), t).
  imm::map<version, version_info> install_version_objects;

  // Objects stored for the choice (Break(d), t).
  imm::map<dep, ValueType> break_dep_objects;

  // The total number of keys in this set.
  size_type curr_size;

  class do_dump_entry
  {
    std::ostream &out;
    bool &first;
  public:
    do_dump_entry(std::ostream &_out, bool &_first)
      : out(_out), first(_first)
    {
      first = true;
    }

    bool operator()(const choice &c, ValueType value) const
    {
      if(first)
	first = false;
      else
	out << ", ";

      out << c << " -> " << value;

      return true;
    }
  };
public:
  generic_choice_indexed_map()
    : curr_size()
  {
  }

  size_type size() const { return curr_size; }

  void put(const choice &c, ValueType value)
  {
    switch(c.get_type())
      {
      case choice::install_version:
	{
	  typename imm::map<version, version_info>::node
	    found_ver = install_version_objects.lookup(c.get_ver());

	  version_info inf;
	  if(found_ver.isValid())
	    inf = found_ver.getVal().second;


	  if(!c.get_from_dep_source())
	    {
	      if(!inf.not_from_dep_source.get_has_value())
		++curr_size;
	      inf.not_from_dep_source = value;
	    }
	  else
	    {
	      if(inf.from_dep_source.put(c.get_dep(), value))
		++curr_size;
	    }

	  install_version_objects.put(c.get_ver(), inf);
	}
	break;

      case choice::break_soft_dep:
	if(break_dep_objects.put(c.get_dep(), value))
	  ++curr_size;
	break;
      }
  }

  /** \brief Retrieve the value bound to a choice.
   *
   *  \param c        The choice to look up.
   *  \param output   A location in which to store the result.
   *                  Only modified if c is contained in the map.
   *
   *  \return \b true if c was contained in this map, \b false
   *  otherwise.
   */
  bool try_get(const choice &c, ValueType &output) const
  {
    switch(c.get_type())
      {
      case choice::install_version:
	{
	  typename imm::map<version, version_info>::node
	    found_ver = install_version_objects.lookup(c.get_ver());

	  if(found_ver.isValid())
	    {
	      const version_info &ver_inf(found_ver.getVal().second);

	      if(!c.get_from_dep_source())
		{
		  if(ver_inf.not_from_dep_source.get_has_value())
		    {
		      output = ver_inf.not_from_dep_source.get_value();
		      return true;
		    }
		  else
		    return false;
		}
	      else
		{
		  typename imm::map<dep, ValueType>::node
		    found_dep = ver_inf.from_dep_source.lookup(c.get_dep());

		  if(found_dep.isValid())
		    {
		      output = found_dep.getVal().second;
		      return true;
		    }
		  else
		    return false;
		}
	    }
	  else
	    return false;
	}

      case choice::break_soft_dep:
	{
	  typename imm::map<dep, ValueType>::node
	    found_dep = break_dep_objects.lookup(c.get_dep());

	  if(found_dep.isValid())
	    output = found_dep.getVal().second;

	  return found_dep.isValid();
	}
      }

    return false;
  }

  /** \brief Remove a binding from this map.
   *
   *  Only an exact match will be erased.
   */
  void erase(const choice &c)
  {
    switch(c.get_type())
      {
      case choice::install_version:
	{
	  typename imm::map<version, version_info>::node
	    found_ver = install_version_objects.lookup(c.get_ver());

	  version_info inf;
	  if(found_ver.isValid())
	    inf = found_ver.getVal().second;


	  if(!c.get_from_dep_source())
	    {
	      if(inf.not_from_dep_source.get_has_value())
		--curr_size;
	      inf.not_from_dep_source = maybe<ValueType>();
	    }
	  else
	    {
	      if(inf.from_dep_source.erase(c.get_dep()))
		--curr_size;
	    }

	  install_version_objects.put(c.get_ver(), inf);
	}
	break;

      case choice::break_soft_dep:
	if(break_dep_objects.erase(c.get_dep()))
	  --curr_size;
	break;
      }
  }

  /** \brief Dump this map to a stream, if ValueType supports
   *  operator<<.
   */
  void dump(std::ostream &out) const
  {
    out << "{";
    bool first = true;
    for_each(do_dump_entry(out, first));
    out << "}";
  }

private:
  // Applies the given function to (choice, value) for each (choice,
  // value) pair in the set of dependency -> value bindings.
  template<typename F>
  struct for_each_from_dep_source
  {
    version v;
    F f;

    for_each_from_dep_source(const version &_v, F _f)
      : v(_v), f(_f)
    {
    }

    bool operator()(const std::pair<dep, ValueType> &p) const
    {
      return f(choice::make_install_version_from_dep_source(v, p.first, -1),
	       p.second);
    }
  };
  // Applies the given function to (choice, value) for each (choice,
  // value) pair in the set of dependency -> value bindings
  // representing broken-dependency choices.
  template<typename F>
  struct for_each_break_soft_dep
  {
    F f;

    for_each_break_soft_dep(F _f)
      : f(_f)
    {
    }

    bool operator()(const std::pair<dep, ValueType> &p) const
    {
      return f(choice::make_break_soft_dep(p.first, -1),
	       p.second);
    }
  };

  // Applies a function to (choice, value) for each entry associated
  // with each version that it is applied to.
  template<typename F>
  class for_each_version_info
  {
    F f;

  public:
    for_each_version_info(F _f)
      : f(_f)
    {
    }

    bool operator()(const std::pair<version, version_info> &p) const
    {
      const version &v(p.first);
      const version_info &v_inf(p.second);

      if(v_inf.not_from_dep_source.get_has_value())
	{
	  if(!f(choice::make_install_version(v, -1),
		v_inf.not_from_dep_source.get_value()))
	    return false;
	}

      return v_inf.from_dep_source.for_each(for_each_from_dep_source<F>(v, f));
    }
  };

public:
  /** \brief Apply the given function object to (c, value) for each
   *  entry (c -> value) in this map.
   */
  template<typename F>
  bool for_each(F f) const
  {
    return
      install_version_objects.for_each(for_each_version_info<F>(f)) &&
      break_dep_objects.for_each(for_each_break_soft_dep<F>(f));
  }

  /** \brief Apply the given function object to (c', value) for each
   *  mapping (c' -> value) in this set such that c' is contained in
   *  c.
   *
   *  If f returns false, the iteration will abort.  Modifications to
   *  this set after the iteration begins do not affect which values
   *  are visited.
   */
  template<typename F>
  bool for_each_key_contained_in(const choice &c, F f) const
  {
    switch(c.get_type())
      {
      case choice::install_version:
	{
	  const version &v(c.get_ver());
	  typename imm::map<version, version_info>::node
	    found_ver(install_version_objects.lookup(v));
	  // Note that since we save a reference to the original,
	  // immutable node, we will always iterate over everything
	  // even if the structure of the tree is changed.

	  if(found_ver.isValid())
	    {
	      const version_info &ver_inf(found_ver.getVal().second);

	      if(!c.get_from_dep_source())
		{
		  // If the choice isn't a from-dep-source choice,
		  // apply the function to the not-from-dep-source
		  // cell and to all the from-dep-source cells.
		  if(ver_inf.not_from_dep_source.get_has_value())
		    {
		      if(!f(choice::make_install_version(v, -1),
			    ver_inf.not_from_dep_source.get_value()))
			return false;
		    }

		  for_each_from_dep_source<F> from_dep_source_f(v, f);
		  if(!ver_inf.from_dep_source.for_each(from_dep_source_f))
		    return false;
		}
	      else
		{
		  // If the choice is a from-dep-source choice, find
		  // the particular dependency that it applies to and
		  // apply f.
		  typename imm::map<dep, ValueType>::node found_dep =
		    ver_inf.from_dep_source.lookup(c.get_dep());

		  if(found_dep.isValid())
		    {
		      const dep &d(c.get_dep());
		      if(!f(choice::make_install_version_from_dep_source(v, d, -1),
			    found_dep.getVal().second))
			return false;
		    }
		}
	    }
	}
	break;

      case choice::break_soft_dep:
	{
	  const dep &d(c.get_dep());
	  typename imm::map<dep, ValueType>::node
	    found = break_dep_objects.lookup(d);

	  if(found.isValid())
	    {
	      if(!f(choice::make_break_soft_dep(d, -1),
		    found.getVal().second))
		return false;
	    }
	}
	break;
      }

    return true;
  }

private:
  // Whenever something is visited, aborts the for_each and returns
  // false.
  class not_visited
  {
  public:
    bool operator()(const choice &c, const ValueType &v) const
    {
      return false;
    }
  };

public:
  /** \brief Test whether this set contains a key that is contained in
   *  the given choice.
   */
  bool contains_key(const choice &c) const
  {
    return !for_each_key_contained_in(c, not_visited());
  }
};

/** \brief A set of objects, indexed for retrieval by an associated
 *  choice.
 *
 *  This differs from choice_set in that it stores tuples (choice, T)
 *  where "T" can be any LessThanComparable type.  It supports quickly
 *  iterating over all the tuples for which the choice is contained in
 *  an input choice.
 *
 *  \tparam T        The type of object stored in this set.
 *  \tparam Compare  How to compare T objects.
 */
template<typename PackageUniverse, typename T, typename Compare = aptitude::util::compare3_f<T> >
class generic_choice_indexed_set
{
  typedef generic_choice<PackageUniverse> choice;

  generic_choice_indexed_map<PackageUniverse, imm::set<T, Compare> > parent;

  // The comparison object.
  Compare compare;

public:
  generic_choice_indexed_set(Compare _compare = Compare())
    : compare(_compare)
  {
  }

  /** \brief Insert a tuple (c, t) into this set. */
  void insert(const choice &c, T t)
  {
    imm::set<T> values;
    parent.try_get(c, values);

    values.insert(t);
    parent.put(c, values);
  }

  /** \brief Remove a tuple (c, t) from this set. */
  void remove(const choice &c, T t)
  {
    imm::set<T> values;
    if(parent.try_get(c, values))
      {
	values.erase(t);
	if(values.empty())
	  parent.erase(c);
	else
	  parent.put(c, values);
      }
  }

  // Applies a function to each element of a set for a given choice.
  template<typename F>
  struct for_each_with_choice
  {
    choice c;
    F f;

    for_each_with_choice(const choice &_c, F _f)
      : c(_c), f(_f)
    {
    }

    bool operator()(T t) const
    {
      return f(c, t);
    }
  };

  template<typename F>
  struct for_each_set
  {
    F f;

    for_each_set(F _f)
      : F(_f)
    {
    }

    bool operator()(const choice &c, const imm::set<T> &values) const
    {
      return values.for_each(for_each_with_choice<F>(c, f));
    }
  };

  /** \brief Apply the given function object to (c', t) for each pair
   *  (c', t) in this set such that c' is contained in c.
   *
   *  If f returns false, the iteration will abort.  Modifications to
   *  this set after the iteration begins do not affect which values
   *  are visited.
   */
  template<typename F>
  bool for_each_contained_in(const choice &c, F f) const
  {
    return parent.for_each_key_contained_in(c, for_each_set<F>(f));
  }
};

template<typename PackageUniverse, typename ValueType>
std::ostream &operator<<(std::ostream &out, const generic_choice_indexed_map<PackageUniverse, ValueType> &m)
{
  m.dump(out);
  return out;
}

#endif // CHOICE_INDEXED_SET_H
