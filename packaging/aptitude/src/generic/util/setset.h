// setset.h                             -*-c++-*-
//
//   Copyright (C) 2005, 2008-2009 Daniel Burrows
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

#ifndef SETSET_H
#define SETSET_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <map>
#include <set>
#include <vector>
#include "immset.h"

#include <map>

/** \brief Efficient representation of sets-of-sets and sets-of-maps
 *
 * 
 *  Efficient representation of sets-of-sets and sets-of-maps which can
 *  be indexed by a subset/superset relation.  In particular, given a
 *  set of sets SS and a set T, provides the operation "is there an
 *  element S' of SS such that each element of S' stands in a relation
 *  R to an equivalent element of T" as well as its dual.  (with R
 *  being the universal relation, this is just a straight subset
 *  relation, but other general types of subsumption are also posible)
 *  
 *  WARNING: this class is completely non-threadsafe.  Because
 *  find_subset is very heavily used (over a million calls in many test
 *  cases), it has a number of optimizations, including one that means
 *  YOU MAY NOT HAVE MULTIPLE SIMULTANEOUS CALLERS OF FIND_SUBSET as it
 *  uses pre-allocated data structures to speed up its operation (by a
 *  factor of 3 or so).
 * 
 *  \file setset.h
 */

/** Maintains a set of imm::sets of Val objects, arranged so that
 *  quick answers to the query "does the set S subsume any element of
 *  this set?" can be produced.  At the moment this object increases
 *  monotonically, to keep things simple.
 */
template<typename Val, typename Compare = aptitude::util::compare3_f<Val> >
class setset
{
private:
  struct entry
  {
    imm::set<Val, Compare> s;
    /** Field used in the subset-testing algorithm. */
    mutable unsigned int hit_count;

    entry(const imm::set<Val, Compare> &_s)
      :s(_s), hit_count(0)
    {
    }

    entry()
      :hit_count(0)
    {
    }
  };

  class CompareLT
  {
    Compare comparer;

  public:
    CompareLT()
    {
    }

    CompareLT(Compare _comparer)
      : comparer(_comparer)
    {
    }

    bool operator()(const Val &v1, const Val &v2) const
    {
      return comparer(v1, v2) < 0;
    }
  };

  typedef std::vector<entry> entries_list;
  entries_list entries;

  typedef std::pair<typename entries_list::size_type, Val> index_entry;

  typedef std::map<Val, std::vector<index_entry>, CompareLT> index_type;

  index_type sets_by_key;

  void reset_counts() const
  {
    for(typename entries_list::const_iterator i = entries.begin();
	i != entries.end(); ++i)
      i->hit_count = 0;
  }

public:
  class const_iterator
  {
    typename entries_list::const_iterator real_iter;
  public:
    const_iterator(const typename entries_list::const_iterator &_real_iter)
      :real_iter(_real_iter)
    {
    }

    const_iterator()
    {
    }

    const imm::set<Val, Compare> &operator*() const
    {
      return real_iter->s;
    }

    const imm::set<Val, Compare> *operator->() const
    {
      return &real_iter->s;
    }

    const_iterator &operator++()
    {
      ++real_iter;
      return *this;
    }

    const_iterator &operator--()
    {
      --real_iter;
      return *this;
    }

    bool operator<(const const_iterator &other) const
    {
      return real_iter < other.real_iter;
    }

    bool operator<=(const const_iterator &other) const
    {
      return real_iter <= other.real_iter;
    }

    bool operator>(const const_iterator &other) const
    {
      return real_iter > other.real_iter;
    }

    bool operator>=(const const_iterator &other) const
    {
      return real_iter >= other.real_iter;
    }

    bool operator==(const const_iterator &other) const
    {
      return real_iter == other.real_iter;
    }

    bool operator!=(const const_iterator &other) const
    {
      return real_iter != other.real_iter;
    }

    const_iterator operator-(typename entries_list::size_type x) const
    {
      return real_iter - x;
    }

    const_iterator operator+(typename entries_list::size_type x) const
    {
      return real_iter + x;
    }

    const_iterator &operator-=(typename entries_list::size_type x)
    {
      real_iter -= x;
      return *this;
    }

    const_iterator &operator+=(typename entries_list::size_type x)
    {
      real_iter += x;
      return *this;
    }
  };

  typedef typename entries_list::size_type size_type;

private:
  // Used to construct a set traversal that populates the sets_by_key
  // structure.
  struct populate_sets_by_key
  {
    const typename entries_list::size_type index;

    index_type &sets_by_key;
  public:
    populate_sets_by_key(index_type &_sets_by_key,
			 typename entries_list::size_type _index)
      :index(_index), sets_by_key(_sets_by_key)
    {
    }

    bool operator()(const Val &v) const
    {
      sets_by_key[v].push_back(index_entry(index, v));

      return true;
    }
  };

  // Used by find_subset to tally up intersections.
  template<typename R>
  struct tally_intersections
  {
    const entries_list &entries;
    const index_type &sets_by_key;

    const R &r;

  public:
    tally_intersections(const entries_list &_entries,
			const index_type &_sets_by_key,
			const R &_r)
      :entries(_entries), sets_by_key(_sets_by_key), r(_r)
    {
    }

    // For each set containing v, add 1 to its hit count.
    bool operator()(const Val &v) const
    {
      typename index_type::const_iterator found
	= sets_by_key.find(v);

      if(found != sets_by_key.end())
	{
	  const std::vector<index_entry> &vals = found->second;

	  for(typename std::vector<index_entry>::const_iterator vi
		= vals.begin(); vi != vals.end(); ++vi)
	    if(r(vi->second, v))
	      ++entries[vi->first].hit_count;
	}

      return true;
    }
  };

  template<class T>
  struct universal_relation
  {
  public:
    bool operator()(const T &t1, const T &t2) const
    {
      return true;
    }
  };

public:
  setset()
  {
  }

  setset(size_type n,
	 const Compare &comparer = Compare())
    :sets_by_key(comparer)
  {
  }

  const_iterator begin() const
  {
    return entries.begin();
  }

  const_iterator end() const
  {
    return entries.end();
  }

  size_type size() const
  {
    return entries.size();
  }

  /** Add a new set into this set. */
  void insert(const imm::set<Val, Compare> &s)
  {
    typename entries_list::size_type index = entries.size();

    entries.push_back(s);
    s.for_each(populate_sets_by_key(sets_by_key, index));
  }

  /** Find an arbitrary element that is a subset of s. */
  template<typename R>
  const_iterator find_subset(const imm::set<Val, Compare> &s,
			     const R &r) const
  {
    reset_counts();
    // For each element that intersects s, count how many times it
    // intersects.  If every element of a set intersects s (i.e., the
    // count is equal to the set's size) then it is a subset of s.
    s.for_each(tally_intersections<R>(entries, sets_by_key, r));

    for(typename entries_list::const_iterator i
	  = entries.begin(); i != entries.end(); ++i)
      if(i->s.size() == i->hit_count)
        return i;

    return end();
  }

  const_iterator find_subset(const imm::set<Val, Compare> &s) const
  {
    return find_subset(s, universal_relation<Val>());
  }
};

template<typename Key, typename Val,
	 typename Compare = aptitude::util::compare3_f<Key> >
class mapset
{
  typedef setset<std::pair<Key, Val>,
		 imm::key_compare<Key, Val, Compare> > mapset_type;

  mapset_type S;

public:
  typedef typename mapset_type::size_type size_type;

  class const_iterator
  {
    typename mapset_type::const_iterator realiter;
  public:
    const_iterator(const typename mapset_type::const_iterator &_realiter)
      :realiter(_realiter)
    {
    }

    const_iterator(const const_iterator &other)
      :realiter(other.realiter)
    {
    }

    const_iterator &operator=(const const_iterator &other)
    {
      realiter = other.realiter;
      return *this;
    }

    imm::map<Key, Val> operator*() const
    {
      return *realiter;
    }

    bool operator==(const const_iterator &other) const
    {
      return realiter == other.realiter;
    }

    bool operator!=(const const_iterator &other) const
    {
      return realiter != other.realiter;
    }

    bool operator<(const const_iterator &other) const
    {
      return realiter < other.realiter;
    }

    bool operator>(const const_iterator &other) const
    {
      return realiter > other.realiter;
    }

    bool operator<=(const const_iterator &other) const
    {
      return realiter <= other.realiter;
    }

    bool operator>=(const const_iterator &other) const
    {
      return realiter >= other.realiter;
    }

    const_iterator &operator++()
    {
      ++realiter;
      return *this;
    }

    const_iterator &operator--()
    {
      --realiter;
      return *this;
    }

    const_iterator operator+(size_type i) const
    {
      return realiter + i;
    }

    const_iterator operator+=(size_type i) const
    {
      return realiter + i;
    }

    const_iterator operator-(size_type i) const
    {
      return realiter - i;
    }

    const_iterator operator-=(size_type i) const
    {
      return realiter - i;
    }
  };

  mapset(size_type n = 0,
	 const Compare &cmp = Compare())
    :S(n, cmp)
  {
  }

  const_iterator begin() const
  {
    return S.begin();
  }

  const_iterator end() const
  {
    return const_iterator(S.end());
  }

  size_type size() const
  {
    return S.size();
  }

  void insert(const imm::map<Key, Val, Compare> &m)
  {
    S.insert(m.get_bindings());
  }

  const_iterator find_submap(const imm::map<Key, Val, Compare> &m) const
  {
    return const_iterator(S.find_subset(m.get_bindings()));
  }

  template<typename R>
  const_iterator find_submap(const imm::map<Key, Val, Compare> &m,
			     const R &r) const
  {
    return const_iterator(S.find_subset(m.get_bindings(), r));
  }
};

template<typename Val, typename Compare>
std::ostream &operator<<(std::ostream &out, const setset<Val, Compare> &s)
{
  out << "{";
  bool first = true;
  for(typename setset<Val, Compare>::const_iterator it = s.begin();
      it != s.end(); ++it)
    {
      if(first)
	first = false;
      else
	out << ", ";

      out << *it;
    }
  out << "}";

  return out;
}

template<typename Key, typename Val, typename Compare>
std::ostream &operator<<(std::ostream &out, const mapset<Key, Val, Compare> &ms)
{
  out << "{";
  bool first = true;
  for(typename mapset<Key, Val, Compare>::const_iterator it = ms.begin();
      it != ms.end(); ++it)
    {
      if(first)
	first = false;
      else
	out << ", ";

      out << it->first;
      out << " -> ";
      out << it->second;
    }
  out << "}";

  return out;
}

#endif // SETSET_H

