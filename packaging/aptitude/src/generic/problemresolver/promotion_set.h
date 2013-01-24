/** \file promotion_set.h */             // -*-c++-*-

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

#ifndef PROMOTION_SET_H
#define PROMOTION_SET_H

#include <algorithm>
#include <list>
#include <map>
#include <set>
#include <vector>

#include <iostream>

#include <boost/unordered_map.hpp>

#include <generic/util/compare3.h>
#include <generic/util/immset.h>
#include <generic/util/maybe.h>

#include <cwidget/generic/util/ref_ptr.h>

#include <loggers.h>

#include <boost/unordered_map.hpp>
#include <boost/unordered_set.hpp>
#include <boost/variant.hpp>

#include "choice.h"
#include "choice_indexed_map.h"
#include "choice_set.h"
#include "incremental_expression.h"
#include "cost.h"
#include "cost_limits.h"

/** \brief Represents a cost promotion: the knowledge that
 *  a set of choices forces a solution to a higher cost.
 */
template<typename PackageUniverse>
class generic_promotion
{
public:
  typedef generic_choice<PackageUniverse> choice;
  typedef generic_choice_set<PackageUniverse> choice_set;

private:
  choice_set choices;
  cost promotion_cost;
  // An expression that is "true" when this promotion is valid and
  // "false" otherwise; it is NULL if the promotion is universally
  // valid.  Invalid promotions are culled from the promotion set.
  //
  // Currently the only invalid promotions are ones that are due to a
  // constraint posted by the user which was later retracted.  All
  // other promotions have a NULL pointer here.
  cwidget::util::ref_ptr<expression<bool> > valid_condition;

public:
  generic_promotion()
    : choices(), promotion_cost(), valid_condition()
  {
  }

  /** \brief Create a new promotion. */
  generic_promotion(const choice_set &_choices, const cost &_promotion_cost)
    : choices(_choices), promotion_cost(_promotion_cost), valid_condition()
  {
  }

  /** \brief Create a new promotion with a validity condition. */
  generic_promotion(const choice_set &_choices,
		    const cost &_promotion_cost,
		    const cwidget::util::ref_ptr<expression<bool> > &_valid_condition)
    : choices(_choices),
      promotion_cost(_promotion_cost),
      valid_condition(_valid_condition)
  {
  }

  const choice_set &get_choices() const { return choices; }
  const cost &get_cost() const { return promotion_cost; }
  const cwidget::util::ref_ptr<expression<bool> > &get_valid_condition() const { return valid_condition; }

  /** \brief Return a promotion representing both of the
   *  input promotions.
   *
   *  The output promotion's cost is the least upper bound of the
   *  input promotions' costs.  If only one promotion was required to
   *  achieve this upper bound, then a promotion that is sufficient
   *  will be selected and returned directly.  Otherwise, a new
   *  promotion will be created -- most likely expanding the trigger
   *  sets and validity conditions of the input promotions in the
   *  process.
   */
  static generic_promotion least_upper_bound(const generic_promotion &p1,
					     const generic_promotion &p2)
  {
    const cost &p1_cost(p1.get_cost());
    const cost &p2_cost(p2.get_cost());

    // If the least upper bound has the same content as one of the two
    // promotions, return that promotion directly.
    if(p1_cost.is_above_or_equal(p2_cost))
      return p1;
    else if(p2_cost.is_above_or_equal(p1_cost))
      return p2;

    // Otherwise we'll have to construct a new combined promotion,
    // being careful with regard to validity conditions and triggers.

    cost new_cost =
      cost::least_upper_bound(p1_cost, p2_cost);

    const choice_set &p1_choices(p1.get_choices());
    const choice_set &p2_choices(p2.get_choices());

    const cwidget::util::ref_ptr<expression<bool> > &p1_valid(p1.get_valid_condition());
    const cwidget::util::ref_ptr<expression<bool> > &p2_valid(p2.get_valid_condition());

    choice_set new_choices;

    // Minor optimization: I "know" that the current implementation of
    // insert_or_narrow means that it's potentially more efficient to
    // start with the larger set and incorporate the smaller one.
    if(p1_choices.size() > p2_choices.size())
      {
	new_choices = p1_choices;
	new_choices.insert_or_narrow(p2_choices);
      }
    else
      {
	new_choices = p2_choices;
	new_choices.insert_or_narrow(p2_choices);
      }

    // Note that this will compute a somewhat inefficient validity
    // condition when applied across several expressions.
    cwidget::util::ref_ptr<expression<bool> > new_valid;
    if(p1_valid.valid())
      {
        if(p2_valid.valid())
          new_valid = and_e::create(p1_valid, p2_valid);
        else
          new_valid = p1_valid;
      }
    else
      new_valid = p2_valid;

    return generic_promotion(new_choices, new_cost, new_valid);
  }

  int compare(const generic_promotion &other) const
  {
    using aptitude::util::compare3;

    const int promotion_cmp =
      compare3(promotion_cost, other.promotion_cost);

    if(promotion_cmp != 0)
      return promotion_cmp;
    else
      return compare3(choices, other.choices);
  }

  bool operator<(const generic_promotion &other) const
  {
    return compare(other) < 0;
  }

  bool operator==(const generic_promotion &other) const
  {
    if(promotion_cost != other.promotion_cost)
      return false;

    if(!(choices == other.choices))
      return false;

    return true;
  }

  bool operator!=(const generic_promotion &other) const
  {
    if(promotion_cost != other.promotion_cost)
      return true;

    if(!(choices == other.choices))
      return true;

    return false;
  }
};

namespace aptitude
{
  namespace util
  {
    template<typename PackageUniverse>
    class compare3_f<generic_promotion<PackageUniverse> >
    {
    public:
      int operator()(const generic_promotion<PackageUniverse> &p1,
		     const generic_promotion<PackageUniverse> &p2) const
      {
	return p1.compare(p2);
      }
    };
  }
}

template<typename PackageUniverse>
std::ostream &operator<<(std::ostream &out, const generic_promotion<PackageUniverse> &p)
{
  out << "(T" << p.get_cost() << ": " << p.get_choices();

  if(p.get_valid_condition().valid())
    // Output p.get_valid_condition() if it isn't null.
    out << "; V:" << p.get_valid_condition();

  out << ")";

  return out;
}

/** \brief Interface used by the promotion set to call back into the
 *  main resolver.
 */
template<typename PackageUniverse>
class promotion_set_callbacks
{
  virtual void promotion_retracted(const generic_promotion<PackageUniverse> &p) = 0;
};

/** \brief Represents a set of "promotions": mappings from sets of
 *  choices to costs implied by those choices.
 *
 *  Wraps up the various customizations of dense_setset for this case.
 *
 *  Requirements for this structure:
 *
 *
 *   1. We want to be able to find a set of choices quickly from a
 *      superset of its elements, and then resolve that set to the
 *      cost it matches (this requires indexing on version OR
 *      dependency, depending on what type of choice we have).
 *
 *   2. When a new cost promotion is inserted, it should override any
 *      lower promotions that it is a subset of or equal to, but not
 *      higher ones.  Conversely, if a new cost promotion contains an
 *      existing promotion that has a higher cost, it should not be
 *      inserted.
 *
 *   3. We need to be able to learn which promotions would match a
 *      step with a single extra choice, and what that choice is.
 *      This is used to update the solver status of a step.
 *
 *   4. We need to be able to learn which promotions containing a
 *      particular choice would match a step with a single extra
 *      choice, and one choice that should be contained in the result.
 *      This is used to update the solver status when generating a
 *      step successor.
 *
 *  In (4) and (5), the caller passes in a list of versions that it's
 *  interested in.  Only promotions that are matched by adding one of
 *  these versions are returned, and for each version, a maximal
 *  promotion is returned.
 *
 *  This object gets some of its efficiencies from actually knowing
 *  the structure of choices (e.g., it indexes choices to break soft
 *  dependencies differently from choices to install versions).
 *
 *  Iterators on the promotion set are stable (erasing an iterator
 *  will of course invalidate it, but other iterators will still be
 *  valid).
 *
 *  \sa generic_choice, generic_choice_set
 */
template<typename PackageUniverse>
class generic_promotion_set
{
public:
  typedef typename PackageUniverse::package package;
  typedef typename PackageUniverse::version version;
  typedef typename PackageUniverse::dep dep;

  typedef generic_choice<PackageUniverse> choice;
  typedef generic_choice_set<PackageUniverse> choice_set;
  typedef generic_promotion<PackageUniverse> promotion;

private:
  logging::LoggerPtr logger;
  promotion_set_callbacks<PackageUniverse> &callbacks;

  struct entry;

  /** \brief The structure used to store information about
   *  a promotion.
   */
  struct entry
  {
    promotion p;

    /** \brief An expression that will retract this entry when it
     *  becomes true.
     *
     *  Stored via a ref_ptr and not as a member partly because
     *  otherwise I'd need a nasty hack to avoid circular references,
     *  and partly because currently most promotions don't have a
     *  validity condition, so this lets us save some space (by
     *  storing NULL for most promotions).
     */
    cwidget::util::ref_ptr<expression<bool> > retraction_expression;

    /** \brief Used when searching for a promotion.
     *
     *  True if this was touched by the current search.
     *
     *  Never copied when entries are copied.
     */
    mutable bool active : 1;

    /** \brief Used when searching for a promotion.
     *
     *  Never copied when entries are copied.
     */
    mutable unsigned int hit_count : (8*(sizeof(int))-1);

    entry(const promotion &_p)
      : p(_p),
	retraction_expression(),
	active(false),
	hit_count(0)
    {
    }
  };

  typedef std::list<entry> entries_holder;
  typedef typename entries_holder::const_iterator entry_const_ref;
  typedef typename entries_holder::iterator entry_ref;

  /** \brief An expression that ejects a promotion from its parent set
   *  when that promotion becomes invalid.
   */
  class eject_promotion_when_invalid : public expression_wrapper<bool>
  {
    entry_ref entry_to_drop;
    generic_promotion_set *parent;

    eject_promotion_when_invalid(const cwidget::util::ref_ptr<expression<bool> > &
				 promotion_valid_expression,
				 const entry_ref &_entry_to_drop,
				 generic_promotion_set *_parent)
      : expression_wrapper<bool>(promotion_valid_expression),
	entry_to_drop(_entry_to_drop),
	parent(_parent)
    {
      eassert(get_child().valid());
    }

  public:
    static cwidget::util::ref_ptr<eject_promotion_when_invalid>
    create(const cwidget::util::ref_ptr<expression<bool> > &promotion_valid_expression,
	   const entry_ref &entry_to_drop,
	   generic_promotion_set *parent)
    {
      return new eject_promotion_when_invalid(promotion_valid_expression,
					      entry_to_drop,
					      parent);
    }

    void dump(std::ostream &out)
    {
      out << "drop-if(" << get_child() << ")";
    }

    void changed(bool new_value)
    {
      // This should never be invoked twice, but add a safety check in
      // case it is.
      if(parent == NULL)
	{
	  LOG_ERROR(aptitude::Loggers::getAptitudeResolverSearchCosts(),
		    "Internal error: a promotion was ejected twice!");
	  return;
	}

      if(!new_value)
	{
	  parent->eject(entry_to_drop);
	  parent = NULL;
	}
    }
  };

  /** \brief Stores the promotions that exist. */
  entries_holder entries;

  // The number of promotions that would produce a conflict.  Used
  // only for the sake of display.
  unsigned int num_conflicts;

  /** \brief An entry in the index related to install_version entries.
   *
   *  This stores the choice that generated this index entry (used for
   *  quick comparisons), as well as a reference to the entry object
   *  in one of the main entry lists.
   */
  struct install_version_index_entry
  {
    /** \brief The entries that contain this version, not installed
     *  from the dependency source.
     */
    std::vector<entry_ref> not_from_dep_source_entries;

    /** \brief The entries that contain this version installed from
     *  the dependency source, indexed by the dependency they solved.
     *
     *  Each entry in this map includes all the elements of
     *  not_from_dep_source_entries: when we want to find the indexed
     *  hits for a version that was installed from a dependency
     *  source, we must also include the indexed hits for the
     *  "generic" version.  This is the simplest way to achieve that
     *  without allocating space during the search (instead we
     *  pre-allocate the exact list we'll want for each index
     *  location).  This moves the cost to the point where the
     *  structure is built, and in my off-the-cuff estimation this
     *  should be a win, since the promotion set is a read-mostly
     *  structure.
     */
    boost::unordered_map<dep, std::vector<entry_ref> > from_dep_source_entries;
  };

  /** \brief The version count used to set up the internal index. */
  int num_versions;

  /** \brief The index of install_version choices.
   *
   *  This is an array of num_versions elements, indexed by version
   *  ID, each of which is a pointer to an index entry.
   *
   *  We use an array of pointers instead of an array of values to
   *  reduce the memory usage.  Each index entry takes 36 bytes on a
   *  32-bit Debian installation as of this writing, and I expect it
   *  would take 72 bytes on a 64-bit installation.  The index entry
   *  array will normally be very sparse, and so this decreases the
   *  space consumption by nearly 90% (to 4 bytes / 8 bytes).  For the
   *  current apt cache, this means that an empty index on a 32-bit
   *  machine consumes just over 200k, rather than just under two
   *  megabytes.
   */
  install_version_index_entry **install_version_index;

  // The index for break_soft_dep choices is (conceptually) just a map
  // that takes a dependency to a list of the entries that contain it.
  typedef std::vector<entry_ref> break_soft_dep_index_entry;

  // And in fact, that's also what it actually is.  We don't use an
  // array here because there are lots of dependencies (e.g., around
  // 180,000 as of this writing), most dependencies are not soft, and
  // I *think* that soft dependencies won't usually participate in
  // promotions.
  //
  // \todo Would a boost::unordered_multimap be better here?  I don't
  // understand the cost/benefit tradeoffs to using one.
  boost::unordered_map<dep, break_soft_dep_index_entry> break_soft_dep_index;

  // Used to drop backpointers to an entry, one choice at a time.  Not
  // as efficient as the bulk operations below, but more general.
  class drop_choice
  {
    install_version_index_entry **install_version_index;
    boost::unordered_map<dep, break_soft_dep_index_entry> &break_soft_dep_index;
    // The entry being removed.
    entry_ref victim;

    static void do_drop(std::vector<entry_ref> &entries,
			const entry_ref &victim)
    {
      typename std::vector<entry_ref>::iterator new_end =
	std::remove(entries.begin(), entries.end(), victim);
      entries.erase(new_end, entries.end());
    }

  public:
    drop_choice(install_version_index_entry **_install_version_index,
		boost::unordered_map<dep, break_soft_dep_index_entry> &_break_soft_dep_index,
		const entry_ref &_victim)
      : install_version_index(_install_version_index),
	break_soft_dep_index(_break_soft_dep_index),
	victim(_victim)
    {
    }

    bool operator()(const choice &c) const
    {
      switch(c.get_type())
	{
	case choice::install_version:
	  {
	    const int id = c.get_ver().get_id();
	    install_version_index_entry *index_entry = install_version_index[id];
	    if(index_entry != NULL)
	      {
		if(!c.get_from_dep_source())
		  {
		    do_drop(index_entry->not_from_dep_source_entries, victim);
		    for(typename boost::unordered_map<dep, std::vector<entry_ref> >::iterator
			  it = index_entry->from_dep_source_entries.begin();
			it != index_entry->from_dep_source_entries.end();
			++it)
		      do_drop(it->second, victim);
		  }
		else
		  {
		    const typename boost::unordered_map<dep, std::vector<entry_ref> >::iterator
		      found = index_entry->from_dep_source_entries.find(c.get_dep());
		    if(found != index_entry->from_dep_source_entries.end())
		      do_drop(found->second, victim);
		  }
	      }
	  }
	  break;

	case choice::break_soft_dep:
	  {
	    const typename boost::unordered_map<dep, break_soft_dep_index_entry>::iterator
	      found = break_soft_dep_index.find(c.get_dep());
	    if(found != break_soft_dep_index.end())
	      do_drop(found->second, victim);
	  }
	  break;
	}

      return true;
    }
  };

  void eject(const entry_ref &victim)
  {
    promotion p(victim->p);

    erase(iterator(victim));
    callbacks.promotion_retracted(p);
  }

public:
  typedef unsigned int size_type;
  size_type size() const { return entries.size(); }
  size_type conflicts_size() const { return num_conflicts; }

private:
  // The iterators on this set hide the entry objects and other
  // metadata, returning only the promotions.
  template<typename entry_iter>
  class iterator_base
  {
    entry_iter entry_it;

    friend class generic_promotion_set;

    // This overload is used for non-end iterators.
    iterator_base(entry_iter _entry_it)
      : entry_it(_entry_it)
    {
    }

  public:
    iterator_base()
    {
    }

    const promotion &operator*() const
    {
      return entry_it->p;
    }

    const promotion *operator->() const
    {
      return &entry_it->p;
    }

    iterator_base &operator++()
    {
      ++entry_it;
      return *this;
    }

    template<typename other_entry_iter>
    bool operator==(const iterator_base<other_entry_iter> &other) const
    {
      return entry_it == other.entry_it;
    }

    template<typename other_entry_iter>
    bool operator!=(const iterator_base<other_entry_iter> &other) const
    {
      return entry_it != other.entry_it;
    }
  };

public:
  typedef iterator_base<typename entries_holder::const_iterator> const_iterator;

  typedef iterator_base<typename entries_holder::iterator> iterator;

  const_iterator begin() const
  {
    return const_iterator(entries.begin());
  }

  iterator begin()
  {
    return iterator(entries.begin());
  }

  const_iterator end() const
  {
    return const_iterator(entries.end());
  }

  iterator end()
  {
    return iterator(entries.end());
  }

  void erase(const iterator &victim)
  {
    if(victim == end())
      {
	LOG_ERROR(logger, "Can't erase an end iterator.");
	return;
      }
    else
      {
	const promotion &p(*victim);

	LOG_TRACE(logger, "Ejecting promotion: " << p);
	p.get_choices().for_each(drop_choice(install_version_index,
					     break_soft_dep_index,
					     victim.entry_it));

	entries.erase(victim.entry_it);
      }
  }

private:
  /** \brief Find the list of index entries associated with the given
   *  choice, or NULL if it is not indexed.
   */
  const std::vector<entry_ref> *find_index_list(const choice &c) const
  {
    switch(c.get_type())
      {
      case choice::install_version:
	{
	  const int id = c.get_ver().get_id();
	  const install_version_index_entry *index_entry = install_version_index[id];

	  if(index_entry != NULL)
	    {
	      if(!c.get_from_dep_source())
		{
		  LOG_TRACE(logger, "find_index_list(" << c
			    << ") returning a list of "
			    << index_entry->not_from_dep_source_entries.size()
			    << " index entries from the not-from-dep-source list.");
		  return &index_entry->not_from_dep_source_entries;
		}
	      else
		{
		  typename boost::unordered_map<dep, std::vector<entry_ref> >::const_iterator found =
		    index_entry->from_dep_source_entries.find(c.get_dep());

		  if(found != index_entry->from_dep_source_entries.end())
		    {
		      LOG_TRACE(logger, "find_index_list(" << c << ") returning a list of "
				<< found->second.size()
				<< " index entries from the dep-source list for "
				<< found->first);
		      return &found->second;
		    }
		  else
		    {
		      LOG_TRACE(logger, "find_index_list(" << c <<
				") returning a list of "
				<< index_entry->not_from_dep_source_entries.size()
				<< " index entries from the not-from-dep-source-list: there are no from-dep index entries for " << c.get_dep());
		      return &index_entry->not_from_dep_source_entries;
		    }
		}
	    }
	  else
	    {
	      LOG_TRACE(logger, "find_index_list(" << c
			<< ") returning an empty list: there is no index cell for "
			<< c.get_ver());
	      return NULL;
	    }

	  break;
	}

      case choice::break_soft_dep:
	{
	  typename boost::unordered_map<dep, break_soft_dep_index_entry>::const_iterator found =
	    break_soft_dep_index.find(c.get_dep());
	  if(found == break_soft_dep_index.end())
	    {
	      LOG_TRACE(logger, "find_index_list(" << c
			<< ") returning an empty list: there is no index cell for "
			<< c.get_dep());
	      return NULL;
	    }
	  else
	    {
	      LOG_TRACE(logger, "find_index_list(" << c
			<< ") returning a list of "
			<< found->second.size() << " entries.");
	      return &found->second;
	    }
	}
	break;

      default:
	LOG_ERROR(logger, "tally_intersections: bad choice type " << c.get_type());
	return NULL;
      }
  }

  /** \brief Apply an operation to each entry that matches the input
   * set.
   *
   *  The one catch here is that this isn't a simple subset relation.
   *  We look for sets where the value stored in the promotion
   *  "contains" the value in the input set (or vice versa, depending
   *  on the mode).  So we need to deal with the fact that certain
   *  choices can "contain" other choices.
   *
   *  Currently the only choices that can contain other choices are
   *  version installations.  An install_version choice that is not
   *  from a dependency source contains any install_version choice
   *  that is from a dependency source, but not vice versa.  This
   *  means that, e.g., a search node that installs a version will
   *  always match a promotion that contains an install_version action
   *  for that version that is not from the dependency source --
   *  regardless of whether the search node's version is from a
   *  dependency source.
   *
   *  This function is used when traversing a set of choices to find a
   *  sub- or super-set of a collection of choices.  It is not a
   *  generic traversal function.  In particular, it will abort early
   *  if it detects that there will never be any matching promotions.
   *  (if we're trying to find supersets of an input set and one
   *  element is not matched by anything, we can abort immediately)
   *
   *  The only reason this is generalized is because we use a two pass
   *  algorithm.  In the first pass, we count how many times we
   *  "touch" each entry; in the second pass, we find matching entries
   *  according to the output rule and zero out all the counts we set
   *  in the first pass.  Using a generic traversal function means
   *  that the traversal logic is automatically the same in both
   *  passes.  This also makes it easy to have different "readouts",
   *  so (eg) most of the time we can just return "true, it matched"
   *  or "false, it didn't match", but if we're trying to throw out
   *  conflicts that are redundant with something we inserted, we'll
   *  instead build up a list of the matching conflicts.
   */
  template<typename Op>
  struct traverse_intersections
  {
    const generic_promotion_set &parent;
    // If \b true, we are looking for a subset of the input.  If \b
    // false, we are looking for a superset.  This affects how we
    // compare choices when they are not identical, but one is a
    // "subset" of the other.
    bool subset_mode;
    Op op;

  public:
    traverse_intersections(const generic_promotion_set &_parent,
			   bool _subset_mode,
			   const Op &_op)
      : parent(_parent),
	subset_mode(_subset_mode),
	op(_op)
    {
    }

    /** \brief Extract the operation.
     *
     *  Useful for operations that accumulate information in the
     *  operation structure.
     */
    const Op &get_op() const { return op; }

    /** \brief For all the entries in this structure containing a
     *  choice that contains (if subset_mode is true) or that is
     *  contained by (if subset_mode is false) the choice c, increment
     *  the overlap count stored in the entry.
     */
    bool operator()(const choice &c) const
    {
      // The list of entries that we need to operate on.
      const std::vector<entry_ref> *entries = parent.find_index_list(c);

      if(entries == NULL || entries->empty())
	{
	  // If nothing in this set contains the current choice AND we
	  // are in superset mode, abort early: nothing will ever be a
	  // superset of the input set.
	  if(!subset_mode)
	    {
	      LOG_DEBUG(parent.logger, "traverse_intersections: breaking out of set traversal at "
			<< c << " because nothing matches it and we are looking for a superset.");
	      return false;
	    }
	  else
	    return true;
	}
      else
	{
	  for(typename std::vector<entry_ref>::const_iterator it = entries->begin();
	      it != entries->end(); ++it)
	    if(!op(*it))
	      return false;

	  return true;
	}
    }
  };

  // Sets visited entries to be active, and increments their hit
  // counts.
  struct increment_entry_count_op
  {
    logging::LoggerPtr logger;

    increment_entry_count_op(const logging::LoggerPtr &_logger)
      : logger(_logger)
    {
    }

    bool operator()(entry_ref r) const
    {
      LOG_TRACE(logger, "increment_entry_count: incrementing the hit count for " << r->p);
      r->active = true;
      ++r->hit_count;

      return true;
    }
  };

  // To find a subset of an input set S, we find an entry that is hit
  // by S exactly as many times as it has elements.  This works
  // because we assume that it's impossible for an element of S to hit
  // two distinct elements (no two choices in a search node can match
  // each other).
  //
  // This computes the upper bound of all the elements that were
  // matched.  We don't return all elements because we don't need to
  // (this represents testing whether a set matches an existing
  // promotion).  NB: the only reason for returning a promotion
  // pointer rather than a boolean is so that we can provide
  // diagnostic logging.
  class find_entry_subset_op
  {
    // A collection of conditions under which the returned promotion
    // is true.  Each intersected promotion's validity condition is
    // thrown in here.
    mutable std::vector<cwidget::util::ref_ptr<expression<bool> > > rval_valid_conditions;
    // The cost to return.
    mutable cost rval_cost;

    logging::LoggerPtr logger;

  public:
    find_entry_subset_op(const logging::LoggerPtr &_logger)
      : logger(_logger)
    {
    }

    const std::vector<cwidget::util::ref_ptr<expression<bool> > > &get_rval_valid_conditions() const
    {
      return rval_valid_conditions;
    }

    const cost &get_rval_cost() const
    {
      return rval_cost;
    }

    bool operator()(entry_ref r) const
    {
      if(r->active)
	{
	  if(r->hit_count == r->p.get_choices().size())
	    {
	      if(r->p.get_cost().is_above_or_equal(rval_cost))
		{
		  cost new_cost =
		    cost::least_upper_bound(r->p.get_cost(), rval_cost);

		  LOG_DEBUG(logger, "find_entry_subset_op: resetting the hit count for "
			    << r->p << " to 0 and incorporating it into the result (return value: "
			    << rval_cost << " -> " << new_cost);

		  rval_cost = new_cost;
		  rval_valid_conditions.push_back(r->p.get_valid_condition());
		}
	      else
		LOG_TRACE(logger, "find_entry_subset_op: resetting the hit count for "
			  << r->p << " to 0, but not incorporating it into the result, because its cost "
			  << r->p.get_cost() << " is lower than the current highest cost "
			  << rval_cost);
	    }
	  else
	    LOG_TRACE(logger, "find_entry_subset_op: " << r->p
		      << " is not matched (needed " << r->p.get_choices().size()
		      << " hits, but got only " << r->hit_count);

	  r->active = false;
	  r->hit_count = 0;
	}
      else
	// Note that if the hit-count is 0, we must have already
	// processed this entry.
	LOG_TRACE(logger, "find_entry_subset_op: skipping already processed promotion " << r->p);

      return true;
    }
  };

  /** \brief Retrieve all the supersets of the input set: entries
   *  whose trigger sets are supersets of the input promotion, and
   *  whose costs are less than or equal to the input set's
   *  cost (bearing mind that costs are only
   *  partially ordered).
   *
   *  As a side effect, resets all hit counts to 0.
   *
   *  This is used to purge redundant entries when a new entry is
   *  inserted.
   */
  class find_entry_supersets_op
  {
    // Where to store the output entry references.
    std::vector<entry_ref> &output_entries;
    // How many hits to require from each entry we process.
    unsigned int required_hits;
    // costs smaller than or equal to this limit are not
    // returned:
    cost maximum_cost;
    logging::LoggerPtr logger;

  public:
    /** \brief Create a find-supersets operation.
     *
     *  \param _output_entries   The location in which to place the
     *                           supersets that are found.
     *  \param _required_hits    The number of elements in the set
     *                           that we are searching for supersets
     *                           of; only entries with this many hits
     *                           are returned.
     *  \param _maximum_cost     The maximum cost to examine; only entries
     *                           with this cost or lower are returned.
     *  \param _logger           The logger to use to write messages
     *                           about this process.
     */
    find_entry_supersets_op(std::vector<entry_ref> &_output_entries,
			    unsigned int _required_hits,
			    const cost &_maximum_cost,
			    const logging::LoggerPtr &_logger)
      : output_entries(_output_entries),
	required_hits(_required_hits),
	maximum_cost(_maximum_cost),
	logger(_logger)
    {
    }

    bool operator()(entry_ref r) const
    {
      if(r->active)
	{
	  if(cost::greatest_lower_bound(maximum_cost,
						  r->p.get_cost()) != r->p.get_cost())
	    {
	      LOG_DEBUG(logger, "find_entry_supersets_op: resetting the hit count for "
		       << r->p << " to 0, but not returning it, because its cost "
		       << r->p.get_cost() << " is not below the maximum cost "
		       << maximum_cost);
	    }
	  else if(r->hit_count == required_hits)
	    {
	      LOG_DEBUG(logger, "find_entry_supersets_op: resetting the hit count for " << r->p << " to 0 and adding it to the output list.");
	      output_entries.push_back(r);
	    }
	  else
	    {
	      if(r->hit_count > required_hits)
		// Something is quite wrong if this happens.
		LOG_ERROR(logger, "find_entry_supersets_op: resetting the hit count for " << r->p << " to 0, but not returning it: invalid hit count " << r->hit_count << " (expected maximum of " << required_hits << ")");
	      else
		LOG_TRACE(logger, "find_entry_supersets_op: resetting the hit count for " << r->p << " to 0, but not returning it: it only has " << r->hit_count << " hits (needed " << required_hits << ")");
	    }

	  r->active = false;
	  r->hit_count = 0;
	}
      else
	LOG_TRACE(logger, "find_entry_supersets_op: skipping already processed promotion " << r->p);

      return true;
    }
  };

public:
  /** \brief Compute the upper bound of all the promotions contained
   * in the given set of choices.
   *
   *  Implements requirement (1).
   */
  cost find_highest_promotion_cost(const choice_set &choices) const
  {
    LOG_TRACE(logger, "Entering find_highest_promotion_cost(" << choices << ")");

    traverse_intersections<increment_entry_count_op>
      increment_f(*this, true, increment_entry_count_op(logger));
    traverse_intersections<find_entry_subset_op>
      find_result_f(*this, true, find_entry_subset_op(logger));
    const find_entry_subset_op &find_result(find_result_f.get_op());

    choices.for_each(increment_f);
    // We have to run this even if the increment aborted, since we
    // need to reset all the counters to 0 for the next run.
    choices.for_each(find_result_f);

    return find_result.get_rval_cost();
  }

  /** \brief A functor that, when applied to a Boolean value,
   *  returns the opposite of that value.
   *
   *  Used to check whether for_each hit any "true" values.  Note that
   *  correctness relies on the fact that for_each returns "true" when
   *  it didn't hit anything.
   *
   *  The reason for inverting the result is that it allows for_each
   *  to short-circuit correctly.
   */
  template<typename T>
  class not_f
  {
  public:
    bool operator()(const choice &c, const T &t) const
    {
      return false;
    }
  };

  /** \brief Function object that updates the output map for
   *  find_highest_incipient_promotion and friends.
   *
   *  This is initialized with a promotion, the output domain, and an
   *  output location, then invoked with various choices.  For each of
   *  the choices that is contained in the output domain, the
   *  corresponding cell in the output map is updated to the upper
   *  bound of the promotion it stores and the update object's
   *  promotion.
   *
   *  Normally the choices this is invoked on represent the choices in
   *  the promotion.
   */
  template<typename T>
  class update_incipient_output
  {
    const promotion &p;
    const generic_choice_indexed_map<PackageUniverse, T> &output_domain;
    boost::unordered_map<choice, promotion> &output;

  public:
    update_incipient_output(const promotion &_p,
			    const generic_choice_indexed_map<PackageUniverse, T> &_output_domain,
			    boost::unordered_map<choice, promotion> &_output)
      : p(_p), output_domain(_output_domain), output(_output)
    {
    }

    bool operator()(const choice &c) const
    {
      if(!output_domain.for_each_key_contained_in(c, not_f<T>()))
	{
	  typedef typename boost::unordered_map<choice, promotion>::iterator
	    out_iterator;

	  std::pair<out_iterator, out_iterator> found =
	    output.equal_range(c);

	  if(found.first == found.second)
	    output.insert(found.first, std::make_pair(c, p));
	  else
	    found.first->second =
	      promotion::least_upper_bound(found.first->second, p);
	}

      return true;
    }
  };

  /** \brief Finds near-subsets of the input set (one that
   *         would be a subset if exactly one choice was added
   *         to the input).
   *
   *  Similar to find_entry_subset_op, but finds incipient subsets as
   *  well as subsets, and fills in a map with its results rather than
   *  simply storing the single highest cost.
   */
  template<typename T>
  class find_incipient_entry_subset_op
  {
    const generic_choice_indexed_map<PackageUniverse, T> &output_domain;
    boost::unordered_map<choice, promotion> &output_incipient;
    maybe<promotion> &output_non_incipient;
    logging::LoggerPtr logger;

  public:
    find_incipient_entry_subset_op(const generic_choice_indexed_map<PackageUniverse, T> &_output_domain,
				   boost::unordered_map<choice, promotion> &_output_incipient,
				   maybe<promotion> &_output_non_incipient,
				   const logging::LoggerPtr &_logger)
      : output_domain(_output_domain),
	output_incipient(_output_incipient),
	output_non_incipient(_output_non_incipient),
	logger(_logger)
    {
    }

    bool operator()(entry_ref r) const
    {
      if(r->active)
	{
	  if((unsigned)r->hit_count + 1 == r->p.get_choices().size())
	    {
	      LOG_DEBUG(logger, "find_incipient_entry_subset_op: generating incipient output entries for " << r->p << " and resetting its hit count to 0.");
	      update_incipient_output<T> updater(r->p, output_domain, output_incipient);
	      r->p.get_choices().for_each(updater);
	    }
	  else if(r->hit_count == r->p.get_choices().size())
	    {
	      if(output_non_incipient.get_has_value())
		output_non_incipient =
		  promotion::least_upper_bound(output_non_incipient.get_value(), r->p);
	      else
		output_non_incipient = r->p;
	    }
	  else
	    LOG_DEBUG(logger, "find_incipient_entry_subset_op: " << r->p << " is not an incipient promotion; resetting its hit count to 0 and not returning it.");

	  r->active = false;
	  r->hit_count = 0;
	}
      else
	LOG_TRACE(logger, "find_incipient_entry_subset_op: skipping already processed promotion " << r->p);

      return true;
    }
  };

  /** \brief A function object that, for each choice it is applied to,
   *  adds an entry to the given output set for the choice if there is
   *  a single-element promotion containing that choice.  If an entry
   *  already exists, it is updated only if the new promotion is not
   *  less than or equal to the current one (i.e., it is larger or
   *  unrelated).
   */
  template<typename T>
  class find_unary_promotions
  {
    const generic_promotion_set &promotions;

    boost::unordered_map<choice, promotion> &output;

  public:
    find_unary_promotions(const generic_promotion_set &_promotions,
			  boost::unordered_map<choice, promotion> &_output)
      : promotions(_promotions),
	output(_output)
    {
    }

    bool operator()(const choice &c, const T &t) const
    {
      const std::vector<entry_ref> *entries = promotions.find_index_list(c);

      if(entries != NULL)
	{
	  for(typename std::vector<entry_ref>::const_iterator
		it = entries->begin();
	      it != entries->end(); ++it)
	    {
	      const promotion &p((*it)->p);

	      if(p.get_choices().size() == 1)
		{
		  choice p_c;
		  // The single choice in the promotion *must* contain
		  // c, or else it wouldn't be indexed under c.
		  eassert(p.get_choices().get_containing_choice(c, p_c));

		  typedef typename boost::unordered_map<choice, promotion>::iterator out_iterator;
		  std::pair<out_iterator, out_iterator> found =
		    output.equal_range(p_c);

		  if(found.first == found.second)
		    output.insert(found.first, std::make_pair(p_c, p));
		  else
		    found.first->second =
		      promotion::least_upper_bound(p, found.first->second);
		}
	    }
	}

      return true;
    }
  };
public:

  /** \brief Find the highest-cost incipient promotion containing a
   *  particular choice.
   *
   *  An incipient promotion is one that doesn't match now, but that
   *  would match if a single choice was added to the set of choices.
   *
   *  \param choices  The choice set to test.
   *  \param output_domain
   *                  Additional choices, one of which must be contained
   *                  in every incipient promotion.  The return values
   *                  are organized according to which of these choices
   *                  each one contained, and only the highest-cost
   *                  promotion for each choice is returned.
   *                  output_domain must be disjoint with choices.
   *  \param output_incipient   A map in which to store the results of the search.
   *                  Choices in output_domain that were matched are
   *                  mapped to the highest-cost promotion that they
   *                  would trigger.
   *  \param output_non_incipient   A location in which to store the
   *                  promotion, if any, that was found in the choice set
   *                  itself.
   *
   *  The values that output_domain associates with the choices it
   *  stores are ignored.
   *
   *  In the common use case, "choices" represents the actions taken
   *  by a step and "output_domain" contains all the solvers of the
   *  step, each mapped to the dependencies that it solves.
   */
  template<typename T>
  void find_highest_incipient_promotions(const choice_set &choices,
					 const generic_choice_indexed_map<PackageUniverse, T> &output_domain,
					 boost::unordered_map<choice, promotion> &output_incipient,
					 maybe<promotion> &output_non_incipient) const
  {
    LOG_TRACE(logger, "Entering find_highest_incipient_promotions(" << choices << ", " << output_domain << ")");

    traverse_intersections<increment_entry_count_op>
      increment_f(*this, true, increment_entry_count_op(logger));
    traverse_intersections<find_incipient_entry_subset_op<T> >
      find_result_f(*this, true,
		    find_incipient_entry_subset_op<T>(output_domain,
						      output_incipient,
						      output_non_incipient,
						      logger));

    choices.for_each(increment_f);
    // We have to run this even if the increment aborted, since we
    // need to reset all the counters to 0 for the next run.
    choices.for_each(find_result_f);

    // The above code won't find promotions that include only values
    // in the output domain.  Look for those by hand.
    output_domain.for_each(find_unary_promotions<T>(*this, output_incipient));
  }

private:
  // find_highest_promotion_containing helpers:

  /** \brief Used to build the local indices for
   *  find_highest_promotion_containing().
   */
  struct build_local_indices
  {
    // Maps versions to choices associated with installing those
    // versions.
    boost::unordered_map<version, choice> &choices_by_install_version;

    // Stores the set of broken soft dependencies.
    boost::unordered_set<dep> &broken_soft_deps;

    logging::LoggerPtr logger;

    build_local_indices(boost::unordered_map<version, choice> &_choices_by_install_version,
			boost::unordered_set<dep> &_broken_soft_deps,
			const logging::LoggerPtr &_logger)
      : choices_by_install_version(_choices_by_install_version),
	broken_soft_deps(_broken_soft_deps),
	logger(_logger)
    {
    }

    bool operator()(const choice &c) const
    {
      switch(c.get_type())
	{
	case choice::install_version:
	  LOG_TRACE(logger, "Adding entry to the local index: "
		    << c.get_ver() << " |-> " << c);
	  choices_by_install_version[c.get_ver()] = c;
	  break;

	case choice::break_soft_dep:
	  LOG_TRACE(logger, "Adding broken soft dep to the local index: "
		    << c.get_dep());
	  broken_soft_deps.insert(c.get_dep());
	  break;

	default:
	  LOG_ERROR(logger, "Bad choice type " << c.get_type());
	  break;
	}

      return true;
    }
  };

  /** \brief Used to check whether a set of choices matches the values
   *  stored in the local indices.
   *
   *  This returns true if exactly "num_mismatches" of its input
   *  choices are NOT in the local indices.  It is used with
   *  num_mismatches=0 to check that promotions are strictly contained
   *  in the input, and with num_mismatches=1 to check for promotions
   *  that would match if a single choice was added.
   */
  struct check_choices_in_local_indices
  {
    /** \brief Set to true if all the choices were found, and false
     *  otherwise.
     */
    bool &rval;

    // The number of mismatches to look for.  Decremented as we
    // encounter mismatches.  If it becomes negative, the traversal
    // aborts.
    int &num_mismatches;

    // Maps versions to choices associated with installing those
    // versions.
    const boost::unordered_map<version, choice> &choices_by_install_version;

    // Stores the set of broken soft dependencies.
    const boost::unordered_set<dep> &broken_soft_deps;

    logging::LoggerPtr logger;

    check_choices_in_local_indices(const boost::unordered_map<version, choice> &_choices_by_install_version,
				   const boost::unordered_set<dep> &_broken_soft_deps,
				   const logging::LoggerPtr &_logger,
				   int &_num_mismatches,
				   bool &_rval)
      : rval(_rval),
	num_mismatches(_num_mismatches),
	choices_by_install_version(_choices_by_install_version),
	broken_soft_deps(_broken_soft_deps),
	logger(_logger)
    {
      rval = (num_mismatches == 0);
    }

    bool operator()(const choice &c) const
    {
      switch(c.get_type())
	{
	case choice::install_version:
	  {
	    typename boost::unordered_map<version, choice>::const_iterator found =
	      choices_by_install_version.find(c.get_ver());

	    bool ok = false;
	    if(found == choices_by_install_version.end())
	      LOG_TRACE(logger, "The choice " << c << " is not in the local indices: " << c.get_ver() << " is not in the install version index.");
	    // OK, check that this choice contains the corresponding
	    // choice in the index.  (remember, the index is built
	    // from the set that we are finding a subset of)
	    else if(!c.get_from_dep_source())
	      // Installations not from the dep source match anything.
	      ok = true;
	    else
	      {
		const choice &found_c = found->second;
		if(!found_c.get_from_dep_source())
		  LOG_TRACE(logger, "The choice " << c << " is not in the local indices: the corresponding version entry " << found_c << " is not from a dep source.");
		else if(found_c.get_dep() != c.get_dep())
		  LOG_TRACE(logger, "The choice " << c << " is not in the local indices: the corresponding version entry " << found_c << " is from a different dep.");
		else
		  ok = true;
	      }

	    if(ok)
	      LOG_TRACE(logger, "The choice " << c << " contains the input choice " << found->second);
	    else
	      --num_mismatches;
	  }

	  break;

	case choice::break_soft_dep:
	  {
	    typename boost::unordered_set<dep>::const_iterator found =
	      broken_soft_deps.find(c.get_dep());

	    if(found == broken_soft_deps.end())
	      {
		LOG_TRACE(logger, "The choice " << c << " is not in the local indices: " << c.get_dep() << " is not in the list of broken soft deps.");
		--num_mismatches;
	      }
	    else
	      LOG_TRACE(logger, "The choice " << c << " contains the input choice " << *found);
	  }

	  break;

	default:
	  LOG_ERROR(logger, "Bad choice type " << c.get_type());
	  --num_mismatches;
	  break;
	}

      // We succeeded if we found exactly the desired number of
      // mismatches.  If the desired number is still above zero, then
      // we could still succeed; otherwise we fail hard.
      rval = (num_mismatches == 0);
      return num_mismatches >= 0;
    }
  };

public:
  /** \brief Find a highest cost promotion that is a subset of the
   *  given set of choices *and* that contains the given choice.
   *
   *  Implements requirement (1).
   *
   *  Returns a default-initialized promotion if nothing was found.
   */
  promotion find_highest_promotion_containing(const choice_set &choices,
					      const choice &c) const
  {
    LOG_TRACE(logger, "Entering find_highest_promotion_containing(" << choices << ", " << c << ")");

    const std::vector<entry_ref> *index_entries = find_index_list(c);

    if(index_entries == NULL || index_entries->empty())
      {
	LOG_TRACE(logger, "find_highest_promotion_containing: There are no index entries for " << c << "; returning a minimal promotion.");
	return promotion();
      }
    else
      {
	// Build local indices, used to make it reasonable to compare all
	// the promotions in index_entries to the input choice list.

	boost::unordered_map<version, choice> choices_by_install_version;
	boost::unordered_set<dep> broken_soft_deps;
	build_local_indices build_indices_f(choices_by_install_version,
					    broken_soft_deps, logger);


	LOG_TRACE(logger, "find_highest_promotion_containing: Building local index.");
	choices.for_each(build_indices_f);


	LOG_TRACE(logger, "find_highest_promotion_containing: Matching indexed entries for " << c << " to the local index.");

	promotion rval;
	for(typename std::vector<entry_ref>::const_iterator it = index_entries->begin();
	    it != index_entries->end(); ++it)
	  {
	    bool contains_match = false;
	    int num_mismatches = 0;
	    check_choices_in_local_indices
	      all_choices_found_f(choices_by_install_version,
				  broken_soft_deps,
				  logger,
				  num_mismatches,
				  contains_match);

	    const promotion &p((*it)->p);
	    p.get_choices().for_each(all_choices_found_f);
	    if(contains_match)
	      {
		promotion upper_bound = promotion::least_upper_bound(rval, p);
		if(upper_bound.get_cost() != rval.get_cost())
		  {
		    LOG_TRACE(logger, "find_highest_promotion_containing: found a new promotion: " << p
			      << " (previous promotion was " << rval
			      << ", new is " << upper_bound << ")");
		    rval = upper_bound;
		  }		  
	      }
	  }

	return rval;
      }
  }

  /** \brief Find the highest-cost incipient promotion containing a
   *  particular choice.
   *
   *  An incipient promotion is one that doesn't match now, but that
   *  would match if a single choice was added to the set of choices.
   *
   *  \tparam Pred    A function type used to filter the returned
   *                  promotions.
   *
   *  \param choices  The choice set to test.
   *  \param c        A choice that must be contained in every returned
   *                  promotion.
   *  \param output_domain
   *                  Additional choices, one of which must be contained
   *                  in every returned promotion.  The return values
   *                  are organized according to which of these choices
   *                  each one contained, and only the highest-cost
   *                  promotion for each choice is returned.
   *  \param pred     A predicate used to filter the returned promotions.
   *                  Only promotions for which pred returns \b true are
   *                  returned.
   *  \param output   A map in which to store the results of the search.
   *                  Choices in output_domain that were matched are
   *                  mapped to the highest-cost promotion that they
   *                  would trigger.
   *
   *  The values that output_domain associates with the choices it
   *  stores are ignored.
   *
   *  In the common use case, "choices" represents the actions taken
   *  by a step, "c" is a new solver being added, and "output_domain"
   *  contains all the solvers of the step, each mapped to the
   *  dependencies that it solves.  The predicate is used to throw
   *  away promotions that are invalid for whatever reason if the
   *  target step is a "blessed" solution.
   */
  template<typename Pred, typename T>
  void find_highest_incipient_promotions_containing(const choice_set &choices,
						    const choice &c,
						    const generic_choice_indexed_map<PackageUniverse, T> &output_domain,
						    Pred pred,
						    boost::unordered_map<choice, promotion> &output) const
  {
    LOG_TRACE(logger, "Entering find_highest_incipient_promotions_containing(" << choices << ", " << c << ", " << output_domain << ")");

    const std::vector<entry_ref> *index_entries = find_index_list(c);

    if(index_entries == NULL || index_entries->empty())
      {
	LOG_TRACE(logger, "find_highest_incipient_promotion_containing: There are no index entries for " << c << "; returning an empty map.");
      }
    else
      {
	// Build local indices, used to make it reasonable to compare all
	// the promotions in index_entries to the input choice list.

	boost::unordered_map<version, choice> choices_by_install_version;
	boost::unordered_set<dep> broken_soft_deps;
	build_local_indices build_indices_f(choices_by_install_version,
					    broken_soft_deps, logger);


	LOG_TRACE(logger, "find_highest_incipient_promotion_containing: Building local index.");
	choices.for_each(build_indices_f);


	LOG_TRACE(logger, "find_highest_incipient_promotion_containing: Matching indexed entries for " << c << " to the local index.");

	for(typename std::vector<entry_ref>::const_iterator it = index_entries->begin();
	    it != index_entries->end(); ++it)
	  {
	    const promotion &p((*it)->p);

	    LOG_TRACE(logger, "find_highest_incipient_promotion_containing: testing " << p << ".");

	    if(!pred(p))
	      continue;

	    bool is_incipient = false;
	    int num_mismatches = 1;
	    check_choices_in_local_indices
	      choices_found_f(choices_by_install_version,
			      broken_soft_deps, logger,
			      num_mismatches, is_incipient);

	    p.get_choices().for_each(choices_found_f);
	    if(is_incipient)
	      {
		update_incipient_output<T> updater(p, output_domain, output);
		LOG_TRACE(logger, "find_highest_incipient_promotion_containing: found a match: " << p);
		p.get_choices().for_each(updater);
	      }
	  }
      }
  }

private:
  /** \brief Retrieve all the promotions that are supersets of the
   * given promotion.
   *
   *  This is used internally to purge redundant entries when adding a
   *  new entry.  The goal is to find all the entries that are not at
   *  a higher cost, and for which every set of choices that contained
   *  them would also contain the new entry.  (i.e., removing those
   *  entries has no effect on the costs assigned to search nodes,
   *  because the new entry assigns the same nodes an equal or higher
   *  cost)
   *
   *  \param p      The promotion whose supersets should be returned.
   *  \param output A vector in which to store the results.
   */
  void find_superseded_entries(const promotion &p,
			       std::vector<entry_ref> &output) const
  {
    traverse_intersections<increment_entry_count_op>
      increment_f(*this, false, increment_entry_count_op(logger));
    traverse_intersections<find_entry_supersets_op>
      find_results_f(*this, false,
		     find_entry_supersets_op(output,
					     p.get_choices().size(),
					     p.get_cost(),
					     logger));

    const choice_set &choices(p.get_choices());
    choices.for_each(increment_f);
    choices.for_each(find_results_f);
  }

  /** \brief Collect the versions and soft dependencies related
   *  to a single choice.
   */
  static void collect_indexers(const choice &c,
			       boost::unordered_set<version> &installed_versions,
			       boost::unordered_set<dep> &broken_soft_deps,
			       const logging::LoggerPtr &logger)
  {
    switch(c.get_type())
      {
      case choice::install_version:
	LOG_TRACE(logger, "collect_indexers: adding " << c.get_ver() << " to the set of installed versions.");
	installed_versions.insert(c.get_ver());
	break;

      case choice::break_soft_dep:
	LOG_TRACE(logger, "collect_indexers: adding " << c.get_dep() << " to the set of broken soft deps.");
	broken_soft_deps.insert(c.get_dep());
	break;

      default:
	LOG_ERROR(logger, "collect_indexers: bad choice type " << c.get_type());
	break;
      }
  }

  /** \brief Function object that invokes collect_indexers() on a
   *  choice object.
   */
  struct do_collect_indexers
  {
    boost::unordered_set<version> &installed_versions;
    boost::unordered_set<dep> &broken_soft_deps;
    logging::LoggerPtr logger;

    do_collect_indexers(boost::unordered_set<version> &_installed_versions,
			boost::unordered_set<dep> &_broken_soft_deps,
			const logging::LoggerPtr &_logger)
      : installed_versions(_installed_versions),
	broken_soft_deps(_broken_soft_deps),
	logger(_logger)
    {
    }

    bool operator()(const choice &c) const
    {
      collect_indexers(c, installed_versions, broken_soft_deps, logger);
      return true;
    }
  }; 

  /** \brief Collect the versions and soft dependencies related
   *  to a single entry.
   */
  static void collect_indexers(const entry &e,
			       boost::unordered_set<version> &installed_versions,
			       boost::unordered_set<dep> &broken_soft_deps,
			       const logging::LoggerPtr &logger)
  {
    e.p.get_choices().for_each(do_collect_indexers(installed_versions,
						   broken_soft_deps,
						   logger));
  }

  /** \brief Collect the versions and soft dependencies related to
   *  each choice in a cost.
   */
  static void collect_indexers(const std::list<entry> &cost_entries,
			       boost::unordered_set<version> &installed_versions,
			       boost::unordered_set<dep> &broken_soft_deps,
			       const logging::LoggerPtr &logger)
  {
    for(typename std::list<entry>::const_iterator it = cost_entries.begin();
	it != cost_entries.end(); ++it)
      collect_indexers(*it, installed_versions, broken_soft_deps, logger);
  }

  /** \brief Drop entries from the given vector, using the given
   *  predicate to decide which ones to drop.
   */
  template<typename Pred>
  static void erase_vector_entries(std::vector<entry_ref> &entries,
				   const logging::LoggerPtr &logger,
				   const Pred &pred)
  {
    if(logger->isEnabledFor(logging::TRACE_LEVEL))
      {
	for(typename std::vector<entry_ref>::const_iterator it =
	      entries.begin(); it != entries.end(); ++it)
	  if(pred(*it))
	    LOG_TRACE(logger, "  Removing " << (*it)->p);
      }

    typename std::vector<entry_ref>::iterator new_end = std::remove_if(entries.begin(), entries.end(), pred);
    entries.erase(new_end, entries.end());
  }

  /** \brief Remove all the promotion index entries for the given
   *  set of installed versions.
   *
   *  \tparam Pred The predicate type to use in deciding
   *               which entries to drop.
   *  \param pred  The predicate to use in deciding
   *               which entries to drop.
   */
  template<typename Pred>
  void drop_install_version_index_entries(const boost::unordered_set<version> &installed_versions,
					  const Pred &pred)
  {
    for(typename boost::unordered_set<version>::const_iterator it = installed_versions.begin();
	it != installed_versions.end(); ++it)
      {
	install_version_index_entry *index_entry(install_version_index[it->get_id()]);

	if(index_entry != NULL)
	  {
	    LOG_TRACE(logger, "Purging dead references from the index entries for " << *it << ":");
	    erase_vector_entries(index_entry->not_from_dep_source_entries,
				 logger, pred);
	    bool from_dep_source_map_empty = true;
	    for(typename boost::unordered_map<dep, std::vector<entry_ref> >::iterator
		  from_dep_source_it
		  = index_entry->from_dep_source_entries.begin();
		from_dep_source_it !=
		  index_entry->from_dep_source_entries.end();
		++from_dep_source_it)
	      {
		erase_vector_entries(from_dep_source_it->second, logger, pred);
		if(!from_dep_source_it->second.empty())
		  from_dep_source_map_empty = false;
	      }

	    if(index_entry->not_from_dep_source_entries.empty() &&
	       from_dep_source_map_empty)
	      {
		LOG_DEBUG(logger, "All index entries for " << *it
			  << " have been removed; dropping the index cell.");
		delete index_entry;
		install_version_index[it->get_id()] = NULL;
	      }
	  }
	else
	  LOG_ERROR(logger, "The version " << *it << " didn't actually have index entries, but it should have.");
      }
  }

  template<typename Pred>
  void drop_broken_soft_dep_index_entries(const boost::unordered_set<dep> &broken_soft_deps,
					  const Pred &pred)
  {
    for(typename boost::unordered_set<dep>::iterator it = broken_soft_deps.begin();
	it != broken_soft_deps.end(); ++it)
      {
	typename boost::unordered_map<dep, break_soft_dep_index_entry>::iterator
	  found = break_soft_dep_index.find(*it);

	if(found == break_soft_dep_index.end())
	  // Indicates an inconsistency in the book-keeping.
	  LOG_ERROR(logger, "Unable to find an index list for " << *it << ", but one should exist.");
	else
	  {
	    LOG_TRACE(logger, "Purging dead references from the index entries for " << *it << ":");
	    std::vector<entry_ref> &index_entries = found->second;
	    erase_vector_entries(index_entries, logger, pred);

	    if(index_entries.empty())
	      {
		LOG_DEBUG(logger, "All index entries for " << *it
			  << " have been removed; dropping the index cell.");
		break_soft_dep_index.erase(found);
	      }
	  }
      }
  }

  /** \brief Function object that inserts choices into the index
   *  structures one at a time.
   */
  struct make_index_entries
  {
    // A reference to the newly inserted promotion.
    entry_ref new_entry;
    install_version_index_entry **install_version_index;
    boost::unordered_map<dep, break_soft_dep_index_entry> &break_soft_dep_index;
    logging::LoggerPtr logger;

    make_index_entries(entry_ref _new_entry,
		       install_version_index_entry **_install_version_index,
		       boost::unordered_map<dep, break_soft_dep_index_entry> &_break_soft_dep_index,
		       const logging::LoggerPtr &_logger)
      : new_entry(_new_entry),
	install_version_index(_install_version_index),
	break_soft_dep_index(_break_soft_dep_index),
	logger(_logger)
    {
    }

    bool operator()(const choice &c) const
    {
      switch(c.get_type())
	{
	case choice::install_version:
	  if(!c.get_from_dep_source())
	    LOG_TRACE(logger, "Inserting an index entry: " << c.get_ver()
		      << " |-> " << new_entry->p);
	  else
	    LOG_TRACE(logger, "Inserting an index entry: " << c.get_ver()
		      << "[" << c.get_dep() << "] |-> " << new_entry->p);
	  {
	    const int id = c.get_ver().get_id();
	    install_version_index_entry *index_entry = install_version_index[id];
	    if(index_entry == NULL)
	      {
		LOG_DEBUG(logger, "Creating a new index cell for " << c.get_ver());
		index_entry = new install_version_index_entry;
		install_version_index[id] = index_entry;
	      }

	    if(!c.get_from_dep_source())
	      {
		LOG_TRACE(logger, "Inserting " << c << " into the not-from-dep-source-list.");
		index_entry->not_from_dep_source_entries.push_back(new_entry);
		for(typename boost::unordered_map<dep, std::vector<entry_ref> >::iterator
		      from_dep_source_it = index_entry->from_dep_source_entries.begin();
		    from_dep_source_it != index_entry->from_dep_source_entries.end();
		    ++from_dep_source_it)
		  {
		    LOG_TRACE(logger, "Inserting " << c << " into the from-dep-source list for " << from_dep_source_it->first << ".");
		    from_dep_source_it->second.push_back(new_entry);
		  }
	      }
	    else
	      {
		typename boost::unordered_map<dep, std::vector<entry_ref> >::iterator found =
		  index_entry->from_dep_source_entries.find(c.get_dep());

		if(found == index_entry->from_dep_source_entries.end())
		  {
		    LOG_DEBUG(logger, "Creating a new from-dep index cell for " << c.get_dep());
		    found = index_entry->from_dep_source_entries.insert(found, std::make_pair(c.get_dep(), std::vector<entry_ref>()));
		    // Make sure the new cell contains all the entries
		    // in the not-from-dep-source list.
		    found->second.insert(found->second.end(),
					 index_entry->not_from_dep_source_entries.begin(),
					 index_entry->not_from_dep_source_entries.end());
		  }

		LOG_TRACE(logger, "Inserting " << c << " into the from-dep-source-list.");
		found->second.push_back(new_entry);
	      }
	  }
	  break;

	case choice::break_soft_dep:
	  LOG_TRACE(logger, "Inserting an index entry: "
		    << c.get_dep() << " |-> " << new_entry->p);
	  {
	    const dep &d(c.get_dep());
	    // We could just do a straightforward insertion, but doing
	    // things this way lets us provide better debug traces
	    // (more info about when memory is being allocated).
	    typename boost::unordered_map<dep, break_soft_dep_index_entry>::iterator found =
	      break_soft_dep_index.find(d);

	    if(found == break_soft_dep_index.end())
	      {
		LOG_DEBUG(logger, "Creating a new index cell for " << d);
		found = break_soft_dep_index.insert(found,
						    std::make_pair(d, break_soft_dep_index_entry()));
	      }

	    found->second.push_back(new_entry);
	  }
	  break;

	default:
	  LOG_ERROR(logger, "Bad choice type " << c.get_type());
	  break;
	}

      return true;
    }
  };

  /** \brief Predicate testing whether an entry reference is in 
   *  a set of dropped entries.
   */
  struct entry_ref_in_dropped_set_pred
  {
    const boost::unordered_set<const entry *> &dropped_set;

    entry_ref_in_dropped_set_pred(const boost::unordered_set<const entry *> &_dropped_set)
      : dropped_set(_dropped_set)
    {
    }

    bool operator()(entry_ref r) const
    {
      return dropped_set.find(&*r) != dropped_set.end();
    }
  };

public:
  /** \brief Insert a promotion into this set.
   *
   *  The promotion will not be inserted if an existing promotion of
   *  the same cost or higher is a subset of it; otherwise,
   *  it will be inserted and all existing promotions of the same cost
   *  operation or lower that are supersets of the new promotion will
   *  be removed.  (promotions with an unrelated cost are
   *  unaffected)
   *
   *  \return   an iterator pointing at the new promotion if one was
   *            actually inserted, or an end iterator otherwise.
   */
  iterator insert(const promotion &p)
  {
    const cost &p_cost = p.get_cost();
    const choice_set &choices = p.get_choices();

    LOG_DEBUG(logger, "Inserting " << p << " into the promotion set.");

    // Note that we can suppress adding a promotion if there are
    // several promotions that would jointly imply it.  e.g., if the
    // set {a, b} has a promotion (0, +1) and {b, c} has a promotion
    // (+1, 0), we don't need to store a promotion for {a, b, c} with
    // the operation (+1, +1), as it wouldn't add any information.
    const cost highest(find_highest_promotion_cost(choices));
    if(highest.is_above_or_equal(p_cost))
      {
	LOG_INFO(logger, "Canceling the insertion of " << p << ": it is redundant with the existing or inferred promotion " << highest);
	return end();
      }
    else
      {
	std::vector<entry_ref> superseded_entries;
	find_superseded_entries(p, superseded_entries);

	if(!superseded_entries.empty())
	{
	  // Purge the index entries associated with these superseded
	  // entries.
	  boost::unordered_set<version> installed_versions;
	  boost::unordered_set<dep> broken_soft_deps;

	  for(typename std::vector<entry_ref>::const_iterator it = superseded_entries.begin();
	      it != superseded_entries.end(); ++it)
	    collect_indexers(**it, installed_versions, broken_soft_deps, logger);

	  LOG_TRACE(logger, "Removing index entries associated with the superseded entries.");
	  // Build a set of pointers that we'll use to figure out
	  // which references-to-entries need to be dropped.
	  //
	  // Note: This relies on knowing that pointers to entries are
	  // stable as long as we don't modify the lists that contain
	  // them.
	  boost::unordered_set<const entry *> superseded_entries_set;
	  for(typename std::vector<entry_ref>::const_iterator it = superseded_entries.begin();
	      it != superseded_entries.end(); ++it)
	    superseded_entries_set.insert(&**it);
	  entry_ref_in_dropped_set_pred dropped_f(superseded_entries_set);
	  drop_install_version_index_entries(installed_versions,
					     dropped_f);
	  drop_broken_soft_dep_index_entries(broken_soft_deps,
					     dropped_f);
	}

	LOG_TRACE(logger, "Removing the superseded entries themselves.");
	for(typename std::vector<entry_ref>::const_iterator it = superseded_entries.begin();
	    it != superseded_entries.end(); ++it)
	  {
	    entry_ref ent(*it);
	    LOG_TRACE(logger, "Removing " << ent->p);

	    if(ent->p.get_cost().get_structural_level() >= cost_limits::conflict_structural_level)
	      --num_conflicts;

	    entries.erase(ent);
          }

	LOG_TRACE(logger, "Inserting " << p);

	// Insert the new entry into the list of entries in this cost.
	const entry_ref new_entry =
	  entries.insert(entries.end(), entry(p));
	if(p.get_cost().get_structural_level() >= cost_limits::conflict_structural_level)
	  ++num_conflicts;

	LOG_TRACE(logger, "Building index entries for " << p);
	p.get_choices().for_each(make_index_entries(new_entry,
						    install_version_index,
						    break_soft_dep_index,
						    logger));

	return iterator(new_entry);
      }
  }

  /** \brief Throw away all the promotions in this set. */
  void clear()
  {
    entries.clear();
    break_soft_dep_index.clear();
    num_conflicts = 0;
    for(int i = 0; i < num_versions; ++i)
      {
	delete install_version_index[i];
	install_version_index[i] = NULL;
      }
  }

  generic_promotion_set(const PackageUniverse &u,
			promotion_set_callbacks<PackageUniverse> &_callbacks)
    : logger(aptitude::Loggers::getAptitudeResolverSearchCosts()),
      callbacks(_callbacks),
      num_conflicts(0),
      num_versions(u.get_version_count()),
      install_version_index(new install_version_index_entry*[num_versions])
  {
    for(int i = 0; i < num_versions; ++i)
      install_version_index[i] = NULL;
  }

  ~generic_promotion_set()
  {
    for(int i = 0; i < num_versions; ++i)
      delete install_version_index[i];
    delete[] install_version_index;
  }
};

template<typename PackageUniverse>
std::ostream &operator<<(std::ostream &out, const generic_promotion_set<PackageUniverse> &s)
{
  out << "{";
  for(typename generic_promotion_set<PackageUniverse>::const_iterator it =
	s.begin(); it != s.end(); ++it)
    {
      if(it != s.begin())
	out << ", ";
      out << *it;
    }
  out << "}";

  return out;
}

#endif
