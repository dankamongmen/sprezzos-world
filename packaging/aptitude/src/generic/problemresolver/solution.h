// solution.h                                             -*-c++-*-
//
//   Copyright (C) 2005, 2007-2011 Daniel Burrows
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
// The solution class for the problem resolver.

#ifndef SOLUTION_H
#define SOLUTION_H

#include <iostream>
#include <map>
#include <set>

#include <cwidget/generic/util/ref_ptr.h>
#include <generic/util/immset.h>
#include <generic/util/refcounted_base.h>
#include "choice.h"
#include "choice_set.h"
#include "cost.h"

/** \brief The solution class for the problem resolver.
 * 
 *  \file solution.h
 */

template<typename PackageUniverse>
class solution_weights;

/** \brief Represents the initial state of a dependency search.
 *
 *  This is optimized under the assumption that overrides of package
 *  versions will be rare, but not unheard of.  (note: this is O(1),
 *  but in reality it might not be optimized due to poor locality and
 *  the low number of overrides; if it's really an issue some
 *  profiling of different approaches -- sparse binary tree, sparse
 *  array, etc -- would be handy)
 */
template<typename PackageUniverse>
class resolver_initial_state
{
  class impl : public aptitude::util::refcounted_base_threadsafe
  {
    // A collection of indices into the vector of overridden versions,
    // indexed by package ID; -1 means to use the real current
    // version.  This lets us avoid allocating space for
    // non-overridden versions while keeping good locality for the
    // list of versions.
    //
    // If NULL, all packages have their real current version.
    int *overridden_versions;
    // The size that the version array has / will have (depending on
    // whether the array is NULL).
    int num_overridden_versions;

    // Stores the versions that have been overridden;
    // overridden_versions indexes into this list.
    std::vector<typename PackageUniverse::version> version_store;

    /** \brief Override p (if necessary) to the version v. */
    void map_package(const typename PackageUniverse::package &p,
		     const typename PackageUniverse::version &v)
    {
      if(overridden_versions == NULL)
	{
	  if(p.current_version() == v)
	    return;

	  overridden_versions = new int[num_overridden_versions];
	  for(int i = 0; i < num_overridden_versions; ++i)
	    overridden_versions[i] = -1;
	}

      const int p_id = p.get_id();
      if(overridden_versions[p_id] == -1)
	{
	  if(p.current_version() == v)
	    return;

	  // Allocate a new slot.
	  const int slot = (int)version_store.size();
	  version_store.push_back(v);
	  overridden_versions[p_id] = slot;
	}
      else
	{
	  const int slot = overridden_versions[p_id];
	  version_store[slot] = v;
	}
    }

    struct do_map_package
    {
      impl &state;

      do_map_package(impl &_state)
	: state(_state)
      {
      }

      bool operator()(const std::pair<typename PackageUniverse::package, typename PackageUniverse::version> &pair) const
      {
	state.map_package(pair.first, pair.second);
	return true;
      }
    };

  public:
    /** \brief Create a new initial state that sets the given packages
     *  to the given versions.
     *
     *  \param mappings   Each package in this map will be treated as
     *                    "starting at" the version it is mapped to.
     *
     *  \param package_count The number of packages in the universe;
     *                          used to allocate space for internal
     *                          structures.
     */
    impl(const imm::map<typename PackageUniverse::package, typename PackageUniverse::version> &mappings,
	 int package_count)
      : overridden_versions(NULL),
	num_overridden_versions(package_count)
    {
      mappings.for_each(do_map_package(*this));
    }

    bool empty() const
    {
      if(overridden_versions == NULL)
	return true;

      for(int i = 0; i < num_overridden_versions; ++i)
	if(overridden_versions[i] != -1)
	  return false;

      return true;
    }

    ~impl()
    {
      delete[] overridden_versions;
    }

    typename PackageUniverse::version version_of(const typename PackageUniverse::package &p) const
    {
      int slot;
      if(overridden_versions == NULL)
        slot = -1;
      else
        slot = overridden_versions[p.get_id()];

      if(slot == -1)
	return p.current_version();
      else
	return version_store[slot];
    }

    void get_initial_versions(std::set<typename PackageUniverse::version> &out) const
    {
      out.insert(version_store.begin(), version_store.end());
    }
  };

  // If invalid, this represents an empty set of initial versions;
  // otherwise, a pointer to the real object that contains the initial
  // version set.
  cwidget::util::ref_ptr<impl> the_impl;

public:
  resolver_initial_state()
  {
  }

  resolver_initial_state(const resolver_initial_state &state)
    : the_impl(state.the_impl)
  {
  }

  resolver_initial_state(const imm::map<typename PackageUniverse::package, typename PackageUniverse::version> &mappings,
			 int package_count)
    : the_impl(mappings.empty()
	       ? cwidget::util::ref_ptr<impl>()
	       : new impl(mappings, package_count))
  {
  }

  resolver_initial_state &operator=(const resolver_initial_state &other)
  {
    the_impl = other.the_impl;
    return *this;
  }

  bool empty() const
  {
    return !the_impl || the_impl->empty();
  }

  typename PackageUniverse::version version_of(const typename PackageUniverse::package &p) const
  {
    if(the_impl.valid())
      return the_impl->version_of(p);
    else
      return p.current_version();
  }

  /** \brief Retrieve the initial installations stored in this object.
   *
   *  \param out  A set into which the initially installed versions
   *              are placed; it is guaranteed that no two output
   *              versions have the same package.
   */
  void get_initial_versions(std::set<typename PackageUniverse::version> &out) const
  {
    if(the_impl.valid())
      return the_impl->get_initial_versions(out);
  }
};

/** Represents a partial or complete solution to a dependency
 *  problem.  Solutions are transparently refcounted to save on
 *  memory and avoid copies.
 *
 *  A solution is simply a set of choices.  Solutions also carry
 *  around the set of dependencies that they don't solve, for the sake
 *  of convenience.
 *
 *  Solution identity is based on both the mapping stored in the
 *  solution and on the set of unfixed soft dependencies stored in it.
 *  Dependencies in unfixed_soft_deps are removed from broken_deps so
 *  it's easy to check whether a solution is complete.
 */
template<class PackageUniverse>
class generic_solution
{
public:
  // Let the resolver tester poke around in our internals.
  friend class ResolverTest;

  typedef typename PackageUniverse::package package;
  typedef typename PackageUniverse::version version;
  typedef typename PackageUniverse::dep dep;
  typedef generic_choice<PackageUniverse> choice;
  typedef generic_choice_set<PackageUniverse> choice_set;

private:
  /** Hide this, it's meaningless. */
  bool operator<(const generic_solution &other) const;

  class solution_rep
  {
    /** \brief The initial state of this solution.
     *
     *  This is currently assumed to be the same for all solutions in
     *  a given resolver run (i.e., operator== and friends don't check
     *  it); if it's not, you have some serious weirdness.  The
     *  pointer goes away after the resolver is done with its
     *  computations, but nothing else should use it anyway.
     *
     *  We need this mainly so that version_of works properly with no
     *  extra arguments.  (maybe instead I should just accept having
     *  to wrap it all the time?)
     */
    const resolver_initial_state<PackageUniverse> initial_state;

    /** \brief The choices made in this solution. */
    choice_set choices;

    /** The score of this solution. */
    int score;

    /** \brief The cost of this solution.
     *
     *  Informational only; not considered when comparing solutions by
     *  identity.
     */
    cost sol_cost;

    /** The reference count of this solution. */
    mutable unsigned int refcount;

  public:
    void incref() const {++refcount;}
    void decref() const {eassert(refcount>0); if(--refcount==0) delete this;}

    /** Construct a new solution_rep directly. */
    solution_rep(const choice_set &_choices,
		 const resolver_initial_state<PackageUniverse> &_initial_state,
		 int _score,
		 const cost &_sol_cost)
      : initial_state(_initial_state), choices(_choices),
	score(_score),
	sol_cost(_sol_cost),
	refcount(1)
    {
    }

    const resolver_initial_state<PackageUniverse> &get_initial_state() const
    {
      return initial_state;
    }

    const choice_set &get_choices() const
    {
      return choices;
    }

    int get_score() const {return score;}
    const cost &get_cost() const { return sol_cost; }

    version version_of(const package &pkg) const
    {
      version rval;
      if(choices.get_version_of(pkg, rval))
	return rval;
      else
	return initial_state.version_of(pkg);
    }

    /** \return true iff this solution touches the given package. */
    bool package_modified(const package &pkg) const
    {
      version dummy;
      return choices.get_version_of(pkg);
    }
  }; // End solution representation.

  solution_rep *real_soln;

  /** Create a solution directly from a rep; assumes control of the
   *  reference passed in as an argument (i.e., doesn't incref() it)
   */
  generic_solution(solution_rep *r)
    :real_soln(r)
  {
  }

  /** Wrapper structure used to pass a raw set of choices into
   *  broken_under().  Used to determine the set of packages broken by
   *  a solution before the solution is actually created.
   */
  struct solution_map_wrapper
  {
    const choice_set &choices;
    const resolver_initial_state<PackageUniverse> &initial_state;
  public:
    solution_map_wrapper(const choice_set &_choices,
			 const resolver_initial_state<PackageUniverse> &_initial_state)
      : choices(_choices),
	initial_state(_initial_state)
    {
    }

    version version_of(const package &p) const
    {
      version rval;
      if(choices.get_version_of(p, rval))
	return rval;
      else
	return initial_state.version_of(p);
    }
  };

public:
  generic_solution():real_soln(0) {}

  generic_solution(const generic_solution &other)
    :real_soln(other.real_soln)
  {
    if(real_soln)
      real_soln->incref();
  }

  generic_solution(const choice_set &choices,
		   const resolver_initial_state<PackageUniverse> &initial_state,
		   int score,
		   const cost &sol_cost)
    : real_soln(new solution_rep(choices, initial_state, score, sol_cost))
  {
  }

  /** Generate a new, identical solution that shares no memory with
   *  this solution and hence is safe to hand to another thread.  Note
   *  that as with other refcounted structures, the handover should be
   *  done "at one remove" by allocating a generic_solution object on
   *  the heap.
   */
  generic_solution clone() const
  {
    return generic_solution(new solution_rep(get_choices().clone(),
					     get_initial_state(),
					     get_score(),
					     get_cost()));
  }


  ~generic_solution()
  {
    if(real_soln)
      real_soln->decref();
  }

  /** Make this an invalid reference. */
  void nullify()
  {
    if(real_soln)
      real_soln->decref();
    real_soln=0;
  }

  generic_solution &operator=(const generic_solution &other)
  {
    if(other.real_soln)
      other.real_soln->incref();
    if(real_soln)
      real_soln->decref();
    real_soln=other.real_soln;

    return *this;
  }

  /** \note solutions are compared by pointer identity, not by
   *  value.
   *
   *  \return \b true if the solutions are the same solution.
   */
  bool operator==(const generic_solution &other) const
  {
    return real_soln == other.real_soln;
  }

  /** Without this method, some random function that produces
   *  incorrect results is used.
   *
   *  \return \b true if the solutions are not the same solution.
   */
  bool operator!=(const generic_solution &other) const
  {
    return real_soln != other.real_soln;
  }

  bool valid() const
  {
    return real_soln != 0;
  }

  const generic_solution &get_parent() const
  {
    return real_soln->get_parent();
  }

  const choice_set &get_choices() const
  {
    return real_soln->get_choices();
  }

  /** \return the initial state of the solution. */
  const resolver_initial_state<PackageUniverse> &get_initial_state() const
  {
    return real_soln->get_initial_state();
  }

  /** \return the score of the scolution */
  int get_score() const
  {
    return real_soln->get_score();
  }

  /** \return The cost of this solution. */
  const cost &get_cost() const
  {
    return real_soln->get_cost();
  }

  version version_of(const package &pkg) const
  {
    return real_soln->version_of(pkg);
  }

  bool package_modified(const package &pkg) const
  {
    return real_soln->package_modified(pkg);
  }

  // The following operators are used to place the solution components
  // in order by name, to better permit comparison of debugging output
  // between versions.
  struct ver_name_lt
  {
  public:
    int cmp(const version &v1, const version &v2) const
    {
      // EW: I don't have a formal standard on what get_name()
      // returns, so force it to be a string here:
      int pcmp = std::string(v1.get_package().get_name()).compare(v2.get_package().get_name());

      if(pcmp != 0)
	return pcmp;
      else
	return std::string(v1.get_name()).compare(v2.get_name());
    }

    bool operator()(const version &v1, const version &v2) const
    {
      return cmp(v1, v2) < 0;
    }
  };

  struct dep_name_lt
  {
  public:
    bool operator()(const dep &d1, const dep &d2) const
    {
      ver_name_lt vlt;

      int scmp = vlt.cmp(d1.get_source(), d2.get_source());

      if(scmp != 0)
	return scmp < 0;
      else
	{
	  typename dep::solver_iterator si1 = d1.solvers_begin();
	  typename dep::solver_iterator si2 = d2.solvers_begin();

	  while(!si1.end() && !si2.end())
	    {
	      scmp = vlt.cmp(*si1, *si2);

	      if(scmp != 0)
		return scmp < 0;

	      ++si1;
	      ++si2;
	    }

	  if(si1.end())
	    {
	      if(si2.end())
		return false;
	      else
		return true;
	    }
	  else
	    return false;
	}
    }
  };

  struct choice_name_lt
  {
  public:
    bool operator()(const choice &c1,
		    const choice &c2) const
    {
      if(c1.get_type() < c2.get_type())
	return true;
      else if(c2.get_type() < c1.get_type())
	return false;
      else
	switch(c1.get_type())
	  {
	  case choice::install_version:
	    return ver_name_lt()(c1.get_ver(), c2.get_ver());

	  case choice::break_soft_dep:
	    return dep_name_lt()(c1.get_dep(), c2.get_dep());

	  default:
	    eassert(!"Unhandled choice type in choice_name_lt.");
	  }
    }
  };

  template<typename T>
  struct accumulate
  {
    std::vector<T> &ts;

    accumulate(std::vector<T> &_ts)
      : ts(_ts)
    {
    }

    bool operator()(const T &t) const
    {
      ts.push_back(t);
      return true;
    }
  };

  template<typename T, typename S>
  struct accumulate_1st
  {
    std::vector<T> &ts;

    accumulate_1st(std::vector<T> &_ts)
      : ts(_ts)
    {
    }

    bool operator()(const std::pair<T, S> &p) const
    {
      ts.push_back(p.first);
      return true;
    }
  };

  void dump(std::ostream &out, bool show_order = false) const
  {
    std::vector<choice> choices;
    get_choices().for_each(accumulate<choice>(choices));
    sort(choices.begin(), choices.end(), choice_name_lt());


    out << "<";
    for(typename std::vector<choice>::const_iterator it = choices.begin();
	it != choices.end(); ++it)
      {
	if(it != choices.begin())
	  out << ", ";
	out << *it;
	if(show_order)
	  out << "[#" << it->get_id() << "]";
      }
    out << ">;";

    out << "C" << get_cost() << "S" << get_score();
  }

  /** Compare choices by their ID */
  struct choice_id_compare
  {
  public:
    bool operator()(const choice &c1,
		    const choice &c2) const
    {
      return c1.get_id() < c2.get_id();
    }
  };
}; // End solution wrapper

/** \brief Dump a solution without showing the order of the entries. */
template<typename PackageUniverse>
std::ostream &operator<<(std::ostream &out, const generic_solution<PackageUniverse> &sol)
{
  sol.dump(out);
  return out;
}


/** Represents the current score weights for a resolver.  Used to
 *  calculate scores at the time a solution is instantiated.
 */
template<typename  PackageUniverse>
struct solution_weights
{
  typedef typename PackageUniverse::package package;
  typedef typename PackageUniverse::version version;
  typedef typename PackageUniverse::dep dep;
  typedef typename generic_solution<PackageUniverse>::choice choice;
  typedef typename generic_solution<PackageUniverse>::choice_set choice_set;

  /** \brief Represents a score assigned to a collection of choices. */
  class joint_score
  {
    choice_set choices;
    int score;

  public:
    joint_score(const choice_set &_choices, int _score)
      : choices(_choices), score(_score)
    {
    }

    const choice_set &get_choices() const { return choices; }
    int get_score() const { return score; }
  };

  /** \brief Compare two choices only by the actions they take,
   *  ignoring information such as whether they were triggered by a
   *  dependency source.
   */
  struct compare_choices_by_action
  {
    bool operator()(const choice &c1, const choice &c2) const
    {
      if(c1.get_type() < c2.get_type())
	return true;
      else if(c2.get_type() < c1.get_type())
	return false;
      else
	switch(c1.get_type())
	  {
	  case choice::install_version:
	    return c1.get_ver() < c2.get_ver();

	  default:
	    return c1 < c2;
	  }
    }
  };

  typedef std::map<choice, std::vector<joint_score>, compare_choices_by_action> joint_score_set;

  /** How much to reward long and/or broken solutions.  Typically
   *  negative to penalize such things, or 0 to ignore them.
   */
  int step_score, broken_score, unfixed_soft_score;


  /** How much to reward real solutions -- solutions that fix all
   *  dependencies.  Make this big to immediately pick them up, or
   *  small to ignore "bad" solutions (at the risk of running out of
   *  time if no better solution pops up).
   */
  int full_solution_score;

  /** The scores to apply to individual package versions.
   */
  int *version_scores;

private:
  /** \brief Scores that apply to simultaneous collections of choices.
   *
   *  \todo This should use smarter indexing, I think.
   */
  joint_score_set joint_scores;

  /** \brief The initial state of the resolver.
   *
   *  This is currently assumed to be the same for all solutions in
   *  a given resolver run (i.e., operator== and friends don't check
   *  it); if it's not, you have some serious weirdness.  The
   *  pointer goes away after the resolver is done with its
   *  computations, but nothing else should use it anyway.
   *
   *  We need this mainly so that version_of works properly with no
   *  extra arguments.  (maybe instead I should just accept having
   *  to wrap it all the time?)
   */
  const resolver_initial_state<PackageUniverse> initial_state;

  /** \brief A list of the joint scores added to this
   *  set of weights, in order.
   *
   *  Each entry is a pair containing the versions that are affected
   *  and the score to add.
   */
  std::vector<std::pair<imm::set<version>, int> > joint_scores_list;

public:
  solution_weights(int _step_score, int _broken_score,
		   int _unfixed_soft_score, int _full_solution_score,
		   unsigned long num_versions,
		   const resolver_initial_state<PackageUniverse> &_initial_state)
    :step_score(_step_score), broken_score(_broken_score),
     unfixed_soft_score(_unfixed_soft_score),
     full_solution_score(_full_solution_score),
     version_scores(new int[num_versions]),
     initial_state(_initial_state)
  {
    for(unsigned long i = 0; i < num_versions; ++i)
      version_scores[i] = 0;
  }

  ~solution_weights()
  {
    delete[] version_scores;
  }

private:
  class build_joint_score_choice_set
  {
    choice_set &output;
    const resolver_initial_state<PackageUniverse> &initial_state;
    bool &any_is_current;
  public:
    build_joint_score_choice_set(choice_set &_output,
				 bool &_any_is_current,
				 const resolver_initial_state<PackageUniverse> &_initial_state)
      : output(_output),
	initial_state(_initial_state),
	any_is_current(_any_is_current)
    {
    }

    bool operator()(const version &version) const
    {
      if(version == initial_state.version_of(version.get_package()))
	any_is_current = true;

      output.insert_or_narrow(choice::make_install_version(version, 0));

      return true;
    }
  };

  class add_to_joint_scores
  {
    typedef typename solution_weights<PackageUniverse>::joint_score joint_score;
    typedef typename solution_weights<PackageUniverse>::joint_score_set joint_score_set;

    joint_score_set &s;
    joint_score score;
  public:
    add_to_joint_scores(joint_score_set &_s, const joint_score &_score)
      : s(_s), score(_score)
    {
    }

    bool operator()(const choice &c) const
    {
      const typename joint_score_set::iterator found = s.find(c);

      if(found == s.end())
	s[c].push_back(score);
      else
	found->second.push_back(score);

      return true;
    }
  };

public:
  void add_joint_score(const imm::set<version> &versions, int score)
  {
    choice_set choices;
    bool any_is_current = false;
    versions.for_each(build_joint_score_choice_set(choices,
						   any_is_current,
						   initial_state));

    if(any_is_current)
      return;

    joint_scores_list.push_back(std::make_pair(versions, score));

    choices.for_each(add_to_joint_scores(joint_scores,
					 typename solution_weights<PackageUniverse>::joint_score(choices, score)));
  }

  const joint_score_set &get_joint_scores() const { return joint_scores; }
  const std::vector<std::pair<imm::set<version>, int> > &
  get_joint_scores_list() const { return joint_scores_list; }
};

#endif // SOLUTION_H
