// problemresolver.h                  -*-c++-*-
//
//   Copyright (C) 2005, 2007-2010 Daniel Burrows
//
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.  You should have
//   received a copy of the GNU General Public License along with this
//   program; see the file COPYING.  If not, write to the Free
//   Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
//   MA 02111-1307, USA.

//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License as
//  published by the Free Software Foundation; either version 2 of the
//  License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; see the file COPYING.  If not, write to
//  the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//  Boston, MA 02111-1307, USA.
//
//

#ifndef PROBLEMRESOLVER_H
#define PROBLEMRESOLVER_H

#include <algorithm>
#include <deque>
#include <map>
#include <queue>
#include <set>
#include <vector>

#include <iostream>
#include <sstream>

#include <limits.h>

#include "choice.h"
#include "choice_set.h"
#include "dump_universe.h"
#include "exceptions.h"
#include "incremental_expression.h"
#include "promotion_set.h"
#include "solution.h"
#include "resolver_undo.h"
#include "search_graph.h"
#include "cost.h"
#include "cost_limits.h"

#include <cwidget/generic/threads/threads.h>
#include <cwidget/generic/util/eassert.h>
#include <cwidget/curses++.h>

#include <generic/util/dense_setset.h>
#include <generic/util/maybe.h>

#include <boost/flyweight.hpp>
#include <boost/make_shared.hpp>
#include <boost/unordered_set.hpp>

/** \brief Generic problem resolver
 *
 * 
 *  Generic problem resolver (generic because I want to be able to do
 *  regression-testing for once, if I can figure out how, and Packages
 *  files make lousy regression-tests).
 * 
 *  \file problemresolver.h
 */

template<typename Obj1, typename Obj2, typename Uni>
inline void eassert_fail_on_2objs_soln(const std::string &file,
				       size_t line,
				       const std::string &func,
				       const std::string &exp,
				       const Obj1 &obj1,
				       const char *obj1name,
				       const Obj2 &obj2,
				       const char *obj2name,
				       const generic_solution<Uni> &soln)
{
  std::ostringstream out;
  out << "In the context ";
  soln.dump(out, true);
  out << " with " << obj1name << "=" << obj1;
  out << " and " << obj2name << "=" << obj2;
  throw cwidget::util::AssertionFailure(file, line, func, exp, out.str());
}

template<typename Obj1, typename Obj2>
inline void eassert_fail_on_2objs(const std::string &file,
				  size_t line,
				  const std::string &func,
				  const std::string &exp,
				  const Obj1 &obj1,
				  const char *obj1name,
				  const Obj2 &obj2,
				  const char *obj2name)
{
  std::ostringstream out;
  out << "With " << obj1name << "=" << obj1;
  out << " and " << obj2name << "=" << obj2;
  throw cwidget::util::AssertionFailure(file, line, func, exp, out.str());
}

template<typename Obj, typename Uni>
inline void eassert_fail_on_soln_obj(const std::string &file,
				     size_t line,
				     const std::string &func,
				     const std::string &exp,
				     const generic_solution<Uni> &soln,
				     const char *objtype,
				     const char *objname,
				     const Obj &o)
{
  std::ostringstream out;
  out << "In context ";
  soln.dump(out, true);
  out << " on " << objtype << " " << objname << "=";
  out << o;
  throw cwidget::util::AssertionFailure(file, line, func, exp, out.str());
}

template<typename Uni>
inline void eassert_fail_on_soln(const std::string &file,
				 size_t line,
				 const std::string &func,
				 const std::string &exp,
				 const generic_solution<Uni> &soln)
{
  std::ostringstream out;
  out << "In context ";
  soln.dump(out, true);
  throw cwidget::util::AssertionFailure(file, line, func, exp, out.str());
}

#define eassert_on_2objs_soln(invariant, obj1, obj2, soln) \
  do { if(!(invariant)) \
         eassert_fail_on_2objs_soln(__FILE__, __LINE__, __PRETTY_FUNCTION__, #invariant, obj1, #obj1, obj2, #obj2, soln); \
     } while(0)

#define eassert_on_2objs(invariant, obj1, obj2) \
  do { if(!(invariant)) \
         eassert_fail_on_2objs(__FILE__, __LINE__, __PRETTY_FUNCTION__, #invariant, obj1, #obj1, obj2, #obj2); \
     } while(0)

#define eassert_on_obj(invariant, soln, objtype, obj) \
  do { if(!(invariant)) \
         eassert_fail_on_soln_obj(__FILE__, __LINE__, __PRETTY_FUNCTION__, #invariant, soln, objtype, #obj, obj); \
     } while(0)

#define eassert_on_dep(invariant, soln, dep) \
  eassert_on_obj(invariant, soln, "dependency", dep)

#define eassert_on_pkg(invariant, soln, pkg) \
  eassert_on_obj(invariant, soln, "package", pkg)

#define eassert_on_ver(invariant, soln, ver) \
  eassert_on_obj(invariant, soln, "version", ver)

#define eassert_on_soln(invariant, soln) \
  do { if(!(invariant)) \
         eassert_fail_on_soln(__FILE__, __LINE__, __PRETTY_FUNCTION__, #invariant, soln); \
     } while(0)

/** A dummy iterator that's always an "end" iterator. */
template<class V>
struct dummy_end_iterator
{
public:
  /** \return \b true */
  static bool end() {return true;}
};

/** Aborts: this iterator is always invalid. */
template<class V>
static inline const V &operator*(const dummy_end_iterator<V>&)
{
  abort();
}

/** Aborts. */
template<class V>
static inline dummy_end_iterator<V> operator++(dummy_end_iterator<V>&)
{
  abort();
}

/** \defgroup problemresolver Aptitude's problem resolver
 *
 *  \section Overview
 *
 *  This is a replacement for the standard apt problem resolver.  It
 *  uses a different algorithm which, while it is less efficient,
 *  allows the program to have more direct control over the results,
 *  allows the user to pick and choose from multiple solutions, and
 *  can produce a more user-friendly explanation of its actions.
 *
 *  The problem resolver class is templated on abstract packages.
 *  Normally this will be the system of apt packages described by
 *  aptitude_universe, but for the purpose of testing the algorithm a
 *  simpler package system, the dummy_universe, is used.
 *
 *  Each package version has an associated "score" which indicates,
 *  essentially, how hard the problem resolver will try to keep it on
 *  the system (or, for negative scores, to remove it from the
 *  system).  The sum of all installed package's scores is used as a
 *  heuristic in a directed search of the solution space.
 *
 *  \sa \subpage abstract_universe, generic_problem_resolver
 *
 *  \page abstract_universe The abstract package universe interface
 *
 *  The package universe interface consists of the following
 *  interrelated Concepts (see the <A
 *  HREF="http://www.sgi.com/tech/stl/stl_introduction.html">STL
 *  documentation</A> for a definition of what a Concept is):
 *
 *  - \subpage universe_universe
 *  - \subpage universe_package
 *  - \subpage universe_version
 *  - \subpage universe_dep
 *  - \subpage universe_installation
 *
 *  Note that in order to allow APT structures to be wrapped with
 *  minimal overhead, all iterators in this section are "APT-style"
 *  iterators: instead of calculating container bounds by invoking an
 *  "end" method on the container, each iterator has a predicate
 *  method end() which returns \b true if the iterator is an "end"
 *  iterator.
 *
 *  \sa \ref problemresolver
 *
 *  \page universe_universe Universe concept
 *
 *  The package universe is the base type representing a domain of
 *  package relationships.  It contains classes representing the
 *  various objects in the domain, along with methods to retrieve
 *  information about the number of entities in the universe and to
 *  iterate over global lists of entities.
 *
 *  aptitude_universe and dummy_universe are models of the generic
 *  universe concept.
 *
 *  \sa \ref universe_package, \ref universe_version, \ref universe_dep
 *
 *  A class modelling the Universe concept should provide the
 *  following members:
 *
 *  - <b>package</b>: a model of \ref universe_package "the Package concept".
 *
 *  - <b>version</b>: a model of \ref universe_version "the Version concept".
 *
 *  - <b>dep</b>: a model of \ref universe_dep "the Dependency concept".
 *
 *  - <b>package_iterator</b>: an iterator over the list of all the
 *  \ref universe_package "package"s in this universe.
 *
 *  - <b>dep_iterator</b>: an iterator over the list of all the \ref
 *  universe_dep "dependencies" in this universe.
 *
 *  - <b>get_package_count()</b>: returns the number of \ref
 *  universe_package "package"s in the universe.
 *
 *  - <b>get_version_count()</b>: returns the number of \ref
 *  universe_version "version"s in the universe.
 *
 *  - <b>packages_begin()</b>: returns a <b>package_iterator</b>
 *  pointing at the first \ref universe_package "package" (in an
 *  arbitrary ordering) in the universe.
 *
 *  - <b>deps_begin()</b>: returns a \b dep_iterator pointing at the
 *  first \ref universe_dep "dependency" (in an arbitrary ordering) in
 *  the universe.
 *
 *  - <b>bool is_candidate_for_initial_set(const dep &)</b>: returns
 *    \b true if the dependency should be in the set of dependencies
 *    the resolver initially sets out to solve.  Dependencies for
 *    which this returns "false" will not be deliberately solved
 *    (although they might be coincidentally solved by other changes
 *    the resolver makes).  This is a member of the universe and not
 *    the dependency concept because I forsee the program possibly
 *    wanting to create universes with different behavior in this
 *    regard at some point in the future.
 *
 *
 *  \page universe_package Package concept
 *
 *  A package is simply a unique collection of \ref universe_version
 *  "versions".  No two packages in the same universe may share
 *  versions, and \e exactly one version of a package is installed at
 *  any given time.  A package has a "currently installed version",
 *  which is the version that should be included in the starting point
 *  of a solution search.
 *
 *  \sa \ref universe_universe, \ref universe_version, \ref universe_dep
 *
 *  A class modelling the Package concept should provide the following
 *  members:
 *
 *  - <b>version_iterator</b>: an iterator over the list of \ref
 *  universe_version "versions" of a package.
 *
 *  - <b>get_name()</b>: returns a string that uniquely names this
 *  package.  This may be used for debugging output or when dumping a
 *  portable representation of a dependency problem.
 *
 *  - <b>get_id()</b>: returns an integer between 0 and
 *  U.get_package_count()-1 (where U is the \ref universe_universe
 *  "universe" to which the package belongs) that uniquely identifies
 *  this package in U.
 *
 *  - <b>bool operator==(package)</b>, <b>operator!=(package)</b>:
 *  compare packages by identity.  Two package objects compare equal
 *  if and only if they represent the same package.
 *
 *  - <b>bool operator<(package)</b>: an arbitrary total ordering on
 *  packages.  This should be appropriate for, eg, placing packages
 *  into a balanced tree structure.
 *
 *  - \anchor universe_package_current_version
 *  <b>current_version()</b>: returns the "currently installed \ref
 *  universe_version "version"" of the package.  For instance, the
 *  apt_universe class considers the InstVersion of a package to be
 *  its "current version".
 *
 *  - <b>versions_begin()</b>: returns a version_iterator pointing to
 *  the head of the list of versions of this package, provided in an
 *  arbitrary order.
 *
 *  \page universe_version Version concept
 *
 *  A version is simply a particular variant of a package that may be
 *  installed.  When the abstract package system is modelling a
 *  concrete universe (such as the APT universe), versions typically
 *  correspond either to a real version of the package, or to the
 *  package's removal.
 *
 *  Each version contains a link to its parent package, as well as
 *  lists of forward and reverse dependencies.
 *
 *  \sa \ref universe_universe, \ref universe_package, \ref universe_dep
 *
 *  A class modelling the Version concept should provide the following
 *  members:
 *
 *  - <b>dep_iterator</b>: an iterator class for forward dependencies.
 *
 *  - <b>revdep_iterator</b>: an iterator class for reverse dependencies.
 *
 *  - <b>get_name()</b>: returns a string that uniquely identifies
 *  this version among all the versions of the same package.
 *
 *  - <b>get_id()</b>: returns a number between 0 and
 *  U.get_version_count()-1 (where U is the \ref universe_universe
 *  "universe" to which this version belongs) uniquely identifying
 *  this version.
 *
 *  - <b>package get_package()</b>: returns the \ref universe_package
 *  "package" of which this is a version.
 *
 *  - <b>dep_iterator deps_begin()</b>: returns a \b dep_iterator
 *  pointing to the first \ref universe_dep "dependency" in the list
 *  of forward dependencies.
 *
 *  - <b>revdep_iterator revdeps_begin()</b>: returns a \b
 *  revdep_iterator pointing to the first \ref universe_dep
 *  "dependency" in the list of reverse dependencies.
 *
 *    \note Although it would be straightforward to define the reverse
 *    dependencies of a version as the set of dependencies that
 *    impinge on that version, they are \e not defined in this manner.
 *    For technical reasons and in order to keep the wrapper to the
 *    APT package system thin, the reverse dependencies are only
 *    required to obey the following rule: if \e v1 and \e v2 are
 *    versions of the same \ref universe_package "package", then for
 *    any \ref universe_dep "dependency" \e d such that \e v1 is a
 *    target of \e d and \e v2 is not, or vice versa, \e d appears in
 *    \e either the reverse dependency list of \e v1 or the reverse
 *    dependency list of \e v2.
 *
 *  - <b>operator==(version)</b>, <b>operator!=(version)</b>:
 *  compare versions by identity.  Two version objects compare equal
 *  if and only if they represent the same version of the same
 *  package.
 *
 *  - <b>operator<(version)</b>: an arbitrary total ordering on
 *  versions.  This should be appropriate for, eg, placing versions
 *  into a balanced tree structure.
 *
 *  \page universe_dep Dependency concept
 *
 *  A dependency indicates that if a particular \ref universe_version
 *  "version" is installed, at least one member of a set of \ref
 *  universe_version "version"s must also be installed.  The first
 *  \ref universe_version "version" is the "source" of the dependency,
 *  while the remaining versions are "solvers" of the dependency.  A
 *  dependency may be "soft", indicating that it is legal (but
 *  undesirable) for the dependency to remain broken at the end of a
 *  solution search.
 *
 *  \todo "solvers" should be renamed to "targets", as a dependency
 *  can also be resolved by removing its source.
 *
 *  \sa \ref universe_universe, \ref universe_package, \ref
 *  universe_version, \ref universe_installation
 *
 *  A class modelling the Dependency concept should provide the
 *  following members:
 *
 *  - <b>solver_iterator</b>: an iterator over the versions that are
 *  targets of this dependency.
 *
 *  - <b>version get_source()</b>: returns the source \ref
 *  universe_version "version" of this dependency.
 *
 *  - <b>solver_iterator solvers_begin()</b>: returns a
 *  solver_iterator pointing to the first target of this dependency.
 *
 *  - <b>bool is_soft()</b>: returns \b true if the dependency is "soft".
 *  For instance, in the Debian package system, a Recommends
 *  relationship is considered to be a soft dependency.
 *
 *  - <b>bool solved_by(version)</b>: return \b true if the given \ref
 *  universe_version "version" solves this dependency, either by
 *  removing the source of the dependency or by installing one of its
 *  targets.
 *
 *  - <b>template &lt;typename Installation&gt; bool broken_under(Installation)</b>: return \b true if this dependency
 *  is broken by the given \ref universe_installation "installation";
 *  a solution breaks a dependency if and only if it installs the
 *  dependency's source and none of its targets.
 *
 *  - <b>operator==(dependency)</b>, <b>operator!=(dependency)</b>:
 *  compare dependencies by identity.  Two dependency objects compare
 *  equal if and only if they represent the same dependency.
 *  Duplicated dependencies may compare as distinct entities.
 *
 *  - <b>operator<(dependency)</b>: an arbitrary total ordering on
 *  dependencies.  This should be appropriate for, eg, placing
 *  dependencies into a balanced tree structure.
 *
 *  \page universe_installation Installation concept
 *
 *  An Installation represents a potential state of an abstract
 *  dependency system; that is, a set of installed versions (or
 *  rather, a total function from packages to versions).
 *
 *  The generic_solution class is a model of the Installation concept.
 *
 *  \sa \ref abstract_universe
 *
 *  A class modelling the Installation concept should provide the
 *  following members:
 *
 *  - <b>version version_of(package)</b>: look up the currently
 *  installed \ref universe_version "version" of the given \ref
 *  universe_package "package" according to this installation.
 */

/** \brief A generic package problem resolver.
 *
 *  \param PackageUniverse A model of the \ref universe_universe
 *  Universe concept.
 *
 *  Searches from the starting node on a best-first basis; i.e., it
 *  repeatedly pulls the "best" node off its work queue, returns it if
 *  it is a solution, and enqueues its successors if not.  The
 *  successor nodes to a given search node are generated by selecting
 *  a single dependency that is broken at that node and enqueuing all
 *  possibly ways of fixing it.  The score of a node is affected by:
 *
 *  - A penalty/bonus applied to each package version.
 *
 *  - A bonus for each step of the solution (used to discourage the
 *  resolver from "backing up" unnecessarily).
 *
 *  - A penalty is added for each broken dependency that has not yet
 *  been processed.  This aims at directing the search towards "less
 *  broken" situations.
 *
 *  - A penalty for soft dependencies (i.e., Recommends) which were
 *  processed and left broken.
 *
 *  - A bonus for nodes that have no unprocessed broken dependencies.
 *
 *  Note that these are simply the default biases set by aptitude; any
 *  of these scores may be changed at will (including changing a
 *  penalty to a bonus or vice versa!).
 *
 *  \sa \ref problemresolver, \ref universe_universe
 */
template<class PackageUniverse>
class generic_problem_resolver : public promotion_set_callbacks<PackageUniverse>
{
public:
  friend class ResolverTest;

  typedef typename PackageUniverse::package package;
  typedef typename PackageUniverse::version version;
  typedef typename PackageUniverse::dep dep;

  typedef generic_solution<PackageUniverse> solution;
  typedef generic_choice<PackageUniverse> choice;
  typedef generic_choice_set<PackageUniverse> choice_set;
  typedef generic_promotion<PackageUniverse> promotion;
  typedef generic_promotion_set<PackageUniverse> promotion_set;
  typedef generic_search_graph<PackageUniverse> search_graph;
  typedef generic_compare_choices_by_effects<PackageUniverse> compare_choices_by_effects;
  typedef generic_promotion_queue_entry<PackageUniverse> promotion_queue_entry;

  typedef typename search_graph::step step;

  /** Information about the sizes of the various resolver queues. */
  struct queue_counts
  {
    size_t open;
    size_t closed;
    size_t deferred;
    size_t conflicts;
    /** \brief The number of deferred packages that are not at
     *  defer_cost.
     */
    size_t promotions;

    /** \b true if the resolver has finished searching for solutions.
     *  If open is empty, this member distinguishes between the start
     *  and the end of a search.
     */
    bool finished;

    /** \brief The search cost of the next solution to consider.
     */
    cost current_cost;

    queue_counts()
      : open(0), closed(0), deferred(0), conflicts(0), promotions(0),
	finished(false),
	current_cost(cost_limits::minimum_cost)
    {
    }
  };

private:
  logging::LoggerPtr logger;
  bool debug;

  search_graph graph;

  /** Hash function for packages: */
  struct ExtractPackageId
  {
  public:
    size_t operator()(const package &p) const
    {
      return p.get_id();
    }
  };

  typedef ExtractPackageId PackageHash;

  /** Compares steps according to their "goodness": their effective
   *  cost, then thier score, then their contents.
   *
   *  The comparisons are reversed, so better solutions compare
   *  "below" worse ones.
   */
  struct step_goodness_compare
  {
    const search_graph &graph;

    step_goodness_compare(const search_graph &_graph)
      : graph(_graph)
    {
    }

    bool operator()(int step_num1, int step_num2) const
    {
      // Optimization: a step always equals itself.
      if(step_num1 == step_num2)
	return false;

      const step &step1(graph.get_step(step_num1));
      const step &step2(graph.get_step(step_num2));

      // Note that *lower* costs come "before" higher costs, hence the
      // reversed comparison there.
      int cost_cmp = step1.final_step_cost.compare(step2.final_step_cost);
      if(cost_cmp != 0)
	return cost_cmp < 0;
      else if(step2.score < step1.score)
	return true;
      else if(step1.score < step2.score)
	return false;
      else
	return step2.actions < step1.actions;
    }
  };

  /** \brief Represents the "essential" information about a step.
   *
   *  This information consists of the step's scores and its actions.
   *  Since these values never change over the course of a search and
   *  are unique to a step, they can be used to form an index of steps
   *  to avoid duplicates.  Also, since the scores are "mostly"
   *  unique, they can be used as an up-front "hash" of the step, to
   *  avoid an expensive set comparison operation.
   *
   *  Note that we don't try to pre-detect duplicates, because that
   *  would be a lot more complicated and it's not clear that it's
   *  worth the trouble (plus, it's not clear what should happen if
   *  all the children of a step are duplicates; it should be thrown
   *  out, but not given a conflict cost!).
   */
  class step_contents
  {
    int score;
    int action_score;
    choice_set actions;
    std::size_t hash;

    struct combine_hashes
    {
      std::size_t &hash;

      combine_hashes(std::size_t &_hash)
	: hash(_hash)
      {
      }

      bool operator()(const choice &c) const
      {
	boost::hash_combine(hash, c);
	return true;
      }
    };

    void init_hash()
    {
      hash = 0;
      boost::hash_combine(hash, score);
      boost::hash_combine(hash, action_score);
      actions.for_each(combine_hashes(hash));
    }

  public:
    step_contents()
      : score(0), action_score(0), actions()
    {
      init_hash();
    }

    step_contents(int _score, int _action_score,
		  const choice_set &_actions)
      : score(_score), action_score(_action_score), actions(_actions)
    {
      init_hash();
    }

    step_contents(const step &s)
      : score(s.score), action_score(s.action_score),
	actions(s.actions)
    {
      init_hash();
    }

    std::size_t get_hash() const
    {
      return hash;
    }

    bool operator==(const step_contents &other) const
    {
      if(score != other.score)
	return false;
      else if(action_score != other.action_score)
	return false;

      // Speed hack: order by size first to avoid traversing the whole
      // tree.
      if(actions.size() != other.actions.size())
	return false;
      else
	return actions == other.actions;
    }
  };

  class hash_step_contents
  {
  public:
    std::size_t operator()(const step_contents &contents) const
    {
      return contents.get_hash();
    }
  };

  class instance_tracker;
  friend class instance_tracker;
  class instance_tracker
  {
    generic_problem_resolver &r;
  public:
    instance_tracker(generic_problem_resolver &_r)
      :r(_r)
    {
      cwidget::threads::mutex::lock l(r.execution_mutex);
      if(r.solver_executing)
	throw DoubleRunException();
      else
	r.solver_executing = true;
    }

    ~instance_tracker()
    {
      cwidget::threads::mutex::lock l(r.execution_mutex);
      eassert(r.solver_executing);

      r.solver_executing = false;
      r.solver_cancelled = false;
    }
  };

  /** \brief Used to convert a choice set into a model of Installation. */
  class choice_set_installation
  {
    const choice_set &actions;
    const resolver_initial_state<PackageUniverse> &initial_state;

  public:
    choice_set_installation(const choice_set &_actions,
			    const resolver_initial_state<PackageUniverse> &_initial_state)
      : actions(_actions),
	initial_state(_initial_state)
    {
    }

    version version_of(const package &p) const
    {
      version rval;
      if(actions.get_version_of(p, rval))
	return rval;
      else
	return initial_state.version_of(p);
    }
  };

  /** \brief The initial state of the resolver.
   *
   *  If this is not NULL, we need to use a more clever technique to
   *  get all the broken deps.  Can we get away with just iterating
   *  over all deps and calling broken_under()?
   */
  resolver_initial_state<PackageUniverse> initial_state;

  // Information regarding the weight given to various parameters;
  // packaged up in a struct so it can be easily used by the solution
  // constructors.
  solution_weights<PackageUniverse> weights;

  /** \brief The cost applied for each soft dependency that's left
   *  unresolved.
   */
  cost unfixed_soft_cost;

  /** Solutions whose score is smaller than this value will be
   *  discarded rather than being enqueued.
   */
  int minimum_score;

  /** The number of "future" steps to examine after we find a solution
   *  in order to find a better one.
   */
  int future_horizon;

  /** The universe in which we are solving problems. */
  const PackageUniverse universe;

  /** If \b true, we have exhausted the list of solutions. */
  bool finished:1;


  // Multithreading support variables.
  //
  //  These variables ensure (as a sanity-check) that only one thread
  //  executes the solver function at once, and allow the executing
  //  instance to be cleanly terminated.  They are managed by the
  //  instance_tracker class (see above).

  /** If \b true, a thread is currently executing in the solver. */
  bool solver_executing : 1;

  /** If \b true, the currently executing thread should stop at the
   *  next opportunity.
   */
  bool solver_cancelled : 1;

  /** Mutex guarding the solver_executing and stop_solver variables.
   *
   *  If a routine wants to execute some code conditionally based on
   *  whether the resolver is currently executing, it should grab this
   *  mutex, test solver_executing, and run the code if
   *  solver_executing is \b false.
   */
  cwidget::threads::mutex execution_mutex;



  queue_counts counts;

  /** Mutex guarding the cache of resolver status information. */
  cwidget::threads::mutex counts_mutex;



  /** All the steps that have not yet been processed.
   *
   *  Steps are sorted by cost, then by score, then by their contents.
   */
  std::set<int, step_goodness_compare> pending;

  /** \brief Counts how many steps are deferred. */
  int num_deferred;

  /** Solutions generated "in the future", stored by reference to
   *  their step numbers.
   *
   *  The main reason this is persistent at the moment is so we don't
   *  lose solutions if find_next_solution() throws an exception.
   */
  std::set<int, step_goodness_compare> pending_future_solutions;

  /** \brief Stores already-seen search nodes that had their
   *  successors generated.
   *
   *  Each search node is mapped to the "canonical" step number that
   *  corresponds to it.  A list of clones is accumulated at that
   *  step, along with a single copy of the promotion set for all the
   *  clones.
   */
  boost::unordered_map<step_contents, int, hash_step_contents> closed;

  /** Stores cost promotions: sets of installations that will force a
   *  solution to a higher cost of the search.
   */
  promotion_set promotions;

  /** \brief Stores the tail of the queue of cost promotions.
   *
   *  Steps hold reference-counted pointers to links in the chain
   *  corresponding to where the tail was when they were inserted.  If
   *  there have not been "very many" insertions since a step was
   *  generated, then instead of checking the global promotion set,
   *  we'll just apply the individual promotions one at a time.  New
   *  promotions will be filled in here.
   */
  boost::shared_ptr<promotion_queue_entry> promotion_queue_tail;

  /** The initial set of broken dependencies.
   */
  imm::set<dep> initial_broken;

  /** The intrinsic cost of each version (indexed by version).
   *
   *  Store here instead of in the weights table because costs are the
   *  resolver's responsibility, not the solution object's (because
   *  they are not a pure function of the contents of the solution; a
   *  solution might get a higher cost if we can prove that it will
   *  eventually have one anyway).
   */
  cost *version_costs;

  /** \brief Used to track whether a single choice is approved or
   *  rejected.
   *
   *  The variables are used as the leaves in a large Boolean
   *  expression tree that is used to efficiently update the deferred
   *  status of steps and solvers.
   */
  class approved_or_rejected_info
  {
    // Like a var_e, but prints a choice when it's dumped.  This is a
    // trade-off of memory for much better debugging.  Having separate
    // classes instead of using a Boolean flag should save a little
    // space at run-time.
    class approve_ver_e : public var_e<bool>
    {
      version v;

      approve_ver_e(bool value, const version &_v)
	: var_e<bool>(value), v(_v)
      {
      }

    public:
      static cwidget::util::ref_ptr<var_e<bool> >
      create(bool value, const version &v)
      {
	return new approve_ver_e(value, v);
      }

      void dump(std::ostream &out)
      {
	out << "A(" << v << ")";
      }
    };

    class reject_ver_e : public var_e<bool>
    {
      version v;

      reject_ver_e(bool value, const version &_v)
	: var_e<bool>(value), v(_v)
      {
      }

    public:
      static cwidget::util::ref_ptr<var_e<bool> >
      create(bool value, const version &v)
      {
	return new reject_ver_e(value, v);
      }

      void dump(std::ostream &out)
      {
	out << "R(" << v << ")";
      }
    };

    class approve_break_e : public var_e<bool>
    {
      dep d;

      approve_break_e(bool value, const dep &_d)
	: var_e<bool>(value), d(_d)
      {
      }

    public:
      static cwidget::util::ref_ptr<var_e<bool> >
      create(bool value, const dep &d)
      {
	return new approve_break_e(value, d);
      }

      void dump(std::ostream &out)
      {
	out << "AB(" << d << ")";
      }
    };

    class reject_break_e : public var_e<bool>
    {
      dep d;

      reject_break_e(bool value, const dep &_d)
	: var_e<bool>(value), d(_d)
      {
      }

    public:
      static cwidget::util::ref_ptr<var_e<bool> >
      create(bool value, const dep &d)
      {
	return new reject_break_e(value, d);
      }

      void dump(std::ostream &out)
      {
	out << "RB(" << d << ")";
      }
    };

    // True if the choice is rejected.
    cwidget::util::ref_ptr<var_e<bool> > rejected;
    // True if the choice is approved.
    cwidget::util::ref_ptr<var_e<bool> > approved;

    approved_or_rejected_info()
      : rejected(var_e<bool>::create(false)),
	approved(var_e<bool>::create(false))
    {
    }

  public:
    approved_or_rejected_info(const version &v, bool supportTrace)
      : rejected(supportTrace ? reject_ver_e::create(false, v) : var_e<bool>::create(false)),
	approved(supportTrace ? approve_ver_e::create(false, v) : var_e<bool>::create(false))
    {
    }

    approved_or_rejected_info(const dep &d, bool supportTrace)
      : rejected(supportTrace ? reject_break_e::create(false, d) : var_e<bool>::create(false)),
	approved(supportTrace ? approve_break_e::create(false, d) : var_e<bool>::create(false))
    {
    }

    const cwidget::util::ref_ptr<var_e<bool> > &get_rejected() const
    {
      return rejected;
    }

    const cwidget::util::ref_ptr<var_e<bool> > &get_approved() const
    {
      return approved;
    }
  };

  /** \brief Stores the approved and rejected status of versions. */
  std::map<version, approved_or_rejected_info> user_approved_or_rejected_versions;

  /** \brief Stores the approved and rejected status of dependencies. */
  std::map<dep, approved_or_rejected_info> user_approved_or_rejected_broken_deps;

  /** \brief Expression class that calls back into the resolver when
   *         the value of its sub-expression changes.
   *
   *  The attached information is the choice that this expression
   *  affects; that choice must have an associated dependency.
   */
  class deferral_updating_expression : public expression_wrapper<bool>
  {
    choice deferred_choice;

    generic_problem_resolver &resolver;

    deferral_updating_expression(const cwidget::util::ref_ptr<expression<bool> > &_child,
				 const choice &_deferred_choice,
				 generic_problem_resolver &_resolver)
      : expression_wrapper<bool>(_child),
	deferred_choice(_deferred_choice),
	resolver(_resolver)
    {
      // Sanity-check.
      if(deferred_choice.get_type() == choice::install_version)
	eassert(deferred_choice.get_has_dep());
    }

  public:
    static cwidget::util::ref_ptr<deferral_updating_expression>
    create(const cwidget::util::ref_ptr<expression<bool> > &child,
	   const choice &deferred_choice,
	   generic_problem_resolver &resolver)
    {
      return new deferral_updating_expression(child,
					      deferred_choice,
					      resolver);
    }

    void changed(bool new_value)
    {
      if(!new_value)
	resolver.deferral_retracted(deferred_choice,
				    deferred_choice.get_dep());
      else
	{
	  LOG_TRACE(resolver.logger, deferred_choice << " is now deferred; marking it as such in all active steps.");
	  // Note that this is not quite right in logical terms.
	  // Technically, the promotion we generate should contain the
	  // choice that led to the deferral.  However, that's not
	  // right either: we don't have a choice object that can
	  // fully describe the reason this deferral occurred.
	  //
	  // However, this isn't actually a problem: the promotion
	  // will be used only to produce a generalized promotion, and
	  // generalization would remove the choice that was deferred
	  // anyway.  So we can just produce an (incorrect) empty
	  // promotion.
	  //
	  // (the alternative is to expand the types of choices we can
	  // handle, but that would impose costs on the rest of the
	  // program for a feature that would never be used)
	  promotion p(choice_set(), cost_limits::defer_cost,
		      get_child());
	  resolver.increase_solver_cost_everywhere_with_dep(deferred_choice, p);
	}
    }
  };

  /** \brief Comparison operator on choices that treats choices which
   *  could have distinct deferral status as different.
   *
   *  In particular, dependencies are always significant, even if the
   *  choice is not a from-dep-source choice.
   */
  struct compare_choices_for_deferral
  {
    bool operator()(const choice &c1, const choice &c2) const
    {
      if(c1.get_type() < c2.get_type())
	return true;
      else if(c2.get_type() < c1.get_type())
	return false;
      else switch(c1.get_type())
	     {
	     case choice::install_version:
	       if(c1.get_from_dep_source() < c2.get_from_dep_source())
		 return true;
	       else if(c2.get_from_dep_source() < c1.get_from_dep_source())
		 return false;
	       else if(c1.get_ver() < c2.get_ver())
		 return true;
	       else if(c2.get_ver() < c1.get_ver())
		 return false;
	       else if(c1.get_has_dep() < c2.get_has_dep())
		 return true;
	       else if(c2.get_has_dep() < c1.get_has_dep())
		 return false;
	       else if(!c1.get_has_dep())
		 return false;
	       else if(c1.get_dep() < c2.get_dep())
		 return true;
	       else
		 return false;

	     case choice::break_soft_dep:
	       return c1.get_dep() < c2.get_dep();

	     default: return false; // Treat all invalid choices as equal.
	     }
    }
  };

  /** \brief Memoizes "is this deferred?" expressions for every choice
   *  in the search.
   *
   *  Each expression is contained in a wrapper whose sole purpose is
   *  to recompute the cost of the corresponding choice in all steps
   *  when it fires.
   *
   *  \sa build_is_deferred, build_is_deferred_real
   */
  std::map<choice, expression_weak_ref<expression_box<bool> >,
	   compare_choices_for_deferral> memoized_is_deferred;

  /** \brief If the given step is already "seen", mark it as a clone
   *  and return true (telling our caller to abort).
   */
  bool is_already_seen(int stepNum)
  {
    step &s(graph.get_step(stepNum));

    typename boost::unordered_map<step_contents, int, hash_step_contents>::const_iterator found =
      closed.find(step_contents(s));
    if(found != closed.end() && found->second != stepNum)
      {
	LOG_TRACE(logger, "Step " << s.step_num << " is irrelevant: it was already encountered in this search.");
	graph.add_clone(found->second, stepNum);
	return true;
      }
    else
      return false;
  }

  void sanity_check_not_deferred(const step &s)
  {
#ifdef ENABLE_RESOLVER_SANITY_CHECKS
    if(logger->isErrorEnabled())
      {
	if(!s.is_deferred_listener.valid())
	  {
	    if(s.step_num != 0)
	      LOG_ERROR(logger, "Step " << s.step_num << " has no deferral listener.");
	  }
	else if(s.is_deferred_listener->get_value())
	  LOG_ERROR(logger, "Step " << s.step_num << " claims not to be deferred, but it is.");

	for(typename choice_set::const_iterator it = s.actions.begin();
	    it != s.actions.end(); ++it)
	  {
	    if(build_is_deferred_listener(*it)->get_value())
	      LOG_ERROR(logger, "Step " << s.step_num << " claims not to be deferred, but it contains the deferred choice " << (*it) << ".");

	    // Manually examine the other ways of solving this.
	    switch(it->get_type())
	      {
	      case choice::install_version:
		if(is_rejected(it->get_ver()))
		  LOG_ERROR(logger, "Step " << s.step_num << " contains the rejected choice " << (*it));

		if(it->get_has_dep())
		  {
		    if(!is_mandatory(it->get_ver()))
		      {
			if(it->get_dep().is_soft() &&
			   is_approved_broken(it->get_dep()))
			  LOG_ERROR(logger, "Step " << s.step_num << " contains the choice " << (*it) << ", but its dependency " << it->get_dep() << " is approved to be broken.");

			for(typename dep::solver_iterator si = it->get_dep().solvers_begin();
			    !si.end(); ++si)
			  {
			    if(*si != it->get_ver() && is_mandatory(*si))
			      LOG_ERROR(logger, "Step " << s.step_num << " contains the choice " << (*it) << ", but the alternative " << (*si) << " is approved.");
			  }

			version source(it->get_dep().get_source());
			for(typename package::version_iterator vi = source.get_package().versions_begin();
			    !vi.end(); ++vi)
			  {
			    if(*vi != it->get_ver() &&
			       *vi != source &&
			       is_mandatory(*vi))
			      LOG_ERROR(logger, "Step " << s.step_num << " contains the choice " << (*it) << ", but the resolution " << (*vi) << " is approved.");
			  }
		      }
		  }
		else
		  LOG_ERROR(logger, "Step " << s.step_num << " contains the choice " << (*it) << " with no declared dependency.");
		break;

	      case choice::break_soft_dep:
		if(is_hardened(it->get_dep()))
		  LOG_ERROR(logger, "Step " << s.step_num << " contains the rejected choice " << (*it));

		if(!is_approved_broken(it->get_dep()))
		  {
		    for(typename dep::solver_iterator si = it->get_dep().solvers_begin();
			!si.end(); ++si)
		      {
			if(is_mandatory(*si))
			  LOG_ERROR(logger, "Step " << s.step_num << " contains the choice " << (*it) << ", but the alternative " << (*si) << " is approved.");
		      }
		  }
		else
		  LOG_ERROR(logger, "Step " << s.step_num << " contains the choice " << (*it) << " with no declared action.");
		break;
	      }
	  }
      }
#endif
  }

  void sanity_check_promotions(const step &s)
  {
#ifdef ENABLE_RESOLVER_SANITY_CHECKS
    boost::unordered_map<choice, promotion>
      global_incipient;
    maybe<promotion>
      non_incipient;
    promotions.find_highest_incipient_promotions(s.actions,
						 s.deps_solved_by_choice,
						 global_incipient,
						 non_incipient);

    if(non_incipient.get_has_value() &&
       s.final_step_cost < non_incipient.get_value().get_cost())
      LOG_ERROR(logger, "In step " << s.step_num << ": the embedded promotion "
		<< non_incipient.get_value() << " was not applied.");
    else
      {
	typename promotion_set::const_iterator found_promotion =
	  promotions.find_highest_promotion_for(s.actions);

	if(found_promotion != promotions.end() &&
	   s.final_step_cost < found_promotion->get_cost())
	  LOG_ERROR(logger, "In step " << s.step_num << ": the embedded promotion "
		    << *found_promotion << " was not applied.");
      }

    typedef generic_solver_information<PackageUniverse> solver_information;
    typedef generic_dep_solvers<PackageUniverse> dep_solvers;


    for(typename imm::map<dep, boost::flyweight<dep_solvers> >::const_iterator it =
	  s.unresolved_deps.begin(); it != s.unresolved_deps.end(); ++it)
      {
	const dep &d(it->first);
	imm::map<choice, solver_information, compare_choices_by_effects>
	  solvers = it->second.get_solvers();


	for(typename imm::map<choice, solver_information, compare_choices_by_effects>::const_iterator
	    it = solvers.begin(); it != solvers.end(); ++it)
	{
	  const choice &solver(it->first);
	  const solver_information &solver_inf(it->second);

	  // Verify that the deps-solved-by-choice list maps to this
	  // dep.
	  imm::list<dep> found_solved_deps;
	  if(!s.deps_solved_by_choice.try_get(solver, found_solved_deps) ||
	     std::find(found_solved_deps.begin(), found_solved_deps.end(), d) == found_solved_deps.end())
	    LOG_ERROR(logger, "In step " << s.step_num << ": no backlink from the choice "
		      << solver << " to the dependency " << d << "; backlinks are " << found_solved_deps << ".");

	  boost::unordered_map<choice, promotion> incipient;
	  promotions.find_highest_incipient_promotions_containing(s.actions,
								  solver,
								  s.deps_solved_by_choice,
								  discards_blessed(s.is_blessed_solution,
										   *this,
										   s),
								  incipient);

	  typename boost::unordered_map<choice, promotion>::const_iterator
	    found_incipient = incipient.find(solver);
	  if(found_incipient != incipient.end() &&
	     solver_inf.get_cost() < found_incipient->second.get_cost())
	    LOG_ERROR(logger, "In step " << s.step_num
		      << ": incipient promotion " << found_incipient->second
		      << " was never applied to " << solver << ".");

	  choice_set test_set(s.actions);
	  test_set.insert_or_narrow(solver);
	  typename promotion_set::const_iterator found_promotion =
	    promotions.find_highest_promotion_containing(test_set, solver);
	  if(found_promotion != promotions.end() &&
	     solver_inf.get_cost() < found_promotion->get_cost())
	    LOG_ERROR(logger, "In step " << s.step_num
		      << ": the incipient promotion " << *found_promotion
		      << " was not detected as incipient for " << solver);
	}
      }
#endif
  }

  /** \return \b true if the given cost will always cause any step to
   *  be deferred.
   *
   *  Note that costs which would cause a step to be discarded aren't
   *  considered to be defer costs.
   */
  bool is_defer_cost(const cost &cost) const
  {
    return
      cost.get_structural_level() == cost_limits::defer_structural_level &&
      !is_discard_cost(cost);
  }

  /** \return \b true if the given cost will always cause any step to
   *  be discarded.
   *
   *  \param op the operation to test.
   */
  bool is_discard_cost(const cost &cost) const
  {
    return
      cost.get_structural_level() >= cost_limits::already_generated_structural_level;
  }

  /** \return \b true if the given step is "irrelevant": that is,
   *  either it was already generated and placed in the closed queue,
   *  or it was marked as having a conflict, or it is infinitely
   *  "bad".
   */
  bool irrelevant(const step &s)
  {
    const cost &s_cost = s.final_step_cost;
    if(is_discard_cost(s_cost))
      {
	LOG_TRACE(logger, "Step " << s.step_num << " is irrelevant: its cost " << s_cost << " indicates it should be discarded.");
	return true;
      }

    if(s.score < minimum_score)
      {
	LOG_TRACE(logger, "Step " << s.step_num << " is irrelevant: it has infinite badness " << s.score << "<" << minimum_score);
	return true;
      }

    sanity_check_not_deferred(s);

    return false;
  }

  /** \brief Adjust the cost of a step, keeping everything consistent. */
  void set_base_step_cost(int step_num,
                          const cost &c)
  {
    step &s(graph.get_step(step_num));

    s.base_step_cost = c;

    update_final_step_cost(step_num);
  }

  /** \brief Update the operation that computes the *effective* cost
   *  of a step.
   *
   *  Updates the effective cost itself as a side effect.
   */
  void set_effective_step_cost(int step_num,
                               const cost &c)
  {
    step &s(graph.get_step(step_num));

    s.effective_step_cost = c;

    update_final_step_cost(step_num);
  }

  /** \brief Update the effective cost of a step.
   *
   *  This should only be invoked by set_base_step_cost and
   *  set_effective_step_cost.  It recomputes the effective cost
   *  from its components and performs any follow-up work that's
   *  needed to enforce consistency.
   */
  void update_final_step_cost(int step_num)
  {
    step &s(graph.get_step(step_num));

    cost new_final_step_cost =
      s.effective_step_cost + s.base_step_cost;

    if(s.final_step_cost == new_final_step_cost)
      return;

    if(s.is_blessed_solution && is_discard_cost(new_final_step_cost))
      {
	LOG_TRACE(logger, "Step " << s.step_num
		  << " is a blessed solution; ignoring the attempt to promote it to cost " << new_final_step_cost);
	return;
      }

    LOG_TRACE(logger, "Setting the final cost of step " << step_num
	      << " to " << new_final_step_cost);

    bool was_in_pending =  (pending.erase(step_num) > 0);
    bool was_in_pending_future_solutions =  (pending_future_solutions.erase(step_num) > 0);


    if(is_defer_cost(s.final_step_cost))
      {
	if(was_in_pending)
	  --num_deferred;
	if(was_in_pending_future_solutions)
	  --num_deferred;
      }

    s.final_step_cost = new_final_step_cost;


    if(was_in_pending)
      pending.insert(step_num);
    if(was_in_pending_future_solutions)
      pending_future_solutions.insert(step_num);


    if(is_defer_cost(s.final_step_cost))
      {
	if(was_in_pending)
	  ++num_deferred;
	if(was_in_pending_future_solutions)
	  ++num_deferred;
      }
    else if(!is_discard_cost(s.final_step_cost))
      {
	// Clear the "finished" flag if this is now a pending
	// candidate.
	if(finished &&
	   (was_in_pending || was_in_pending_future_solutions))
	  {
	    LOG_TRACE(logger, "Step " << s.step_num << " is now a candidate: clearing the \"finished\" flag.");
	    finished = false;
	  }
      }
  }

  /** \brief Add a promotion to the global set of promotions.
   *
   *  This routine handles all the book-keeping that needs to take
   *  place.
   */
  void add_promotion(const promotion &p)
  {
    if(p.get_choices().size() == 0)
      LOG_TRACE(logger, "Ignoring the empty promotion " << p);
    else if(promotions.insert(p) != promotions.end())
      {
	LOG_TRACE(logger, "Added the promotion " << p
		  << " to the global promotion set.");

	promotion_queue_tail->set_promotion(p);
	eassert(promotion_queue_tail->get_has_contents());
	promotion_queue_tail = promotion_queue_tail->get_next();

	LOG_TRACE(logger, "The promotion queue now contains "
		  << promotion_queue_tail->get_index() << " promotions with "
		  << promotion_queue_tail->get_action_sum() << " total actions.");
      }
    else
      LOG_TRACE(logger, "Did not add " << p
		<< " to the global promotion set: it was redundant with an existing promotion.");
  }

  // Used as a callback by subroutines that want to add a promotion to
  // the global set.
  class promotion_adder
  {
    generic_problem_resolver &resolver;

  public:
    promotion_adder(generic_problem_resolver &_resolver)
      : resolver(_resolver)
    {
    }

    void operator()(int step_num, const promotion &p) const
    {
      resolver.add_promotion(step_num, p);
    }
  };

  /** \brief Add a promotion to the global set of promotions
   *  for a particular step.
   *
   *  This routine handles all the book-keeping that needs to take
   *  place: it adds the promotion to the global set, adds it to the
   *  list of promotions that need to be tested against all existing
   *  steps, and also attaches it to the step graph.
   */
  void add_promotion(int step_num, const promotion &p)
  {
    add_promotion(p);
    graph.schedule_promotion_propagation(step_num, p);
  }

  // Helper function for find_steps_containing_incipient_promotion.
  // Processes mapping information and updates the counters; when a
  // hit is found, inserts it into an output set.
  //
  // We don't immediately invoke a callback, because that might
  // actually lead to doing the wrong thing.  If several different
  // solvers (say, different from-source-dep solvers that install the
  // same version) hit the same promotion, then applying the promotion
  // immediately when we see the first solver might remove the other
  // ones.  When the caller finishes tracing the tree for the first
  // solver, it'll notice that the root step lacks the other solver
  // and not visit anything.
  class update_incipient_promotion_information
  {
    generic_problem_resolver &resolver;
    const unsigned int promotion_search_index;
    const unsigned int promotion_size;
    const choice &promotion_c;
    std::set<int> &active_hits; // Steps that contain the promotion in
			        // their action set.
    std::set<int> &incipient_hits; // Steps that contain the promotion
  				   // as an incipient promotion.

    void too_many_choices(int choices) const
    {
      LOG_ERROR(resolver.logger, "Internal error: the choice " << promotion_c
		<< " is match number " << choices
		<< " against a promotion of size " << promotion_size);
    }

  public:
    update_incipient_promotion_information(generic_problem_resolver &_resolver,
					   unsigned int _promotion_search_index,
					   unsigned int _promotion_size,
					   const choice &_promotion_c,
					   std::set<int> &_active_hits,
					   std::set<int> &_incipient_hits)
      : resolver(_resolver),
	promotion_search_index(_promotion_search_index),
	promotion_size(_promotion_size),
	promotion_c(_promotion_c),
	active_hits(_active_hits),
	incipient_hits(_incipient_hits)
    {
    }

    bool operator()(const choice &c,
		    const typename search_graph::choice_mapping_type how,
		    int step_num) const
    {
      step &s(resolver.graph.get_step(step_num));
      LOG_TRACE(resolver.logger, "update_incipient_promotion_information: visiting step " << step_num << " for " << c);

      if(s.last_promotion_search != promotion_search_index)
	{
	  s.last_promotion_search = promotion_search_index;
	  s.choice_set_hit_count = 0;
	  s.solver_set_hit_count = 0;
	}
      // Short-circuit if we can't possibly match.
      else if(s.solver_set_hit_count > 1)
	return true;

      switch(how)
	{
	case search_graph::choice_mapping_action:
	  LOG_TRACE(resolver.logger, "Hit step " << step_num << " as an action with " << c);
	  ++s.choice_set_hit_count;
	  if(s.choice_set_hit_count > promotion_size)
	    {
	      too_many_choices(s.choice_set_hit_count);
	      return true;
	    }
	  break;

	case search_graph::choice_mapping_solver:
	  LOG_TRACE(resolver.logger, "Hit step " << step_num << " as a solver with " << c);
	  if(s.solver_set_hit_count == 0)
	    {
	      s.solver_set_hit_count = 1;
	      s.first_solver_hit = promotion_c;
	    }
	  else if(s.first_solver_hit.contains(promotion_c))
	    // This is the same choice we already stored, so do
	    // nothing.
	    return true;
	  else if(promotion_c.contains(s.first_solver_hit))
	    // This is a more general version of the choice we already
	    // stored.
	    {
	      s.first_solver_hit = promotion_c;
	      return true;
	    }
	  else
	    // If we see a new solver, increment the solver hit count
	    // (thus marking this solver as invalid).
	    ++s.solver_set_hit_count;

	  break;
	}

      if(s.solver_set_hit_count <= 1 &&
	 s.choice_set_hit_count + s.solver_set_hit_count == promotion_size)
	{
	  if(s.solver_set_hit_count == 0)
	    active_hits.insert(step_num);
	  else
	    incipient_hits.insert(step_num);
	}

      return true;
    }
  };

  template<typename Callbacks>
  class find_steps_containing_incipient_promotion
  {
    generic_problem_resolver &resolver;
    unsigned int promotion_search_index;
    const promotion &p;
    Callbacks callbacks;

  public:
    find_steps_containing_incipient_promotion(generic_problem_resolver &_resolver,
					      unsigned int _promotion_search_index,
					      const promotion &_p,
					      Callbacks _callbacks)
      : resolver(_resolver),
	promotion_search_index(_promotion_search_index),
	p(_p),
	callbacks(_callbacks)
    {
    }

    bool operator()(const choice &c) const
    {
      std::set<int> active_hits;
      std::set<int> incipient_hits;

      update_incipient_promotion_information
	update_f(resolver,
		 promotion_search_index,
		 p.get_choices().size(),
		 c,
		 active_hits,
		 incipient_hits);

      resolver.graph.for_each_step_related_to_choice(c, update_f);

      for(std::set<int>::const_iterator it = active_hits.begin();
	  it != active_hits.end(); ++it)
	{
	  int step_num(*it);

	  LOG_DEBUG(resolver.logger, "Step " << step_num
		    << " contains " << p
		    << " as an active promotion.");
	  callbacks.active(step_num);
	}

      for(std::set<int>::const_iterator it = incipient_hits.begin();
	  it != incipient_hits.end(); ++it)
	{
	  int step_num(*it);
	  step &s(resolver.graph.get_step(step_num));

	  callbacks.incipient(step_num, s.first_solver_hit);
	}

      return true;
    }
  };

  /** \brief For every step containing the promotion, invoke
   *  callbacks.incipient(step_num, solver) if the promotion is
   *  contained as an incipient promotion, and
   *  callbacks.active(step_num) if it's contained in the action set.
   */
  template<typename Callbacks>
  void for_each_promotion_hit(const promotion &p, Callbacks callbacks)
  {
    find_steps_containing_incipient_promotion<Callbacks>
      find_promotion_f(*this,
		       graph.get_and_increment_promotion_search_index(),
		       p,
		       callbacks);
    p.get_choices().for_each(find_promotion_f);
  }

  /** \brief For each match of a promotion, increase the cost
   *  operation in a solver set if it's incipient; increase the
   *  effective cost operation if it's not.
   */
  class do_process_promotion
  {
    generic_problem_resolver &resolver;
    const promotion &p;

  public:
    do_process_promotion(generic_problem_resolver &_resolver,
			 const promotion &_p)
      : resolver(_resolver), p(_p)
    {
    }

    void incipient(int step_num, const choice &solver) const
    {
      resolver.increase_solver_cost(resolver.graph.get_step(step_num),
                                    p, solver);
    }

    void active(int step_num) const
    {
      step &s = graph.get_step(step_num);
      resolver.increase_effective_step_cost(s, p);
      resolver.graph.schedule_promotion_propagation(step_num, p);
    }
  };

  /** \brief Find existing steps that contain the given promotion and
   *  apply it to them.
   */
  void process_promotion(const promotion &p)
  {
    LOG_TRACE(logger, "Processing the promotion " << p << " and applying it to all existing steps.");

    for_each_promotion_hit(p, do_process_promotion(*this, p));
  }


  // NB: this might visit more than is strictly necessary, since the
  // solver might only need to be recomputed when it's attached to a
  // particular dependency.  It would take too much book-keeping to
  // figure out which dependency needs to be re-examined ahead of
  // time.
  //
  // Only solvers whose cost equals the given cost are recomputed.
  class recompute_solver_cost_for_dep
  {
    generic_problem_resolver &resolver;
    step &s;
    const cost &c;

  public:
    recompute_solver_cost_for_dep(generic_problem_resolver &_resolver,
				  step &_s,
				  const cost &_c)
      : resolver(_resolver), s(_s), c(_c)
    {
    }

    bool operator()(const choice &solver,
		    const imm::list<dep> &deps) const
    {
      for(typename imm::list<dep>::const_iterator it = deps.begin();
	  it != deps.end(); ++it)
	resolver.recompute_solver_cost(s, *it, solver, c.get_structural_level());

      return true;
    }
  };

  /** \brief For each promotion hit, recompute the solver cost if it's
   *  an incipient promotion and the step cost otherwise.
   *
   *  The associated cost is used as a filter.  While it's
   *  hard in general to tell if a promotion affected a step (without
   *  actually tracking the set of promotions that touched it), we can
   *  be sure that if a promotion affected a step, the structural level
   *  of the step was increased to the structural level of the
   *  promotion.  Thus, if the step's structural level is *below* that
   *  of the promotion, it couldn't have been affected by the
   *  promotion and hence doesn't need to be recomputed.
   *
   *  (that's a useful heuristic because most of the promotions that
   *  we'll retract promote to the deferral structural level)
   */
  class recompute_cost_hits
  {
    generic_problem_resolver &resolver;
    const cost &c;

  public:
    recompute_cost_hits(generic_problem_resolver &_resolver,
			const cost &_c)
      : resolver(_resolver), c(_c)
    {
    }

    void incipient(int step_num, const choice &solver) const
    {
      step &s(resolver.graph.get_step(step_num));

      recompute_solver_cost_for_dep
	recompute_solver_cost_f(resolver, s, c);

      s.deps_solved_by_choice.for_each_key_contained_in(solver,
							recompute_solver_cost_f);
    }

    void active(int step_num) const
    {
      step &s(resolver.graph.get_step(step_num));

      // The promotion could only have affected the step's effective
      // cost if the effective cost's structural level is at least as
      // high.
      if(s.effective_step_cost.get_structural_level() >= c.get_structural_level())
	resolver.recompute_effective_step_cost(resolver.graph.get_step(step_num));
    }
  };

  void promotion_retracted(const promotion &p)
  {
    LOG_TRACE(logger, "Retracting all cost assignments that might be linked to " << p);

    for_each_promotion_hit(p, recompute_cost_hits(*this, p.get_cost()));
  }



  /** \brief Process all pending promotions. */
  void process_pending_promotions()
  {
#if 0
    // Processing promotions like this turned out to be too expensive.
    LOG_TRACE(logger, "Processing pending promotions and applying them to all steps.");
    while(!pending_promotions.empty())
      {
	promotion p(pending_promotions.front());
	pending_promotions.pop_front();

	process_promotion(p);
      }
#endif
  }

  class do_drop_deps_solved_by
  {
    step &s;
    logging::LoggerPtr logger;

  public:
    do_drop_deps_solved_by(step &_s, const logging::LoggerPtr &_logger)
      : s(_s), logger(_logger)
    {
    }

    bool operator()(const choice &c, const imm::list<dep> &deps) const
    {
      LOG_TRACE(logger, "Removing dependencies solved by " << c);

      for(typename imm::list<dep>::const_iterator it = deps.begin();
	  it != deps.end(); ++it)
	{
	  const dep &d(*it);

	  // Need to look up the solvers of the dep in order to know
	  // the number of solvers that it was entered into the
	  // by-num-solvers set with.
	  typename imm::map<dep, typename step::flyweight_dep_solvers>::node
	    solvers = s.unresolved_deps.lookup(d);

	  if(solvers.isValid())
	    {
	      const typename step::dep_solvers &
		dep_solvers(solvers.getVal().second);

	      LOG_TRACE(logger,
			"Removing the dependency " << d
			<< " with a solver set of " << dep_solvers.dump_solvers());
	      const typename step::dep_solvers::solvers_size_type
		num_solvers = dep_solvers.get_solvers_size();
	      s.unresolved_deps_by_num_solvers.erase(std::make_pair(num_solvers, d));
	    }
	  else
	    LOG_TRACE(logger, "The dependency " << d
		      << " has no solver set, assuming it was already solved.");

	  s.unresolved_deps.erase(d);
	}

      s.deps_solved_by_choice.erase(c);
      return true;
    }
  };

  /** \brief Drop all dependencies from the given set that are solved
   *  by the given choice.
   */
  void drop_deps_solved_by(const choice &c, step &s) const
  {
    choice c_general(c.generalize());
    LOG_TRACE(logger, "Dropping dependencies in step "
	      << s.step_num << " that are solved by " << c_general);

    s.deps_solved_by_choice.for_each_key_contained_in(c_general,
						      do_drop_deps_solved_by(s, logger));

    LOG_TRACE(logger, "Done dropping dependencies in step "
	      << s.step_num << " that are solved by " << c_general);
  }

  class add_to_structural_reasons
  {
    typename step::dep_solvers &target;

  public:
    add_to_structural_reasons(typename step::dep_solvers &_target)
      : target(_target)
    {
    }

    bool operator()(const choice &c) const
    {
      target.add_structural_reason(c);
      return true;
    }
  };

  // Helper for strike_choice.  For each dependency that's solved by a
  // choice, remove the choice from the solvers list of that
  // dependency.  Also, update the global search graph's reverse index
  // so it doesn't map the choice to that dependency any more.
  class do_strike_choice
  {
    step &s;
    const choice_set &reasons;
    search_graph &graph;
    generic_problem_resolver &resolver;
    logging::LoggerPtr logger;

  public:
    do_strike_choice(step &_s,
		     const choice_set &_reasons,
		     search_graph &_graph,
		     generic_problem_resolver &_resolver,
		     const logging::LoggerPtr &_logger)
      : s(_s),
	reasons(_reasons),
	graph(_graph),
	resolver(_resolver),
	logger(_logger)
    {
    }

    // One slight subtlety here: the "victim" passed in might differ
    // from the "victim" passed to strike_choice.  Here, "victim" is
    // the choice linked to the actual solver; i.e., it could be a
    // from-dep-source choice.  In strike_choice, "victim" could be
    // more general.  Using the more specific victim means that we
    // remove the correct entries (there could be both general and
    // specific entries that need to be removed when striking a broad
    // choice).
    bool operator()(const choice &victim,
		    imm::list<dep> solved_by_victim) const
    {
      LOG_TRACE(logger, "Removing the choice " << victim
		<< " from the solver lists of " << solved_by_victim
		<< " in step " << s.step_num);

      for(typename imm::list<dep>::const_iterator it = solved_by_victim.begin();
	  it != solved_by_victim.end(); ++it)
	{
	  const dep &d(*it);
	  choice victim_with_dep(victim.copy_and_set_dep(d));

	  // Remove this step from the set of steps related to the
	  // solver that was deleted.
	  graph.remove_choice(victim_with_dep, s.step_num, d);

	  // Find the current number of solvers so we can yank the
	  // dependency out of the unresolved-by-num-solvers set.
	  typename imm::map<dep, typename step::flyweight_dep_solvers>::node
	    current_solver_set_found = s.unresolved_deps.lookup(d);

	  if(current_solver_set_found.isValid())
	    {
	      const typename step::dep_solvers &
		current_solvers(current_solver_set_found.getVal().second);
	      const typename step::dep_solvers::solvers_size_type
		current_num_solvers = current_solvers.get_solvers_size();

	      typename step::dep_solvers new_solvers(current_solvers);

	      new_solvers.remove_solver(victim_with_dep);
	      add_to_structural_reasons adder(new_solvers);
	      reasons.for_each(adder);

	      LOG_TRACE(logger,
			"Removing the choice " << victim_with_dep
			<< " from the solver set of " << d
			<< " in step " << s.step_num
			<< ", new solvers: " << new_solvers.dump_solvers());

	      // Actually update the solvers of the dep.
	      {
		const typename step::flyweight_dep_solvers
		  memoized_new_solvers(new_solvers);
		s.unresolved_deps.put(d, memoized_new_solvers);
	      }

	      const typename step::dep_solvers::solvers_size_type
		new_num_solvers = new_solvers.get_solvers_size();

	      if(current_num_solvers != new_num_solvers)
		{
		  LOG_TRACE(logger, "Changing the number of solvers of "
			    << d << " from " << current_num_solvers
			    << " to " << new_num_solvers
			    << " in step " << s.step_num);
		  // Update the number of solvers.
		  s.unresolved_deps_by_num_solvers.erase(std::make_pair(current_num_solvers, d));
		  s.unresolved_deps_by_num_solvers.insert(std::make_pair(new_num_solvers, d));
		}

	      // Rescan the solvers, maybe updating the step's cost.
	      resolver.check_solvers_cost(s, new_solvers);
	    }
	  else
	    LOG_TRACE(logger, "The dependency " << d
		      << " has no solver set, assuming it was already solved.");
	}

      LOG_TRACE(logger, "Removing all solved-by links for " << victim
		<< " in step " << s.step_num);
      s.deps_solved_by_choice.erase(victim);

      return true;
    }
  };

  /** \brief Strike the given choice and any choice that it contains
   *         from all solver lists in the given step.
   */
  void strike_choice(step &s,
		     const choice &victim,
		     const choice_set &reason)
  {
    // Generalize the choice set by removing the solver that's being
    // struck.
    choice_set generalized_reason(reason);
    generalized_reason.remove_overlaps(victim);
    LOG_TRACE(logger, "Striking " << victim
	      << " from all solver lists in step " << s.step_num
	      << " with the reason set " << generalized_reason);


    do_strike_choice striker_f(s, generalized_reason, graph, *this, logger);
    s.deps_solved_by_choice.for_each_key_contained_in(victim, striker_f);
  }

  /** \brief Strike choices that are structurally forbidden by the
   *  given choice from the given step, and update the set of
   *  forbidden versions.
   */
  void strike_structurally_forbidden(step &s,
				     const choice &c)
  {
    switch(c.get_type())
      {
      case choice::install_version:
	{
	  {
	    choice_set reason;
	    // Throw out the from-dep-sourceness.
	    reason.insert_or_narrow(choice::make_install_version(c.get_ver(),
								 c.get_dep(),
								 c.get_id()));

	    for(typename package::version_iterator vi =
		  c.get_ver().get_package().versions_begin();
		!vi.end(); ++vi)
	      {
		version current(*vi);

		if(current != c.get_ver())
		  {
		    LOG_TRACE(logger,
			      "Discarding " << current
			      << ": monotonicity violation");
		    strike_choice(s,
				  choice::make_install_version(current, -1),
				  reason);
		  }
	      }
	  }


	  if(c.get_from_dep_source())
	    {
	      const version &c_ver = c.get_ver();
	      choice_set reason;
	      reason.insert_or_narrow(c);
	      for(typename dep::solver_iterator si =
		    c.get_dep().solvers_begin();
		  !si.end(); ++si)
		{
		  version current(*si);

		  if(current != c_ver)
		    {
		      LOG_TRACE(logger,
				"Discarding " << current
				<< ": forbidden by the resolution of "
				<< c.get_dep());
		      strike_choice(s,
				    choice::make_install_version(current, -1),
				    reason);
		      s.forbidden_versions.put(current, c);
		    }
		}
	    }
	}
	break;

      case choice::break_soft_dep:
	// \todo For broken soft deps, forbid each target of the
	// dependency.
	break;
      }
  }

  approved_or_rejected_info &get_approved_or_rejected_info(const version &v)
  {
    typename std::map<version, approved_or_rejected_info>::iterator found =
      user_approved_or_rejected_versions.find(v);

    if(found == user_approved_or_rejected_versions.end())
      found = user_approved_or_rejected_versions.insert(std::make_pair(v, approved_or_rejected_info(v, logger->isEnabledFor(logging::TRACE_LEVEL)))).first;

    return found->second;
  }

  approved_or_rejected_info &get_approved_or_rejected_info(const dep &d)
  {
    typename std::map<dep, approved_or_rejected_info>::iterator found =
      user_approved_or_rejected_broken_deps.find(d);

    if(found == user_approved_or_rejected_broken_deps.end())
      found = user_approved_or_rejected_broken_deps.insert(std::make_pair(d, approved_or_rejected_info(d, logger->isEnabledFor(logging::TRACE_LEVEL)))).first;

    return found->second;
  }

  approved_or_rejected_info &get_approved_or_rejected_info(const choice &c)
  {
    switch(c.get_type())
      {
      case choice::install_version:
	return get_approved_or_rejected_info(c.get_ver());

      case choice::break_soft_dep:
	return get_approved_or_rejected_info(c.get_dep());

      default:
	// There's nothing that's safe to return in this case.
	eassert(!"This should never happen.");
      }
  }

  /** \brief Build an expression that computes whether the given
   *  choice is deferred.
   *
   *  This is normally invoked through its memoized frontend (without
   *  the _real suffix).
   */
  cwidget::util::ref_ptr<expression<bool> > build_is_deferred_real(const choice &c)
  {
    switch(c.get_type())
      {
      case choice::install_version:
	{
	  // We can't correctly compute deferral information for a
	  // choice with no dependency.  And since this should only be
	  // invoked on a solver, it's an error if there is no
	  // dependency: all solvers ought to have a dependency
	  // attached.
	  eassert(c.get_has_dep());

	  const version &c_ver(c.get_ver());
	  const approved_or_rejected_info &c_info =
	    get_approved_or_rejected_info(c);

	  const cwidget::util::ref_ptr<expression<bool> > &c_rejected(c_info.get_rejected());
	  const cwidget::util::ref_ptr<expression<bool> > &c_approved(c_info.get_approved());

	  // Versions are deferred if they are rejected, OR if they
	  // are NOT approved AND some other solver of the same
	  // dependency (which might include breaking it!) is
	  // approved.
	  std::vector<cwidget::util::ref_ptr<expression<bool> > >
	    others_approved;
	  const dep &c_dep(c.get_dep());
	  for(typename dep::solver_iterator si = c_dep.solvers_begin();
	      !si.end(); ++si)
	    {
	      version solver(*si);
	      if(solver != c_ver)
		others_approved.push_back(get_approved_or_rejected_info(solver).get_approved());
	    }

	  // Other solvers also include other versions of the source,
	  // if the dependency isn't a soft dependency.
	  if(!c_dep.is_soft())
	    {
	      const version c_dep_source(c_dep.get_source());
	      for(typename package::version_iterator vi =
		    c_dep_source.get_package().versions_begin();
		  !vi.end(); ++vi)
		{
		  version solver(*vi);
		  if(solver != c_dep_source && solver != c_ver)
		    others_approved.push_back(get_approved_or_rejected_info(solver).get_approved());
		}
	    }
	  else
	    others_approved.push_back(get_approved_or_rejected_info(c_dep).get_approved());

	  return
	    or_e::create(c_rejected,
			 and_e::create(not_e::create(c_approved),
				       or_e::create(others_approved.begin(),
						    others_approved.end())));
	}
	break;

      case choice::break_soft_dep:
	{
	  const dep &c_dep(c.get_dep());
	  const approved_or_rejected_info &c_info =
	    get_approved_or_rejected_info(c_dep);



	  const cwidget::util::ref_ptr<expression<bool> > &c_rejected(c_info.get_rejected());
	  const cwidget::util::ref_ptr<expression<bool> > &c_approved(c_info.get_approved());

	  // Broken dependencies are deferred if they are rejected, OR
	  // if they are NOT approved AND some solver of the same
	  // dependency is approved.
	  std::vector<cwidget::util::ref_ptr<expression<bool> > >
	    others_approved;
	  for(typename dep::solver_iterator si = c_dep.solvers_begin();
	      !si.end(); ++si)
	    {
	      version solver(*si);
	      others_approved.push_back(get_approved_or_rejected_info(solver).get_approved());
	    }

	  return
	    or_e::create(c_rejected,
			 and_e::create(not_e::create(c_approved),
				       or_e::create(others_approved.begin(),
						    others_approved.end())));
	}
	break;

      default:
	LOG_ERROR(logger, "Internal error: bad choice type " << c.get_type());
	return var_e<bool>::create(false);
      }
  }

  /** \brief Memoized version of build_is_deferred. */
  cwidget::util::ref_ptr<expression_box<bool> > build_is_deferred_listener(const choice &c)
  {
    typename std::map<choice, expression_weak_ref<expression_box<bool> > >::const_iterator
      found = memoized_is_deferred.find(c);

    if(found != memoized_is_deferred.end())
      {
	const expression_weak_ref<expression_box<bool> > &ref(found->second);
	if(ref.get_valid())
	  return ref.get_value();
      }

    cwidget::util::ref_ptr<expression<bool> > expr(build_is_deferred_real(c));
    cwidget::util::ref_ptr<expression_box<bool> > rval(deferral_updating_expression::create(expr, c, *this));
    memoized_is_deferred[c] = rval;
    return rval;
  }

  class invoke_recompute_solver_cost
  {
    generic_problem_resolver &resolver;
    const dep &d;

  public:
    invoke_recompute_solver_cost(generic_problem_resolver &_resolver,
				    const dep &_d)
      : resolver(_resolver),
	d(_d)
    {
    }

    bool operator()(const choice &c,
		    typename search_graph::choice_mapping_type how,
		    int step_num) const
    {
      step &s(resolver.graph.get_step(step_num));

      if(how == search_graph::choice_mapping_action)
	resolver.recompute_effective_step_cost(s);
      else
	resolver.recompute_solver_cost(s, d, c);

      return true;
    }
  };

  /** \brief Invoked when a solver's cost needs to be recomputed.
   *
   *  Locates the solver in each step that it solves, tosses its cost,
   *  and recomputes it from scratch.
   */
  void deferral_retracted(const choice &deferral_choice,
			  const dep &deferral_dep)
  {
    LOG_TRACE(logger, "The choice " << deferral_choice << " is no longer deferred; recomputing its cost in all steps.");

    invoke_recompute_solver_cost recompute_f(*this, deferral_dep);
    graph.for_each_step_related_to_choice_with_dep(deferral_choice,
						   deferral_dep,
						   recompute_f);
  }

  /** \brief Recompute the cost of a single solver in the
   *  given step.
   *
   *  Previous versions of this code would only recompute the cost
   *  operation if it was equal to the cost of the promotion being
   *  retracted.  However, with the introduction of partially ordered
   *  costs, upper-bounding, and nonidempotent operations, this isn't
   *  sound any more.  There is exactly one case where it's definitely
   *  safe, though: costs that don't change anything but the
   *  structural cost.  Not coincidentally, this is also the only type
   *  of operation that gets retracted.
   */
  void recompute_solver_cost(step &s,
                             const dep &solver_dep,
                             const choice &solver,
                             const int &check_structural_level = cost_limits::minimum_level,
                             bool do_check_structural_level = false)
  {
    typename imm::map<dep, typename step::flyweight_dep_solvers>::node
      found_solvers(s.unresolved_deps.lookup(solver_dep));

    if(found_solvers.isValid())
      {
	typename step::dep_solvers new_dep_solvers(found_solvers.getVal().second);

	const typename step::solver_information *
	  found_solver = new_dep_solvers.lookup_solver_information(solver);
	if(found_solver == NULL)
	  LOG_ERROR(logger, "Internal error: the choice " << solver
		    << " is listed in the reverse index for step "
		    << s.step_num << " as a solver for "
		    << solver_dep << ", but it doesn't appear in that step.");
	else if(!do_check_structural_level ||
		(found_solver->get_cost().get_structural_level() == check_structural_level &&
		 !found_solver->get_cost().get_has_user_levels()))
	  {
	    cost new_cost;
	    cwidget::util::ref_ptr<expression_box<bool> > new_cost_is_deferred;
	    cwidget::util::ref_ptr<expression<bool> > new_cost_valid;
	    choice solver_with_dep(solver.copy_and_set_dep(solver_dep));
	    get_solver_intrinsic_promotion_with_deferrals(solver_with_dep,
							  new_cost,
							  new_cost_is_deferred,
							  new_cost_valid);

	    typename step::solver_information
	      new_solver_inf(new_cost,
			     choice_set(),
			     new_cost_valid,
			     new_cost_is_deferred);
	    new_dep_solvers.set_solver_information(solver_with_dep, new_solver_inf);
	    {
	      typename step::flyweight_dep_solvers
		memoized_new_dep_solvers(new_dep_solvers);

	      s.unresolved_deps.put(solver_dep, memoized_new_dep_solvers);
	    }
	    LOG_TRACE(logger, "Recomputed the cost of "
		      << solver << " in the solver list of "
		      << solver_dep << " in step " << s.step_num
		      << "; new solver list: " << new_dep_solvers);


	    find_promotions_for_solver(s, solver_with_dep);
	    // Recompute the step's cost from scratch.
	    //
	    // \todo Only do this if the cost went down, and just do a
	    // local recomputation otherwise?
	    recompute_effective_step_cost(s);
	  }
      }
  }

  /** \brief Predicate used to throw away promotions that would
   *  promote a blessed solution to the already-generated cost (or
   *  discard it for containing a "conflict").
   *
   *  Returns \b true if a promotion does *not* discard a "blessed"
   *  solution.
   *
   *  Whether a promotion would throw away a solver is determined by
   *  testing that promotion against the containing step's current
   *  cost; if applying the promotion would raise the cost too much,
   *  the solver is struck.
   */
  class discards_blessed
  {
    // True if the step being processed is blessed.  If not, all
    // promotions are allowed through; otherwise only promotions below
    // the already-generated cost are allowed through.
    bool blessed;

    const generic_problem_resolver &resolver;

    const step &s;

  public:
    discards_blessed(bool _blessed,
		     const generic_problem_resolver &_resolver,
		     const step &_s)
      : blessed(_blessed),
	resolver(_resolver),
	s(_s)
    {
    }

    bool operator()(const promotion &p) const
    {
      if(blessed)
	return !resolver.is_discard_cost(p.get_cost() + s.base_step_cost);
      else
	return true;
    }
  };

  /** \brief Find promotions triggered by the given solver and
   *  increment its cost accordingly.
   */
  void find_promotions_for_solver(step &s,
				  const choice &solver)
  {
    // \note imm::list<dep> is used so we don't have two separate
    // instantiations of
    // find_highest_incipient_promotions_containing().
    generic_choice_indexed_map<PackageUniverse, imm::list<dep> > output_domain;
    boost::unordered_map<choice, promotion> triggered_promotions;

    output_domain.put(solver, imm::list<dep>());

    discards_blessed discards_blessed_p(s.is_blessed_solution, *this, s);
    promotions.find_highest_incipient_promotions_containing(s.actions,
							    solver,
							    output_domain,
							    discards_blessed_p,
							    triggered_promotions);

    // Sanity-check.
    if(triggered_promotions.size() > 1)
      LOG_ERROR(logger,
		"Internal error: found " << triggered_promotions.size()
		<< " (choice -> promotion) mappings for a single choice.");

    for(typename boost::unordered_map<choice, promotion>::const_iterator it =
	  triggered_promotions.begin();
	it != triggered_promotions.end(); ++it)
      increase_solver_cost(s, it->second, it->first);
  }

  cost get_intrinsic_solver_cost(const choice &c)
  {
    switch(c.get_type())
      {
      case choice::install_version:
	return version_costs[c.get_ver().get_id()];

      case choice::break_soft_dep:
        return unfixed_soft_cost;

      default:
	LOG_ERROR(logger, "get_intrinsic_solver_cost: bad choice type: " << c);
	return cost_limits::minimum_cost;
      }
  }

  /** \brief Compute the basic cost information of a choice.
   *
   *  This is the cost it will have unless it's hit by a promotion,
   *  along with information about conditions on its cost.
   */
  void get_solver_intrinsic_promotion_with_deferrals(const choice &c,
						     cost &out_cost,
						     cwidget::util::ref_ptr<expression_box<bool> > &out_c_is_deferred,
						     cwidget::util::ref_ptr<expression<bool> > &out_cost_valid)
  {
    // Right now only deferrals can be retracted; other cost
    // assignments are immutable.
    out_c_is_deferred = build_is_deferred_listener(c);

    out_cost = cost_limits::minimum_cost;
    if(out_c_is_deferred->get_value())
      {
	out_cost = cost_limits::defer_cost;
	out_cost_valid = out_c_is_deferred->get_child();
      }
    else
      {
	out_cost_valid = cwidget::util::ref_ptr<expression<bool> >();
	out_cost = get_intrinsic_solver_cost(c);
      }
  }

  /** \brief Get the intrinsic cost of a single solver.
   *
   *  Currently a convenience wrapper for
   *  get_solver_intrinsic_promotion.
   */
  cost get_solver_intrinsic_cost_with_deferrals(const choice &c)
  {
    cost rval;
    cwidget::util::ref_ptr<expression_box<bool> > dummy_is_deferred;
    cwidget::util::ref_ptr<expression<bool> > dummy_cost_valid;

    get_solver_intrinsic_promotion_with_deferrals(c, rval,
						  dummy_is_deferred, dummy_cost_valid);

    return rval;
  }

  /** \brief Add a solver to a list of dependency solvers for a
   *  particular step.
   *
   *  \param s         The step to update.
   *  \param solvers   The solvers set to fill in.
   *  \param d         The dependency whose solvers are being updated
   *                   (used to update the reverse map).
   *  \param solver    The choice to add.
   *
   *  If the solver is structurally forbidden in the step, the reason
   *  is added to the structural reasons list of the solvers
   *  structure; otherwise, the choice is added to the solvers list.
   */
  void add_solver(step &s,
		  typename step::dep_solvers &solvers,
		  const dep &d,
		  const choice &solver)
  {
    // The solver is structurally forbidden if it is in the forbidden
    // map, OR if another version of the same package is selected.
    //
    // First we check if the solver is forbidden, and return early if
    // it is.
    if(solver.get_type() == choice::install_version)
      {
	eassert(solver.get_has_dep());
	eassert(solver.get_dep() == d);

	const version ver(solver.get_ver());
	version selected;
	if(s.actions.get_version_of(ver.get_package(), selected))
	  {
	    if(selected == ver)
	      // There's not really anything we can do to fix this: the
	      // dependency just shouldn't be inserted at all!
	      LOG_ERROR(logger,
			"Internal error: the solver "
			<< solver << " of a supposedly unresolved dependency is already installed in step "
			<< s.step_num);
	    else
	      {
		LOG_TRACE(logger,
			  "Not adding " << solver
			  << ": monotonicity violation due to "
			  << selected);
		choice reason(choice::make_install_version(selected, -1));
		solvers.add_structural_reason(reason);
	      }

	    return; // If the package is already modified, abort.
	  }
	else if(initial_state.version_of(ver.get_package()) == ver)
	  {
	    LOG_TRACE(logger, "Not adding " << solver
		      << ": it is the current version of the package "
		      << ver.get_package());
	    // No structural reason -- this is just never a candidate.
	    return;
	  }
	else
	  {
	    typename imm::map<version, choice>::node forbidden_found =
	      s.forbidden_versions.lookup(ver);

	    if(forbidden_found.isValid())
	      {
		const choice &reason(forbidden_found.getVal().second);

		LOG_TRACE(logger,
			  "Not adding " << solver
			  << ": it is forbidden due to the action "
			  << reason);
		solvers.add_structural_reason(reason);

		return;
	      }
	  }
      }

    // The choice is added with its intrinsic cost to start with.
    // Later, in step 6 of the update algorithm, we'll find promotions
    // that include the new solvers and update costs appropriately.
    cost choice_cost;
    cwidget::util::ref_ptr<expression_box<bool> > choice_is_deferred;
    cwidget::util::ref_ptr<expression<bool> > choice_cost_valid;
    get_solver_intrinsic_promotion_with_deferrals(solver, choice_cost, choice_is_deferred, choice_cost_valid);

    LOG_TRACE(logger, "Adding the solver " << solver
	      << " with initial cost " << choice_cost);
    {
      typename step::solver_information
	new_solver_inf(choice_cost,
		       choice_set(),
		       choice_cost_valid,
		       choice_is_deferred);
      solvers.set_solver_information(solver, new_solver_inf);
    }

    // Update the deps-solved-by-choice map (add the dep being
    // processed to the list).
    imm::list<dep> old_deps_solved;

    if(!s.deps_solved_by_choice.try_get(solver, old_deps_solved))
      s.deps_solved_by_choice.put(solver, imm::list<dep>::make_singleton(d));
    else
      {
	imm::list<dep> new_deps_solved(imm::list<dep>::make_cons(d, old_deps_solved));
	s.deps_solved_by_choice.put(solver, new_deps_solved);
      }

    graph.bind_choice(solver, s.step_num, d);
  }

  /** \brief Compute the greatest lower bound of the costs
   *  of the solvers of a single dependency.
   */
  class find_solvers_cost_lower_bound
  {
    maybe<cost> &output_cost;

  public:
    find_solvers_cost_lower_bound(maybe<cost> &_output_cost)
      : output_cost(_output_cost)
    {
      output_cost = maybe<cost>();
    }

    bool operator()(const std::pair<choice, typename step::solver_information> &p) const
    {
      const cost &p_cost(p.second.get_cost());

      if(output_cost.get_has_value())
	output_cost = cost::greatest_lower_bound(output_cost.get_value(),
                                                 p_cost);
      else
	output_cost = p_cost;

      return true;
    }
  };

  /** \brief Compute the upper bound of the lower bound of each
   *  dependency's costs.
   *
   *  \note In principle, it should sometimes be possible to \e
   *  compose costs instead of upper-bounding them, yielding stronger
   *  results (and better promotions).  However, it's not safe to do
   *  that unless we're sure that the costs don't result from
   *  "overlapping subcases", and trying to compute precisely, or even
   *  somewhat imprecisely, when that's the case seems to be NP-hard
   *  (related to independent set).
   */
  class find_dep_cost_upper_bound
  {
    cost &output_cost;
    generic_problem_resolver &resolver;

  public:
    find_dep_cost_upper_bound(cost &_output_cost,
                              generic_problem_resolver &_resolver)
      : output_cost(_output_cost), resolver(_resolver)
    {
    }

    bool operator()(const std::pair<dep, typename step::dep_solvers> &p) const
    {
      maybe<cost> dep_cost;

      p.second.for_each_solver(find_solvers_cost_lower_bound(dep_cost));

      cost new_output_cost =
	(dep_cost.get_has_value() && !output_cost.is_above_or_equal(dep_cost.get_value()))
	? cost::least_upper_bound(output_cost, dep_cost.get_value())
	: output_cost;

      if(output_cost != new_output_cost)
	{
	  LOG_TRACE(resolver.logger, "Updating the cost from "
		    << output_cost << " to "
		    << new_output_cost << " for the dependency " << p.first);
	  output_cost = new_output_cost;
	}

      return true;
    }
  };

  // Look for deferrals and increase the target cost to the
  // deferral cost if necessary.
  class find_deferrals
  {
    cost &output_cost;
    generic_problem_resolver &resolver;

  public:
    find_deferrals(cost &_output_cost,
		   generic_problem_resolver &_resolver)
      : output_cost(_output_cost), resolver(_resolver)
    {
    }

    bool operator()(const choice &c) const
    {
      cost choice_cost;

      switch(c.get_type())
	{
	case choice::install_version:
	  choice_cost = resolver.version_costs[c.get_ver().get_id()];
	  break;

	default:
	  break;
	}

      if(resolver.build_is_deferred_listener(c)->get_value())
	choice_cost = cost::least_upper_bound(cost_limits::defer_cost,
                                              choice_cost);

      const cost new_output_cost =
	cost::least_upper_bound(output_cost, choice_cost);

      if(output_cost != new_output_cost)
	{
	  LOG_TRACE(resolver.logger, "Updating the cost from "
		    << output_cost << " to "
		    << new_output_cost
		    << " for the action " << c);
	  output_cost = new_output_cost;
	}

      return true;
    }
  };

  /** \brief Recompute a step's effective cost from scratch.
   *
   *  It is assumed that all the solvers in the step have the correct
   *  cost; the recomputation is based on them.
   *
   *  This is a bit of a sledgehammer.  The places where this is used
   *  could be tuned to not need it; currently I'm just hoping it's
   *  invoked few enough times to not matter.
   *
   *  (this could be done incrementally using the accumulation
   *  abilities of imm::set/imm::map; however, as noted above, I hope
   *  that this isn't invoked often enough for that to matter)
   */
  void recompute_effective_step_cost(step &s)
  {
    LOG_TRACE(logger, "Recomputing the final cost of step " << s.step_num
	      << " (was " << s.final_step_cost << ")");

    cost new_cost(cost_limits::minimum_cost);
    s.unresolved_deps.for_each(find_dep_cost_upper_bound(new_cost, *this));

    // In addition to checking solvers, we need to check the action
    // set.  Look for existing promotions *and* for deferred entries.
    //
    // \todo Since this doesn't just find deferrals, it's really badly
    // named.  The name should be changed to something like
    // "find_deferrals_and_promotions".
    s.actions.for_each(find_deferrals(new_cost, *this));

    cost found_promotion =
      promotions.find_highest_promotion_cost(s.actions);

    if(!new_cost.is_above_or_equal(found_promotion))
      {
	cost new_new_cost =
	  cost::least_upper_bound(new_cost,
                                  found_promotion);

	if(new_new_cost != new_cost)
	  {
	    LOG_TRACE(logger, "Updating the cost from "
		      << new_cost << " to "
		      << new_new_cost
		      << " for a promotion.");
	    new_cost = new_new_cost;
	  }
      }

    set_effective_step_cost(s.step_num, new_cost);
  }

  // Calculate the cost of a dep-solvers set.
  class calculate_cost
  {
    cost &output_cost;
    // I need "first" here because there isn't a maximal cost, so we
    // use a small hack here: if we never see a cost, we just return
    // an arbitrary cost that forces a discard; otherwise, we take the
    // maximum of the costs that were seen.  Ideally we would start
    // with the highest cost and iteratively decrease it, but the
    // theoretical top value (INT_MAX for each slot index) is
    // infinite, or at least too large to create.
    bool &first;

  public:
    calculate_cost(cost &_output_cost,
                   bool &_first)
      : output_cost(_output_cost),
        first(_first)
    {
      output_cost = cost_limits::maximum_structural_level_cost;
      first = true;
    }

    bool operator()(const std::pair<choice, typename step::solver_information> &entry) const
    {
      if(first)
        {
          output_cost = entry.second.get_cost();
          first = false;
        }
      else
        output_cost =
          cost::greatest_lower_bound(entry.second.get_cost(),
                                     output_cost);

      return true;
    }
  };

  // Build a generalized promotion from the entries of a dep-solvers
  // set.
  class build_promotion
  {
    choice_set &output_reasons;
    std::vector<cwidget::util::ref_ptr<expression<bool> > > & output_valid_conditions;

  public:
    build_promotion(choice_set &_output_reasons,
		    std::vector<cwidget::util::ref_ptr<expression<bool> > > &_output_valid_conditions)
      : output_reasons(_output_reasons),
	output_valid_conditions(_output_valid_conditions)
    {
      output_reasons = choice_set();
      output_valid_conditions.clear();
    }

    bool operator()(const std::pair<choice, typename step::solver_information> &entry) const
    {
      // Correctness here depends on the fact that the reason set is
      // pre-generalized (the solver itself is already removed).
      output_reasons.insert_or_narrow(entry.second.get_reasons());
      cwidget::util::ref_ptr<expression<bool> > cost_valid(entry.second.get_cost_valid());
      if(cost_valid.valid())
	output_valid_conditions.push_back(cost_valid);

      return true;
    }
  };

  /** \brief If the given dependency solver set implies a promotion,
   *         attempt to insert that promotion; also, update the cost
   *         of the step to be at least the lowest cost of any of the
   *         solvers in the given list.
   *
   *  If the set is empty, this just inserts a conflict.
   */
  void check_solvers_cost(step &s, const typename step::dep_solvers &solvers)
  {
    cost c;
    choice_set reasons;
    std::vector<cwidget::util::ref_ptr<expression<bool> > > valid_conditions;

    {
      bool first = true;
      solvers.for_each_solver(calculate_cost(c, first));
    }

    // If no promotion would be emitted and the step cost wouldn't be
    // changed, don't do anything.  The "apply" test effectively
    // checks whether c consists only of maximizing operations that
    // are below the current search cost, in which case there's no
    // point in saving it.
    if(c + get_current_search_cost() == get_current_search_cost() &&
       s.effective_step_cost.is_above_or_equal(c))
      return;

    solvers.for_each_solver(build_promotion(reasons, valid_conditions));

    for(typename imm::list<choice>::const_iterator it =
	  solvers.get_structural_reasons().begin();
	it != solvers.get_structural_reasons().end(); ++it)
      {
	reasons.insert_or_narrow(*it);
      }

    cwidget::util::ref_ptr<expression<bool> > valid_condition;
    switch(valid_conditions.size())
      {
      case 0:
	// If there are no validity conditions, don't create one for
	// the promotion.
	break;

      case 1:
	// If there's just one validity condition, copy it to the
	// promotion.
	valid_condition = valid_conditions.front();
	break;

      default:
	// If there are multiple validity conditions, the promotion
	// depends on them all.
	valid_condition = and_e::create(valid_conditions.begin(),
					valid_conditions.end());
	break;
      }


    promotion p(reasons, c, valid_condition);
    LOG_TRACE(logger, "Emitting a new promotion " << p
	      << " at step " << s.step_num);

    add_promotion(s.step_num, p);
    increase_effective_step_cost(s, p);
  }

  /** \brief Increases the effective cost of a single step. */
  void increase_effective_step_cost(step &s,
                                    const promotion &p)
  {
    const cost &p_cost(p.get_cost());

    if(!s.effective_step_cost.is_above_or_equal(p_cost))
      {
        cost new_effective_step_cost =
          cost::least_upper_bound(p_cost, s.effective_step_cost);

        set_effective_step_cost(s.step_num, new_effective_step_cost);
      }
  }

  // Increases the cost of each dependency in each dependency list
  // this is applied to.  Helper for increase_solver_cost.
  struct do_increase_solver_cost
  {
    step &s;
    const cost &new_cost;
    const choice_set &new_choices;
    const cwidget::util::ref_ptr<expression<bool> > &valid_condition;
    generic_problem_resolver &resolver;
    logging::LoggerPtr logger;

  public:
    do_increase_solver_cost(step &_s,
                            const cost &_new_cost,
                            const choice_set &_new_choices,
                            const cwidget::util::ref_ptr<expression<bool> > &_valid_condition,
                            generic_problem_resolver &_resolver,
                            const logging::LoggerPtr &_logger)
      : s(_s),
	new_cost(_new_cost),
	new_choices(_new_choices),
	valid_condition(_valid_condition),
	resolver(_resolver),
	logger(_logger)
    {
    }

    bool operator()(const choice &solver, const imm::list<dep> &solved) const
    {
      for(typename imm::list<dep>::const_iterator it = solved.begin();
	  it != solved.end(); ++it)
	{
	  const dep &d(*it);
	  choice solver_with_dep(solver.copy_and_set_dep(d));

	  typename imm::map<dep, typename step::flyweight_dep_solvers>::node current_solver_set_found =
	    s.unresolved_deps.lookup(d);

	  if(current_solver_set_found.isValid())
	    {
	      const typename step::dep_solvers &current_solvers(current_solver_set_found.getVal().second);

	      typename step::dep_solvers new_solvers(current_solvers);

	      // Sanity-check: verify that the solver really
	      // resides in the solver set of this dependency.
	      const typename step::solver_information *solver_found =
		new_solvers.lookup_solver_information(solver_with_dep);

	      if(solver_found == NULL)
		LOG_ERROR(logger, "Internal error: in step " << s.step_num
			  << ", the solver " << solver
			  << " is claimed to be a solver of " << d
			  << " but does not appear in its solvers list.");
	      else
		{
		  const typename step::solver_information &old_inf =
		    *solver_found;

		  // Don't do anything if the cost won't
		  // increase.  Empirically, the resolver was wasting
		  // lots of time and memory increasing costs when the
		  // solver costs hadn't actually changed.
                  if(!old_inf.get_cost().is_above_or_equal(new_cost))
                    {
                      cost updated_solver_cost =
                        cost::least_upper_bound(old_inf.get_cost(),
                                                new_cost);

		      typename step::solver_information
			new_inf(updated_solver_cost,
				new_choices,
				valid_condition,
				old_inf.get_is_deferred_listener());
		      new_solvers.set_solver_information(solver_with_dep, new_inf);

		      typename step::flyweight_dep_solvers
			memoized_new_solvers(new_solvers);
		      s.unresolved_deps.put(d, memoized_new_solvers);
		      resolver.check_solvers_cost(s, new_solvers);

		      LOG_TRACE(logger, "Increased the cost of "
				<< solver_with_dep << " to " << updated_solver_cost
				<< " in the solvers list of "
				<< d << " in step " << s.step_num
				<< " with the reason set " << new_choices
				<< " and validity condition " << valid_condition
				<< "; new solvers list: " << new_solvers);

		    }
		}
	    }
	}

      return true;
    }
  };

  /** \brief Increase the cost of a solver (for instance, because a
   *  new incipient promotion was detected).
   */
  void increase_solver_cost(step &s,
                            const promotion &p,
                            const choice &solver)
  {
    LOG_TRACE(logger, "Applying the promotion " << p
	      << " to the solver " << solver
	      << " in the step " << s.step_num);
    const cost &new_cost(p.get_cost());
    // There are really two cases here: either the cost was increased
    // to the point that the solver should be ejected, or the cost
    // should just be bumped up a bit.  Either way, we might end up
    // changing the cost of the whole step.
    if(is_discard_cost(new_cost + s.base_step_cost))
      {
	// \todo this throws away information about whether we're at
	// the already-generated structural level.  This isn't that
	// important, except that it means that the already-generated
	// level will become fairly meaningless.  I could store this
	// information at the cost of a little extra space in every
	// solver cell, or I could get rid of the already-generated
	// level (just use the conflict level), or I could not worry
	// about it.

	strike_choice(s, solver, p.get_choices());
      }
    else
      {
	choice_set new_choices(p.get_choices());
	new_choices.remove_overlaps(solver);

	const cwidget::util::ref_ptr<expression<bool> > &
	  valid_condition(p.get_valid_condition());

	LOG_TRACE(logger, "Increasing the cost of " << solver
		  << " to " << new_cost << " in all solver lists in step "
		  << s.step_num << " with the reason set " << new_choices);

	do_increase_solver_cost
	  do_increase_solver_cost_f(s,
                                    new_cost,
                                    new_choices,
                                    valid_condition,
                                    *this,
                                    logger);

	s.deps_solved_by_choice.for_each_key_contained_in(solver,
							  do_increase_solver_cost_f);
      }
  }

  /** \brief Increase the cost of each solver that it's
   *  applied to.
   */
  class do_increase_solver_cost_everywhere
  {
    generic_problem_resolver &r;
    const choice &solver;
    const promotion &p;

  public:
    do_increase_solver_cost_everywhere(generic_problem_resolver &_r,
					  const choice &_solver,
					  const promotion &_p)
      : r(_r), solver(_solver), p(_p)
    {
    }

    bool operator()(const choice &c,
		    typename search_graph::choice_mapping_type tp,
		    int step_num) const
    {
      step &s(r.graph.get_step(step_num));

      switch(tp)
	{
	case search_graph::choice_mapping_solver:
	  r.increase_solver_cost(s, p, solver);
	  break;

	case search_graph::choice_mapping_action:
	  r.increase_effective_step_cost(s, p);
	}

      return true;
    }
  };

  /** \brief Increase the cost of a solver everywhere it
   *  appears with the same dependency: that is, both in solver lists
   *  and in action sets.
   */
  void increase_solver_cost_everywhere_with_dep(const choice &solver,
						   const promotion &p)
  {
    LOG_TRACE(logger, "Increasing the cost of " << solver
	      << " according to the promotion " << p
	      << " in all active steps.");
    do_increase_solver_cost_everywhere
      increase_solver_cost_everywhere_f(*this, solver, p);

    graph.for_each_step_related_to_choice_with_dep(solver,
						   solver.get_dep(),
						   increase_solver_cost_everywhere_f);
  }

  class do_find_promotions_for_solver
  {
    generic_problem_resolver &r;
    step &s;

  public:
    do_find_promotions_for_solver(generic_problem_resolver &_r,
				  step &_s)
      : r(_r), s(_s)
    {
    }

    bool operator()(const std::pair<choice, typename step::solver_information> &p) const
    {
      r.find_promotions_for_solver(s, p.first);
      return true;
    }
  };

  /** \brief Check for promotions at each solver of the given
   *  dependency.
   */
  void find_promotions_for_dep_solvers(step &s, const dep &d)
  {
    typename imm::map<dep, typename step::flyweight_dep_solvers>::node found =
      s.unresolved_deps.lookup(d);

    if(found.isValid())
      {
	do_find_promotions_for_solver find_promotions_f(*this, s);
	const typename step::dep_solvers &dep_solvers(found.getVal().second);
	dep_solvers.for_each_solver(find_promotions_f);
      }
  }

  /** \brief Add a single unresolved dependency to a step.
   *
   *  This routine does not check that the dependency is really
   *  unresolved.
   */
  void add_unresolved_dep(step &s, const dep &d)
  {
    if(s.unresolved_deps.domain_contains(d))
      {
	LOG_TRACE(logger, "The dependency " << d << " is already unresolved in step "
		  << s.step_num << ", not adding it again.");
	return;
      }

    LOG_TRACE(logger, "Marking the dependency " << d << " as unresolved in step "
	      << s.step_num);

    // Build up a list of the possible solvers of the dependency.
    typename step::dep_solvers solvers;
    for(typename dep::solver_iterator si = d.solvers_begin();
	!si.end(); ++si)
      add_solver(s, solvers, d, choice::make_install_version(*si, d, -1));

    // If it isn't a soft dependency, consider removing the source to
    // fix it.
    if(!d.is_soft())
      {
	version source(d.get_source());
	package source_pkg(source.get_package());

	for(typename package::version_iterator vi = source_pkg.versions_begin();
	    !vi.end(); ++vi)
	  {
	    version ver(*vi);

	    if(ver != source)
	      add_solver(s, solvers, d,
			 choice::make_install_version_from_dep_source(ver, d, -1));
	  }
      }
    else
      add_solver(s, solvers, d,
		 choice::make_break_soft_dep(d, -1));

    typename step::flyweight_dep_solvers
      memoized_solvers(solvers);
    s.unresolved_deps.put(d, memoized_solvers);
    LOG_TRACE(logger, "Marked the dependency " << d
	      << " as unresolved in step " << s.step_num
	      << " with solver list " << solvers);

    const int num_solvers = solvers.get_solvers_size();
    s.unresolved_deps_by_num_solvers.insert(std::make_pair(num_solvers, d));

    find_promotions_for_dep_solvers(s, d);
    check_solvers_cost(s, solvers);
  }

  /** \brief Find all the dependencies that are unresolved in step s
   *  and that involve c in some way, then add them to the unresolved
   *  set.
   *
   *  c must already be contained in s.actions.
   */
  void add_new_unresolved_deps(step &s, const choice &c)
  {
    switch(c.get_type())
      {
      case choice::install_version:
	{
	  choice_set_installation
	    test_installation(s.actions, initial_state);

	  version new_version = c.get_ver();
	  version old_version = initial_state.version_of(new_version.get_package());

	  LOG_TRACE(logger, "Finding new unresolved dependencies in step "
		    << s.step_num << " caused by replacing "
		    << old_version << " with " << new_version << ".");

	  // Check reverse deps of the old version.
	  for(typename version::revdep_iterator rdi = old_version.revdeps_begin();
	      !rdi.end(); ++rdi)
	    {
	      dep rd(*rdi);

	      if(rd.broken_under(test_installation))
		{
		  if(!(rd.is_soft() &&
		       s.actions.contains(choice::make_break_soft_dep(rd, -1))))
		    add_unresolved_dep(s, rd);
		}
	    }

	  for(typename version::revdep_iterator rdi = new_version.revdeps_begin();
	      !rdi.end(); ++rdi)
	    {
	      dep rd(*rdi);

	      if(rd.broken_under(test_installation))
		{
		  if(!(rd.is_soft() &&
		       s.actions.contains(choice::make_break_soft_dep(rd, -1))))
		    add_unresolved_dep(s, rd);
		}
	    }

	  for(typename version::dep_iterator di = new_version.deps_begin();
	      !di.end(); ++di)
	    {
	      dep d(*di);

	      if(d.broken_under(test_installation))
		// Note: no need to test if this was chosen to be
		// broken, because it can't possibly have been
		// broken until now.
		add_unresolved_dep(s, d);
	    }
	}

	break;

      case choice::break_soft_dep:
	// Leaving a soft dependency broken never causes any other
	// dependency to become broken.
	break;
      }
  }

  /** \brief Find incipient promotions for the given step that contain
   *  the given choice.
   */
  void find_new_incipient_promotions(step &s,
				     const choice &c)
  {
    boost::unordered_map<choice, promotion> output;

    discards_blessed discards_blessed_p(s.is_blessed_solution,
					*this,
					s);
    promotions.find_highest_incipient_promotions_containing(s.actions,
							    c,
							    s.deps_solved_by_choice,
							    discards_blessed_p,
							    output);

    for(typename boost::unordered_map<choice, promotion>::const_iterator it =
	  output.begin(); it != output.end(); ++it)
      increase_solver_cost(s, it->second, it->first);
  }

  /** \brief Update a step's score to compute its successor, given
   *  that the given choice was added to its action set.
   */
  void extend_score_to_new_step(step &s, const choice &c) const
  {
    s.action_score += weights.step_score;

    switch(c.get_type())
      {
      case choice::break_soft_dep:
	s.action_score += weights.unfixed_soft_score;
	break;

      case choice::install_version:
	{
	  const version &ver(c.get_ver());
	  const version old_ver(initial_state.version_of(ver.get_package()));
	  const int new_score = weights.version_scores[ver.get_id()];
	  const int old_score = weights.version_scores[old_ver.get_id()];

	  LOG_TRACE(logger, "Modifying the score of step "
		    << s.step_num << " by "
		    << std::showpos << (new_score - old_score)
		    << std::noshowpos << " to account for the replacement of "
		    << old_ver << " (score " << old_score << ") by "
		    << ver << " (score " << new_score << ")");

	  s.action_score += new_score;
	  s.action_score -= old_score;

	  // Look for joint score constraints triggered by adding this
	  // choice.
	  const typename solution_weights<PackageUniverse>::joint_score_set::const_iterator
	    joint_scores_found = weights.get_joint_scores().find(c);

	  if(joint_scores_found != weights.get_joint_scores().end())
	    {
	      typedef typename solution_weights<PackageUniverse>::joint_score joint_score;
	      for(typename std::vector<joint_score>::const_iterator it =
		    joint_scores_found->second.begin();
		  it != joint_scores_found->second.end(); ++it)
		{
		  if(s.actions.contains(it->get_choices()))
		    {
		      LOG_TRACE(logger, "Adjusting the score of "
				<< s.step_num << " by "
				<< std::showpos << it->get_score()
				<< std::noshowpos
				<< " for a joint score constraint on "
				<< it->get_choices());
		      s.action_score += it->get_score();
		    }
		}
	    }
	}
      }

    s.score = s.action_score + s.unresolved_deps.size() * weights.broken_score;
    if(s.unresolved_deps.empty())
      {
	LOG_TRACE(logger, "Modifying the score of step "
		  << s.step_num << " by "
		  << std::showpos << weights.full_solution_score << std::noshowpos
		  << " because it is a full solution.");
	s.score += weights.full_solution_score;
      }
    LOG_TRACE(logger, "Updated the score of step "
	      << s.step_num << " to " << s.score);
  }

  /** \brief Fill in a new step with a successor of the parent step
   *         generated by performing the given action.
   *
   *  \param parent      The parent step.
   *  \param output      The new step that will be filled in.
   *  \param c_original  The choice that was used to create this step.
   */
  void generate_single_successor(const step &parent,
				 step &output,
				 const choice &c_original)
  {
    // First, verify that there are no un-discharged promotions in the
    // parent step.  Normally this won't be true, since we check the
    // parent step right before generating its successors.  However,
    // sometimes one of the successors will output a promotion.  If
    // that promotion hits the parent step, then we have a problem:
    // the child will be generated with the old cost (since the parent
    // doesn't have the promotion applied yet), but its position in
    // the queue of pending promotions will be after that promotion
    // (since it's set when the child is created).
    //
    // There are two ways I could have solved this:
    //
    //   (1) Set the child's position in the promotion queue to the
    //       parent's position instead of based on when it was
    //       created.  (this would be the location of the queue tail
    //       when the parent started generating children)
    //   (2) Update the parent between generating successors.
    //
    // Both should be fairly cheap, but I went with (2): if there are
    // no promotions, it's very cheap to find that out, and if there
    // are promotions, it's cheaper to apply them once in the parent
    // than applying them to each of the children.
    check_for_new_promotions(parent.step_num);


    choice c(c_original);
    c.set_id(parent.actions.size());

    // The intrinsic operation just includes the solver itself.  The
    // intrinsic *promotion*, on the other hand, includes information
    // about whether it's deferred.  The intrinsic promotion isn't
    // taken into account right here; later, we'll recompute the
    // effective cost from scratch and include the intrinsic
    // promotion when we do.
    cost c_cost = get_intrinsic_solver_cost(c);

    // Copy all the state information over so we can work in-place on
    // the output set.
    output.actions = parent.actions;
    // A brief note on scores.
    //
    // These values are wrong.  They will be corrected at the bottom
    // of this routine.  However, they will be referenced before that,
    // in order to check whether this step exists in "pending" yet.
    // It doesn't, so it won't be found no matter what we include here
    // (which is as it should be).  But by including dummy values, we
    // avoid reading uninitialized memory, which means that (a) any
    // problems that do occur will be deterministic, and (b) valgrind
    // won't spit out false positives.
    output.action_score = parent.action_score;
    output.score = parent.score;
    output.reason = c;
    output.base_step_cost = c_cost + parent.base_step_cost;
    output.effective_step_cost = cost_limits::minimum_cost;
    output.final_step_cost = output.effective_step_cost + output.base_step_cost;
    output.is_deferred_listener = build_is_deferred_listener(c);
    output.unresolved_deps = parent.unresolved_deps;
    output.unresolved_deps_by_num_solvers = parent.unresolved_deps_by_num_solvers;
    output.deps_solved_by_choice = parent.deps_solved_by_choice;
    output.forbidden_versions = parent.forbidden_versions;
    output.promotion_queue_location = promotion_queue_tail;


    LOG_TRACE(logger, "Generating a successor to step " << parent.step_num
	      << " for the action " << c
	      << " with intrinsic cost " << c_cost
	      << " and outputting to step " << output.step_num);

    // We need a dependency to correctly generate the deferral
    // information.
    if(c.get_type() == choice::install_version && !c.get_has_dep())
      LOG_ERROR(logger, "No dependency attached to the choice " << c
		<< " used to generate step " << output.step_num
		<< ", expect trouble ahead.");

    // Insert the new choice into the output list of choices.  This
    // will be used below (in steps 3, 4, 5, 6 and 7).
    output.actions.insert_or_narrow(c);

    // Rescan the solvers list to find the new cost.  I
    // could also handle this incrementally using the powers of
    // immset.  However, I think it's probably not worth it: doing
    // that would make this case cheaper, but increasing a solver in
    // an existing step would become pricier (and we do that a lot
    // more often).  On the other hand: if combining operations
    // becomes more sophisticated, it might be necessary to always do
    // it incrementally *anyway*.
    //
    // Of course, this must be performed after we update the action
    // set (above).
    recompute_effective_step_cost(output);

    // Don't do this, because it isn't necessary and will cause
    // trouble.
    //
    // By definition, this step is a child of a step where c occurs as
    // a solver, hence it will be contained in the tree trace rooted
    // at the step where c was introduced as a solver.  If we also
    // added c as an action here, we would have to somehow guard in
    // the search graph code against traversing this subtree twice,
    // which is a bit of a nuisance and not necessary.
    //
    //  graph.bind_choice(c, output.step_num, c.get_dep());


    // 1. Find the list of solved dependencies and drop each one from
    // the map of unresolved dependencies, and from the set sorted by
    // the number of solvers.
    //
    // Note that some of these dependencies might not be unresolved
    // any more.
    drop_deps_solved_by(c, output);

    // 2. Drop the version from the reverse index of choices to solved
    // dependencies.
    output.deps_solved_by_choice.erase(c);

    // 3. For any versions that are structurally forbidden by this
    // choice, locate those choices in the reverse index, strike them
    // from the corresponding solver lists, add forcing reasons to the
    // corresponding force list, and drop them from the reverse index.
    //
    // 4. Add solvers of the dep, if it was a from-source choice, to
    // the set of forbidden versions.
    strike_structurally_forbidden(output, c);

    // 5. Find newly unsatisfied dependencies.  For each one that's
    // not in the unresolved map, add it to the unresolved map with
    // its solvers paired with their intrinsic costs.
    // Structurally forbidden solvers are not added to the solvers
    // list; instead, they are used to create the initial list of
    // forcing reasons.  Also, insert the dependency into the
    // by-num-solvers list and insert it into the reverse index for
    // each one of its solvers.
    //
    // This also processes the incipient promotions that are completed
    // by each solver of a dependency.  \todo If the solvers were
    // stored in a central list, the number of promotion lookups
    // required could be vastly decreased.
    add_new_unresolved_deps(output, c);

    // 6. Find incipient promotions for the new step.
    find_new_incipient_promotions(output, c);

    // Compute the new score.  Do this after everything else so
    // solutions get the expected bonus (otherwise we don't know
    // whether this step has unresolved dependencies).
    extend_score_to_new_step(output, c);

    LOG_TRACE(logger, "Generated step " << output.step_num
	      << " (" << output.actions.size() << " actions): " << output.actions << ";T" << output.final_step_cost
	      << "S" << output.score);

    if(is_discard_cost(output.final_step_cost))
      // TODO: this is wrong!  Should check for deferral, not discarding.
      ++num_deferred;

    pending.insert(output.step_num);
  }

  class do_generate_single_successor
  {
    int parent_step_num;
    generic_problem_resolver &resolver;

    bool &first;

  public:
    do_generate_single_successor(int _parent_step_num,
				 generic_problem_resolver &_resolver,
				 bool &_first)
      : parent_step_num(_parent_step_num), resolver(_resolver), first(_first)
    {
      first = true;
    }

    bool operator()(const std::pair<choice, typename step::solver_information> &solver_pair) const
    {
      if(first)
	first = false;
      else
	resolver.graph.get_last_step().is_last_child = false;

      const choice &solver(solver_pair.first);

      step &parent = resolver.graph.get_step(parent_step_num);
      step &output = resolver.graph.add_step();
      output.parent = parent.step_num;
      if(parent.first_child == -1)
	parent.first_child = output.step_num;
      output.is_last_child = true;

      // It would be nice to save a promotion lookup later by passing
      // the solver cost along here.  However, that runs into some
      // tricky issues with separating the intrinsic and extrinsic
      // operations on the solver.
      resolver.generate_single_successor(parent,
					 output,
					 solver);

      return true;
    }
  };

  /** Build the successors of a search step for the best target
   *  dependency.
   */
  void generate_successors(int step_num, std::set<package> *visited_packages)
  {
    LOG_TRACE(logger, "Generating successors for step " << step_num);

    // \todo Should thread visited_packages through all the machinery
    // above.

    step &s(graph.get_step(step_num));

    // Find the "best" unresolved dependency.
    typename imm::set<std::pair<int, dep> >::node best =
      s.unresolved_deps_by_num_solvers.get_minimum();

    if(!best.isValid())
      {
	LOG_ERROR(logger, "Internal error: can't generate successors at step "
		  << step_num << " since it has no unresolved dependencies.");
	return;
      }

    typename imm::map<dep, typename step::flyweight_dep_solvers>::node bestSolvers =
      s.unresolved_deps.lookup(best.getVal().second);

    if(!bestSolvers.isValid())
      {
	LOG_ERROR(logger, "Internal error: step " << step_num
		  << " contains the dependency " << best.getVal().second
		  << " in the list of unresolved dependencies by number of solvers, but not in the main list of unresolved dependencies.");
	return;
      }

    const typename step::dep_solvers &bestDepSolvers(bestSolvers.getVal().second);
    if(bestDepSolvers.get_solvers_size() == 0)
      LOG_ERROR(logger, "Internal error: a step containing a dependency with no solvers was not promoted to the conflict structural level.");

    LOG_TRACE(logger, "Generating successors for step " << step_num
	      << " for the dependency " << bestDepSolvers
	      << " with " << best.getVal().first << " solvers: "
	      << bestDepSolvers.dump_solvers());
    bool first_successor = false;
    do_generate_single_successor generate_successor_f(s.step_num, *this,
						      first_successor);
    bestDepSolvers.for_each_solver(generate_successor_f);
  }

  void do_log(const char *sourceName,
              int sourceLine,
              logging::log_level level,
              logging::LoggerPtr logger,
              const std::string &msg)
  {
    // HACK: Block logging to stdout if running in curses (c.f. main.cc)
    if(debug && (cwidget::rootwin == (cwidget::cwindow) NULL))
      std::cout << msg << std::endl;
  }

public:

  /** Construct a new generic_problem_resolver.
   *
   *  \param _score_score the score per "step" of a (partial) solution.  Typically negative.
   *  \param _broken_score the score to add per broken dependency of a (partial) solution.  Typically negative.
   *  \param _unfixed_soft_score the score to add per soft dependency LEFT UNFIXED.  Typically negative.
   *  \param infinity a score value that will be considered to be "infinite".  Solutions
   *  with less than -infinity points will be immediately discarded.
   *  \param _full_solution_score a bonus for goal nodes (things
   *  that solve all dependencies)
   *  \param _future_horizon  The number of steps to keep searching after finding a
   *                          solution in the hope that a better one is "just around
   *                          the corner".
   *  \param _initial_state   A set of package actions to treat as part
   *                          of the initial state (empty to just
   *                          use default versions for everything).
   *  \param _universe the universe in which we are working.
   */
  generic_problem_resolver(int _step_score, int _broken_score,
			   int _unfixed_soft_score,
			   int infinity,
			   int _full_solution_score,
                           const cost &_unfixed_soft_cost,
			   int _future_horizon,
			   const imm::map<package, version> &_initial_state,
			   const PackageUniverse &_universe)
    :logger(aptitude::Loggers::getAptitudeResolverSearch()),
     debug(false),
     graph(promotions),
     initial_state(_initial_state, _universe.get_package_count()),
     weights(_step_score, _broken_score, _unfixed_soft_score,
	     _full_solution_score, _universe.get_version_count(),
	     initial_state),
     unfixed_soft_cost(_unfixed_soft_cost),
     minimum_score(-infinity),
     future_horizon(_future_horizon),
     universe(_universe), finished(false),
     solver_executing(false), solver_cancelled(false),
     pending(step_goodness_compare(graph)),
     num_deferred(0),
     pending_future_solutions(step_goodness_compare(graph)),
     closed(),
     promotions(_universe, *this),
     promotion_queue_tail(new promotion_queue_entry(0, 0)),
     version_costs(new cost[_universe.get_version_count()])
  {
    logger->connect_message_logged(sigc::mem_fun(*this, &generic_problem_resolver::do_log));

    LOG_DEBUG(logger, "Creating new problem resolver: step_score = " << _step_score
	      << ", broken_score = " << _broken_score
	      << ", unfixed_soft_score = " << _unfixed_soft_score
	      << ", infinity = " << infinity
	      << ", full_solution_score = " << _full_solution_score
	      << ", future_horizon = " << _future_horizon
	      << ", initial_state = " << _initial_state);

    // Used for sanity-checking below.
    choice_set empty_choice_set;
    choice_set_installation empty_step(empty_choice_set,
				       initial_state);

    // Find all the broken deps.
    for(typename PackageUniverse::dep_iterator di = universe.deps_begin();
	!di.end(); ++di)
      {
	dep d(*di);

	if(!universe.is_candidate_for_initial_set(d))
	  {
	    // This test is slow and only used for logging:
	    if(logger->isEnabledFor(logging::TRACE_LEVEL))
	      {
		if(!d.broken_under(initial_state))
		  LOG_TRACE(logger, "Not using " << d
			    << " as an initially broken dependency because it is flagged as a dependency that shouldn't be in the initial set.");
	      }
	  }
	else if(d.broken_under(initial_state))
	  {
	    if(!d.broken_under(empty_step))
	      LOG_ERROR(logger, "Internal error: the dependency "
			<< d << " is claimed to be broken, but it doesn't appear to be broken in the initial state.");
	    else
	      {
		LOG_INFO(logger, "Initially broken dependency: " << d);
		initial_broken.insert(d);
	      }
	  }
      }
  }

  ~generic_problem_resolver()
  {
    delete[] version_costs;
  }

  /** \brief Get the dependencies that were initially broken in this
   *  resolver.
   *
   *  This might be different from the dependencies that are
   *  "intrinsically" broken in the universe, if there are
   *  hypothesized initial installations.
   */
  const imm::set<dep> get_initial_broken() const
  {
    return initial_broken;
  }

  const PackageUniverse &get_universe() const
  {
    return universe;
  }

  int get_step_score() {return weights.step_score;}
  int get_broken_score() {return weights.broken_score;}
  int get_unresolved_soft_dep_score() {return weights.unfixed_soft_score;}
  int get_infinity() {return -minimum_score;}
  int get_full_solution_score() {return weights.full_solution_score;}

  /** Enables or disables debugging.  Debugging is initially
   *  disabled.
   *
   *  This is a backwards-compatibility hook; in the future, the
   *  logging framework should be used to enable debugging.  This
   *  function enables all possible debug messages by setting the
   *  level for all resolver domains to TRACE.
   */
  void set_debug(bool new_debug)
  {
    if(new_debug)
      logger->setLevel(logging::TRACE_LEVEL);
    else
      logger->setLevel(logging::OFF_LEVEL);

    debug = new_debug;
  }

  /** Clears all the internal state of the solver, discards solutions,
   *  zeroes out scores.  Call this routine after changing the state
   *  of packages to avoid inconsistent results.
   */
  void reset()
  {
    finished=false;
    pending.clear();
    pending_future_solutions.clear();
    promotion_queue_tail = boost::make_shared<promotion_queue_entry>(0, 0);
    graph.clear();
    closed.clear();

    for(size_t i=0; i<universe.get_version_count(); ++i)
      weights.version_scores[i]=0;
  }

  /** \return \b true if no solutions have been examined yet.
   *  This implies that it is safe to modify package scores.
   */
  bool fresh() const
  {
    // \todo Have a Boolean flag that tracks this explicitly.
    return
      pending.empty() && pending_future_solutions.empty() &&
      closed.empty() && !finished;
  }

  /** \return \b true if the open queue is empty. */
  bool exhausted() const
  {
    return !(pending_contains_candidate() || pending_future_solutions_contains_candidate())
      && finished;
  }

  /** \return the initial state of the resolver. */
  const resolver_initial_state<PackageUniverse> &get_initial_state() const
  {
    return initial_state;
  }

  /** \brief Apply the given operation to search nodes that include
   * the given set of choices.
   *
   *  If cost has a structural level of defer_level, the promotion
   *  will be lost when the user changes the set of rejected packages.
   */
  void add_promotion(const choice_set &choices,
		     const cost &promotion_cost)
  {
    add_promotion(promotion(choices, promotion_cost));
  }

  /** Tells the resolver how highly to value a particular package
   *  version.  All scores are relative, and a higher score will
   *  result in a bias towards that version appearing in the final
   *  solution.
   */
  void set_version_score(const version &ver, int score)
  {
    eassert(ver.get_id()<universe.get_version_count());
    weights.version_scores[ver.get_id()]=score;
  }

  /** As set_version_score, but instead of replacing the current score
   *  increment it.
   */
  void add_version_score(const version &ver, int score)
  {
    eassert(ver.get_id()<universe.get_version_count());
    weights.version_scores[ver.get_id()]+=score;
  }

  /** \brief Set the cost of a version.
   *
   *  Overrides any previous cost that was set on this
   *  version.  Should not be called once dependency resolution is
   *  running, or you'll get inconsistent results.
   */
  void set_version_cost(const version &ver, const cost &c)
  {
    eassert(ver.get_id() < universe.get_version_count());
    version_costs[ver.get_id()] = c;
  }

  /** \brief Combine the given cost with the operation of
   *  the given version.
   *
   *  Should not be called once dependency resolution is running, or
   *  you'll get inconsistent results.
   */
  void modify_version_cost(const version &ver,
			      const cost &c)
  {
    eassert(ver.get_id() < universe.get_version_count());
    cost &version_cost = version_costs[ver.get_id()];

    version_cost = version_cost + c;
  }

  /** \return the score of the version ver. */
  int get_version_score(const version &ver)
  {
    eassert(ver.get_id()<universe.get_version_count());
    return weights.version_scores[ver.get_id()];
  }

  /** \brief Add a score to all solutions that install the given
   *  collection of versions.
   *
   *  Note that this does not mean "all solutions that result in the
   *  given set of versions being installed".  The versions must be
   *  newly installed in the solution; if any version is the current
   *  version of its package, this call has no effect.
   */
  void add_joint_score(const imm::set<version> &versions, int score)
  {
    weights.add_joint_score(versions, score);
  }

  /** Reject future solutions containing this version.
   */
  void reject_version(const version &ver, undo_group *undo = NULL)
  {
    approved_or_rejected_info &inf(get_approved_or_rejected_info(ver));

    if(!inf.get_rejected()->get_value())
      {
	LOG_TRACE(logger, "Rejecting " << ver);

	if(undo != NULL)
	  undo->add_item(new undo_resolver_manipulation<PackageUniverse, version>(this, ver, &generic_problem_resolver<PackageUniverse>::unreject_version));

	inf.get_rejected()->set_value(true);
	unmandate_version(ver, undo);
      }
  }

  /** Cancel any rejection of ver, allowing the resolver to once
   *  again generate solutions containing it.
   */
  void unreject_version(const version &ver, undo_group *undo = NULL)
  {
    approved_or_rejected_info &inf(get_approved_or_rejected_info(ver));

    if(inf.get_rejected()->get_value())
      {
	LOG_TRACE(logger, "Canceling the rejection of " << ver);

	if(undo != NULL)
	  undo->add_item(new undo_resolver_manipulation<PackageUniverse, version>(this, ver, &generic_problem_resolver<PackageUniverse>::reject_version));

	inf.get_rejected()->set_value(false);
      }
  }

  void mandate_version(const version &ver, undo_group *undo = NULL)
  {
    approved_or_rejected_info &inf(get_approved_or_rejected_info(ver));

    if(!inf.get_approved()->get_value())
      {
	LOG_TRACE(logger, "Mandating " << ver);

	if(undo != NULL)
	  undo->add_item(new undo_resolver_manipulation<PackageUniverse, version>(this, ver, &generic_problem_resolver<PackageUniverse>::unmandate_version));

	inf.get_approved()->set_value(true);
	unreject_version(ver, undo);
      }
  }

  void unmandate_version(const version &ver, undo_group *undo = NULL)
  {
    approved_or_rejected_info &inf(get_approved_or_rejected_info(ver));

    if(inf.get_approved()->get_value())
      {
	LOG_TRACE(logger, "Un-mandating " << ver);

	if(undo != NULL)
	  undo->add_item(new undo_resolver_manipulation<PackageUniverse, version>(this, ver, &generic_problem_resolver<PackageUniverse>::mandate_version));

	inf.get_approved()->set_value(false);
      }
  }

  /** Query whether the given version is rejected. */
  bool is_rejected(const version &ver) const
  {
    typename std::map<version, approved_or_rejected_info>::const_iterator found =
      user_approved_or_rejected_versions.find(ver);

    return
      found != user_approved_or_rejected_versions.end() &&
      found->second.get_rejected()->get_value();
  }

  /** Query whether the given version is mandated. */
  bool is_mandatory(const version &ver) const
  {
    typename std::map<version, approved_or_rejected_info>::const_iterator found =
      user_approved_or_rejected_versions.find(ver);

    return
      found != user_approved_or_rejected_versions.end() &&
      found->second.get_approved()->get_value();
  }

  /** Query whether the given dependency is hardened. */
  bool is_hardened(const dep &d) const
  {
    typename std::map<dep, approved_or_rejected_info>::const_iterator found =
      user_approved_or_rejected_broken_deps.find(d);

    return
      found != user_approved_or_rejected_broken_deps.end() &&
      found->second.get_rejected()->get_value();
  }

  /** Harden the given dependency. */
  void harden(const dep &d, undo_group *undo = NULL)
  {
    eassert(d.is_soft());

    approved_or_rejected_info &inf(get_approved_or_rejected_info(d));

    if(!inf.get_rejected()->get_value())
      {
	LOG_TRACE(logger, "Hardening " << d);

	if(undo != NULL)
	  undo->add_item(new undo_resolver_manipulation<PackageUniverse, dep>(this, d, &generic_problem_resolver<PackageUniverse>::unharden));

	inf.get_rejected()->set_value(true);
	unapprove_break(d, undo);
      }
  }

  /** Un-harden (soften?) the given dependency. */
  void unharden(const dep &d, undo_group *undo = NULL)
  {
    approved_or_rejected_info &inf(get_approved_or_rejected_info(d));

    if(inf.get_rejected()->get_value())
      {
	LOG_TRACE(logger, "Un-hardening " << d);

	if(undo != NULL)
	  undo->add_item(new undo_resolver_manipulation<PackageUniverse, dep>(this, d, &generic_problem_resolver<PackageUniverse>::harden));

	inf.get_rejected()->set_value(false);
      }
  }

  /** \return \b true if the given dependency is in the
   *  approved-broken state.
   */
  bool is_approved_broken(const dep &d) const
  {
    typename std::map<dep, approved_or_rejected_info>::const_iterator found =
      user_approved_or_rejected_broken_deps.find(d);

    return
      found != user_approved_or_rejected_broken_deps.end() &&
      found->second.get_approved()->get_value();
  }

  /** Approve the breaking of the given dependency. */
  void approve_break(const dep &d, undo_group *undo = NULL)
  {
    approved_or_rejected_info &inf(get_approved_or_rejected_info(d));

    if(!inf.get_approved()->get_value())
      {
	LOG_TRACE(logger, "Approving breaking " << d);

	if(undo != NULL)
	  undo->add_item(new undo_resolver_manipulation<PackageUniverse, dep>(this, d, &generic_problem_resolver<PackageUniverse>::unapprove_break));

	inf.get_approved()->set_value(true);
	unharden(d, undo);
      }
  }

  /** Cancel the required breaking of the given dependency. */
  void unapprove_break(const dep &d, undo_group *undo = NULL)
  {
    approved_or_rejected_info &inf(get_approved_or_rejected_info(d));

    if(inf.get_approved()->get_value())
      {
	LOG_TRACE(logger, "Un-approving breaking " << d);

	if(undo != NULL)
	  undo->add_item(new undo_resolver_manipulation<PackageUniverse, dep>(this, d, &generic_problem_resolver<PackageUniverse>::approve_break));

	inf.get_approved()->set_value(false);
      }
  }

  /** Cancel any find_next_solution call that is executing in the
   *  background.  If no such call is executing, then the next call
   *  will immediately be cancelled.
   */
  void cancel_solver()
  {
    cwidget::threads::mutex::lock l(execution_mutex);
    solver_cancelled = true;
  }

  /** Remove any pending find_next_solution cancellation. */
  void uncancel_solver()
  {
    cwidget::threads::mutex::lock l(execution_mutex);
    solver_cancelled = false;
  }

  /** Atomically read the current queue sizes of this resolver. */
  queue_counts get_counts()
  {
    maybe_update_deferred_and_counts();

    cwidget::threads::mutex::lock l(counts_mutex);
    return counts;
  }

  size_t get_num_deferred() const
  {
    return num_deferred;
  }

  /** Update the cached queue sizes. */
  void update_counts_cache()
  {
    cwidget::threads::mutex::lock l(counts_mutex);
    counts.open       = pending.size();
    counts.closed     = closed.size();
    counts.deferred   = get_num_deferred();
    counts.conflicts  = promotions.conflicts_size();
    counts.promotions = promotions.size() - counts.conflicts;
    counts.finished   = finished;
    counts.current_cost = get_current_search_cost();
  }

  /** If no resolver is running, run through the deferred list and
   *  update the counts cache.  In particular, this allows the
   *  'are-we-out-of-solutions' state to be updated immediately when
   *  something like reject_version is called.
   */
  void maybe_update_deferred_and_counts()
  {
    cwidget::threads::mutex::lock l(execution_mutex);
    if(!solver_executing)
      {
	update_counts_cache();
      }
  }

  /** \brief Returns the "current" search cost, the cost of the next
   *  solution that would be considered (or the minimum cost if
   *  pending is empty).
   */
  cost get_current_search_cost() const
  {
    if(pending.empty())
      return cost_limits::minimum_cost;
    else
      return graph.get_step(*pending.begin()).final_step_cost;
  }

private:
  /** \brief Returns \b true if the open queue contains a step that
   *  can be processed.
   */
  bool pending_contains_candidate() const
  {
    if(pending.empty())
      return false;

    const step &s = graph.get_step(*pending.begin());

    return
      !is_discard_cost(s.final_step_cost) &&
      !is_defer_cost(s.final_step_cost);
  }

  /** \brief Returns \b true if the pending future solutions queue
   *  contains a step that can be processed.
   */
  bool pending_future_solutions_contains_candidate() const
  {
    if(pending_future_solutions.empty())
      return false;

    const step &s = graph.get_step(*pending_future_solutions.begin());

    return
      !is_discard_cost(s.final_step_cost) &&
      !is_defer_cost(s.final_step_cost);
  }

  // Counts how many action hits existed in a promotion, allowing up
  // to one mismatch (which it stores).
  class count_action_hits
  {
    unsigned int &action_hits;
    boost::optional<choice> &mismatch;
    const step &s;

  public:
    count_action_hits(unsigned int &_action_hits,
		      boost::optional<choice> &_mismatch,
		      const step &_s)
      : action_hits(_action_hits),
	mismatch(_mismatch),
	s(_s)
    {
      action_hits = 0;
      mismatch.reset();
    }

    bool operator()(const choice &c) const
    {
      if(s.actions.has_contained_choice(c))
	++action_hits;
      else if(!mismatch)
	mismatch = c;
      else
	return false;

      return true;
    }
  };

  /** \brief Apply a single promotion to a single step. */
  void apply_promotion(step &s, const promotion &p)
  {
    if(s.effective_step_cost.is_above_or_equal(p.get_cost()))
      LOG_TRACE(logger, "Not applying " << p
		<< " to step " << s.step_num << ": the step cost "
		<< s.effective_step_cost << " is above the promotion cost.");
    else
      {
	LOG_TRACE(logger, "Testing the promotion " << p
		  << " against step " << s.step_num);

	unsigned int action_hits;
	boost::optional<choice> mismatch;
	if(!p.get_choices().for_each(count_action_hits(action_hits,
						       mismatch,
						       s)))
	  LOG_TRACE(logger, "Too many mismatches against " << p
		    << ", not applying it.");
	else
	  {
	    const unsigned int p_size(p.get_choices().size());
	    if(action_hits == p_size)
	      {
		LOG_TRACE(logger, "Step " << s.step_num
			  << " contains " << p << " as an active promotion.");
		increase_effective_step_cost(s, p);
	      }
	    else if(action_hits + 1 < p_size)
	      LOG_TRACE(logger, "Step " << s.step_num
			<< " does not contain " << p <<".");
	    else if(!mismatch)
	      LOG_ERROR(logger, "Internal error: found an incipient promotion with no mismatches!");
	    else if(!s.deps_solved_by_choice.contains_key(*mismatch))
	      LOG_TRACE(logger, "Step " << s.step_num
			<< " almost contains " << p
			<< " as an incipient promotion, but the choice "
			<< *mismatch << " is not a solver.");
	    else
	      {
		LOG_TRACE(logger, "Step " << s.step_num
			  << " contains " << p
			  << " as an incipient promotion for the choice "
			  << *mismatch << ".");
		increase_solver_cost(s, p, *mismatch);
	      }
	  }
      }
  }

  /** \brief Check for promotions that have been added
   *  since a step was generated, and apply them.
   *
   *  \param step_num  The index of the step to test.
   */
  void check_for_new_promotions(int step_num)
  {
    step &s = graph.get_step(step_num);

    eassert(promotion_queue_tail.get() != NULL);
    eassert(s.promotion_queue_location.get() != NULL);

    const promotion_queue_entry &current_tail = *promotion_queue_tail;
    const promotion_queue_entry &step_location = *s.promotion_queue_location;

    LOG_TRACE(logger, "The current promotion tail has index "
	      << current_tail.get_index() << " and action sum "
	      << current_tail.get_action_sum() << "; step "
	      << step_num << " points to a promotion cell with index "
	      << step_location.get_index() << " and action sum "
	      << step_location.get_action_sum() << ", for a difference of "
	      << (current_tail.get_index() - step_location.get_index())
	      << " steps and "
	      << (current_tail.get_action_sum() - step_location.get_action_sum())
	      << " actions.");

    if(promotion_queue_tail != s.promotion_queue_location)
      eassert(step_location.get_has_contents());

    // We can check for promotions either by starting with the current
    // step and looking for promotions that match it, or by looking at
    // each new promotion and checking whether it matches the current
    // step.  The cost of the first one is approximately the size of
    // the step; the cost of the second one is approximately the
    // number of choices contained in the intervening promotions.
    //
    // This isn't a perfect heuristic, but it should allow the
    // resolver to use the new promotions if there are just a handful,
    // or the solution if there are a huge number of new promotions
    // for some reason.  Somewhere in the middle it might be a little
    // inefficient since I haven't perfectly calibrated the break-even
    // point, but the idea is more to take advantage of the "few
    // promotions optimization" while avoiding its potential downside.
    const unsigned int extra_actions = current_tail.get_action_sum() - step_location.get_action_sum();
    if(extra_actions <= s.actions.size() + s.deps_solved_by_choice.size())
      {
	LOG_TRACE(logger, "Applying each new promotion to step "
		  << s.step_num << ".");
	for(const promotion_queue_entry *qEnt = &step_location;
	    qEnt->get_has_contents(); qEnt = qEnt->get_next().get())
	  apply_promotion(s, qEnt->get_promotion());

	s.promotion_queue_location = promotion_queue_tail;
      }
    else
      {
	boost::unordered_map<choice, promotion> incipient_promotions;
	maybe<promotion> non_incipient_promotion;


	promotions.find_highest_incipient_promotions(s.actions,
						     s.deps_solved_by_choice,
						     incipient_promotions,
						     non_incipient_promotion);

	if(non_incipient_promotion.get_has_value())
	  {
	    const promotion &p(non_incipient_promotion.get_value());
	    LOG_TRACE(logger, "Found a new promotion in the action set of step "
		      << step_num << ": " << p);
	    increase_effective_step_cost(s, p);
	  }

	// Note that some promotions might be generated as a result of
	// increasing costs; to be perfectly correct we should account
	// for those.  On the other hand, this might not be necessary;
	// any promotions that are generated should by definition
	// already be included in the step, correct?
	s.promotion_queue_location = promotion_queue_tail;
	for(typename boost::unordered_map<choice, promotion>::const_iterator it =
	      incipient_promotions.begin();
	    it != incipient_promotions.end(); ++it)
	  increase_solver_cost(s, it->second, it->first);
      }
  }

  /** \brief Process the given step number and generate its
   *  successors.
   */
  void process_step(int step_num, std::set<package> *visited_packages)
  {
    bool done;

    do
      {
	done = true;

	step &s = graph.get_step(step_num);

	LOG_INFO(logger, "Examining step " << step_num
		 << " (" << s.actions.size() << " actions): " << s.actions << ";T" << s.final_step_cost
		 << "S" << s.score);

	if(is_discard_cost(s.final_step_cost) ||
           is_defer_cost(s.final_step_cost))
	  {
	    LOG_ERROR(logger, "Internal error: the cost of step "
		      << s.step_num
		      << " is at an unprocessed structural level, so why is it a candidate?");
	    // Bail out.
	    return;
	  }

	check_for_new_promotions(step_num);

	sanity_check_promotions(s);

	if(is_already_seen(step_num))
	  {
	    LOG_DEBUG(logger, "Dropping already visited search node in step " << s.step_num);
	  }
	else if(irrelevant(s))
	  {
	    LOG_DEBUG(logger, "Dropping irrelevant step " << s.step_num);
	  }
	// The step might have been promoted to the defer structural level by
	// check_for_new_promotions.
	else if(is_defer_cost(s.final_step_cost))
	  {
	    LOG_DEBUG(logger, "Skipping newly deferred step " << s.step_num);
	    // Stick it back into the open queue.
	    pending.insert(step_num);
	  }
	else
	  {
	    LOG_TRACE(logger, "Processing step " << step_num);

	    closed[step_contents(s.score, s.action_score, s.actions)] =
	      step_num;

	    // If all dependencies are satisfied, we found a solution.
	    if(s.unresolved_deps.empty())
	      {
		LOG_INFO(logger, " --- Found solution at step " << s.step_num
			 << ": " << s.actions << ";T" << s.final_step_cost
			 << "S" << s.score);

		// Remember this solution, so we don't try to return it
		// again in the future.
		choice_set generalized_actions;
		for(typename choice_set::const_iterator it = s.actions.begin();
		    it != s.actions.end(); ++it)
		  generalized_actions.insert_or_narrow(it->generalize());
		promotion already_generated_promotion(generalized_actions,
						      cost_limits::already_generated_cost);
		add_promotion(step_num, already_generated_promotion);

		s.is_blessed_solution = true;
		pending_future_solutions.insert(step_num);
	      }
	    // Nope, let's go enqueue successor nodes.
	    else
	      {
		generate_successors(step_num, visited_packages);
                const step &first_child = graph.get_step(s.first_child);

		// If we enqueued *exactly* one successor, then this
		// was a forced dependency and we should process that
		// successor before returning.
		if(s.first_child != -1 && first_child.is_last_child &&
		   !is_defer_cost(first_child.final_step_cost) &&
                   !is_discard_cost(first_child.final_step_cost))
		  {
		    LOG_TRACE(logger, "Following forced dependency resolution from step "
			      << step_num << " to step " << s.first_child);
		    step_num = s.first_child;
		    // We need to remove the child from the pending
		    // queue, so that it doesn't get processed by the
		    // main loop.
		    pending.erase(s.first_child);
		    done = false;
		  }
	      }
	  }
      } while(!done);
  }

public:
  /** Try to find the "next" solution: remove partial solutions from
   *  the open queue and place them in the closed queue until one of
   *  the following occurs:
   *
   *   - The number of broken dependencies drops to 0, in which case
   *     there is much rejoicing and we return successfully.
   *
   *   - The upper limit on the number of steps to perform is exceeded,
   *     in which case we just give up and report failure.  (this is a
   *     guard against exponential blowup)
   *
   *   - We run out of potential solutions to try; failure.
   *
   *  \param max_steps the maximum number of solutions to test.
   *
   *  \param visited_packages
   *           if not NULL, each package that influences the
   *           resolver's choices will be placed here.
   *
   *  \return a solution that fixes all broken dependencies
   *
   * \throws NoMoreSolutions if the potential solution list is exhausted.
   * \throws NoMoreTime if no solution is found within max_steps steps.
   *
   *  \todo when throwing NoMoreSolutions or NoMoreTime, maybe we
   *        should include the "least broken" solution seen.
   */
  solution find_next_solution(int max_steps,
			      std::set<package> *visited_packages)
  {
    // This object is responsible for managing the instance variables
    // that control threaded operation: it sets solver_executing when
    // it is created and clears both solver_executing and
    // solver_cancelled when it is destroyed.
    instance_tracker t(*this);
 

    // Counter for checking how long we've been running and for
    // debugging (see below).
    int odometer = 0;

    // Counter for how many "future" steps are left.
    //
    // Because this is a local variable and not a class member, the
    // future horizon will restart each time this routine is called.
    // But since we always run it out when we find a solution, the
    // worst thing that can happen is that we search a little too much
    // if there are lots of solutions near each other.  If this
    // becomes a practical problem, it shouldn't be too hard to
    // implement better behavior; the most difficult thing will be
    // defining the best semantics for it.  Another possibility would
    // be to always return the first "future" solution that we find.
    int most_future_solution_steps = 0;

    if(finished)
      throw NoMoreSolutions();

    // If nothing has been processed, then we need to start a new
    // search.
    if(pending.empty() && pending_future_solutions.empty() && closed.empty())
      {
	LOG_INFO(logger, "Starting a new search.");

	step &root = graph.add_step();
	root.action_score = 0;
	root.score = initial_broken.size() * weights.broken_score;
	if(initial_broken.empty())
	  root.score += weights.full_solution_score;

	root.base_step_cost = cost_limits::minimum_cost;
	root.effective_step_cost = cost_limits::minimum_cost;
	root.final_step_cost = cost_limits::minimum_cost;
	root.promotion_queue_location = promotion_queue_tail;

	for(typename imm::set<dep>::const_iterator it = initial_broken.begin();
	    it != initial_broken.end(); ++it)
	  add_unresolved_dep(root, *it);

	LOG_TRACE(logger, "Inserting the root at step " << root.step_num
		  << " with cost " << root.final_step_cost);
	pending.insert(root.step_num);
      }

    while(max_steps > 0 &&
	  pending_contains_candidate() &&
	  most_future_solution_steps <= future_horizon)
      {
	if(most_future_solution_steps > 0)
	  LOG_TRACE(logger, "Speculative \"future\" resolver tick ("
		    << most_future_solution_steps << "/" << future_horizon << ").");

	// Threaded operation: check whether we have been cancelled.
	{
	  cwidget::threads::mutex::lock l(execution_mutex);
	  if(solver_cancelled)
	    throw InterruptedException(odometer);
	}

	update_counts_cache();


	int curr_step_num = *pending.begin();
	pending.erase(pending.begin());

	++odometer;

	process_step(curr_step_num, visited_packages);

	// Keep track of the "future horizon".  If we haven't found a
	// solution, we aren't using those steps up at all.
	// Otherwise, we count up until we hit 50.
	if(pending_future_solutions_contains_candidate())
	  ++most_future_solution_steps;
	else
	  most_future_solution_steps = 0;

	LOG_TRACE(logger, "Done generating successors.");

	--max_steps;


	// Propagate any new promotions that we discovered up the
	// search tree.
	graph.run_scheduled_promotion_propagations(promotion_adder(*this));
	process_pending_promotions();
      }

    if(logger->isEnabledFor(logging::TRACE_LEVEL))
      {
	if(most_future_solution_steps > future_horizon)
	  LOG_TRACE(logger, "Done examining future steps for a better solution.");
	else if(!pending_contains_candidate())
	  LOG_TRACE(logger, "Exhausted all search branches.");
	else
	  LOG_TRACE(logger, "Ran out of time.");
      }

    if(pending_future_solutions_contains_candidate())
      {
	int best_future_solution = *pending_future_solutions.begin();
	step &best_future_solution_step = graph.get_step(best_future_solution);
	if(!is_defer_cost(best_future_solution_step.final_step_cost) &&
           !is_discard_cost(best_future_solution_step.final_step_cost))
	  {
	    sanity_check_not_deferred(best_future_solution_step);

	    solution rval(best_future_solution_step.actions,
			  initial_state,
			  best_future_solution_step.score,
			  best_future_solution_step.final_step_cost);

	    LOG_INFO(logger, " *** Converged after " << odometer << " steps.");

	    LOG_INFO(logger, " *** open: " << pending.size()
		     << "; closed: " << closed.size()
		     << "; promotions: " << promotions.size()
		     << "; deferred: " << get_num_deferred());

	    LOG_INFO(logger, "--- Returning the future solution "
		     << rval << " from step " << best_future_solution);

	    pending_future_solutions.erase(pending_future_solutions.begin());

	    return rval;
	  }
      }

    // Oh no, we either ran out of solutions or ran out of steps.

    if(pending_contains_candidate())
      {
	LOG_INFO(logger, " *** Out of time after " << odometer << " steps.");
	throw NoMoreTime();
      }

    finished=true;

    update_counts_cache();

    LOG_INFO(logger, " *** Out of solutions after " << odometer << " steps.");
    LOG_INFO(logger ,
	     " *** open: " << pending.size()
	     << "; closed: " << closed.size()
	     << "; promotions: " << promotions.size()
	     << "; deferred: " << get_num_deferred());

    throw NoMoreSolutions();
  }

  void dump_scores(std::ostream &out)
  {
    out << "{" << std::endl;
    for(typename PackageUniverse::package_iterator i=universe.packages_begin();
	!i.end(); ++i)
      {
	bool any_modified=false;

	for(typename PackageUniverse::package::version_iterator j=(*i).versions_begin();
	    !j.end(); ++j)
	  if(weights.version_scores[(*j).get_id()]!=0)
	    any_modified=true;

	if(any_modified)
	  {
	    out << "  SCORE " << (*i).get_name() << " <";

	    for(typename PackageUniverse::package::version_iterator j=(*i).versions_begin();
		!j.end(); ++j)
	      if(weights.version_scores[(*j).get_id()]!=0)
		out << " " << (*j).get_name() << " " << weights.version_scores[(*j).get_id()];

	    out << " >" << std::endl;
	  }
      }

    for(typename std::vector<std::pair<imm::set<version>, int> >::const_iterator it =
	  weights.get_joint_scores_list().begin();
	it != weights.get_joint_scores_list().end(); ++it)
      {
	out << "  SCORE { ";
	for(typename imm::set<version>::const_iterator vIt = it->first.begin();
	    vIt != it->first.end(); ++vIt)
	  {
	    out << *vIt << " ";
	  }
	out << "} " << it->second << std::endl;
      }

    out << "}" << std::endl;
  }
};

#endif
