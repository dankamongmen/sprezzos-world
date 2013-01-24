/** \file search_graph.h */     // -*-c++-*-


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

#ifndef SEARCH_GRAPH_H
#define SEARCH_GRAPH_H

#include <loggers.h>

#include "choice.h"
#include "choice_indexed_map.h"
#include "choice_set.h"
#include "promotion_set.h"
#include "solution.h"
#include "cost.h"
#include "cost_limits.h"

#include <generic/util/compare3.h>
#include <generic/util/immlist.h>
#include <generic/util/immset.h>

#include <boost/flyweight.hpp>
#include <boost/flyweight/hashed_factory.hpp>
#include <boost/functional/hash.hpp>
#include <boost/optional.hpp>
#include <boost/shared_ptr.hpp>

// solver_information and dep_solvers are top-level declarations so
// they can easily get an operator<< that works.

/** \brief Information about a single solver of a dependency.
 *
 *  The solver itself is not stored here; this just tracks
 *  metadata.
 */
template<typename PackageUniverse>
class generic_solver_information
{
public:
  typedef generic_choice<PackageUniverse> choice;
  typedef generic_choice_set<PackageUniverse> choice_set;

private:
  class combine_reason_hashes
  {
    std::size_t &target;
  public:
    combine_reason_hashes(std::size_t &_target)
      : target(_target)
    {
    }

    bool operator()(const choice &c) const
    {
      boost::hash_combine(target, c);

      return true;
    }
  };

  class choice_set_with_hash
  {
    choice_set choices;
    std::size_t hash;

    static std::size_t get_choice_set_hash(const choice_set &choices)
    {
      std::size_t rval = 0;
      choices.for_each(combine_reason_hashes(rval));

      return rval;
    }

  public:
    choice_set_with_hash(const choice_set &_choices)
      : choices(_choices), hash(get_choice_set_hash(choices))
    {
    }

    bool operator==(const choice_set_with_hash &other) const
    {
      return hash == other.hash && choices == other.choices;
    }

    const choice_set &get_choices() const { return choices; }
    std::size_t get_hash() const { return hash; }
  };

  class hash_choice_set_with_hash
  {
  public:
    std::size_t operator()(const choice_set_with_hash &c) const
    {
      return c.get_hash();
    }
  };

  struct foo
  {
    template<typename X>
    struct apply
    {
      typedef std::allocator<X> type;
    };
  };

  // Contrary to the Boost documentation, there are no default
  // arguments to hashed_factory, so I have to write out the default
  // values of the second two arguments.
  typedef boost::flyweight<choice_set_with_hash,
			   boost::flyweights::hashed_factory<hash_choice_set_with_hash> >
  choice_set_flyweight;

  cost t_op;
  choice_set_flyweight reasons;
  cwidget::util::ref_ptr<expression<bool> > cost_valid;
  cwidget::util::ref_ptr<expression_box<bool> > is_deferred_listener;

public:
  generic_solver_information()
    : t_op(),
      reasons(),
      cost_valid(),
      is_deferred_listener()
  {
  }

  /** \brief Create a new solver_information.
   *
   *  \param _t_op    The cost of the associated solver.
   *  \param _reason  The reasons for the solver's cost (other than
   *                  the solver itself).
   *  \param _cost valid
   *                  A pure expression indicating whether the cost of
   *                  this solver is valid.  Used to create promotions.
   *  \param _is_deferred_listener
   *                  A side-effecting expression whose sub-expression
   *                  is true exactly when this solver violates a
   *                  user-imposed constraint.  A reference is stored
   *                  here to ensure that the expression remains alive
   *                  while this solver exists.
   */
  generic_solver_information(const cost &_t_op,
			     const choice_set &_reasons,
			     const cwidget::util::ref_ptr<expression<bool> > &_cost_valid,
			     const cwidget::util::ref_ptr<expression_box<bool> > &_is_deferred_listener)
    : t_op(_t_op), reasons(_reasons),
      cost_valid(_cost_valid),
      is_deferred_listener(_is_deferred_listener)
  {
  }

  /** \brief Retrieve the cost of the associated solver. */
  const cost &get_cost() const { return t_op; }

  /** \brief Retrieve the reason that this solver has the cost
   *  that it does.
   */
  const choice_set &get_reasons() const { return reasons.get().get_choices(); }

  /** \brief Retrieve an expression that returns whether the
   *  cost is valid (if true, the cost is always valid).
   */
  const cwidget::util::ref_ptr<expression<bool> > &
  get_cost_valid() const
  {
    return cost_valid;
  }

  /** \brief Retrieve the listener that tracks whether the solver
   *  is deferred.
   */
  const cwidget::util::ref_ptr<expression_box<bool> > &
  get_is_deferred_listener() const
  {
    return is_deferred_listener;
  }

  /** \brief Retrieve an expression that returns whether the
   *  solver is deferred.
   */
  cwidget::util::ref_ptr<expression<bool> >
  get_is_deferred() const
  {
    if(is_deferred_listener.valid())
      return is_deferred_listener->get_child();
    else
      return cwidget::util::ref_ptr<expression<bool> >();
  }

  std::size_t get_hash_value() const
  {
    std::size_t rval = 0;
    boost::hash_combine(rval, cost_valid.unsafe_get_ref());
    boost::hash_combine(rval, is_deferred_listener.unsafe_get_ref());
    boost::hash_combine(rval, t_op);
    boost::hash_combine(rval, reasons.get().get_hash());

    return rval;
  }

  /** \brief Compare two solver_information objects.
   *
   *  solver_informations are compared according to their fields.
   *  This comparison is used to memoize solver_information objects;
   *  the resolver is careful to ensure that solvers which can be
   *  merged have the same deferred listeners and cost-valid
   *  expressions.
   */
  int compare(const generic_solver_information &other) const
  {
    if(cost_valid < other.cost_valid)
      return -1;
    else if(other.cost_valid < cost_valid)
      return -1;
    else if(is_deferred_listener < other.is_deferred_listener)
      return -1;
    else if(other.is_deferred_listener < is_deferred_listener)
      return 1;
    else
      {
	const int cost_compare =
	  aptitude::util::compare3<cost>(t_op, other.t_op);

	if(cost_compare != 0)
	  return cost_compare;
	else
	  return aptitude::util::compare3<choice_set>(reasons.get().get_choices(), other.reasons.get().get_choices());
      }
  }

  bool operator==(const generic_solver_information &other) const
  {
    return compare(other) == 0;
  }

  bool operator<(const generic_solver_information &other) const
  {
    return compare(other) < 0;
  }
};

template<typename PackageUniverse>
std::size_t hash_value(const generic_solver_information<PackageUniverse> &inf)
{
  return inf.get_hash_value();
}

template<typename PackageUniverse> class generic_dep_solvers;
template<typename PackageUniverse>
std::ostream &operator<<(std::ostream &out, const generic_dep_solvers<PackageUniverse> &solvers);

template<typename PackageUniverse> class generic_dump_solvers;
template<typename PackageUniverse>
std::ostream &operator<<(std::ostream &out, const generic_dump_solvers<PackageUniverse> &solvers);

template<typename PackageUniverse>
class generic_dump_solvers
{
  typedef generic_choice<PackageUniverse> choice;
  typedef generic_solver_information<PackageUniverse> solver_information;

  const std::vector<std::pair<choice, solver_information> > &solvers;

  friend std::ostream &operator<<<PackageUniverse>(std::ostream &out, const generic_dump_solvers &);
  friend class generic_dep_solvers<PackageUniverse>;

  generic_dump_solvers(const std::vector<std::pair<choice, solver_information> > &_solvers)
    : solvers(_solvers)
  {
  }
};

template<typename PackageUniverse>
inline std::ostream &operator<<(std::ostream &out, const generic_dump_solvers<PackageUniverse> &dump_solvers)
{
  typedef generic_choice<PackageUniverse> choice;
  typedef generic_solver_information<PackageUniverse> solver_information;

  out << "{";
  for(typename std::vector<std::pair<choice, solver_information> >::const_iterator
	it = dump_solvers.solvers.begin(); it != dump_solvers.solvers.end(); ++it)
    {
      if(it != dump_solvers.solvers.begin())
	out << ", ";

      out << it->first << " -> " << it->second;
    }

  out << "}";

  return out;
}

/** \brief A structure that tracks the state of the solvers of a
 *  dependency.
 */
template<typename PackageUniverse>
class generic_dep_solvers
{
public:
  typedef generic_choice<PackageUniverse> choice;
  typedef generic_solver_information<PackageUniverse> solver_information;

private:
  typedef std::vector<std::pair<choice, solver_information> > solvers_list;
  solvers_list solvers;
  imm::list<choice> structural_reasons;
  mutable std::size_t hash_cache;
  mutable bool hash_dirty;

  friend std::ostream &operator<<<PackageUniverse>(std::ostream &out, const generic_dep_solvers &);

  class hash_choices
  {
    std::size_t &hash_val;
  public:
    hash_choices(std::size_t &_hash_val)
      : hash_val(_hash_val)
    {
    }

    bool operator()(const std::pair<choice, solver_information> &p) const
    {
      boost::hash_combine(hash_val, p);

      return true;
    }
  };

  class compare_by_solver
  {
  public:
    bool operator()(const std::pair<choice, solver_information> &p1,
		    const std::pair<choice, solver_information> &p2) const
    {
      return p1.first < p2.first;
    }

    bool operator()(const std::pair<choice, solver_information> &p1,
		    const choice &c2) const
    {
      return p1.first < c2;
    }

    bool operator()(const choice &c1,
		    const std::pair<choice, solver_information> &p2) const
    {
      return c1 < p2.first;
    }

    bool operator()(const choice &c1,
		    const choice &c2) const
    {
      return c1 < c2;
    }
  };

  class find_solver
  {
    const choice &solver;

  public:
    find_solver(const choice &_solver)
      : solver(_solver)
    {
    }

    bool operator()(const std::pair<choice, solver_information> &p) const
    {
      return p.first == solver;
    }
  };

public:
  generic_dep_solvers()
    : hash_dirty(true)
  {
  }

  generic_dep_solvers(const std::vector<std::pair<choice, solver_information> > &_solvers,
		      const imm::list<choice> &_structural_reasons)
    : solvers(_solvers), structural_reasons(_structural_reasons),
      hash_dirty(true)
  {
    std::sort(solvers.begin(), solvers.end(), compare_by_solver());
  }

  std::size_t get_hash_value() const
  {
    if(hash_dirty)
      {
	hash_cache = 0;
	boost::hash_combine(hash_cache, solvers);
	// Try to avoid any confusion caused by mixing the structural
	// reasons with the solvers.
	boost::hash_combine(hash_cache, 987654321);
	for(typename imm::list<choice>::const_iterator it = structural_reasons.begin();
	    it != structural_reasons.end(); ++it)
	  boost::hash_combine(hash_cache, *it);

	hash_dirty = false;
      }

    return hash_cache;
  }

  /** \brief Manipulators. */

  // @{
  void add_structural_reason(const choice &c)
  {
    hash_dirty = true;
    structural_reasons.push_front(c);
  }

  bool set_solver_information(const choice &c, const solver_information &inf)
  {
    hash_dirty = true;
    const std::pair<typename solvers_list::iterator, typename solvers_list::iterator>
      insert_loc = std::equal_range(solvers.begin(), solvers.end(), c, compare_by_solver());

    if(insert_loc.first == insert_loc.second)
      {
	solvers.insert(insert_loc.first, std::make_pair(c, inf));
	return true;
      }
    else
      {
	insert_loc.first->second = inf;
	return false;
      }
  }

  bool remove_solver(const choice &c)
  {
    hash_dirty = true;
    typename solvers_list::iterator new_end(std::remove_if(solvers.begin(), solvers.end(),
							   find_solver(c)));

    if(new_end == solvers.end())
      return false;
    else
      {
	solvers.erase(new_end, solvers.end());
	return true;
      }
  }
  // @}

  /** \brief Look up information about the given solver.
   *
   *  \return a pointer to the solver's information, or \b NULL if it
   *  is not in this set of solvers.
   */
  const solver_information *
  lookup_solver_information(const choice &solver) const
  {
    std::pair<typename solvers_list::const_iterator, typename solvers_list::const_iterator>
      found = std::equal_range(solvers.begin(), solvers.end(), solver, compare_by_solver());

    if(found.first == found.second)
      return NULL;
    else
      return &found.first->second;
  }

  /** \brief The type used to represent the number of solvers in this set. */
  typedef typename solvers_list::size_type solvers_size_type;

  /** \brief Retrieve the number of solvers in this set. */
  solvers_size_type get_solvers_size() const
  {
    return solvers.size();
  }

  /** \brief Retrieve a wrapper around the solver set that can only be
   *  used to write it to a stream.
   */
  generic_dump_solvers<PackageUniverse> dump_solvers() const
  {
    return generic_dump_solvers<PackageUniverse>(solvers);
  }

  /** \brief Apply the given function object to each (solver,
   *  information) pair in this set.
   */
  template<typename F>
  bool for_each_solver(F f) const
  {
    for(typename solvers_list::const_iterator it = solvers.begin();
	it != solvers.end(); ++it)
      {
	if(!f(*it))
	  return false;
      }

    return true;
  }

  bool operator==(const generic_dep_solvers &other) const
  {
    return solvers == other.solvers && structural_reasons == other.structural_reasons;
  }

  /** \brief Return the reasons that the set of solvers for this
   *  dependency was narrowed.
   */
  const imm::list<choice> &get_structural_reasons() const
  {
    return structural_reasons;
  }

  /** \todo This should use some sort of precomputed mostly-unique
   *  value to speed up comparisons.
   */
  int compare(const generic_dep_solvers &other) const
  {
    const int solvers_cmp = aptitude::util::compare3(solvers, other.solvers);
    if(solvers_cmp != 0)
      return solvers_cmp;
    else
      return aptitude::util::compare3(structural_reasons, other.structural_reasons);
  }

  /** \todo This should use some sort of precomputed mostly-unique
   *  value to speed up comparisons.
   */
  bool operator<(const generic_dep_solvers &other) const
  {
    return compare(other) < 0;
  }
};

template<typename PackageUniverse>
std::size_t hash_value(const generic_dep_solvers<PackageUniverse> &dep_solvers)
{
  return dep_solvers.get_hash_value();
}

/** \brief One entry in the queue of active promotions.
 *
 *  Each entry in the queue contains its index (counting from 0) and
 *  the sum of the number of actions in all the previous promotions in
 *  the queue.  All entries but the last contain a promotion and a
 *  pointer to the next entry.  The nodes in the queue are
 *  reference-counted via boost::shared_ptr, so they can be cleaned up
 *  after all steps have advanced past them.
 */
template<typename PackageUniverse>
class generic_promotion_queue_entry
{
public:
  typedef generic_promotion<PackageUniverse> promotion;

private:
  unsigned int action_sum;
  unsigned int index;
  boost::optional<std::pair<promotion, boost::shared_ptr<generic_promotion_queue_entry> > > contents;

public:
  /** \brief Create a promotion queue entry with no successor link or
   *  stored promotion.
   */
  generic_promotion_queue_entry(unsigned int _action_sum, unsigned int _index)
    : action_sum(_action_sum), index(_index)
  {
  }

  /** \brief Retrieve the sum of the number of actions in all previous
   *  promotions in the queue.
   */
  unsigned int get_action_sum() const { return action_sum; }

  /** \brief Retrieve the index of this entry in the queue. */
  unsigned int get_index() const { return index; }

  /** \brief Fill in the promotion and successor information.
   *
   *  This may be invoked only once on a given promotion queue entry.
   *  It will automatically generate a successor node when it is
   *  invoked.
   *
   *  \return the new queue entry.
   */
  void set_promotion(const promotion &p)
  {
    // Check that we don't have any contents yet.
    eassert(!contents);

    contents = std::make_pair(p, new generic_promotion_queue_entry(action_sum + p.get_choices().size(), index + 1));
  }

  /** \brief Return \b true if this queue entry has a promotion and a
   *  successor.
   */
  bool get_has_contents() const { return contents; }

  /** \brief Retrieve the promotion associated with this queue entry.
   */
  promotion get_promotion() const
  {
    return contents->first;
  }

  /** \brief Retrieve the next link in the queue.
   *
   *  If this is the last entry in the queue, returns a NULL pointer.
   */
  boost::shared_ptr<generic_promotion_queue_entry> get_next() const
  {
    if(contents)
      return contents->second;
    else
      return boost::shared_ptr<generic_promotion_queue_entry>();
  }
};

/** \brief Represents the current search graph.
 *
 *  This structure and its operations track all the visited search
 *  nodes and their parent-child relationships, handle inserting new
 *  steps into the graph, and handle backpropagation of promotions
 *  (currently disabled as it didn't work well in practice).
 */
template<typename PackageUniverse>
class generic_search_graph
{
  typedef typename PackageUniverse::package package;
  typedef typename PackageUniverse::version version;
  typedef typename PackageUniverse::dep dep;

  typedef generic_solution<PackageUniverse> solution;
  typedef generic_choice<PackageUniverse> choice;
  typedef generic_choice_set<PackageUniverse> choice_set;
  typedef generic_promotion<PackageUniverse> promotion;
  typedef generic_promotion_set<PackageUniverse> promotion_set;
  typedef generic_compare_choices_by_effects<PackageUniverse> compare_choices_by_effects;

  // Structures that store the search graph.
  //
  // This information is used to backpropagate promotions/conflicts.
  // If all the children of a step ended up in a conflict, we can use
  // that information to infer a conflict for the step itself.  But
  // since aptitude has a flexible search order, the children might be
  // run after we've "finished" processing the parent step.  So it's
  // necessary to somehow store all the steps with children that are
  // pending evaluation.
  //
  // Actually, *all* steps are stored, not just ones with pending
  // children: this lets us handle situations where promotions are
  // generated more than once for the same step (for instance, if a
  // step eventually ends up at cost 40,000 but has one child that
  // passes through cost 30,000, it will first be promoted to cost
  // 30,000, then later to cost 40,000).
  //
  // The lifetime of a step is as follows:
  //  (1) Created with no children, an initial intrinsic promotion,
  //      and maybe a parent link.
  //  (2) Children added, along with successor constraints.
  //  (3) When each of its children has registered a promotion,
  //      the step's promotion is set and its parent is examined
  //      to see if it should get a promotion now.
  //
  // Regarding (3), what we do is this: whenever a new promotion is
  // computed for a step, it goes into the set of promotions for that
  // step and onto the list of promotions if it was really new.  Then,
  // we add the parent to a set of nodes that should be examined for
  // promotion propagation.  After the step finishes, the nodes in
  // this set are processed for propagation from their children.
  //
  // To save space and keep things compact, the tree is represented as
  // an array (actually a deque), with parent and child links stored
  // as indices into the array.  This made more sense when it had just
  // a few members; maybe now it should be allocated on the heap and
  // reference-counted?
public:
  struct step
  {
    // The index of this step; mainly useful when generating debug
    // output.
    int step_num;
    // If true, this is the last child in its parent's child list.
    // Meaningless if parent is -1 (meaning there is no parent node).
    bool is_last_child : 1;
    // If true, this step is a "blessed" solution.  The cost of a
    // blessed solution cannot be increased past the deferral
    // structural level (hence it will not be discarded).  Blessed
    // solutions are solutions that have been moved into the pending
    // future solutions queue and are just waiting for the future
    // solution "counter" to be exhausted.
    //
    // \todo One subtlety here: it would be nice if we could throw out
    // already-visited solutions if we found another solution that was
    // a strict subset.  However, that's rather unlikely and I don't
    // want to introduce lots of mechanism (e.g., a whole new matching
    // mode for promotions) just to accomplish it.  I could instead
    // just filter out promotions that are exactly the size of a
    // blessed solution -- but that could easily run into problems
    // with generalized promotions built from the already-generated
    // promotion ... better to just say "if we've processed it, it's
    // safe".
    bool is_blessed_solution : 1;
    // Index of the parent step, or -1 if there is no parent.
    int parent;
    // Index of the first child step, or -1 if there are no children.
    // This is always -1 to start with, and is updated when the step's
    // successors are generated.
    int first_child;

    /** \brief The tail of the promotion queue at the time that this
     *  step was created or synchronized with the active promotion
     *  set.
     */
    boost::shared_ptr<generic_promotion_queue_entry<PackageUniverse> > promotion_queue_location;

    /** \brief Members used while searching promotions in existing
     *  steps.
     */

    // @{

    /** \brief The index of the last promotion search.
     *
     *  Initially zero; the promotion searcher uses this to determine
     *  whether the hit counts need to be reset to zero.
     */
    unsigned int last_promotion_search;

    /** \brief The number of times that the choice set was hit by a
     *  promotion.
     *
     *  Initially zero.
     */
    unsigned int choice_set_hit_count;

    /** \brief The number of times that the solver set was hit by a
     * promotion.
     *
     *  Initially zero.
     */
    unsigned int solver_set_hit_count;

    /** \brief The first solver that was found to hit a promotion.
     *
     *  Only meaningful if solver_set_hit_count > 0.
     */
    choice first_solver_hit;

    // @}

    /** \brief Members related to generating a step's
     *  successor.
     */

    // @{

    typedef generic_solver_information<PackageUniverse> solver_information;
    typedef generic_dep_solvers<PackageUniverse> dep_solvers;

    typedef boost::flyweight<dep_solvers> flyweight_dep_solvers;

    /** \brief The actions performed by this step. */
    choice_set actions;

    /** \brief The score of this step. */
    int score;

    /** \brief The combined score due to choices that were made and
     *  distance from the root -- "score" is calculated by adding the
     *  broken-dependency count to this.
     */
    int action_score;

    /** \brief The cost induced by the actions performed at
     * this step.
     *
     *  This is the cost *before* any forward-looking operations are
     *  applied to it.
     */
    cost base_step_cost;

    /** \brief The cumulative effect of all forward-looking operations
     *  applied to this step.
     *
     *  Unlike base_step_cost, this can decrease from step to step
     *  (which is why it's separated; it needs to be recomputed for
     *  each step).  This could be computed incrementally, but the
     *  natural way of doing that would require making a frequent
     *  operation (increasing this value) expensive to make an
     *  infrequent operation (creating a new step) less expensive.
     */
    cost effective_step_cost;

    /** \brief The cost of this step after taking into account
     *  knowledge about the available solutions to its dependencies.
     *
     *  This cost is base_step_cost combined with any inferred cost
     *  operations, and is used to sort the step in the search queue.
     */
    cost final_step_cost;

    /** \brief A side-effecting expression that fires when the most
     *  recently added action becomes deferred or un-deferred.
     *
     *  This is stored here merely to ensure that the corresponding
     *  listeners stay alive.  The other choices in this step are kept
     *  alive by its parents.
     */
    cwidget::util::ref_ptr<expression<bool> > is_deferred_listener;

    /** \brief The dependencies that are unresolved in this step; each
     *	one maps to the reasons that any of its solvers were
     *	dropped.
     */
    imm::map<dep, flyweight_dep_solvers> unresolved_deps;

    /** \brief The unresolved dependencies, sorted by the number of
     *  solvers each one has.
     *
     *  This is a "poor man's heap".
     */
    imm::set<std::pair<int, dep> > unresolved_deps_by_num_solvers;

    /** \brief Maps choices to lists of the dependencies that they
     *  solve.
     *
     *  Every unresolved dependency is represented here, but some
     *  dependencies in each list might already be resolved.  We defer
     *  dropping them to save time and memory (no need to make copies
     *  of (part of) the list just to throw entries away).
     */
    generic_choice_indexed_map<PackageUniverse, imm::list<dep> > deps_solved_by_choice;

    /** \brief Versions that are structurally forbidden and the reason
     *  each one is forbidden.
     */
    imm::map<version, choice> forbidden_versions;

    // @}

    /** \brief Members related to backpropagating promotions. */

    // @{

    // A set listing all the clones of this step (steps that have the
    // same solution).  If this is non-empty, this step is the
    // canonical copy of its clones.  What that means is that the
    // "promotions" set of the canonical copy is used whenever the set
    // of promotions is needed; same for the "promotions" list (but
    // the index of the last promotion propagated to the parent is
    // different in each clone!).
    std::set<int> clones;

    // The canonical copy of this step, or -1 if none.
    int canonical_clone;

    // The choice associated with this step (meaningless if parent ==
    // -1).  Set when the step is created, and used when
    // backpropagating promotions: when the parent is computing its
    // promotion, it removes each child's reason from that child's
    // promotion's choice set.
    choice reason;
    // The choices that constrained the successors to this node.  This
    // is used, along with information from the successors, to compute
    // the promotion associated with this node (if any).
    //
    // This contains versions that either structurally knocked out
    // possible resolutions to the dependency that was selected for
    // expansion, *or* that caused a resolution to hit a conflict /
    // already-generated promotion.  NOTE: the entries in this set
    // might not be represented in every promotion at this step; some
    // promotions could be generated from dependencies that weren't
    // actually expanded.  This is used when accumulating this node's
    // sub-promotions and filling in new promotions; the parent
    // shouldn't examine it.
    //
    // This only has a meaningful value if first_child is not -1.
    choice_set successor_constraints;
    // All the promotions associated with this step; each promotion is
    // universally valid but was discovered in the context of this
    // step.  No attempt is made to eliminate redundant promotions at
    // the moment.
    //
    // If this is a cloned step, this variable will never be used (the
    // canonical clone's version will be used -- but since this is
    // only used when adding new promotions and new promotions are
    // added to the canonical clone, the promotions set isn't used).
    std::set<promotion> promotions;

    // The same, but in the order that they were added; used to
    // quickly partition the list into "new" and "old" promotions.
    //
    // If this is a cloned step, the vector in the canonical clone
    // will be used instead.
    //
    // TODO: should be a list of const_iterators referencing the above
    // set.
    std::vector<promotion> promotions_list;

    // The first index in the promotions list that represents a "new"
    // promotion.  This is always used even in cloned steps (each step
    // could have a different number of promotions that haven't been
    // propagated to its particular parent).
    typename std::vector<promotion>::size_type promotions_list_first_new_promotion;

    // @}

    /** \brief Default step constructor; only exists for use
     *  by STL containers.
     */
    step()
      : is_last_child(true),
	is_blessed_solution(false),
	parent(-1), first_child(-1),
	last_promotion_search(0),
	choice_set_hit_count(0),
	solver_set_hit_count(0),
	first_solver_hit(),
	is_deferred_listener(),
	canonical_clone(-1),
	reason(),
	successor_constraints(), promotions(),
	promotions_list(),
	promotions_list_first_new_promotion(0)
    {
    }

    /** \brief Make a step suitable for use at the root.
     *
     *  The step has no parent, children, promotion, or successor
     *  constraints.
     */
    step(const choice_set &_actions,
	 int _score,
	 int _action_score)
      : is_last_child(true),
	is_blessed_solution(false),
	parent(-1), first_child(-1),
	last_promotion_search(0),
	choice_set_hit_count(0),
	solver_set_hit_count(0),
	first_solver_hit(),
	canonical_clone(-1),
	actions(_actions),
	score(_score),
	action_score(_action_score),
	is_deferred_listener(),
	reason(),
	successor_constraints(), promotions(),
	promotions_list(), promotions_list_first_new_promotion(0)
    {
    }

    /** \brief Make a step with the given parent.
     *
     *  The step initially has no children or successor constraints.
     */
    step(const choice_set &_actions,
	 int _score, int _action_score,
	 int _parent,
	 const choice &_reason, bool _is_last_child)
      : is_last_child(_is_last_child),
	is_blessed_solution(false),
	parent(_parent),
	first_child(-1),
	last_promotion_search(0),
	choice_set_hit_count(0),
	solver_set_hit_count(0),
	first_solver_hit(),
	canonical_clone(-1),
	actions(_actions),
	score(_score),
	action_score(_action_score),
	reason(_reason),
	successor_constraints(), promotions(),
	promotions_list(), promotions_list_first_new_promotion(0)
    {
    }
  };

  /** \brief Describes how a choice occurs in a step. */
  enum choice_mapping_type
    {
      /** \brief The choice is an action performed by the step. */
      choice_mapping_action,

      /** \brief The choice solves a dependency that is unresolved
       *  in the step.
       */
      choice_mapping_solver
    };


  /** \brief Stores all steps where a choice occurs.
   *
   *  Steps where the choice occurs as an action are indexed by the
   *  dependency that's solved.  This allows us to easily pick the
   *  right solver to modify when a deferral is canceled.
   */
  class choice_mapping_info
  {
    // The steps (if any) that introduced this choice as a solver or
    // an action, grouped by the dependency that each one solves.
    imm::map<dep, imm::set<int> > steps;

  public:
    choice_mapping_info()
    {
    }

    choice_mapping_info(const imm::map<dep, imm::set<int> > &_steps)
      : steps(_steps)
    {
    }

    const imm::map<dep, imm::set<int> > &get_steps() const
    {
      return steps;
    }
  };

private:
  /** \brief The maximum number of promotions to propagate
   *  through any one step.
   *
   *  This avoids an exponential growth in the size of promotion sets.
   *  This is disabled for the moment: I was seeing aptitude
   *  allocating truly enormous amounts of memory for propagated
   *  promotions, and the benefits of backpropagation have been
   *  minimal in practice.  If additional enhancements to the resolver
   *  framework are implemented, I expect that we might see
   *  backpropagation become cheaper and more useful, in which case it
   *  could be worth turning it on again (but beware of possible
   *  bitrot!).
   */
  static const unsigned int max_propagated_promotions = 0;

  logging::LoggerPtr logger;

  // We keep a reference to the promotions set so that we can stuff
  // new promotions in during backpropagation.
  promotion_set &promotions;

  std::deque<step> steps;
  // Steps whose children have pending propagation requests.  Stored
  // in reverse order, because we should handle later steps first
  // (since they might be children of earlier steps and thus add new
  // promotions to them).
  std::set<int, std::greater<int> > steps_pending_promotion_propagation;

  /** \brief The step numbers in which a choice was introduced as an
   *  action or a solver.
   *
   *  This is used to efficiently update existing steps that are "hit"
   *  by a new promotion, and to efficiently un-defer steps when the
   *  set of user constraints changes.
   *
   *  This map needs to be updated when a new step is added to the
   *  graph, and also when one of a version's successors is struck.
   */
  generic_choice_indexed_map<PackageUniverse, choice_mapping_info> steps_related_to_choices;

  /** \brief The index of the next promotion search.
   *
   *  This is used as an alternative to maintaining costly structures
   *  to determine which steps we hit and/or to clear hit counts.  Hit
   *  counts in a step are only valid if the step's
   *  last_promotion_search member is equal to the index of the
   *  current promotion search.  Otherwise, they are presumed to be
   *  zero.
   */
  int next_promotion_search_index;

public:
  /** \brief Add an entry to the choice->step reverse index.
   *
   *  \param c         The choice to bind.
   *  \param step_num  The step number in which c occurs.
   *  \param reason    The dependency that this choice solves, if how
   *                   is choice_mapping_solver.
   */
  void bind_choice(const choice &c, int step_num, dep reason)
  {
    // \todo Write a proper operator<<.
    LOG_TRACE(logger, "Marking the choice " << c
	      << " as present in step "
	      << step_num << " with dependency " << reason);

    choice_mapping_info inf;
    steps_related_to_choices.try_get(c, inf);
    imm::map<dep, imm::set<int> >
      new_steps(inf.get_steps());
    imm::set<int>
      new_dep_steps(new_steps.get(reason, imm::set<int>()));

    new_dep_steps.insert(step_num);
    new_steps.put(reason, new_dep_steps);
    steps_related_to_choices.put(c, choice_mapping_info(new_steps));
  }

  /** \brief Remove an entry from the choice->step reverse index.
   *
   *  This will remove the mapping associated with the top of a "tree"
   *  of occurrences.
   *
   *  \warning Correctness of this protocol relies on the fact that
   *  the structure of the resolver means that if a solver becomes
   *  irrelevant in a step, it will also be irrelevant in all children
   *  of that step.  Proof: there are only two ways a solver can
   *  become irrelevant.  It could be structurally excluded (in which
   *  case all children lack it by default), or it could be knocked
   *  out by a promotion.  In the latter case, since children are
   *  supersets of their parents, each child will contain the same
   *  promotion, and hence the choice will be knocked out in every
   *  child that it occurs in.
   *
   *  This also relies on the fact that promotions that knock out
   *  choices are never retracted (if they are, we'll have to be
   *  careful to only reinstate a choice in the top step it occurs in,
   *  but that is not likely in the near future).
   *
   *  \param c         The choice to unbind.
   *  \param step_num  The step number in which c no longer occurs.
   *  \param reason    The dependency that this choice solved, if how
   *                   is choice_mapping_solver.
   */
  void remove_choice(const choice &c, int step_num, const dep &reason)
  {
    // \todo Write a proper operator<<.
    LOG_TRACE(logger, "Marking the choice " << c
              << " as not present in step "
              << step_num);

    choice_mapping_info info;
    if(steps_related_to_choices.try_get(c, info))
      {
        imm::map<dep, imm::set<int> >
          new_steps(info.get_steps());

        typename imm::map<dep, imm::set<int> >::node
          found_solver(new_steps.lookup(reason));

        if(found_solver.isValid())
          {
            imm::set<int>
              new_dep_steps(found_solver.getVal().second);

            new_dep_steps.erase(step_num);
            if(new_dep_steps.empty())
              new_steps.erase(reason);
            else
              new_steps.put(reason, new_dep_steps);
          }

        choice_mapping_info new_info(new_steps);
        steps_related_to_choices.put(c, new_info);
      }
  }

private:
  // Walks down a list of siblings, applying the given function to
  // each of them, until either the function returns false or it runs
  // out of siblings.  If step_num is -1, nothing is visited.
  template<typename F>
  bool visit_siblings(int step_num,
		      F f) const
  {
    while(step_num != -1)
      {
	if(!f(step_num))
	  return false;

	if(get_step(step_num).is_last_child)
	  step_num = -1;
	else
	  ++step_num;
      }

    return true;
  }

  template<typename F>
  class visit_choice_mapping_steps
  {
    // The choice to pass to the sub-function.
    const choice &c;
    const generic_search_graph &graph;

    F f;

    // Could save some code by merging this with
    // visit_choice_mapping_steps_for_dep() (using a virtual
    // function?).
    bool visit(const step &s, choice_mapping_type how) const
    {
      if(!f(c, how, s.step_num))
	return false;
      else
	return graph.visit_siblings(s.first_child, *this);
    }

  public:
    visit_choice_mapping_steps(const choice &_c,
			       const generic_search_graph &_graph, F _f)
      : c(_c), graph(_graph), f(_f)
    {
    }

    bool operator()(int step_num) const
    {
      const step &s(graph.get_step(step_num));
      if(s.actions.contains(c))
	return visit(s, choice_mapping_action);
      else if(s.deps_solved_by_choice.contains_key(c))
	return visit(s, choice_mapping_solver);
      else
	return true;
    }
  };

  /** \brief Apply the given function object to (c', how, step_num)
   *  for each step in each dependency mapping passed to this object.
   */
  template<typename F>
  class visit_choice_dep_mapping
  {
    const choice &c;
    const generic_search_graph &graph;

    F f;

  public:
    visit_choice_dep_mapping(const choice &_c,
			     const generic_search_graph &_graph, F _f)
      : c(_c), graph(_graph), f(_f)
    {
    }

    bool operator()(const std::pair<dep, imm::set<int> > &mapping) const
    {
      return mapping.second.for_each(visit_choice_mapping_steps<F>(c, graph, f));
    }
  };

  /** \brief Apply the given function object to (c', how, step_num)
   *  for each step in each mapping information structure visited.
   */
  template<typename F>
  class visit_choice_mapping
  {
    F f;
    const generic_search_graph &graph;

  public:
    visit_choice_mapping(const generic_search_graph &_graph, F _f)
      : f(_f), graph(_graph)
    {
    }

    bool operator()(const choice &c, const choice_mapping_info &inf) const
    {
      const imm::map<dep, imm::set<int> > &steps(inf.get_steps());
      return steps.for_each(visit_choice_dep_mapping<F>(c, graph, f));
    }
  };

public:
  /** \brief Apply the given function object to (c', how, step_num)
   *  for each binding (c', step_num) in the choice->step reverse
   *  index such that c' is contained in c as indicated by how.
   */
  template<typename F>
  void for_each_step_related_to_choice(const choice &c, F f) const
  {
    visit_choice_mapping<F> visit_mappings_f(*this, f);
    steps_related_to_choices.for_each_key_contained_in(c, visit_mappings_f);
  }

private:
  template<typename F>
  class visit_choice_mapping_steps_solvers_of_dep
  {
    // The choice to pass to the sub-function.  Can't be a reference
    // since it's different from what the parent passes in.
    const choice c;
    // The dependency whose solvers are being visited.
    const dep &d;
    const generic_search_graph &graph;

    F f;

    bool visit(const step &s, choice_mapping_type how) const
    {
      if(!f(c, how, s.step_num))
	return false;
      else
	return graph.visit_siblings(s.first_child, *this);
    }

  public:
    visit_choice_mapping_steps_solvers_of_dep(const choice &_c,
					      const dep &_d,
					      const generic_search_graph &_graph, F _f)
      // Note that it's necessary to modify the dependency stored in
      // the choice: below, we'll do an exact lookup to try to find
      // it, and that requires that the dependency is set properly.
      // Setting it in the constructor avoids setting it every time
      // the object is applied.
      : c(_c.copy_and_set_dep(_d)), d(_d), graph(_graph), f(_f)
    {
    }

    bool operator()(int step_num) const
    {
      const step &s(graph.get_step(step_num));
      // Check if we have a solver in this step first -- if you think
      // about it, it's more likely that this is true than that we
      // have an action.
      typename imm::map<dep, typename step::flyweight_dep_solvers>::node found =
	s.unresolved_deps.lookup(d);

      if(found.isValid() &&
	 found.getVal().second.get().lookup_solver_information(c) != NULL)
	return visit(s, choice_mapping_solver);
      else
	{
	  choice step_choice(choice::make_install_version(version(), -1));
	  if(s.actions.get_choice_contained_by(c, step_choice) &&
	     step_choice.get_dep() == d)
	    return visit(s, choice_mapping_action);
	  else
	    return true;
	}
    }
  };

  template<typename F>
  class visit_choice_mapping_solvers_of_dep
  {
    // The dependency to visit.
    dep d;
    const generic_search_graph &graph;

    F f;

  public:
    visit_choice_mapping_solvers_of_dep(const dep &_d,
					const generic_search_graph &_graph, F _f)
      : d(_d), graph(_graph), f(_f)
    {
    }

    bool operator()(const choice &c, const choice_mapping_info &info) const
    {
      typename imm::map<dep, imm::set<int> >::node
	found(info.get_steps().lookup(d));

      if(found.isValid())
	{
	  visit_choice_mapping_steps_solvers_of_dep<F> visit_step_f(c, d, graph, f);
	  return found.getVal().second.for_each(visit_step_f);
	}
      else
	return true;
    }
  };

public:
  /** \brief Apply the given function to (c', how, step_num) for each
   *  binding (c', how, step_num) in the choice->step reverse index
   *  such that c' is contained in c and c' was added as a solver for
   *  the given dependency.
   */
  template<typename F>
  void for_each_step_related_to_choice_with_dep(const choice &c, const dep &d, F f) const
  {
    visit_choice_mapping_solvers_of_dep<F> visit_mappings_by_dep_f(d, *this, f);
    steps_related_to_choices.for_each_key_contained_in(c, visit_mappings_by_dep_f);
  }

  /** \brief Create a search graph.
   *
   *  \param _promotions  The promotion set associated with this object.
   *                      Backpropagated promotions will be inserted
   *                      into this set.
   *
   *  The given promotion set does not have to be initialized until
   *  another method is invoked on this object.
   */
  generic_search_graph(promotion_set &_promotions)
    : logger(aptitude::Loggers::getAptitudeResolverSearchGraph()),
      promotions(_promotions),
      next_promotion_search_index(0)
  {
  }

  /** \brief Retrieve the nth step. */
  step &get_step(int n)
  {
    eassert(n >= 0);
    eassert((unsigned)n < steps.size());
    return steps[n];
  }

  /** \brief Retrieve the nth step. */
  const step &get_step(int n) const
  {
    eassert(n >= 0);
    eassert((unsigned)n < steps.size());
    return steps[n];
  }

  step &get_last_step()
  {
    eassert(!steps.empty());
    return steps.back();
  }

  const step &get_last_step() const
  {
    eassert(!steps.empty());
    return steps.back();
  }

  /** \brief Retrieve the number of steps. */
  typename std::vector<step>::size_type get_num_steps() const
  {
    return steps.size();
  }

  /** \brief Retrieve the next promotion search index, incrementing it
   *  in the process.
   */
  int get_and_increment_promotion_search_index()
  {
    const int rval = next_promotion_search_index;
    ++next_promotion_search_index;
    return rval;
  }

public:
  step &add_step()
  {
    steps.push_back(step());
    step &rval(steps.back());
    rval.step_num = steps.size() - 1;
    return rval;
  }

  /** \brief Throw away all step information. */
  void clear()
  {
    steps.clear();
    steps_pending_promotion_propagation.clear();
    steps_related_to_choices.clear();
  }

  /** Retrieve the promotions list of the given step, returning the
   *  canonical copy if this step is a clone.
   */
  std::vector<promotion> &get_promotions_list(step &s)
  {
    if(s.canonical_clone == -1)
      return s.promotions_list;
    else
      return get_step(s.canonical_clone).promotions_list;
  }

  /** Retrieve the promotions list of the given step, returning the
   *  canonical copy if this step is a clone.
   */
  const std::vector<promotion> &get_promotions_list(const step &s) const
  {
    if(s.canonical_clone == -1)
      return s.promotions_list;
    else
      return get_step(s.canonical_clone).promotions_list;
  }

private:
  // Used to recursively build the set of all the promotions in the
  // Cartesian product of the sub-promotions that include at least one
  // promotion that's "new".
  //
  // Returns "true" if anything was generated.  We care because we
  // need to know whether to queue the parent of the parent for
  // propagation.
  //
  // Here t_op is the operation we're currently building.
  template<typename AddPromotion>
  bool add_child_promotions(int parentNum, int childNum, bool has_new_promotion,
			    const choice_set &choices, const cost &t_op,
			    const AddPromotion &addPromotion)
  {
    // Where to insert any promotions we run into.
    const step &parent = get_step(parentNum);
    int canonicalParentNum = (parent.canonical_clone == -1
			      ? parentNum
			      : parent.canonical_clone);
    const step &canonicalParent = get_step(canonicalParentNum);
    const std::vector<promotion> &canonicalParentPromotionsList = get_promotions_list(canonicalParent);

    if(canonicalParentNum == parentNum)
      LOG_TRACE(logger, "Propagating promotions from the step " << childNum
		<< " to its parent, step " << parentNum);
    else
      LOG_TRACE(logger, "Propagating promotions from the step " << childNum
		<< " to its parent's canonical clone, step " << parentNum);

    // Don't do anything if the parent has too many propagations
    // already.
    if(canonicalParentPromotionsList.size() >= max_propagated_promotions)
      {
	LOG_TRACE(logger, "Not creating a new promotion: the parent already has too many promotions.");
	return false;
      }

    step &child = get_step(childNum);
    const std::vector<promotion> &canonicalChildPromotionsList = get_promotions_list(child);

    typename std::vector<promotion>::const_iterator begin, end = canonicalChildPromotionsList.end();
    if(child.is_last_child && !has_new_promotion)
      {
	// Only process new promotions if we don't have one yet.
	begin = canonicalChildPromotionsList.begin() + child.promotions_list_first_new_promotion;
	if(begin == end)
	  LOG_TRACE(logger, "No new promotions to process (step " << childNum << ")");
      }
    else
      {
	begin = canonicalChildPromotionsList.begin();
	if(begin == end)
	  LOG_TRACE(logger, "No promotions to process (step " << childNum << ")");
      }

    bool rval = false;
    for(typename std::vector<promotion>::const_iterator it = begin;
	it != end && canonicalParentPromotionsList.size() < max_propagated_promotions; ++it)
      {
	bool current_is_new_promotion =
	  (it - canonicalChildPromotionsList.begin()) >= (signed)child.promotions_list_first_new_promotion;

	choice_set new_choices(choices);

	const promotion &p(*it);
	choice_set p_choices(p.get_choices());

	LOG_TRACE(logger, "Using the successor link of step " << childNum
		  << ", " << child.reason
		  << ", to backpropagate the promotion " << p
		  << " and add it to the current choice set " << choices);

	// Strip out the child's link before merging with the existing
	// choice set.
	p_choices.remove_overlaps(child.reason);
	const cost &p_cost(p.get_cost());
	// Augment the choice set with these new choices.  Narrowing
	// is appropriate: anything matching the promotion should
	// match all the choices we found.
	new_choices.insert_or_narrow(p_choices);

	const cost new_cost =
          (t_op.is_above_or_equal(p_cost)
           ? t_op
           : cost::least_upper_bound(p_cost, t_op));

	// TODO: We used to throw out promotions that were below their
	// parent's cost.  In the new system this *might* correspond
	// to having a cost less than or equal to the parent's
	// amalgamated cost.  I'm not 100% sure of this, and it would
	// require having access to the amalgamated cost, something
	// I'm not going to code up since this routine is currently
	// dead.

	if(child.is_last_child)
	  {
	    promotion new_promotion(new_choices, new_cost);

	    // Emit a new promotion.
	    addPromotion(canonicalParentNum, new_promotion);

	    // Actually output a new promotion in the canonical
	    // parent.
	    LOG_TRACE(logger, "New backpropagated promotion at step "
		      << canonicalParentNum << ": " << new_promotion);

	    rval = true;
	  }
	else
	  {
	    bool new_has_new_promotion = has_new_promotion || current_is_new_promotion;
	    // Recur.
	    bool generated_anything =
	      add_child_promotions(parentNum, childNum + 1,
				   new_has_new_promotion,
				   new_choices, new_cost,
				   addPromotion);

	    rval = rval || generated_anything;
	  }
      }

    child.promotions_list_first_new_promotion = canonicalChildPromotionsList.size();

    return rval;
  }

  // TODO: log when we first fail to add a promotion because we hit
  // the maximum number -- that's actually not trivial to do without
  // complicating the code.
  template<typename AddPromotion>
  void maybe_collect_child_promotions(int stepNum, const AddPromotion &addPromotion)
  {
    step &parentStep(get_step(stepNum));
    LOG_TRACE(logger, "Backpropagating promotions to step " << stepNum);

    if(parentStep.first_child == -1)
      {
	LOG_ERROR(logger, "No children at step " << stepNum << ", so no promotions to backpropagate.");
	return;
      }

    if(add_child_promotions(stepNum, parentStep.first_child,
			    false, parentStep.successor_constraints,
			    cost_limits::maximum_structural_level_cost,
			    addPromotion))
      {
	if(parentStep.parent != -1)
	  {
	    LOG_TRACE(logger, "Scheduling step " << parentStep.parent
		      << " for promotion propagation.");
	    steps_pending_promotion_propagation.insert(parentStep.parent);
	  }
      }
    else
      LOG_TRACE(logger, "No new promotion at step " << stepNum);
  }

public:
  /** \brief Attach a promotion to the given step, and schedule it for
   *  propagation.
   */
  void schedule_promotion_propagation(int stepNum,
				      const promotion &p)
  {
    step &targetStep(get_step(stepNum));

    if(targetStep.canonical_clone != -1)
      {
	LOG_TRACE(logger, "Adding the promotion " << p
		  << " to step " << targetStep.canonical_clone
		  << " instead of to its clone, step "
		  << stepNum << ".");
	schedule_promotion_propagation(targetStep.canonical_clone, p);
	return;
      }

    if(targetStep.promotions.size() == max_propagated_promotions)
      {
	LOG_TRACE(logger, "Not adding the promotion " << p
		  << " to step " << stepNum
		  << " since that step has the maximum number of promotions already.");
	return;
      }

    // TODO: could do a slow check for redundant promotions here?

    std::pair<typename std::set<promotion>::iterator, bool>
      insert_info(targetStep.promotions.insert(p));
    if(insert_info.second)
      {
	targetStep.promotions_list.push_back(p);
	if(targetStep.parent != -1)
	  {
	    LOG_TRACE(logger, "Adding a promotion to step " << stepNum
		      << " and scheduling its parent, step " << targetStep.parent << " for propagation: "
		      << p);
	    steps_pending_promotion_propagation.insert(targetStep.parent);
	  }
	else
	  LOG_TRACE(logger, "Adding a promotion to step " << stepNum
		    << "; it has no parent, so not scheduling propagation: "
		    << p);
      }

    if(!targetStep.clones.empty())
      {
	LOG_TRACE(logger, "Also scheduling the parents of the clones of step " << stepNum << " for propagation.");
	for(std::set<int>::const_iterator it = targetStep.clones.begin();
	    it != targetStep.clones.end(); ++it)
	  {
	    int cloneNum = *it;
	    const step &clone(get_step(cloneNum));

	    if(clone.parent != -1)
	      {
		LOG_TRACE(logger, "Scheduling the parent (step "
			  << clone.parent << ") of a clone (step " << cloneNum
			  << ") of step " << stepNum << " for propagation.");
		steps_pending_promotion_propagation.insert(clone.parent);
	      }
	    else
	      // Should never happen, but be careful.  (this would
	      // mean we had a clone of the root node!  Madness!)
	      LOG_ERROR(logger, "Not scheduling the parent of a clone (step " << cloneNum
			<< ") for propagation: it has no parent (something is very wrong!).");
	  }
	LOG_TRACE(logger, "Done scheduling the clones of step " << stepNum << " for propagation.");
      }
  }

  /** \brief Mark the second step as being a clone of the first step.
   *
   *  \param canonicalNum    The step number to use as the canonical copy.
   *  \param cloneNum        The step number to mark as a clone.
   */
  void add_clone(int canonicalNum, int cloneNum)
  {
    LOG_TRACE(logger, "Marking step " << cloneNum << " as a clone of step " << canonicalNum << ".");

    step &canonicalStep(get_step(canonicalNum));
    step &cloneStep(get_step(cloneNum));

    if(cloneStep.canonical_clone == canonicalNum)
      {
	LOG_TRACE(logger, "Not marking step " << cloneNum
		  << " as a clone of step " << canonicalNum
		  << " since it is already listed as a clone.");
	return;
      }

    // These cases should never come up, so assert against them
    // instead of trying to write clever code to handle them.
    eassert(canonicalNum != cloneNum);
    eassert(cloneStep.canonical_clone == -1);
    eassert(cloneStep.clones.empty());

    canonicalStep.clones.insert(cloneNum);
    cloneStep.canonical_clone = canonicalNum;

    // NB: no need to do anything special with successor_constraints;
    // it's only used to generate promotions and we take those from
    // the canonical clone.
    const int cloneParentNum = cloneStep.parent;
    if(!canonicalStep.promotions.empty() &&
       cloneParentNum != -1)
      {
	LOG_TRACE(logger, "The canonical step " << canonicalNum
		  << " has some promotions, so scheduling the parent (step "
		  << cloneParentNum << ") of the clone (step "
		  << cloneNum << ") for backpropagation.");
	steps_pending_promotion_propagation.insert(cloneParentNum);
      }
  }

  /** \brief Execute any pending promotion propagations. */
  template<typename AddPromotion>
  void run_scheduled_promotion_propagations(const AddPromotion &addPromotion)
  {
    while(!steps_pending_promotion_propagation.empty())
      {
	// Make a temporary copy to iterate over.
	std::set<int, std::greater<int> > tmp;
	tmp.swap(steps_pending_promotion_propagation);
	for(std::set<int, std::greater<int> >::const_iterator it = tmp.begin();
	    it != tmp.end(); ++it)
	  maybe_collect_child_promotions(*it, addPromotion);
      }
  }

private:
  struct is_deferred
  {
    bool operator()(const promotion &p) const
    {
      const cost &p_cost(p.get_cost());

      return
	p_cost.get_structural_level() >= cost_limits::defer_structural_level &&
	p_cost.get_structural_level() < cost_limits::already_generated_structural_level;
    }
  };

public:
  /** \brief Remove any propagated promotions from the deferred
   *  cost.
   *
   *  This should be invoked when the set of deferred solutions might
   *  have changed.
   *
   *  \todo This is no longer right with the incremental resolver; we
   *  can remove exactly the right set of promotions if we want.
   */
  void remove_deferred_propagations()
  {
    is_deferred is_deferred_f;

    for(typename std::deque<step>::iterator step_it = steps.begin();
	step_it != steps.end(); ++step_it)
      {
	step &curr_step(*step_it);

	for(typename std::vector<promotion>::const_iterator p_it =
	      curr_step.promotions_list.begin();
	    p_it != curr_step.promotions_list.end(); ++p_it)
	  {
	    const promotion &p(*p_it);

	    if(is_deferred_f(p))
	      {
		LOG_TRACE(logger, "Removing a promotion from the promotion set of step "
			  << step_it - steps.begin()
			  << ": " << p);
		curr_step.promotions.erase(p);
	      }
	  }

	// Drop the deferred entries from the list of promotions,
	// updating the "new" pointer so that new promotions will be
	// detected as being "new".
	typename std::vector<promotion>::size_type write_loc = 0, read_loc = 0;
	typename std::vector<promotion>::size_type num_old_promotions_deleted = 0;
	while(read_loc < curr_step.promotions_list.size())
	  {
	    while(read_loc < curr_step.promotions_list.size() &&
		  is_deferred_f(curr_step.promotions_list[read_loc]))
	      {
		LOG_TRACE(logger, "Removing a promotion from the promotion list of step "
			  << step_it - steps.begin()
			  << ": " << curr_step.promotions_list[read_loc]);
		if(read_loc < curr_step.promotions_list_first_new_promotion)
		  ++num_old_promotions_deleted;
		++read_loc;
	      }

	    if(read_loc < curr_step.promotions_list.size())
	      {
		if(write_loc != read_loc)
		  curr_step.promotions_list[write_loc] = curr_step.promotions_list[read_loc];

		++write_loc;
		++read_loc;
	      }
	  }
	if(read_loc != write_loc)
	  curr_step.promotions_list.erase(curr_step.promotions_list.begin() + write_loc,
					  curr_step.promotions_list.end());
	curr_step.promotions_list_first_new_promotion -= num_old_promotions_deleted;
      }
  }

  /** \brief Dump a dot-format representation of this graph to the
   *  given stream.
   *
   *  For debugging purposes.
   */
  void write_graph(std::ostream &out)
  {
    out << "digraph {" << std::endl;
    for(typename std::deque<step>::const_iterator it = steps.begin();
	it != steps.end(); ++it)
      {
	out << it->step_num << " [label=\"Step " << it->step_num << "\\n"
	    << it->actions.size() << " actions\", shape=box";

	if(it->is_last_child)
	  out << ", style=filled, fillcolor=lightgray";

	out << "];" << std::endl;

	int i = it->first_child;
	while(i != -1)
	  {
	    const step &child(steps[i]);

	    if(child.parent != it->step_num)
	      // It's a phantom link; graph accordingly.
	      out << it->step_num << " -> " << child.step_num << " [style=dashed];" << std::endl;
	    else
	      out << it->step_num << " -> " << child.step_num << " [label=\"" << child.reason << "\"];" << std::endl;

	    if(child.is_last_child)
	      i = -1;
	    else
	      ++i;
	  }
      }
    out << "}" << std::endl;
  }
};

template<typename PackageUniverse>
std::ostream &operator<<(std::ostream &out, const generic_solver_information<PackageUniverse> &info)
{
  out << "(" << info.get_cost()
      << ":" << info.get_reasons();

  if(info.get_cost_valid().valid())
    {
      out << "; V: ";
      info.get_cost_valid()->dump(out);
    }

  if(info.get_is_deferred_listener().valid())
    {
      out << "; L: ";
      info.get_is_deferred_listener()->dump(out);
    }

  out << ")";

  return out;
}

template<typename PackageUniverse>
std::ostream &operator<<(std::ostream &out,
			 const generic_dep_solvers<PackageUniverse> &solvers)
{
  return out << "("
	     << solvers.structural_reasons
	     << ": "
	     << solvers.dump_solvers()
	     << ")";
}

#endif
