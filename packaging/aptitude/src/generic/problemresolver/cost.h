/** \file cost.h */  // -*-c++-*-

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

#ifndef COST_H
#define COST_H

#include "exceptions.h"

#include <generic/util/compare3.h>

#include <iosfwd>
#include <stdexcept>

#include <boost/flyweight.hpp>
/** \brief Represents the value of a single component of a solution's
 *  cost.
 *
 *  A cost is an ordered sequence of levels.  Levels are integers that
 *  can be modified either by being incremented or by being increased
 *  to be at least the given value.  However, a given slot in a cost
 *  object can only be modified in one of these ways; for instance, a
 *  slot that is the result of an increment can't later be raised to a
 *  minimum value.
 *
 *  The reason for representing levels this way is that it allows the
 *  resolver to support both "increase the cost to X" and "add X to
 *  the cost" operations in a sound manner; in particular, this way
 *  those two operations are both associative and commutative.  (in
 *  this case, they are associative and commutative in the sense that
 *  applying both types of operations always errors out)
 *
 *  Note: the "increase cost to X" operation is important for
 *  respecting priorities (there doesn't seem to be an obvious
 *  alternative there), for backwards-compatibility, and so that the
 *  resolver can easily become non-optimizing if necessary (e.g., if
 *  the optimizing version experiences too many performance problems,
 *  reverting to the old behavior is a simple change to the default
 *  settings).
 *
 *  Note that levels can represent both a cost entry, or a *change* to
 *  a cost entry.  The "combine" method will either merge two changes
 *  into a single change, or apply a change to a level.
 *
 *  Instead of failing out when two different operations are applied,
 *  I could instead resolve the conflict by accumulating lower-bounds
 *  separately from increments and always applying lower-bounds first.
 *  That would resolve the conflict, but it might produce a somewhat
 *  unintuitive result for the user.  I've chosen this route because
 *  at least the behavior is obvious (and I can't think of any use
 *  case for actually merging lower-bounds and increments).
 */
class level
{
public:
  // The level's state; if it's ever been modified, this tracks how it
  // was modified.
  enum state_enum { unmodified, added, lower_bounded };

private:
  // Note: I would use a single iinteger if I thought I could get away
  // with it.

  // Note 2: the reason for using signed integers is that it makes the
  // case of policy-priorities-as-costs a bit clearer.

  int value;

  state_enum state;

  level(int _value, state_enum _state)
    : value(_value), state(_state)
  {
  }

public:
  /** \brief Create a new level at the minimum cost. */
  level() : value(INT_MIN), state(unmodified)
  {
  }

  /** \brief Create a new level with the given value and state "added". */
  static level make_added(int value)
  {
    return level(value, added);
  }

  /** \brief Create a new level with the given value and state "lower_bounded." */
  static level make_lower_bounded(int value)
  {
    return level(value, lower_bounded);
  }

  int get_value() const { return value; }
  state_enum get_state() const { return state; }

  /** \brief Test whether this level is greater than or equal to the
   *  given other level under the natural partial ordering of levels.
   */
  bool is_above_or_equal(const level &other) const
  {
    if(other.state == unmodified)
      return true;
    else if(state != other.state)
      return false;
    else
      switch(state)
	{
	case unmodified:
	  return true;

	case added:
	case lower_bounded:
	  return value >= other.value;
	}

    // Fall-through in case the case statement missed something;
    // outside the case so that the compiler checks that all enum
    // values are handled.
    return false;
  }

  void increase_cost_to(int new_value)
  {
    if(state == added)
      throw CostOperationMismatchException();
    else if(new_value != INT_MIN)
      {
	if(value < new_value)
	  value = new_value;
	state = lower_bounded;
      }
  }

  /** \brief Combine two levels and return a new level.
   *
   *  If either input level is unmodified, the result is the other
   *  level.  Otherwise, the two levels must have the same state, and
   *  the levels are combined according to that state.
   */
  static level combine(const level &l1, const level &l2)
  {
    if(l1.state == unmodified)
      return l2;
    else if(l2.state == unmodified)
      return l1;
    else if(l1.state != l2.state)
      throw CostOperationMismatchException();
    else if(l1.state == lower_bounded)
      return level(std::max<int>(l1.value, l2.value), lower_bounded);
    else // if(l1.state == added)
      {
	if(!(l1.value > 0 || l2.value > 0))
	  throw NonPositiveCostAdditionException();
	else if(l1.value > INT_MAX - l2.value)
          throw CostTooBigException();
        else
	  return level(l1.value + l2.value, added);
      }
  }

  /** \brief Compute the upper bound of two levels.
   *
   *  If the levels have incompatible states, throws an exception;
   *  otherwise, returns a level whose numerical value is the maximum
   *  of the numerical values of the two input levels and whose type
   *  is the upper-bound of their types.
   */
  static level upper_bound(const level &l1, const level &l2)
  {
    if(l1.state == unmodified)
      return l2;
    else if(l2.state == unmodified)
      return l1;
    else if(l1.state != l2.state)
      throw CostOperationMismatchException();
    else
      return level(std::max<int>(l1.value, l2.value),
		   l1.state);
  }

  /** \brief Compute the lower bound of two levels.
   *
   *  If the levels have incompatible states, throws an exception; if
   *  either is "unmodified", returns an unmodified level; otherwise,
   *  returns a level whose numerical value is the minimum of the
   *  numerical values of the two input levels and whose type is the
   *  lower-bound of their types.
   */
  static level lower_bound(const level &l1, const level &l2)
  {
    if(l1.state == unmodified || l2.state == unmodified)
      return level();
    else if(l1.state != l2.state)
      throw CostOperationMismatchException();
    else
      return level(std::min<int>(l1.value, l2.value),
		   l1.state);
  }

  /** \brief Compare two levels.
   *
   *  Only the value of each level is compared.  The additional
   *  information is discarded, so this should not be used to build an
   *  associative data structure where that information might matter.
   */
  int compare(const level &other) const
  {
    return aptitude::util::compare3(value, other.value);
  }

  /** \brief Hashes a level object. */
  std::size_t get_hash_value() const
  {
    std::size_t rval = 0;

    boost::hash_combine(rval, value);
    boost::hash_combine(rval, state);

    return rval;
  }

  /** \brief Returns \b true if two levels are identical (both value
   *  and state).
   */
  bool operator==(const level &other) const
  {
    return value == other.value && state == other.state;
  }
};

std::ostream &operator<<(std::ostream &out, const level &l);

inline std::size_t hash_value(const level &l)
{
  return l.get_hash_value();
}

namespace aptitude
{
  namespace util
  {
    template<>
    class compare3_f<level>
    {
    public:
      int operator()(const level &t1, const level &t2) const
      {
	return t1.compare(t2);
      }
    };
  }
}

/** \brief The cost of a solution describes how "bad" that solution is
 * considered to be.
 *
 *  Costs can be composed with operator+; this operation is
 *  commutative and associative (sort of, see below).  They are
 *  partially ordered in the natural way (if o1 < o2, then for any
 *  cost c, o1(c) < o2(c) -- this ordering exists due to the above
 *  properties) and exist in a lattice.  (least upper and greatest
 *  lower bounds are levelwise minimum and maximum, respectively)
 *
 *  The two primitive cost types are "add X to a level" and "increase
 *  a level to at least X".  These "commute" with each other only in
 *  the sense that any expression involving both types will throw an
 *  exception rather than producing a valid cost.
 */
class cost
{
  // These classes are used to disambiguate constructors.
  class combine_tag { };
  class lower_bound_tag { };
  class upper_bound_tag { };

  // \todo Should have an abstracted "key"" that covers the 3 ways of
  // instantiating this object.  Also, should cache the hash value to
  // avoid recomputing it over and over.
  class cost_impl
  {
    typedef std::vector<level>::size_type level_index;

    // This level is reserved for internal use by the dependency
    // solver and is used to structure the search space.
    int structural_level;

    // The cost is a collection of pairs. each giving the cost at an
    // individual level and the level's location in the cost vector.
    // Level indices that don't occur have the NOP level
    // value. Storing costs this way lets us save a little memory,
    // considering that most operations will probably modify only a
    // few levels.
    //
    // This vector is always sorted, and level numbers are unique.
    std::vector<std::pair<level_index, level> > actions;

  public:
    /** \brief Create a blank cost. */
    cost_impl()
      : structural_level(INT_MIN)
    {
    }

    /** \brief Create a cost in which only the structural level is
     *  set.
     */
    explicit cost_impl(int _structural_level)
      : structural_level(_structural_level)
    {
    }

    /** \brief Create a cost in which a single non-structural level is
     *  set.
     */
    cost_impl(int index, const level &l)
      : structural_level(INT_MIN)
    {
      if(index < 0)
        throw std::out_of_range("Negative index used to construct a cost");

      if(l.get_state() == level::added &&
	 l.get_value() <= 0)
	throw NonPositiveCostAdditionException();

      if(index < 0)
	throw std::out_of_range("User level indices must be non-negative.");

      actions.push_back(std::make_pair(index, l));
    }

    /** \brief Create a cost that combines two other costs. */
    cost_impl(const cost_impl &cost1, const cost_impl &cost2, combine_tag);

    /** \brief Create a new cost that computes the lower bound of
     *  two input costs.
     */
    cost_impl(const cost_impl &cost1, const cost_impl &cost2, lower_bound_tag);

    /** \brief Create a new cost that computes the upper bound of
     *  two input costs.
     */
    cost_impl(const cost_impl &cost1, const cost_impl &cost2, upper_bound_tag);

    /** \brief Return \b true if this cost is greater than or
     *  equal to the other cost under the natural partial order.
     *
     *  Equivalent to testing whether least_upper_bound is this
     *  object, but doesn't create an intermediary.
     */
    bool is_above_or_equal(const cost_impl &other) const;

    /** \brief Compute a hash on this cost.
     *
     *  \note Relies on the fact that the level's hash includes
     *  whether it's an addition or a lower-bound.
     */
    std::size_t get_hash_value() const
    {
      std::size_t rval = 0;

      boost::hash_combine(rval, structural_level);
      boost::hash_combine(rval, actions);

      return rval;
    }

    int get_structural_level() const
    {
      return structural_level;
    }

    level get_user_level(std::size_t idx) const;

    bool get_has_user_levels() const
    {
      return !actions.empty();
    }

    /** \brief Test two costs for equality.
     *
     *  \note Relies on the fact that the level's equality comparison
     *  returns "true" only when the two levels have the same state.
     */
    bool operator==(const cost_impl &other) const
    {
      return
	structural_level == other.structural_level &&
	actions == other.actions;
    }

    /** \brief Compare two costs by their identity.
     */
    int compare(const cost_impl &other) const;

    /** \brief Dump this cost to a stream. */
    void dump(std::ostream &out) const;
  };

  class cost_impl_hasher
  {
  public:
    std::size_t operator()(const cost_impl &cost) const
    {
      return cost.get_hash_value();
    }
  };

  typedef boost::flyweight<cost_impl,
			   boost::flyweights::hashed_factory<cost_impl_hasher> >
  cost_impl_flyweight;

  cost_impl_flyweight impl_flyweight;

  const cost_impl &get_impl() const { return impl_flyweight.get(); }

  /** \brief Create a cost in which only the structural level is set.
   */
  explicit cost(int structural_level)
    : impl_flyweight(cost_impl(structural_level))
  {
  }

  /** \brief Create a cost that combines two other
   *  costs.
   */
  cost(const cost &cost1,
       const cost &cost2)
    : impl_flyweight(cost_impl(cost1.get_impl(), cost2.get_impl(),
                               combine_tag()))
  {
  }

  /** \brief Create a cost in which a single level is set.
   */
  cost(int index, const level &l)
    : impl_flyweight(cost_impl(index, l))
  {
  }

  /** \brief Create a new cost that computes the lower bound of
   *  two input costs.
   */
  cost(const cost &cost1, const cost &cost2, lower_bound_tag)
    : impl_flyweight(cost_impl(cost1.get_impl(), cost2.get_impl(),
                               lower_bound_tag()))
  {
  }

  /** \brief Create a new cost that computes the upper bound of
   *  two input costs.
   */
  cost(const cost &cost1, const cost &cost2, upper_bound_tag)
    : impl_flyweight(cost_impl(cost1.get_impl(), cost2.get_impl(),
                               upper_bound_tag()))
  {
  }

public:
  /** \brief Create the minimum cost.
   *
   *  This cost has no effect when combined with other costs using
   *  operator+.
   */
  cost()
    : impl_flyweight(cost_impl())
  {
  }

  /** \brief Create a cost in which the structural level is set to the
   *  given value.
   *
   *  \param structural_level The structural level of the resulting
   *  cost.
   */
  static cost make_advance_structural_level(int structural_level)
  {
    return cost(structural_level);
  }

  /** \brief Create a cost in which a single user level is raised
   *  to the given value.
   *
   *  \param index The index of the user level to set.
   *
   *  \param value The value to set the user level to.
   */
  static cost make_advance_user_level(int index,
                                      int value)
  {
    return cost(index, level::make_lower_bounded(value));
  }

  /** \brief Create a cost in which a fixed value is added to a
   *  single user level.
   *
   *  \param index The index of the user level to set.
   *
   *  \param value The value to add to the user level.
   */
  static cost make_add_to_user_level(int index,
                                     int value)
  {
    return cost(index, level::make_added(value));
  }

  /** \brief Compute the least upper bound of two costs.
   *
   *  This is the smallest cost that is greater than or equal to both
   *  input costs.
   */
  static cost least_upper_bound(const cost &cost1,
                                const cost &cost2);

  /** \brief Compute the greatest lower bound of two costs.
   *
   *  This is the largest cost that is less than or equal to both
   *  input costs.
   */
  static cost greatest_lower_bound(const cost &cost1,
                                   const cost &cost2);

  /** \brief Return \b true if this cost is greater than or equal
   *  to the other cost under the natural partial order.
   *
   *  Equivalent to testing whether least_upper_bound is this object,
   *  but doesn't create an intermediary.
   */
  bool is_above_or_equal(const cost &other) const
  {
    return get_impl().is_above_or_equal(other.get_impl());
  }

  /** \brief Compose two costs.
   *
   *  The composition of costs is both associative and
   *  commutative.
   */
  cost operator+(const cost &other) const
  {
    return cost(*this, other);
  }

  /** \brief Test whether two costs have the same level values. */
  bool operator==(const cost &other) const
  {
    return impl_flyweight == other.impl_flyweight;
  }

  /** \brief Test whether two costs don't have the same level values. */
  bool operator!=(const cost &other) const
  {
    return impl_flyweight != other.impl_flyweight;
  }

  /** \brief Write a description of a cost to an ostream.
   */
  void dump(std::ostream &out) const
  {
    get_impl().dump(out);
  }

  /** \brief Get the structural level that this cost raises its
   *  target to.
   */
  int get_structural_level() const
  {
    return get_impl().get_structural_level();
  }

  /** \brief Get the value of this cost at a user level. */
  level get_user_level(int idx) const
  {
    return get_impl().get_user_level(idx);
  }

  /** \brief Check whether the cost contains any values at user
   *  levels.
   */
  bool get_has_user_levels() const
  {
    return get_impl().get_has_user_levels();
  }

  std::size_t get_hash_value() const
  {
    return get_impl().get_hash_value();
  }

  /** \brief Compare costs according to their
   *  identity, NOT their natural order.
   *
   *  This is a total ordering that can be used to place costs
   *  into ordered data structures.  It has no relation to the natural
   *  partial ordering on costs that least_upper_bound and
   *  greatest_upper_bound rely upon.
   */
  int compare(const cost &other) const
  {
    const cost_impl &this_cost = get_impl(), &other_cost = other.get_impl();

    // Rely on equality of flyweights being fast.
    if(this_cost == other_cost)
      return 0;
    else
      return this_cost.compare(other_cost);
  }
};

namespace aptitude
{
  namespace util
  {
    template<>
    class compare3_f<cost>
    {
    public:
      int operator()(const cost &cost1, const cost &cost2) const
      {
	return cost1.compare(cost2);
      }
    };
  }
}

std::size_t hash_value(const cost &cost);

std::ostream &operator<<(std::ostream &out, const cost &t);

#endif // COST_H
