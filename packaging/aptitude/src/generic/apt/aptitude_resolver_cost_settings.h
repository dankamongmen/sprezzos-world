/** \file aptitude_resolver_cost_settings.h */ // -*-c++


// Copyright (C) 2010 Daniel Burrows

// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.

// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

#ifndef APTITUDE_RESOLVER_COST_SETTINGS_H
#define APTITUDE_RESOLVER_COST_SETTINGS_H

#include "aptitude_resolver_cost_types.h"

#include <generic/problemresolver/cost.h>

#include <boost/shared_ptr.hpp>
#include <vector>

class aptitude_resolver;
class aptitude_resolver_version;

/** \brief Core data structure for keeping track of which cost
 *  components are active and how they interact.
 *
 *  Only active cost components have their costs actually registered
 *  in the resolver.  Also, this class knows how to parse and
 *  serialize the set of registered components.
 */
class aptitude_resolver_cost_settings
{
  // Hide the implementation.
  class settings_impl;

  boost::shared_ptr<settings_impl> impl;
public:
  /** \brief Reference to a single cost component.
   *
   *  Used to avoid unnecessary string lookups.
   */
  class component
  {
    // I use an integer instead of an iterator to avoid forcing every
    // other module to know the type of this object's internal
    // container.
    //
    // Note: internally, -1 is used to indicate that a component
    // doesn't affect any component of the resolver's cost structure.
    // These components are *also* stored in the settings object, to
    // ensure that they're used consistently, but they have no effect
    // on any resolver cost.
    int id;

    friend class aptitude_resolver_cost_settings;

    component(int _id)
      : id(_id)
    {
    }

  public:
    component()
      : id(-1)
    {
    }

    component(const component &other)
      : id(other.id)
    {
    }

    component &operator=(const component &other)
    {
      id = other.id;
      return *this;
    }

    // I don't implement a full complement of comparison / hash
    // operators because they aren't needed yet.
  };

  /** \brief Possible cost component types. */
  enum component_type
    {
      /** \brief The type of components that can be added together. */
      additive,
      /** \brief The type of components that can be increased stepwise
       *         via max().
       */
      maximized
    };

  /** \brief Validate the given cost settings and construct a cost
   *  settings object from it.
   *
   *  \param settings  The components of the cost.
   */
  explicit aptitude_resolver_cost_settings(const boost::shared_ptr<std::vector<cost_component_structure> > &settings);
  ~aptitude_resolver_cost_settings();

  /** \brief Test whether the given component is relevant to the cost
   *  settings.
   *
   *  If this returns \b false, then modifying this component has no
   *  effect; can be used to short-circuit some matching logic.
   */
  bool is_component_relevant(const component &component) const
  {
    return component.id >= 0;
  }

  /** \brief Write these settings to an ostream. */
  void dump(std::ostream &out) const;

  /** \brief Get or create a cost component.
   *
   *  \param name  The string that the returned component
   *               is bound to.
   *  \param type  The type of the returned component.
   *
   *  \throws CostTypeCheckFailure if this component has already been
   *  instantiated with an incompatible type.
   *
   *  \note this may modify the type of an existing component if its
   *  type couldn't be inferred from the cost settings string.
   */
  component get_or_create_component(const std::string &name,
				    component_type type);

  /** \brief Get a cost that adds a value to a cost component.
   *
   *  The cost component must have type "additive".
   */
  cost add_to_cost(const component &component,
                   int amt);

  /** \brief Get a cost that raises a cost component to an upper
   *  bound.
   *
   *  The cost component must have type "maximized".
   */
  cost raise_cost(const component &component,
                  int amt);
};

inline std::ostream &operator<<(std::ostream &out, const aptitude_resolver_cost_settings &settings)
{
  settings.dump(out);
  return out;
}

#endif // APTITUDE_RESOLVER_COST_SETTINGS_H
