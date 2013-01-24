/** \file aptitude_resolver_cost_settings.cc */

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

#include "aptitude_resolver_cost_settings.h"


#include "aptitude_resolver.h"
#include "aptitude_resolver_cost_syntax.h"

#include <boost/format.hpp>
#include <boost/make_shared.hpp>
#include <boost/multi_index_container.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/multi_index/mem_fun.hpp>
#include <boost/multi_index/random_access_index.hpp>
#include <boost/optional.hpp>

using namespace boost::multi_index;

// This code acts as a frontend that interprets the cost language for
// the resolver.  It type-checks costs to ensure that they don't go
// wrong at "runtime", it drops costs that aren't used by the final
// cost expression, and it pushes costs through to the components they
// actually modify.  For instance, if the first cost component is
// "removals + canceled-upgrades", any addition to "removals" or
// "canceled-upgrades" will actually modify that level in the
// resolver.

class aptitude_resolver_cost_settings::settings_impl
{
  /** \brief Used to track the effect of a named component.
   */
  class component_effect
  {
    int id; // The component index.
    int multiplier; // How much to multiply settings to this component
                    // by.
  public:
    component_effect(int _id, int _multiplier)
      : id(_id), multiplier(_multiplier)
    {
    }

    int get_id() const { return id; }
    int get_multiplier() const { return multiplier; }
  };

  class entry
  {
    std::string name;
    // If the type doesn't have a value, it's because the component
    // hasn't been seen in a typed context yet, so its type is
    // "bottom".
    boost::optional<component_type> type;
    // The actual cost components that are modified by this entry.
    std::vector<component_effect> effects;

  public:
    entry(const std::string &_name,
          const boost::optional<component_type> &_type,
          const std::vector<component_effect> &_effects)
      : name(_name),
        type(_type),
        effects(_effects)
    {
    }

    const std::string &get_name() const { return name; }
    const boost::optional<component_type> &get_type() const { return type; }
    void set_type(const boost::optional<component_type> &new_type)
    {
      type = new_type;
    }
    const std::vector<component_effect> &get_effects() const { return effects; }
    std::vector<component_effect> &get_effects() { return effects; }
  };

  // Modifying functor used to add an effect to an entry.
  class add_effect_f
  {
    component_effect new_effect;
  public:
    add_effect_f(const component_effect &_new_effect)
      : new_effect(_new_effect)
    {
    }

    void operator()(entry &e) const
    {
      e.get_effects().push_back(new_effect);
    }
  };

  // Combine the target value's component type with the incoming type,
  // throwing an error if they're incompatible.
  class merge_types_f
  {
    boost::optional<component_type> type;
    std::string name;

  public:
    merge_types_f(const boost::optional<component_type> &_type)
      : type(_type)
    {
    }

    void operator()(entry &e) const
    {
      if(type)
        {
          if(!e.get_type())
            e.set_type(type);
          else if(type != e.get_type())
            throw CostTypeCheckFailure((boost::format(_("Conflicting types for the cost component %s."))
                                        % e.get_name()).str());
        }
    }
  };

  // Tags for the entry holder.
  class name_t;
  class ordered_t;

  typedef multi_index_container<
    entry,
    indexed_by<
      random_access<tag<ordered_t> >,
      hashed_unique<tag<name_t>,
                    const_mem_fun<entry,
                                  const std::string &,
                                  &entry::get_name> > > >
  entry_holder;

  typedef entry_holder::index<name_t>::type by_name_index;
  typedef entry_holder::index<ordered_t>::type ordered_index;

  entry_holder entries;

  boost::shared_ptr<std::vector<cost_component_structure> > settings;

public:
  explicit settings_impl(const boost::shared_ptr<std::vector<cost_component_structure> > &_settings)
    : settings(_settings)
  {
    // Every time a cost component is named in the given settings
    // object, we look it up (creating it if it doesn't exist), check
    // that it has the appropriate type, and link it to the
    // corresponding entry in this object.

    by_name_index &by_name = entries.get<name_t>();
    ordered_index &ordered = entries.get<ordered_t>();

    int component_idx = 0;
    for(std::vector<cost_component_structure>::const_iterator
          settings_it = settings->begin(); settings_it != settings->end(); ++settings_it)
      {
        cost_component_structure::op op = settings_it->get_combining_op();
        std::vector<cost_component_structure::entry> &component =
          *settings_it->get_entries();

        boost::optional<component_type> type;
        switch(op)
          {
          case cost_component_structure::combine_add:
            type = additive;
            break;

          case cost_component_structure::combine_max:
            type = maximized;
            break;

          case cost_component_structure::combine_none:
            type = boost::optional<component_type>();
            break;

          default:
            throw CostTypeCheckFailure("Internal error: invalid cost component operation code.");
          }

        for(std::vector<cost_component_structure::entry>::const_iterator
              component_it = component.begin(); component_it != component.end(); ++component_it)
          {
            // Unpack this piece of the component.
            const std::string &name = component_it->get_name();
            int scaling_factor = component_it->get_scaling_factor();

            component_effect effect(component_idx, scaling_factor);

            by_name_index::iterator found = by_name.find(name);
            if(found == by_name.end())
              {
                std::vector<component_effect> effects;
                effects.push_back(effect);

                ordered.push_back(entry(name, type, effects));
              }
            else
              {
                by_name.modify(found, merge_types_f(type));
                by_name.modify(found, add_effect_f(effect));
              }
          }

        ++component_idx;
      }
  }

  component get_or_create_component(const std::string &name,
                                    component_type type)
  {
    // Look the component up by name.  If it doesn't exist, we can go
    // ahead and create it.  If it does exist, we need to check that
    // the types match.  In either case, if no "real" cost component
    // is affected by the named component, a component ID of -1 is
    // returned (indicating that the component has no real effect).

    by_name_index &by_name = entries.get<name_t>();
    by_name_index::iterator found = by_name.find(name);

    if(found == by_name.end())
      {
        by_name.insert(entry(name, type, std::vector<component_effect>()));
        return component(-1);
      }
    else
      {
        ordered_index &ordered = entries.get<ordered_t>();
        ordered_index::iterator found_ordered = entries.project<ordered_t>(found);
        component rval(found->get_effects().empty() ? -1 : found_ordered - ordered.begin());

        by_name.modify(found, merge_types_f(type));

        return rval;
      }
  }

  cost add_to_cost(const component &component,
                   int amt)
  {
    // Ignore irrelevant components.
    if(component.id < 0)
      return cost_limits::minimum_cost;

    // Sanity-check that the component's type is additive, then add
    // the given value to each target cost component.

    ordered_index &ordered = entries.get<ordered_t>();

    if((unsigned int)component.id >= ordered.size())
      throw CostTypeCheckFailure("Internal error: mismatch between component ID and the number of components.");

    ordered.modify(ordered.begin() + component.id, merge_types_f(additive));

    const entry &e = ordered[component.id];
    cost rval;
    for(std::vector<component_effect>::const_iterator it = e.get_effects().begin();
        it != e.get_effects().end(); ++it)
      {
        cost c =
          cost::make_add_to_user_level(it->get_id(), amt * it->get_multiplier());

        rval = rval + c;
      }

    return rval;
  }

  cost raise_cost(const component &component,
                  int amt)
  {
    // Ignore irrelevant components.
    if(component.id < 0)
      return cost_limits::minimum_cost;

    // Sanity-check that the component's type is maximized, then add
    // the given value to each target cost component.

    ordered_index &ordered = entries.get<ordered_t>();

    if((unsigned int)component.id >= ordered.size())
      throw CostTypeCheckFailure("Internal error: mismatch between component ID and the number of components.");

    ordered.modify(ordered.begin() + component.id, merge_types_f(maximized));

    cost rval;
    const entry &e = ordered[component.id];
    for(std::vector<component_effect>::const_iterator it = e.get_effects().begin();
        it != e.get_effects().end(); ++it)
      {
        cost c =
          cost::make_advance_user_level(it->get_id(), amt * it->get_multiplier());

        rval = rval + c;
      }

    return rval;
  }

  void dump(std::ostream &out) const
  {
    dump_settings(out, settings);
  }
};

aptitude_resolver_cost_settings::aptitude_resolver_cost_settings(const boost::shared_ptr<std::vector<cost_component_structure> > &settings)
  : impl(boost::make_shared<settings_impl>(settings))
{
}

aptitude_resolver_cost_settings::~aptitude_resolver_cost_settings()
{
}

void aptitude_resolver_cost_settings::dump(std::ostream &out) const
{
  impl->dump(out);
}

aptitude_resolver_cost_settings::component
aptitude_resolver_cost_settings::get_or_create_component(const std::string &name,
                                                         component_type type)
{
  return impl->get_or_create_component(name, type);
}

cost aptitude_resolver_cost_settings::add_to_cost(const component &component,
                                                  int amt)
{
  return impl->add_to_cost(component, amt);
}

cost aptitude_resolver_cost_settings::raise_cost(const component &component,
                                                 int amt)
{
  return impl->raise_cost(component, amt);
}
