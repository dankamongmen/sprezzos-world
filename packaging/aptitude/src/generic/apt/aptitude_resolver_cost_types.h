/** \file aptitude_resolver_cost_types.h */   // -*-c++-*-


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

#ifndef APTITUDE_RESOLVER_COST_TYPES_H
#define APTITUDE_RESOLVER_COST_TYPES_H

#include <boost/make_shared.hpp>
#include <boost/shared_ptr.hpp>

#include <cwidget/generic/util/exception.h>

#include <string>

#include <iosfwd>

class CostTypeCheckFailure : public std::exception
{
  std::string msg;

public:
  CostTypeCheckFailure(const std::string &_msg)
    : msg(_msg)
  {
  }

  ~CostTypeCheckFailure() throw() { }

  const char *what() const throw() { return msg.c_str(); }
};

/** \brief A representation of the syntactic structure of a single
 *  cost component.
 *
 *  Could be used to round-trip cost definitions between a settings
 *  object and the GUI.
 *
 *  Each cost component is a list of sub-components combined with
 *  either max() or "+".  Sub-components can be multiplied by a
 *  scaling factor.
 */
class cost_component_structure
{
public:
  /** \brief The operator used to combine sub-components. */
  enum op
    {
      /** \brief Combine sub-components with "+". */
      combine_add,

      /** \brief Combine sub-components with "max()". */
      combine_max,

      /** \brief Sub-components can't be combined.
       *
       *  Corresponds to a situation where there is only one
       *  sub-component, so there isn't a combining operation that we
       *  could use to infer the sub-component's type.  It is an error
       *  to have more than one sub-component in this case.
       */
      combine_none,
    };

  /** \brief Represents one entry in the cost component. */
  class entry
  {
    std::string name;
    int scaling_factor;

  public:
    entry(const std::string &_name, int _scaling_factor)
      : name(_name),
        scaling_factor(_scaling_factor)
    {
    }

    const std::string &get_name() const { return name; }
    int get_scaling_factor() const { return scaling_factor; }

    bool operator==(const entry &other) const
    {
      return name == other.name && scaling_factor == other.scaling_factor;
    }
  };

private:
  op combining_op;
  boost::shared_ptr<std::vector<entry> > entries;

public:
  cost_component_structure(op _combining_op,
                           const std::vector<entry> &_entries)
    : combining_op(_combining_op),
      entries(boost::make_shared<std::vector<entry> >(_entries))
  {
  }

  template<typename Iter>
  cost_component_structure(op _combining_op, Iter begin, Iter end)
    : combining_op(_combining_op),
      entries(boost::make_shared<std::vector<entry> >(begin, end))
  {
  }

  /** \brief Retrieve the operation used to combine the entries of
   *  this component.
   */
  op get_combining_op() const
  {
    return combining_op;
  }

  /** \brief Retrieve the entries of this component. */
  const boost::shared_ptr<std::vector<entry> > &get_entries() const
  {
    return entries;
  }

  /** \brief Compare two components for structural equality. */
  bool operator==(const cost_component_structure &other) const;
};

std::ostream &operator<<(std::ostream &out, const cost_component_structure &costs);

#endif // APTITUDE_RESOLVER_COST_TYPES_H
