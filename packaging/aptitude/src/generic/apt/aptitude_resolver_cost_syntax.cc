/** \file aptitude_resolver_cost_syntax.cc */


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

#include "aptitude_resolver_cost_syntax.h"

#include <ostream>

#include <generic/util/parsers.h>

namespace
{
  // Parser for individual cost entries.
  //
  // Cost entries consist of either a name or a scaling factor.  Names
  // have the form [a-zA-Z][a-zA-Z0-9-_]*; i.e, they must lead with a
  // letter, but later letters can include other alphanumerics and
  // hyphens.  (quoted strings might come into play eventually, to
  // support GUI users who don't want to know weird syntactic
  // restrictions)
  //
  // Note that this is a lexeme parser and doesn't need to be wrapped.
  class cost_entry_parser : public parsers::parser_base<cost_entry_parser, cost_component_structure::entry>
  {
    struct make_entry
    {
      typedef cost_component_structure::entry result_type;

      result_type operator()(int scaling_factor, const boost::shared_ptr<std::string> &name) const
      {
        return result_type(*name, scaling_factor);
      }
    };

  public:
    template<typename ParseInput>
    cost_component_structure::entry do_parse(ParseInput &input) const
    {
      using namespace parsers;

      // TODO: a convenience routine to parse character sets would be
      // nice.

      // Note that "max" is not an allowed cost name, to make room for
      // using it as an operator in future expansions.

      return apply(make_entry(),
                   (  ( (lexeme(integer()) << lexeme(ch('*'))) | val(1) ) << notFollowedBy(str("max")),
                      container(std::string(), lexeme(alpha() + many(alnum() | ch('-') | ch('_'))))  )).parse(input);
    }

    void get_expected(std::ostream &out) const
    {
      out << "cost value";
    }
  };

  class cost_component_structure_parser : public parsers::parser_base<cost_component_structure_parser, cost_component_structure>
  {
    struct make_cost_component_structure
    {
      typedef cost_component_structure result_type;

      result_type operator()(cost_component_structure::op combining_op,
                             const boost::shared_ptr<std::vector<cost_component_structure::entry> > &entries) const
      {
        return cost_component_structure(combining_op, *entries);
      }
    };

    // Probably the easiest way to decide whether to combine with
    // "add" or "none".  I could do this with just parsers below, but
    // it would get ugly.
    struct make_add_or_none_cost_component_structure
    {
      typedef cost_component_structure result_type;

      result_type operator()(const boost::shared_ptr<std::vector<cost_component_structure::entry> > &entries) const
      {
        return cost_component_structure(entries->size() == 1
                                          ? cost_component_structure::combine_none
                                          : cost_component_structure::combine_add,
                                        *entries);
      }

    };

  public:
    template<typename ParseInput>
    cost_component_structure do_parse(ParseInput &input) const
    {
      using namespace parsers;

      // Parse plan: check for "max" up front first (and check that it
      // can't be a cost component name).  If we see it, parse an
      // argument list to max().  Otherwise, parse one or more cost
      // entries separated by '+'.  If there's only one, we return a
      // structure with the operator set to "none"; otherwise we set
      // it to "add".
      return (  (maybe(lexeme(str("max"))) >>
                 notFollowedBy(alnum() | ch('-') | ch('_')) >>
                 apply(make_cost_component_structure(),
                       (   val(cost_component_structure::combine_max)
                        ,  lexeme(ch('('))
                        >> sepByPlus(lexeme(ch(',')),
                                     cost_entry_parser())
                        << lexeme(ch(')')) )))
               | apply(make_add_or_none_cost_component_structure(),
                       sepByPlus(lexeme(ch('+')),
                                 cost_entry_parser()))  ).parse(input);
    }

    void get_expected(std::ostream &out) const
    {
      out << "a combination of cost components";
    }
  };
}

class unpack_parse_result_visitor : public boost::static_visitor<boost::shared_ptr<std::vector<cost_component_structure> > >
{
public:
  boost::shared_ptr<std::vector<cost_component_structure> > operator()(const parsers::ParseException &ex) const
  {
    throw ResolverCostParseException(ex.what(),
                                     ex.get_raw_msg(),
                                     ex.get_line_number(),
                                     ex.get_column_number());
  }

  boost::shared_ptr<std::vector<cost_component_structure> > operator()(const boost::shared_ptr<std::vector<cost_component_structure> > &rval) const
  {
    return rval;
  }
};

boost::shared_ptr<std::vector<cost_component_structure> >
parse_cost_settings(const std::string &settings)
{
  using namespace parsers;

  boost::variant<boost::shared_ptr<std::vector<cost_component_structure> >, ParseException>
    result = parsers::parse(settings, sepByPlus(lexeme(ch(',')), cost_component_structure_parser()) << eof());

  return boost::apply_visitor(unpack_parse_result_visitor(),
                              result);
}


void dump_settings(std::ostream &out, const boost::shared_ptr<std::vector<cost_component_structure> > &settings)
{
  out << *settings;
}

std::ostream &operator<<(std::ostream &out, const std::vector<cost_component_structure> &settings)
{
  bool first_component = true;
  for(std::vector<cost_component_structure>::const_iterator
        settings_it = settings.begin(); settings_it != settings.end(); ++settings_it)
    {
      const std::vector<cost_component_structure::entry> &entries =
        *settings_it->get_entries();

      if(entries.empty())
        continue;

      if(first_component)
        first_component = false;
      else
        out << ", ";

      out << *settings_it;
    }

  return out;
}

std::ostream &operator<<(std::ostream &out, const boost::shared_ptr<std::vector<cost_component_structure> > &settings)
{
  dump_settings(out, settings);

  return out;
}
