/** \file aptitude_resolver_cost_syntax.h */     // -*-c++-*-

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

#ifndef APTITUDE_RESOLVER_COST_SYNTAX_H
#define APTITUDE_RESOLVER_COST_SYNTAX_H

#include "aptitude_resolver_cost_types.h"

#include <boost/shared_ptr.hpp>
#include <vector>

/** \brief Exception type thrown by parse_cost_settings(). */
class ResolverCostParseException : public std::exception
{
  std::string msg;
  std::string rawMsg;
  int lineNumber;
  int columnNumber;

public:
  ResolverCostParseException(const std::string &_msg,
                             const std::string &_rawMsg,
                             int _lineNumber,
                             int _columnNumber)
    : msg(_msg),
      rawMsg(_rawMsg),
      lineNumber(_lineNumber),
      columnNumber(_columnNumber)
  {
  }

  ~ResolverCostParseException() throw() { }

  const std::string &get_raw_msg() const { return rawMsg; }
  int get_line_number() const { return lineNumber; }
  int get_column_number() const { return columnNumber; }

  const char *what() const throw() { return msg.c_str(); }
};

/** \brief Parse a settings string.
 *
 *  Throws a std::exception if parsing failed.
 */
boost::shared_ptr<std::vector<cost_component_structure> >
parse_cost_settings(const std::string &settings);

/** \brief Write some settings to a stream.
 *
 *  This produces a string that, when passed to parse_cost_settings(),
 *  will produce an identical settings object to one passed as a
 *  parameter to dump_settings().
 */
void dump_settings(std::ostream &out, const boost::shared_ptr<std::vector<cost_component_structure> > &settings);

/** \brief Synonym for dump_settings(). */
std::ostream &operator<<(std::ostream &out, const boost::shared_ptr<std::vector<cost_component_structure> > &settings);

/** \brief Synonym for dump_settings(). */
std::ostream &operator<<(std::ostream &out, const std::vector<cost_component_structure> &settings);

#endif // APTITUDE_RESOLVER_COST_SYNTAX_H

