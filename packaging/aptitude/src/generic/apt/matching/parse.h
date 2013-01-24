// parse.h  -*-c++-*-
//
//  Copyright 2000-2001, 2005, 2007-2008 Daniel Burrows
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; see the file COPYING.  If not, write to
//  the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//  Boston, MA 02111-1307, USA.

#ifndef PARSE_H
#define PARSE_H

#include <cwidget/generic/util/ref_ptr.h>

/** \file parse.h */

/** \brief Code to parse search patterns. */

namespace aptitude
{
  namespace matching
  {
    class pattern;

    /** \brief Parse a string region as a search pattern.
     *
     *  Parses [start, end), treating strings from terminators as
     *  ending the parse if they are encountered at the top syntactic
     *  level.
     *
     *  \throw MatchingException if the parse fails.
     *
     *  \param start   The beginning of the range to parse.
     *                 Will be moved past the parsed expression.
     *  \param end     The end of the range to parse.
     *  \param terminators  A list of strings whose (unescaped)
     *                      presence at the top syntactic level
     *                      signals the end of a search pattern.
     *  \param require_full_parse  If \b true, an error will be
     *                             signalled if part of the region
     *                             is left after parsing (i.e.,
     *                             if start != end).
     *  \param partial      If \b true, if the last word is an
     *                      unadorned Xapian term, it will be
     *                      treated as a wildcard representing
     *                      all the terms in the database.
     *
     *  \return  The parsed expression, or NULL if an
     *  error occurred or if the input range was empty.
     */
    cwidget::util::ref_ptr<pattern>
      parse_with_errors(std::string::const_iterator &start,
			const std::string::const_iterator &end,
			const std::vector<const char *> &terminators,
			bool require_full_parse,
			bool partial);

    /** \brief Parse a string region as a search pattern.
     *
     *  Parses [start, end), treating strings from terminators as
     *  ending the parse if they are encountered at the top syntactic
     *  level.
     *
     *  \param start   The beginning of the range to parse.
     *                 Will be moved past the parsed expression.
     *  \param end     The end of the range to parse.
     *  \param terminators  A list of strings whose (unescaped)
     *                      presence at the top syntactic level
     *                      signals the end of a search pattern.
     *  \param flag_errors  If \b true, apt errors (in _error)
     *                      will be generated for any parsing
     *                      errors that are encountered.
     *  \param require_full_parse  If \b true, an error will be
     *                             signalled if part of the region
     *                             is left after parsing (i.e.,
     *                             if start != end).
     *  \param partial      If \b true, if the last word is an
     *                      unadorned Xapian term, it will be
     *                      treated as a wildcard representing
     *                      all the terms in the database.
     *
     *  \return  The parsed expression, or NULL if an
     *  error occurred or if the input range was empty.
     */
    cwidget::util::ref_ptr<pattern>
      parse(std::string::const_iterator &start,
	    const std::string::const_iterator &end,
	    const std::vector<const char *> &terminators,
	    bool flag_errors,
	    bool require_full_parse,
	    bool partial);


    /** \brief Parse a string as a search pattern.
     *
     *  Parses the given string, treating strings from terminators as
     *  ending the parse if they are encountered at the top syntactic
     *  level.
     *
     *  \param s       The string to parse.
     *  \param terminators  A list of strings whose (unescaped)
     *                      presence at the top syntactic level
     *                      signals the end of a search pattern.
     *  \param flag_errors  If \b true, apt errors (in _error)
     *                      will be generated for any parsing
     *                      errors that are encountered.
     *  \param require_full_parse  If \b true, an error will be
     *                             signalled if part of the string
     *                             is left after parsing.
     *
     *  \return  The parsed expression, or NULL if an
     *  error occurred or if the input string was empty.
     */
    inline cwidget::util::ref_ptr<pattern>
      parse(const std::string &s,
	    const std::vector<const char *> &terminators,
	    bool flag_errors,
	    bool require_full_parse)
    {
      std::string::const_iterator start = s.begin();
      return parse(start, s.end(),
		   terminators,
		   flag_errors,
		   require_full_parse,
		   false);
    }

    inline cwidget::util::ref_ptr<pattern>
      parse(const std::string &s,
	    const std::vector<const char *> &terminators,
	    bool flag_errors,
	    bool require_full_parse,
	    bool partial)
    {
      std::string::const_iterator start = s.begin();
      return parse(start, s.end(),
		   terminators,
		   flag_errors,
		   require_full_parse,
		   partial);
    }

    /** \brief Parse a string as a search pattern. */
    inline cwidget::util::ref_ptr<pattern>
      parse(const std::string &s,
	    bool flag_errors,
	    bool require_full_parse)
    {
      return parse(s, std::vector<const char *>(),
		   flag_errors,
		   require_full_parse);
    }

    /** \brief Parse a string as a search pattern. */
    inline cwidget::util::ref_ptr<pattern>
      parse(const std::string &s,
	    bool flag_errors,
	    bool require_full_parse,
	    bool partial)
    {
      return parse(s, std::vector<const char *>(),
		   flag_errors,
		   require_full_parse,
		   partial);
    }

    /** \brief Parse a string as a search pattern.
     *
     *  Errors will be flagged by generating apt errors, and it is an
     *  error if part of the input string is left over after parsing.
     *
     *  \param s       The string to parse.
     *
     *  \return  The parsed expression, or NULL if an
     *  error occurred or if the input string was empty.
     */
    inline cwidget::util::ref_ptr<pattern>
      parse(const std::string &s)
    {
      return parse(s, true, true);
    }

    /** \brief Parse a string as a search pattern.
     *
     *  It is an error if part of the input string is left over after
     *  parsing.
     *
     *  \throw MatchingException if the pattern cannot be parsed.
     *
     *  \param s       The string to parse.
     *
     *  \return  The parsed expression, or NULL if an
     *  error occurred or if the input string was empty.
     */
    inline cwidget::util::ref_ptr<pattern>
      parse_with_errors(const std::string &s)
    {
      std::string::const_iterator begin(s.begin());
      return parse_with_errors(begin, s.end(),
			       std::vector<const char *>(),
			       true, false);
    }

    /** \brief Parse a string as a possibly incomplete search pattern.
     *
     *  Errors will be flagged by generating apt errors, and it is an
     *  error if part of the input string is left over after parsing.
     *
     *  \param s       The string to parse.
     *
     *  \return  The parsed expression, or NULL if an
     *  error occurred or if the input string was empty.
     */
    inline cwidget::util::ref_ptr<pattern>
      parse_with_extension(const std::string &s)
    {
      return parse(s, true, true, true);
    }
  }
}

#endif
