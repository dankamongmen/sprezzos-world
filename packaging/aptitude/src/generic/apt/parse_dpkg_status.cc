// parse_dpkg_status.cc
//
//   Copyright (C) 2008 Daniel Burrows
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

#include "parse_dpkg_status.h"

#include <aptitude.h>

#include <stdlib.h>

namespace aptitude
{
  namespace apt
  {
    namespace
    {
      // Status constants.
      const std::string status_pmerror("pmerror");
      const std::string status_pmconffile("pmconffile");

      // Parses [[:space:]]* and returns the next character.
      const char *parse_whitespace(const char * &start,
				   const char * const end)
      {
	while(start < end && isspace(*start))
	  ++start;

	return start;
      }

      // Parses [^:]*:, stripping whitespace and returning the text.
      std::string parse_colon_fragment(const char * &start,
				       const char * const end)
      {
	const char * const begin = parse_whitespace(start, end);

	while(start < end && *start != ':')
	  ++start;

	const char *last = start;
	if(start < end)
	  ++start;

	while(last != begin && isspace(last[-1]))
	  --last;

	return std::string(begin, last);
      }

      // Parses '[^']', returning the enclosed text.
      std::string parse_single_quoted_string(const char * start,
					     const char * const end)
      {
	parse_whitespace(start, end);
	if(start == end || *start != '\'')
	  // Erk!
	  return _("<aptitude: internal parse error: no apostrophe>");

	++start;
	const char * const rval_begin = start;
	while(start < end && *start != '\'')
	  ++start;

	if(start >= end)
	  return _("<aptitude: internal parse error: missing terminal apostrophe>");

	const char * const rval_end = start;
	++start;

	return std::string(rval_begin, rval_end);
      }

      std::string strip_range(const char *begin, const char *end)
      {
	while(begin < end && isspace(*begin))
	  ++begin;
	while(end > begin && isspace(end[-1]))
	  --end;

	return std::string(begin, end);
      }
    }

    std::ostream &operator<<(std::ostream &out, const dpkg_status_message &msg)
    {
      switch(msg.get_type())
	{
	case dpkg_status_message::error:
	  out << "error(percent = "
	      << msg.get_percent()
	      << ", package = "
	      << msg.get_package()
	      << ", text = "
	      << msg.get_text()
	      << ")";
	  break;

	case dpkg_status_message::conffile:
	  out << "conffile(existing_filename = "
	      << msg.get_existing_filename()
	      << ", new_filename = "
	      << msg.get_new_filename()
	      << ", percent = "
	      << msg.get_percent()
	      << ", text = "
	      << msg.get_text()
	      << ")";
	  break;

	case dpkg_status_message::status:
	  out << "status(percent = "
	      << msg.get_percent()
	      << ", package = "
	      << msg.get_package()
	      << ", text = "
	      << msg.get_text()
	      << ")";
	  break;
	}

      return out;
    }

    dpkg_status_message dpkg_status_message::parse(const char *buf, size_t len)
    {
      const char * const end = buf + len;

      const char *where = buf;

      // First find out what type of message it is.
      const std::string status(parse_colon_fragment(where, end));

      if(status == status_pmerror)
	{
	  // error: pkg: percent: message

	  const std::string pkg(parse_colon_fragment(where, end));
	  const std::string percent_str(parse_colon_fragment(where, end));
	  const std::string msg(strip_range(where, end));

	  char *percent_parse_end;
	  const double percent = strtod(percent_str.c_str(), &percent_parse_end);

	  return dpkg_status_message::make_error(percent, pkg, msg);
	}
      else if(status == status_pmconffile)
	{
	  // pmconffile: conffile-display-name: percent: 'old-file' 'new-file'

	  const std::string conffile(parse_colon_fragment(where, end));
	  const std::string percent_str(parse_colon_fragment(where, end));
	  const std::string msg(strip_range(where, end));

	  char *percent_parse_end;
	  const double percent = strtod(percent_str.c_str(), &percent_parse_end);

	  const std::string old_filename(parse_single_quoted_string(where, end));
	  const std::string new_filename(parse_single_quoted_string(where, end));

	  return dpkg_status_message::make_conffile(old_filename,
						    new_filename,
						    percent,
						    conffile);
	}
      else
	{
	  // (status): pkg: percent: message

	  const std::string package(parse_colon_fragment(where, end));
	  const std::string percent_str(parse_colon_fragment(where, end));
	  const std::string msg(strip_range(where, end));

	  char *percent_parse_end;
	  const double percent = strtod(percent_str.c_str(), &percent_parse_end);

	  return dpkg_status_message::make_status(percent, package, msg);
	}
    }

    dpkg_status_parser::dpkg_status_parser()
      : msg()
    {
    }

    void dpkg_status_parser::process_input(const char *buf, size_t len)
    {
      const char *begin = buf;
      const char * const end = buf + len;

      const char *where = begin;

      while(where < end)
	{
	  while(where < end && *where != '\n')
	    ++where;

	  msg.append(begin, where);
	  pending_messages.push_back(dpkg_status_message::parse(msg.c_str(), msg.size()));
	  msg.clear();

	  if(where < end)
	    ++where;

	  begin = where;
	}
    }
  }
}
