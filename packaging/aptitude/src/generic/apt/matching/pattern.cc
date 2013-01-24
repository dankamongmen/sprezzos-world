// pattern.cc
//
//   Copyright (C) 2008-2009 Daniel Burrows
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

/// \file pattern.cc

#include "pattern.h"

#include <cwidget/generic/util/ssprintf.h>

#include <aptitude.h>

using cwidget::util::ssprintf;

namespace aptitude
{
  namespace matching
  {
    MatchingException::MatchingException(const std::string &_msg)
      : msg(_msg)
    {
    }

    std::string MatchingException::errmsg() const
    {
      return msg;
    }

    regex::regex(const std::string &pattern, int cflags)
    {
      int err = regcomp(&r, pattern.c_str(), cflags);
      if(err != 0)
	{
	  size_t needed = regerror(err, &r, NULL, 0);

	  char *buf = new char[needed+1];

	  regerror(err, &r, buf, needed+1);

	  std::string msg(ssprintf("Regex compilation error: %s", buf));

	  delete[] buf;

	  throw MatchingException(msg);
	}
    }

    regex::~regex()
    {
      regfree(&r);
    }

    bool regex::exec(const char *s, regmatch_t *matches, size_t num_matches,
		     int eflags) const
    {
      return 0 == regexec(&r, s, num_matches, matches, eflags);
    }

    arch_specification::arch_specification(const std::string &_spec)
      : pams(_spec),
        spec(_spec)
    {
    }

    bool arch_specification::matches(const char * const &arch)
    {
      return pams(arch);
    }

    cwidget::util::ref_ptr<pattern>
    pattern::make_action(const action_type act)
    {
      return new pattern(action, act);
    }

    bool is_pattern(const std::string &s)
    {
      return s.find_first_of("~?") != s.npos;
    }
  }
}

#if 0
// A template switch statement that can be copied&pasted as the
// skeleton for a new pattern-processing function.

switch(p->get_type())
  {
    // Structural patterns:
  case pattern::all_versions:
  case pattern::and_tp:
  case pattern::any_version:
  case pattern::for_tp:
  case pattern::narrow:
  case pattern::not_tp:
  case pattern::or_tp:
  case pattern::widen:

    // Atomic patterns:
  case pattern::archive:
  case pattern::action:
  case pattern::architecture:
  case pattern::automatic:
  case pattern::bind:
  case pattern::broken:
  case pattern::broken_type:
  case pattern::candidate_version:
  case pattern::config_files:
  case pattern::current_version:
  case pattern::depends:
  case pattern::description:
  case pattern::essential:
  case pattern::equal:
  case pattern::false_tp:
  case pattern::foreign_architecture:
  case pattern::garbage:
  case pattern::install_version:
  case pattern::installed:
  case pattern::maintainer:
  case pattern::multiarch:
  case pattern::name:
  case pattern::native_architecture:
  case pattern::new_tp:
  case pattern::obsolete:
  case pattern::origin:
  case pattern::priority:
  case pattern::provides:
  case pattern::reverse_depends:
  case pattern::reverse_provides:
  case pattern::section:
  case pattern::source_package:
  case pattern::source_version:
  case pattern::tag:
  case pattern::task:
  case pattern::term:
  case pattern::true_tp:
  case pattern::upgradable:
  case pattern::user_tag:
  case pattern::version:
  case pattern::virtual_tp:

  default:
    throw MatchingException(std::string("Internal error: unhandled pattern type in ") + __FUNC__);
  }
#endif
