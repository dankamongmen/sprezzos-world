// load_sortpolicy.cc
//
//   Copyright (C) 2001, 2005 Daniel Burrows
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
//
//  Loads sorting policies -- similar to grouping policies.  (in fact, I did
// this with cut&paste, and think maybe I should consider trying to share
// code between the two parsers..)

#include "load_sortpolicy.h"

#include <string>
#include <vector>
#include <map>

#include <ctype.h>

#include <apt-pkg/error.h>

#include "aptitude.h"
#include "pkg_sortpolicy.h"

using namespace std;

typedef vector<string> arglist;
typedef pkg_sortpolicy * (*pkg_sortpolicy_parser)(const arglist &,
						  pkg_sortpolicy *,
						  bool);
typedef map<string, pkg_sortpolicy_parser> parser_map;

pkg_sortpolicy *parse_name_policy(const arglist &args,
				  pkg_sortpolicy *chain,
				  bool reversed)
{
  if(args.size()>0)
    {
      _error->Error(_("Sorting policy '%s' takes no arguments"), "name");
      return NULL;
    }

  return pkg_sortpolicy_name(chain, reversed);
}

pkg_sortpolicy *parse_ver_policy(const arglist &args,
				 pkg_sortpolicy *chain,
				 bool reversed)
{
  if(args.size() > 0)
    {
      _error->Error(_("Sorting policy '%s' takes no arguments"), "version");
      return NULL;
    }

  return pkg_sortpolicy_ver(chain, reversed);
}

pkg_sortpolicy *parse_installsize_policy(const arglist &args,
					 pkg_sortpolicy *chain,
					 bool reversed)
{
  if(args.size()>0)
    {
      _error->Error(_("Sorting policy '%s' takes no arguments"), "installsize");
      return NULL;
    }

  return pkg_sortpolicy_installed_size(chain, reversed);
}

pkg_sortpolicy *parse_priority_policy(const arglist &args,
				      pkg_sortpolicy *chain,
				      bool reversed)
{
  if(args.size()>0)
    {
      _error->Error(_("Sorting policy '%s' takes no arguments"), "priority");
      return NULL;
    }

  return pkg_sortpolicy_priority(chain, reversed);
}


pkg_sortpolicy *parse_debsize_policy(const arglist &args,
					 pkg_sortpolicy *chain,
					 bool reversed)
{
  if(args.size()>0)
    {
      _error->Error(_("Sorting policy '%s' takes no arguments"), "debsize");
      return NULL;
    }

  return pkg_sortpolicy_debsize(chain, reversed);
}


static parser_map parse_types;

static void init_parse_types()
{
  static bool initted_parse_types=false;

  if(!initted_parse_types)
    {
      parse_types["name"]=parse_name_policy;
      parse_types["version"] = parse_ver_policy;
      parse_types["installsize"]=parse_installsize_policy;
      parse_types["debsize"]=parse_debsize_policy;
      parse_types["priority"]=parse_priority_policy;
      initted_parse_types=true;
    }
}

struct parse_atom
{
  string name;
  arglist args;
  bool reversed;

  parse_atom():reversed(false) {}
};

// Parses a chain of sorting policies from the given string and returns them.
pkg_sortpolicy *parse_sortpolicy(string s)
{
  init_parse_types();

  typedef vector<parse_atom> temp_parse_list;
  temp_parse_list parsed;

  string::size_type i=0;

  while(i<s.size())
    {
      parse_atom current;

      if(s[i]=='~')
	{
	  current.reversed=true;
	  i++;
	}

      // Find the first name.  Use tolower for nice case-insensitivity.
      while(i<s.size() && s[i]!=',' && s[i]!='(')
	current.name+=tolower(s[i++]);

      if(current.name.size()==0)
	{
	  _error->Error(_("Invalid zero-length sorting policy name"));
	  return NULL;
	}

      if(i<s.size() && s[i]=='(') // Parse argument list
	{
	  while(i<s.size() && s[i]!=')')
	    {
	      ++i; // Clear out the leading '(' or ','
	      string curarg;
	      while(i<s.size() && s[i]!=',' && s[i]!=')')
		curarg+=s[i++];

	      current.args.push_back(curarg);
	    }

	  if(!(i<s.size() && s[i]==')')) // Unexpected EOT, bail
	    {
	      _error->Error(_("Unmatched '(' in sorting policy description"));
	      return NULL;
	    }

	  ++i; // Clear out the ')'
	}

      parsed.push_back(current);
      // Ew, takes kinda long.  Probably not an issue now, but creating current
      // as a new item in parsed would be faster.

      if(i<s.size() && s[i]==',')
	i++; // Clear out trailing commas.
    }

  // Now run through the parsed stuff from back-to-front and instantiate it.
  pkg_sortpolicy *rval=NULL;
  temp_parse_list::reverse_iterator j=parsed.rbegin();

  while(j!=parsed.rend())
    {
      // Look up the parse function
      parser_map::iterator found=parse_types.find(j->name);

      // Die gracefully if it's bad.
      if(found==parse_types.end())
	{
	  _error->Error(_("Invalid sorting policy type '%s'"),
			  j->name.c_str());
	  delete rval;
	  return NULL;
	}

      // Apply it to the argument list
      pkg_sortpolicy *new_rval=found->second(j->args,
					     rval,
					     j->reversed);

      // Check for failure
      if(!new_rval)
	{
	  delete rval;
	  return NULL;
	}

      rval=new_rval;
      ++j;
    }

  return rval;
}
