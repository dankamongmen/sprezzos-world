// parse.cc
//
//  Copyright 2000-2005, 2007-2009, 2011 Daniel Burrows
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
//
//  Grammer for the condition language.  (TODO: this is what the
//  grammar *will* be, not what it is)
//
//  CONDITION := CONDITION-LIST
//  CONDITION-LIST := CONDITION-AND-GROUP '|' CONDITION-LIST
//                 |  CONDITION-AND-GROUP
//  CONDITION-AND-GROUP := CONDITION-ATOM CONDITION-AND-GROUP
//                      := CONDITION-ATOM
//  CONDITION-ATOM := '(' CONDITION-LIST ')'
//                 |  '!' CONDITION-ATOM
//                 |  '?for' variable-name ':' CONDITION-LIST
//                 |  '?=' variable-name
//                 |  '?' (variable-name ':')?  condition-name '(' arguments... ')'
//                 |  '~'field-id <string>
//                 |  <string>
//
// The (arguments...) to a ?function-style term are parsed
// according to their expected type.  This is unfortunate but
// necessary: since arbitrary strings not containing metacharacters
// are legal condition values, distinguishing conditions from other
// argument types would require the user to type extra punctuation in,
// e.g., ?broken(Depends, ?name(apt.*)).

#include "parse.h"

#include "pattern.h"

#include <generic/apt/apt.h>
#include <generic/apt/aptcache.h>
#include <generic/apt/tags.h>
#include <generic/apt/tasks.h>

#include <aptitude.h>

#include <generic/util/immset.h>
#include <generic/util/util.h>

#include <cwidget/generic/util/ssprintf.h>
#include <cwidget/generic/util/transcode.h>

#include <set>

#include <stdarg.h>

#include <apt-pkg/error.h>
#include <apt-pkg/pkgcache.h>
#include <apt-pkg/pkgrecords.h>
#include <apt-pkg/pkgsystem.h>
#include <apt-pkg/version.h>

#include <cwidget/generic/util/eassert.h>
#include <ctype.h>
#include <stdio.h>
#include <regex.h>
#include <sys/types.h>

using namespace std;
namespace cw = cwidget;

using std::string;
using cw::util::ref_ptr;

namespace aptitude
{
namespace matching
{

namespace
{
  /** \brief Enumeration containing the known types of terms.
   *
   *  This enumeration describes the term types in the surface syntax,
   *  not the term types in the parse tree.
   *
   *  I want to have a table mapping term names to term types,
   *  and so the term type has to be POD.  Well, I could use some
   *  indirection pattern like grouping policies do, but IMNSHO the
   *  payoff in grouping-policy land has not made up for the syntactic
   *  clutter and semantic overhead.  I think that if anything it
   *  would be less valuable here.
   *
   *  Note that term for dependencies and for broken dependencies
   *  are parsed separately below.
   */
  enum term_type
    {
      term_type_action,
      term_type_all,
      term_type_and,
      term_type_any,
      term_type_architecture,
      term_type_archive,
      term_type_automatic,
      term_type_bind,
      term_type_broken,
      term_type_config_files,
      term_type_description,
      term_type_essential,
      term_type_exact_name,
      term_type_false,
      term_type_for,
      term_type_garbage,
      term_type_installed,
      term_type_maintainer,
      term_type_multiarch,
      term_type_name,
      term_type_narrow,
      term_type_new,
      term_type_not,
      term_type_obsolete,
      term_type_or,
      term_type_origin,
      term_type_priority,
      term_type_provides,
      term_type_section,
      term_type_source_package,
      term_type_source_version,
      term_type_tag,
      term_type_task,
      term_type_term,
      term_type_term_prefix,
      term_type_true,
      term_type_upgradable,
      term_type_user_tag,
      term_type_version,
      term_type_virtual,
      term_type_widen
    };

  struct term_info
  {
    /** \brief The string used to pick the term.
     */
    const char *name;

    /** \brief The term type indicated by this struct. */
    term_type type;
  };

  const term_info term_types[] =
  {
    { "action", term_type_action },
    { "all-versions", term_type_all },
    { "and", term_type_and },
    { "any-version", term_type_any },
    { "architecture", term_type_architecture },
    { "archive", term_type_archive },
    { "automatic", term_type_automatic },
    { "bind", term_type_bind },
    { "broken", term_type_broken },
    { "config-files", term_type_config_files },
    { "description", term_type_description },
    { "essential", term_type_essential },
    { "exact-name", term_type_exact_name },
    { "false", term_type_false },
    // ForTranslators: As in the sentence "for x = 5, do BLAH".
    { "for", term_type_for },
    { "garbage", term_type_garbage },
    { "installed", term_type_installed },
    { "maintainer", term_type_maintainer },
    { "multiarch", term_type_multiarch },
    { "name", term_type_name },
    /* ForTranslators: Opposite of widen. Search for "widen" in this file for details. */
    { "narrow", term_type_narrow },
    { "new", term_type_new },
    { "not", term_type_not },
    { "obsolete", term_type_obsolete },
    { "or", term_type_or },
    /* ForTranslators: This specifies who is providing this archive. In the case of Debian the
       string will read 'Debian'. Other providers may use their own string, such as
       "Ubuntu" or "Xandros". */
    { "origin", term_type_origin },
    { "priority", term_type_priority },
    { "provides", term_type_provides },
    { "section", term_type_section },
    { "source-package", term_type_source_package },
    { "source-version", term_type_source_version },
    { "tag", term_type_tag },
    { "task", term_type_task },
    { "term", term_type_term },
    { "term-prefix", term_type_term_prefix },
    { "true", term_type_true },
    { "upgradable", term_type_upgradable },
    { "user-tag", term_type_user_tag },
    { "version", term_type_version },
    { "virtual", term_type_virtual },
    /* ForTranslators: Opposite of narrow. Search for "widen" in this file for details. */
    { "widen", term_type_widen }
  };
}

typedef imm::map<std::string, int> parse_environment;

ref_ptr<pattern> parse_condition_list(string::const_iterator &start,
				      const string::const_iterator &end,
				      const vector<const char *> &terminators,
				      bool wide_context, bool partial,
				      const parse_environment &name_context);


// Check for terminators.  Not terribly efficient, but I expect under
// 3 terminators in any interesting usage.
bool terminate(const string::const_iterator &start,
	       const string::const_iterator &end,
	       const vector<const char *> &terminators)
{
  for(vector<const char *>::const_iterator i = terminators.begin();
      i != terminators.end(); ++i)
    {
      string::const_iterator j1 = start;
      const char *j2 = *i;
      bool matches = true;

      while(j1 != end && *j2 != 0 && matches)
	{
	  if(*j1 != *j2)
	    matches=false;

	  ++j1;
	  ++j2;
	}

      if(matches)
	return true;
    }

  return false;
}

// Parses a dependency type.  Returns (ick) -1 if the type is not
// recognized.
pkgCache::Dep::DepType parse_deptype(const string &s)
{
  if(!strcasecmp(s.c_str(), "depends"))
    return pkgCache::Dep::Depends;
  if(!strcasecmp(s.c_str(), "predepends"))
    return pkgCache::Dep::PreDepends;
  if(!strcasecmp(s.c_str(), "recommends"))
    return pkgCache::Dep::Recommends;
  else if(!strcasecmp(s.c_str(), "suggests"))
    return pkgCache::Dep::Suggests;
  else if(!strcasecmp(s.c_str(), "conflicts"))
    return pkgCache::Dep::Conflicts;
  else if(!strcasecmp(s.c_str(), "breaks"))
    return pkgCache::Dep::DpkgBreaks;
  else if(!strcasecmp(s.c_str(), "replaces"))
    return pkgCache::Dep::Replaces;
  else if(!strcasecmp(s.c_str(), "obsoletes"))
    return pkgCache::Dep::Obsoletes;
  else if(!strcasecmp(s.c_str(), "enhances"))
    return pkgCache::Dep::Enhances;
  else // ewww.
    return (pkgCache::Dep::DepType) -1;
}

namespace
{
  pattern::action_type parse_action(const std::string &s)
  {
    std::string s_lower(s);
    for(std::string::iterator it = s_lower.begin();
	it != s_lower.end(); ++it)
      *it = tolower(*it);

    // Match packages to be installed
    if(s_lower ==  "install")
      return pattern::action_install;

    // Match packages to be upgraded
    else if(s_lower ==  "upgrade")
      return pattern::action_upgrade;

    else if(s_lower ==  "downgrade")
      return pattern::action_downgrade;

    // Match packages to be removed OR purged
    else if(s_lower ==  "remove")
      return pattern::action_remove;

    // Match packages to be purged
    else if(s_lower ==  "purge")
      return pattern::action_purge;

    // Match packages to be reinstalled
    else if(s_lower ==  "reinstall")
      return pattern::action_reinstall;

    // Match held packages
    else if(s_lower ==  "hold")
      return pattern::action_hold;

    else if(s_lower ==  "keep")
      return pattern::action_keep;

    else
      throw MatchingException(ssprintf(_("Unknown action type: %s"),
				       s.c_str()));
  }

  pattern::multiarch_type parse_multiarch(const std::string &s)
  {
    if(!strcasecmp(s.c_str(), "none"))
      return pattern::multiarch_none;
    else if(!strcasecmp(s.c_str(), "foreign"))
      return pattern::multiarch_foreign;
    else if(!strcasecmp(s.c_str(), "same"))
      return pattern::multiarch_same;
    else if(!strcasecmp(s.c_str(), "allowed"))
      return pattern::multiarch_allowed;
    else
      throw MatchingException(ssprintf(_("Unknown multiarch type: %s"), s.c_str()));
  }
}

static
std::string parse_literal_string_tail(string::const_iterator &start,
				      const string::const_iterator end)
{
  std::string rval;

  while(start != end && *start != '"')
    {
      if(*start == '\\')
	{
	  ++start;
	  if(start != end)
	    {
	      switch(*start)
		{
		case 'n':
		  rval += '\n';
		  break;
		case 't':
		  rval += '\t';
		  break;
		default:
		  rval += *start;
		  break;
		}
	      ++start;
	    }
	}
      else
	{
	  rval += *start;
	  ++start;
	}
    }

  if(start == end)
    throw MatchingException(ssprintf(_("Unterminated literal string after %s"), rval.c_str()));

  eassert(*start == '"');
  ++start;

  return rval;
}

// Returns a substring up to the first metacharacter, including escaped
// metacharacters (parentheses, ~, |, and !)
//
// Advances loc to the first character of 's' following the escaped string.
std::string parse_substr(string::const_iterator &start,
			 const string::const_iterator &end,
			 const vector<const char *> &terminators,
			 bool whitespace_breaks)
{
  std::string rval;
  bool done=false;

  // Strip leading whitespace.
  while(start != end && isspace(*start))
    ++start;

  do
    {
      while(start != end &&
	    *start != '(' &&
	    *start != ')' &&
	    *start != '!' &&
	    *start != '~' &&
	    *start != '|' &&
	    *start != '"' &&
	    (!whitespace_breaks || !isspace(*start)) &&
	    !terminate(start, end, terminators))
	{
	  rval += *start;
	  ++start;
	}

      if(start != end && *start == '"')
	{
	  ++start;

	  rval += parse_literal_string_tail(start, end);
	}

      // We quit because we ran off the end of the string or saw a
      // metacharacter.  If the latter case and it was a tilde-escape,
      // add the escaped character to the string and continue.
      if(start != end && start+1 != end && *start == '~')
	{
	  const char next = *(start+1);

	  if(next == '(' || next == ')' ||
	     next == '!' || next == '~' ||
	     next == '|' || next == '"' ||
	     (whitespace_breaks && isspace(next)))
	    {
	      rval += next;
	      start += 2;
	    }
	  else
	    done = true;
	}
      else
	done = true;
    } while(!done);

  return rval;
}

pkgCache::State::VerPriority parse_priority(const string &substr)
{
  const char *s=substr.c_str();

  if(strcasecmp(s, "important") == 0 ||
     (apt_cache_file &&
      strcasecmp(s, (*apt_cache_file)->GetCache().Priority(pkgCache::State::Important)) == 0))
    return pkgCache::State::Important;
  else if(strcasecmp(s, "required") == 0 ||
	  (apt_cache_file &&
	   strcasecmp(s, (*apt_cache_file)->GetCache().Priority(pkgCache::State::Required)) == 0))
    return pkgCache::State::Required;
  else if(strcasecmp(s, "standard") == 0 ||
	  (apt_cache_file &&
	   strcasecmp(s, (*apt_cache_file)->GetCache().Priority(pkgCache::State::Standard)) == 0))
    return pkgCache::State::Standard;
  else if(strcasecmp(s, "optional") == 0 ||
	  (apt_cache_file &&
	   strcasecmp(s, (*apt_cache_file)->GetCache().Priority(pkgCache::State::Optional)) == 0))
    return pkgCache::State::Optional;
  else if(strcasecmp(s, "extra") == 0 ||
	  (apt_cache_file &&
	   strcasecmp(s, (*apt_cache_file)->GetCache().Priority(pkgCache::State::Extra)) == 0))
    return pkgCache::State::Extra;
  else
    throw MatchingException(ssprintf(_("Unknown priority %s"),
				     substr.c_str()));
}

void parse_whitespace(string::const_iterator &start,
		      const string::const_iterator &end)
{
  while(start != end && isspace(*start))
    ++start;
}

void parse_required_character(string::const_iterator &start,
			      const string::const_iterator &end,
			      char c)
{
  while(start != end && isspace(*start))
    ++start;

  if(start == end)
    throw MatchingException(ssprintf(_("Match pattern ends unexpectedly (expected '%c')."),
				     c));
  else if(*start != c)
    throw MatchingException(ssprintf(_("Expected '%c', got '%c'."),
				     c, *start));

  ++start;
}

template<typename arg>
struct parse_method;

void parse_open_paren(string::const_iterator &start,
		      const string::const_iterator &end)
{
  parse_required_character(start, end, '(');
}

void parse_close_paren(string::const_iterator &start,
		       const string::const_iterator &end)
{
  parse_required_character(start, end, ')');
}

void parse_comma(string::const_iterator &start,
		 const string::const_iterator &end)
{
  parse_required_character(start, end, ',');
}

template<>
struct parse_method<string>
{
  string operator()(string::const_iterator &start,
		    const string::const_iterator &end,
		    const std::vector<const char *> &terminators,
		    bool wide_context) const
  {
    return parse_substr(start, end, std::vector<const char *>(), false);
  }
};

template<>
struct parse_method<ref_ptr<pattern> >
{
  ref_ptr<pattern> operator()(string::const_iterator &start,
			      const string::const_iterator &end,
			      const std::vector<const char *> &terminators,
			      bool wide_context,
			      const parse_environment &name_context) const
  {
    return parse_condition_list(start, end, terminators, wide_context, false, name_context);
  }
};

template<typename A1>
ref_ptr<pattern>
parse_unary_term(string::const_iterator &start,
		 const string::const_iterator &end,
		 const std::vector<const char *> &terminators,
		 bool wide_context,
		 const parse_environment &name_context,
		 ref_ptr<pattern> (*k)(const A1 &),
		 const parse_method<A1> &parse1 = parse_method<A1>())
{
  parse_open_paren(start, end);
  A1 a1(parse1(start, end, terminators, wide_context, name_context));
  parse_close_paren(start, end);

  return k(a1);
}

void add_new_terminator(const char *new_terminator,
			std::vector<const char *> &terminators)
{
  for(std::vector<const char*>::const_iterator it = terminators.begin();
      it != terminators.end(); ++it)
    {
      if(strcmp(new_terminator, *it) == 0)
	return;
    }

  terminators.push_back(new_terminator);
}

template<typename A1, typename A2>
ref_ptr<pattern>
parse_binary_term(string::const_iterator &start,
		  const string::const_iterator &end,
		  const std::vector<const char *> &terminators,
		  bool wide_context,
		  const parse_environment &name_context,
		  ref_ptr<pattern> (*k)(const A1 &, const A2 &),
		  const parse_method<A1> &parse1 = parse_method<A1>(),
		  const parse_method<A2> &parse2 = parse_method<A2>())
{
  std::vector<const char *> terminators_plus_comma(terminators);
  add_new_terminator(",", terminators_plus_comma);

  parse_open_paren(start, end);
  A1 a1(parse1(start, end, terminators_plus_comma, wide_context, name_context));
  parse_comma(start, end);
  A2 a2(parse2(start, end, terminators, wide_context, name_context));
  parse_close_paren(start, end);

  return k(a1, a2);
}

template<typename A>
ref_ptr<pattern>
parse_nary_term(string::const_iterator &start,
		const string::const_iterator &end,
		const std::vector<const char *> &terminators,
		bool wide_context,
		const parse_environment &name_context,
		ref_ptr<pattern> (*k)(const std::vector<A> &),
		const parse_method<A> &parse = parse_method<A>())
{
  std::vector<A> rval;

  std::vector<const char *> terminators_plus_comma(terminators);
  add_new_terminator(",", terminators_plus_comma);

  parse_open_paren(start, end);
  parse_whitespace(start, end);
  bool first = true;
  while(start != end && *start != ')')
    {
      if(first)
	first = false;
      else
	parse_comma(start, end);

      A a(parse(start, end, terminators_plus_comma,
		wide_context, name_context));

      rval.push_back(a);
    }
  parse_close_paren(start, end);

  return k(rval);
}

string parse_string_match_args(string::const_iterator &start,
			       const string::const_iterator &end)
{
  parse_open_paren(start, end);
  string substr(parse_substr(start, end, std::vector<const char *>(), false));
  parse_close_paren(start, end);

  return substr;
}

ref_ptr<pattern> parse_term_args(string::const_iterator &start,
				 const string::const_iterator &end,
				 const std::vector<const char *> &terminators,
				 bool wide_context,
				 const parse_environment &name_context)
{
  parse_open_paren(start, end);
  ref_ptr<pattern> p(parse_condition_list(start, end, terminators, wide_context, false, name_context));
  parse_close_paren(start, end);

  return p;
}

ref_ptr<pattern> parse_optional_term_args(string::const_iterator &start,
					  const string::const_iterator &end,
					  const std::vector<const char *> terminators,
					  bool wide_context,
					  const parse_environment &name_context)
{
  while(start != end && isspace(*start))
    ++start;

  if(start != end && *start == '(')
    return parse_term_args(start, end, terminators, wide_context, name_context);
  else
    return NULL;
}

/** \brief Find the index of the given bound variable. */
size_t
get_variable_index(const string &bound_variable,
		   const parse_environment &name_context)
{
  int idx = name_context.get(bound_variable, -1);
  if(idx == -1)
    throw MatchingException(ssprintf("Unknown variable \"%s\".",
				     bound_variable.c_str()));
  else
    return idx;
}

/** \brief Parse the tail of a lambda form.
 *
 *  The full lambda form is:
 *
 *    ?for <variable>: CONDITION-LIST
 *
 *  This function assumes that "?for" has been parsed, so it parses:
 *
 *  <variable>: CONDITION-LIST
 */
ref_ptr<pattern> parse_explicit_term(const std::string &term_name,
				     string::const_iterator &start,
				     const string::const_iterator &end,
				     const std::vector<const char *> &terminators,
				     bool wide_context, bool partial,
				     const parse_environment &name_context)
{
  parse_whitespace(start, end);

  string bound_variable;
  while(start != end && *start != '(' && *start != '!' &&
	*start != '|' && *start != ')' && *start != '?' &&
	*start != '~' && *start != ':' && !isspace(*start) &&
	!terminate(start, end, terminators))
    {
      bound_variable.push_back(*start);
      ++start;
    }

  parse_whitespace(start, end);

  if(start == end)
    throw MatchingException(ssprintf("Unexpected end of pattern following ?%s %s (expected \":\" followed by a search term).",
				     term_name.c_str(),
				     bound_variable.c_str()));
  else if(*start != ':')
    throw MatchingException(ssprintf("Unexpected '%c' following ?%s %s (expected \":\" followed by a search term).",
				     *start, term_name.c_str(), bound_variable.c_str()));

  ++start;

  parse_whitespace(start, end);

  // Variables are case-insensitive and normalized to lower-case.
  for(std::string::iterator it = bound_variable.begin();
      it != bound_variable.end(); ++it)
    *it = tolower(*it);

  // Bind the name to the index that the variable will have in the
  // stack (counting from the bottom of the stack to the top).
  parse_environment name_context2(parse_environment::bind(name_context,
							  bound_variable,
							  name_context.size()));
  ref_ptr<pattern> p(parse_condition_list(start, end,
					  terminators,
					  wide_context,
					  partial,
					  name_context2));

  return pattern::make_for(bound_variable, p);
}

/** \brief Return a term that may or may not have a rebound
 *  variable.
 *
 *  If bound_variable is an empty string, just returns term.
 *  Otherwise, looks up bound_variable in the local environment
 *  (throwing a MatchingException if the lookup fails) and
 *  generates a bind term that wraps the given term.
 */
ref_ptr<pattern> maybe_bind(const string &bound_variable,
			    const ref_ptr<pattern> &term,
			    const parse_environment &name_context)
{
  if(bound_variable.empty())
    return term;
  else
    return pattern::make_bind(get_variable_index(bound_variable,
						 name_context),
			      term);
}

/** \brief Return a ?version term giving consideration
 *  to the special values CURRENT, CANDIDATE, TARGET.
 */
ref_ptr<pattern> parse_version(const string &version)
{
  if(version == "CURRENT")
    return pattern::make_current_version();
  else if(version == "TARGET")
    return pattern::make_install_version();
  else if(version == "CANDIDATE")
    return pattern::make_candidate_version();
  else
    return pattern::make_version(version);
}

/** \brief Return an ?architecture term giving consideration
 *  to the special values native and foreign.
 *
 *  Specifying an arch of "native" will return packages from both the
 *  native arch and also arch "all".  This is the same behaviour
 *  as libapt-pkg when using, e.g., FindPkg.
 */
ref_ptr<pattern> parse_architecture(const string &arch)
{
  if(arch == "native")
    return pattern::make_native_architecture();
  else if(arch == "foreign")
    return pattern::make_foreign_architecture();
  else
    return pattern::make_architecture(arch);
}

// NB: "partial" is passed in because ?for terms can have trailing strings.
ref_ptr<pattern> parse_term_args(const string &term_name,
				 string::const_iterator &start,
				 const string::const_iterator &end,
				 const vector<const char *> &terminators,
				 bool wide_context,
				 bool partial,
				 const parse_environment &name_context)
{
  {
    // This block parses the following forms:
    //
    // ?TYPE(term)
    // ?broken-TYPE
    // ?broken-TYPE(term)
    // ?reverse-TYPE(term)
    // ?broken-reverse-TYPE(term)
    // ?reverse-broken-TYPE(term)
    const std::string broken_prefix("broken-");
    const std::string reverse_prefix("reverse-");

    bool broken = false;
    bool reverse = false;
    std::string suffix;

    if(std::string(term_name, 0, broken_prefix.size()) == broken_prefix)
      {
	broken = true;

	if(std::string(term_name, broken_prefix.size(), reverse_prefix.size()) == reverse_prefix)
	  {
	    reverse = true;
	    suffix = std::string(term_name, broken_prefix.size() + reverse_prefix.size());
	  }
	else
	  suffix = std::string(term_name, broken_prefix.size());
      }
    else if(std::string(term_name, 0, reverse_prefix.size()) == reverse_prefix)
      {
	reverse = true;

	if(std::string(term_name, reverse_prefix.size(), broken_prefix.size()) == broken_prefix)
	  {
	    broken = true;
	    suffix = std::string(term_name, broken_prefix.size() + reverse_prefix.size());
	  }
	else
	  suffix = std::string(term_name, reverse_prefix.size());
      }
    else
      suffix = term_name;

    const pkgCache::Dep::DepType deptype = parse_deptype(suffix);

    while(start != end && isspace(*start) &&
	  !terminate(start, end, terminators))
      ++start;

    if(deptype == -1)
      {
	// Handle the special case of reverse-provides.
	if(reverse && suffix == "provides")
	  return pattern::make_reverse_provides(parse_term_args(start, end,
								terminators,
								false,
								name_context));
	else if(broken || reverse)
	  throw MatchingException(ssprintf(_("Unknown dependency type: %s"),
					   suffix.c_str()));

	// Otherwise what we have isn't a dep term at all, so just
	// don't do anything and try other options.
      }
    else
      {
	if(reverse)
	  {
	    // broken-reverse-TYPE(term) and reverse-broken-TYPE(term)
	    ref_ptr<pattern> p(parse_term_args(start, end,
					       terminators,
					       false,
					       name_context));

	    return pattern::make_reverse_depends(deptype, broken, p);
	  }
	else
	  {
	    // broken-TYPE and broken-TYPE(term) in the first branch,
	    // TYPE(term) in the second.
	    ref_ptr<pattern> p(broken
			       ? parse_optional_term_args(start, end, terminators, false, name_context)
			       : parse_term_args(start, end, terminators, false, name_context));

	    if(p.valid())
	      return pattern::make_depends(deptype, broken, p);
	    else
	      return pattern::make_broken_type(deptype);
	  }
      }
  }

  term_type type;
  bool found = false;

  // Hokey sequential scan.  Why?  Allocating a static map and
  // populating it raises icky issues of thread-safety, when the
  // initializer runs, etc...I'd rather just accept some (hopefully
  // minor) inefficiency.
  for(const term_info *it = term_types;
      !found && (unsigned)(it - term_types) < (sizeof(term_types) / sizeof(term_types[0]));
      ++it)
    {
      if(term_name == it->name)
	{
	  type = it->type;
	  found = true;
	}
    }

  if(!found)
    throw MatchingException(ssprintf(_("Unknown term type: \"%s\"."),
				     term_name.c_str()));

  switch(type)
    {
    case term_type_action:
      {
	std::string s(parse_string_match_args(start, end));
	return pattern::make_action(parse_action(s));
      }
    case term_type_all:
      if(!wide_context)
	/* ForTranslators: Question marks ("?") are used as prefix for function names.
	   Leave the question marks attached to the string placeholders. */
	throw MatchingException(ssprintf(_("The ?%s term must be used in a \"wide\" context (a top-level context, or a context enclosed by ?%s)."),
					 term_name.c_str(),
					 "widen"));
      else
	return pattern::make_all_versions(parse_term_args(start, end, terminators, false, name_context));
    case term_type_and:
      return parse_nary_term<ref_ptr<pattern> >(start, end, terminators, wide_context, name_context, &pattern::make_and);
    case term_type_any:
      if(!wide_context)
	throw MatchingException(ssprintf(_("The ?%s term must be used in a \"wide\" context (a top-level context, or a context enclosed by ?%s)."),
					 term_name.c_str(),
					 "widen"));
      else
	return pattern::make_any_version(parse_term_args(start, end, terminators, false, name_context));
    case term_type_architecture:
      return parse_architecture(parse_string_match_args(start, end));
    case term_type_archive:
      return pattern::make_archive(parse_string_match_args(start, end));
    case term_type_automatic:
      return pattern::make_automatic();
    case term_type_bind:
      {
	parse_whitespace(start, end);
	parse_open_paren(start, end);

	std::vector<const char *> new_terminators;
	new_terminators.push_back(")");
	new_terminators.push_back(",");
	std::string variable_name = parse_substr(start, end, new_terminators, true);
	int idx = get_variable_index(variable_name, name_context);

	parse_whitespace(start, end);
	parse_comma(start, end);
	parse_whitespace(start, end);

	// Remove the comma we pushed at the end of this list, since
	// it's no longer a terminator.
	new_terminators.pop_back();

	ref_ptr<pattern> p = parse_condition_list(start, end, new_terminators, wide_context, false, name_context);
	parse_whitespace(start, end);
	parse_close_paren(start, end);

	return pattern::make_bind(idx, p);
      }
    case term_type_broken:
      return pattern::make_broken();
    case term_type_config_files:
      return pattern::make_config_files();
    case term_type_description:
      return pattern::make_description(parse_string_match_args(start, end));
    case term_type_essential:
      return pattern::make_essential();
    case term_type_exact_name:
      return pattern::make_exact_name(parse_string_match_args(start, end));
    case term_type_false:
      return pattern::make_false();
    case term_type_for:
      return parse_explicit_term(term_name, start, end, terminators, wide_context, partial, name_context);
    case term_type_garbage:
      return pattern::make_garbage();
    case term_type_installed:
      return pattern::make_installed();
    case term_type_maintainer:
      return pattern::make_maintainer(parse_string_match_args(start, end));
    case term_type_multiarch:
      {
	std::string s(parse_string_match_args(start, end));
	return pattern::make_multiarch(parse_multiarch(s));
      }
    case term_type_name:
      return pattern::make_name(parse_string_match_args(start, end));
    case term_type_narrow:
      return parse_binary_term<ref_ptr<pattern>, ref_ptr<pattern> >(start, end, terminators, false, name_context, &pattern::make_narrow);
    case term_type_new:
      return pattern::make_new();
    case term_type_not:
      return pattern::make_not(parse_term_args(start, end, terminators, wide_context, name_context));
    case term_type_obsolete:
      return pattern::make_obsolete();
    case term_type_or:
      return parse_nary_term<ref_ptr<pattern> >(start, end, terminators, wide_context, name_context, &pattern::make_or);
    case term_type_origin:
      return pattern::make_origin(parse_string_match_args(start, end));
    case term_type_priority:
      return pattern::make_priority(parse_priority(parse_string_match_args(start, end)));
    case term_type_provides:
      return parse_unary_term<ref_ptr<pattern> >(start, end, terminators, false, name_context, &pattern::make_provides);
    case term_type_section:
      return pattern::make_section(parse_string_match_args(start, end));
    case term_type_source_package:
      return pattern::make_source_package(parse_string_match_args(start, end));
    case term_type_source_version:
      return pattern::make_source_version(parse_string_match_args(start, end));
    case term_type_tag:
      return pattern::make_tag(parse_string_match_args(start, end));
    case term_type_task:
      return pattern::make_task(parse_string_match_args(start, end));
    case term_type_term:
      return pattern::make_term(parse_string_match_args(start, end));
    case term_type_term_prefix:
      return pattern::make_term_prefix(parse_string_match_args(start, end));
    case term_type_true:
      return pattern::make_true();
    case term_type_upgradable:
      return pattern::make_upgradable();
    case term_type_user_tag:
      return pattern::make_user_tag(parse_string_match_args(start, end));
    case term_type_version:
      return parse_version(parse_string_match_args(start, end));
    case term_type_widen:
      return pattern::make_widen(parse_term_args(start, end, terminators, true, name_context));
    case term_type_virtual:
      return pattern::make_virtual();
    default:
      // This represents an internal error: it should never happen and
      // won't be comprehensible to the user, so there's no point in
      // translating it (if it does happen they should report the
      // literal message to me).
      throw MatchingException(ssprintf("Unexpected term type %d encountered.",
				       type));
    }
}

ref_ptr<pattern> parse_function_style_term_tail(string::const_iterator &start,
						const string::const_iterator &end,
						const vector<const char *> &terminators,
						bool wide_context,
						const parse_environment &name_context)
{
  if(*start == '=')
    {
      ++start;

      parse_whitespace(start, end);

      string bound_variable;
      while(start != end && *start != '(' && *start != '!' &&
	    *start != '|' && *start != ')' && *start != '?' &&
	    *start != '~' && *start != ':' && !isspace(*start) &&
	    !terminate(start, end, terminators))
	{
	  bound_variable.push_back(*start);
	  ++start;
	}


      if(bound_variable.empty())
	throw MatchingException(ssprintf("Unexpected end of pattern following ?=%s (expected a variable name).",
					 bound_variable.c_str()));
      else
	return pattern::make_equal(get_variable_index(bound_variable,
						      name_context));
    }

  // The name is considered to be the next sequence of non-whitespace
  // characters that are not an open paren.

  while(start != end && isspace(*start))
    ++start;

  string raw_name;
  string lower_case_name;
  string bound_variable;
  while(start != end && *start != '(' && *start != '!' &&
	*start != '|' && *start != ')' && *start != '?' &&
	*start != '~' && !isspace(*start) &&
	!terminate(start, end, terminators))
    {
      if(*start == ':')
	{
	  if(!bound_variable.empty())
	    throw MatchingException(ssprintf("Unexpected ':' following \"?%s:%s\".",
					     bound_variable.c_str(), raw_name.c_str()));
	  else
	    {
	      bound_variable = raw_name;
	      for(string::iterator it = bound_variable.begin();
		  it != bound_variable.end(); ++it)
		*it = tolower(*it);
	      raw_name.clear();
	      lower_case_name.clear();
	    }
	}
      else
	{
	  raw_name += *start;
	  lower_case_name += tolower(*start);
	}
      ++start;
    }

  return maybe_bind(bound_variable,
		    parse_term_args(lower_case_name,
				    start,
				    end,
				    terminators,
				    wide_context,
				    false,
				    name_context),
		    name_context);
}

ref_ptr<pattern> parse_atom(string::const_iterator &start,
			    const string::const_iterator &end,
			    const vector<const char *> &terminators,
			    bool wide_context, bool partial,
			    const parse_environment &name_context)
{
  std::string substr;

  while(start != end && isspace(*start))
    ++start;

  while(start != end && *start != '|' && *start != ')' &&
	!terminate(start, end, terminators))
    {
      if(*start == '!')
	{
	  ++start;
	  return pattern::make_not(parse_atom(start, end, terminators,
					      wide_context, partial,
					      name_context));
	}
      else if(*start == '(')
	// Recur into the list, losing the extra terminators (they are
	// treated normally until the closing paren)
	{
	  ++start;
	  ref_ptr<pattern> lst(parse_condition_list(start, end,
						    vector<const char *>(),
						    wide_context,
						    false,
						    name_context));

	  if(!(start != end && *start == ')'))
	    throw MatchingException(_("Unmatched '('"));
	  else
	    {
	      ++start;
	      return lst;
	    }
	}
      else if(*start == '?')
	{
	  ++start;
	  return parse_function_style_term_tail(start, end, terminators,
						wide_context, name_context);
	}
      else if(*start == '~')
	{
	  ++start;
	  while(start != end && isspace(*start))
	    ++start;

	  if(start == end)
	    {
	      return pattern::make_name("~");
	    }
	  else
	    {
	      const char search_flag = *start;

	      ++start;

	      while(start != end && isspace(*start))
		++start;

	      switch(search_flag)
		// Nested switch statements, mmmm...
		// Ok, there really is a reason here.  For all of the match
		// types that need a string argument, some prefix code (see
		// below) is needed to find the string's end.  But this would
		// be worse than unnecessary for the others.  So I have this
		// double check -- first test for anything that doesn't need
		// the prefix code, then work out which of the other forms
		// we have.
		{
		case 'v':
		  return pattern::make_virtual();
		case 'b':
		  return pattern::make_broken();
		case 'g':
		  return pattern::make_garbage();
		case 'c':
		  return pattern::make_config_files();
		case 'i':
		  return pattern::make_installed();
		case 'E':
		  return pattern::make_essential();
		case 'F':
		  return pattern::make_false();
		case 'M':
		  return pattern::make_automatic();
		case 'N':
		  return pattern::make_new();
		case 'T':
		  return pattern::make_true();
		case 'U':
		  return pattern::make_upgradable();
		case 'o':
		  return pattern::make_obsolete();
		case 'P':
		case 'C':
		case 'W':
		  {
		    ref_ptr<pattern> p(parse_atom(start,
						  end,
						  terminators,
						  search_flag == 'W',
						  partial,
						  name_context));

		    switch(search_flag)
		      {
		      case 'C':
			return pattern::make_depends(pkgCache::Dep::Conflicts, false, p);
		      case 'P':
			return pattern::make_provides(p);
		      case 'W':
			return pattern::make_widen(p);
		      }
		  }
		case 'S':
		  {
		    ref_ptr<pattern> filter(parse_atom(start,
						       end,
						       terminators,
						       false,
						       partial,
						       name_context));

		    ref_ptr<pattern> p(parse_atom(start,
						  end,
						  terminators,
						  false,
						  partial,
						  name_context));

		    return pattern::make_narrow(filter, p);
		  }
		case 'D':
		case 'R':
		  {
		    bool do_provides = false;
		    bool broken = false;
		    pkgCache::Dep::DepType type=pkgCache::Dep::Depends;

		    if(start != end && *start == 'B')
		      {
			broken = true;
			++start;
		      }

		    string::const_iterator nextstart = start;

		    while(nextstart != end && isalpha(*nextstart) &&
			  !terminate(nextstart, end, terminators))
		      ++nextstart;

		    while(nextstart != end && isspace(*nextstart))
		      ++nextstart;

		    if(nextstart != end && *nextstart == ':')
		      {
			string tname(start, nextstart);
			stripws(tname);

			start = nextstart;
			++start;

			if(!strcasecmp(tname.c_str(), "provides"))
			  do_provides=true;
			else
			  {
			    type=parse_deptype(tname.c_str());

			    if(type==-1)
			      throw MatchingException(ssprintf(_("Unknown dependency type: %s"),
							       tname.c_str()));
			  }
		      }

		    if(do_provides && broken)
		      throw MatchingException(_("Provides: cannot be broken"));

		    ref_ptr<pattern> p(parse_atom(start, end, terminators,
						  false, partial, name_context));

		    switch(search_flag)
		      {
		      case 'D':
			if(do_provides)
			  return pattern::make_provides(p);
			else
			  return pattern::make_depends(type, broken, p);
		      case 'R':
			if(do_provides)
			  return pattern::make_reverse_provides(p);
			else
			  return pattern::make_reverse_depends(type, broken, p);
		      }
		  }
		default:
		  substr = parse_substr(start, end, terminators, true);
		  switch(search_flag)
		    {
		    case 'a':
		      return pattern::make_action(parse_action(substr));
		    case 'A':
		      return pattern::make_archive(substr);
		    case 'B':
		      {
			pkgCache::Dep::DepType ptype=parse_deptype(substr);

			if(ptype!=-1)
			  return pattern::make_broken_type(ptype);
			else
			  throw MatchingException(ssprintf(_("Unknown dependency type: %s"),
							   substr.c_str()));
		      }
		    case 'd':
		      return pattern::make_description(substr);
		    case 'G':
		      return pattern::make_tag(substr);
		    case 'm':
		      return pattern::make_maintainer(substr);
		    case 'n':
		      return pattern::make_name(substr);
		    case 'O':
		      return pattern::make_origin(substr);
		    case 'p':
		      return pattern::make_priority(parse_priority(substr));
		    case 'r':
		      return parse_architecture(substr);
		    case 's':
		      return pattern::make_section(substr);
		    case 't':
		      return pattern::make_task(substr);
		    case 'V':
		      return parse_version(substr);
		    default:
		      throw MatchingException(ssprintf(_("Unknown pattern type: %c"), search_flag));
		    }
		}
	    }
	}
      else
	{
	  std::string s = parse_substr(start, end, terminators, true);

	  const size_t found = s.rfind(':');
	  if(found == std::string::npos)
	    return pattern::make_name(s);
	  const std::string arch = s.substr(found + 1);
	  const std::string name = s.substr(0, found);
	  return pattern::make_and(pattern::make_exact_name(name),
				   parse_architecture(arch));
	}
    }

  // If we get here, the string was empty.
  throw MatchingException(_("Can't search for \"\""));
}

ref_ptr<pattern> parse_and_group(string::const_iterator &start,
				 const string::const_iterator &end,
				 const vector<const char *> &terminators,
				 bool wide_context, bool partial,
				 const parse_environment &name_context)
{
  std::vector<ref_ptr<pattern> > rval;
  while(start != end && isspace(*start))
    ++start;

  while(start != end && *start != '|' && *start != ')' &&
	!terminate(start, end, terminators))
    {
      ref_ptr<pattern> atom(parse_atom(start, end, terminators,
				       wide_context,
				       partial,
				       name_context));

      rval.push_back(atom);

      while(start != end && isspace(*start))
	++start;
    }

  if(rval.empty())
    throw MatchingException(_("Unexpected empty expression"));

  if(rval.size() == 1)
    return rval.front();
  else
    return pattern::make_and(rval);
}

ref_ptr<pattern> parse_condition_list(string::const_iterator &start,
				      const string::const_iterator &end,
				      const vector<const char *> &terminators,
				      bool wide_context, bool partial,
				      const parse_environment &name_context)
{
  std::vector<ref_ptr<pattern> > grp;

  parse_whitespace(start, end);

  bool first = true;

  while(start != end && *start != ')' && !terminate(start, end, terminators))
    {
      if(first)
	first = false;
      else
	{
	  if(*start == '|')
	    ++start;
	  else
	    throw MatchingException(ssprintf(_("Badly formed expression: expected '|', got '%c'"),
					     *start));
	}


      grp.push_back(parse_and_group(start, end, terminators,
				    wide_context,
				    partial,
				    name_context));

      parse_whitespace(start, end);
    }

  if(grp.size() == 1)
    return grp.front();
  else
    return pattern::make_or(grp.begin(), grp.end());
}

ref_ptr<pattern> parse_with_errors(string::const_iterator &start,
				   const string::const_iterator &end,
				   const std::vector<const char *> &terminators,
				   bool require_full_parse,
				   bool partial)
{
  // Just filter blank strings out immediately.
  while(start != end && isspace(*start) && !terminate(start, end, terminators))
    ++start;

  if(start == end)
    return ref_ptr<pattern>();

  string::const_iterator real_end = end;

  // Move 'end' back as long as there's whitespace, so that we can
  // easily check whether a word is at the end of the string.
  while(start != real_end && isspace(*(real_end - 1)))
    --real_end;

  ref_ptr<pattern> rval(parse_condition_list(start, real_end, terminators,
					     true, partial,
					     parse_environment()));

  while(start != real_end && isspace(*start))
    ++start;

  if(require_full_parse && start != real_end)
    throw MatchingException(_("Unexpected ')'"));
  else
    return rval;
}

ref_ptr<pattern> parse(string::const_iterator &start,
		       const string::const_iterator &end,
		       const std::vector<const char *> &terminators,
		       bool flag_errors,
		       bool require_full_parse,
		       bool partial)
{

  try
    {
      return parse_with_errors(start, end, terminators, require_full_parse, partial);
    }
  catch(MatchingException e)
    {
      if(flag_errors)
	_error->Error("%s", e.errmsg().c_str());

      return ref_ptr<pattern>();
    }
}

}
}
