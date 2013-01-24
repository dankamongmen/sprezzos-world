// pattern.h      -*-c++-*-
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

/// \file pattern.h

#ifndef PATTERN_H
#define PATTERN_H

#include <generic/util/refcounted_base.h>

#include <generic/apt/apt.h>

#include <cwidget/generic/util/exception.h>
#include <cwidget/generic/util/ref_ptr.h>

#include <regex.h>
#include <sys/types.h>

#include <apt-pkg/pkgcache.h>
#include <apt-pkg/cachefilter.h>


namespace aptitude
{
  namespace matching
  {
    /** \brief An exception related to the matching code.
     */
    class MatchingException : public cwidget::util::Exception
    {
      std::string msg;

    public:
      /** \brief Create a matching exception.
       *
       *  \param _msg  The error message associated with this exception.
       */
      MatchingException(const std::string &_msg);

      /** \brief Retrieve the error message associated with this exception. */
      std::string errmsg() const;
    };

    /** \brief Ref-counted wrapper for PackageArchitectureMatchesSpecification.
     */
    class arch_specification : public util::refcounted_base_threadsafe
    {
      APT::CacheFilter::PackageArchitectureMatchesSpecification pams;
      const std::string spec;

    public:
      arch_specification(const std::string &_spec);

      bool matches(const char * const &arch);
      inline bool matches(const pkgCache::VerIterator &ver)
      {
        return matches(ver.Arch());
      }

      inline const std::string &get_specification() const
      {
        return spec;
      }
    };

    /** \brief C++ wrapper for regular expression objects.
     *
     *  This class turns compilation errors into exceptions and
     *  handles safely disposing of the pattern when it's destroyed.
     */
    class regex : public util::refcounted_base_threadsafe
    {
      regex_t r;

      // Forbid copy-construction.
      regex(const regex &);

    public:
      /** \brief Compile a regular expression.
       *
       *  \param pattern   The text of the pattern to compile.
       *  \param cflags    The compilation flags (see regex(3)).
       *
       *  \throw MatchingException if compilation fails.
       */
      regex(const std::string &pattern, int cflags);
      ~regex();

      /** \brief Execute the regular expression.
       *
       *  \param s            The string to match into.
       *  \param matches      The array in which to store group match
       *                      information, or NULL to not store it.
       *  \param num_matches  The number of entries in matches.
       *  \param eflags       The execution flags (see regex(3)).
       *
       *  \return \b true if the expression matched and \b false otherwise.
       */
      bool exec(const char *s, regmatch_t *matches, size_t num_matches,
		int eflags) const;

      /** \brief Execute the regular expression.
       *
       *  \param s            The string to match into.
       *  \param matches      The array in which to store group match
       *                      information, or NULL to not store it.
       *  \param num_matches  The number of entries in matches.
       *  \param eflags       The execution flags (see regex(3)).
       *
       *  \return \b true if the expression matched and \b false otherwise.
       */
      bool exec(const std::string &s, regmatch_t *matches, size_t num_matches,
		int eflags) const
      {
	return exec(s.c_str(), matches, num_matches, eflags);
      }

      /** \brief Execute the regular expression with no execution flags.
       *
       *  \param s            The string to match into.
       *  \param matches      The array in which to store group match
       *                      information, or NULL to not store it.
       *  \param num_matches  The number of entries in matches.
       *
       *  \return \b true if the expression matched and \b false otherwise.
       */
      bool exec(const char *s, regmatch_t *matches, size_t num_matches) const
      {
	return exec(s, matches, num_matches, 0);
      }

      /** \brief Execute the regular expression with no execution flags.
       *
       *  \param s            The string to match into.
       *  \param matches      The array in which to store group match
       *                      information, or NULL to not store it.
       *  \param num_matches  The number of entries in matches.
       *  \param eflags       The execution flags (see regex(3)).
       *
       *  \return \b true if the expression matched and \b false otherwise.
       */
      bool exec(const std::string &s, regmatch_t *matches, size_t num_matches) const
      {
	return exec(s.c_str(), matches, num_matches);
      }

      /** \brief Execute the regular expression without retrieving group information.
       *
       *  \param s       The string to match into.
       *  \param eflags  The execution flags (see regex(3)).
       *
       *  \return \b true if the expression matched and \b false otherwise.
       */
      bool exec(const char *s, int eflags) const
      {
	return exec(s, NULL, 0, eflags);
      }

      /** \brief Execute the regular expression without retrieving group information.
       *
       *  \param s       The string to match into.
       *  \param eflags  The execution flags (see regex(3)).
       *
       *  \return \b true if the expression matched and \b false otherwise.
       */
      bool exec(const std::string &s, int eflags) const
      {
	return exec(s.c_str(), eflags);
      }

      /** \brief Execute the regular expression without retrieving group information
       *  and with no execution flags.
       *
       *  \param s       The string to match into.
       *
       *  \return \b true if the expression matched and \b false otherwise.
       */
      bool exec(const char *s) const
      {
	return exec(s, NULL, 0);
      }

      /** \brief Execute the regular expression without retrieving group information
       *  and with no execution flags.
       *
       *  \param s       The string to match into.
       *
       *  \return \b true if the expression matched and \b false otherwise.
       */
      bool exec(const std::string &s) const
      {
	return exec(s.c_str());
      }
    };

    /** \brief Represents a single node in a match expression.
     *
     *  This is a "poor man's algebraic datatype".  It provides one
     *  named constructor for each form of a match expression; to use
     *  a value of this type, switch on the type tag (get_type()) and
     *  use the accessors corresponding to the appropriate type
     *  (get_TYPE_FIELD()).
     *
     *  \nosubgrouping
     */
    class pattern : public util::refcounted_base_threadsafe
    {
    public:
      /** \name Term types */

      // @{

      /** \brief Represents the type of a term.
       *
       *  Normally this is just the undecorated name, but in a few
       *  cases (when the name conflicts with a C++ reserved word)
       *  _tp is appended.
       */
      enum type
	{
	  /** \brief ?archive(PATTERN)
	   *
	   *  Matches packages by their archive.
	   *
	   *  Fields: regex_info.
	   */
	  archive,
	  /** \brief ?action(ACTION)
	   *
	   *  Matches packages by their action flag.
	   *
	   *  Fields: action_type.
	   */
	  action,
	  /** \brief ?all-versions(PATTERN)
	   *
	   *  Matches a package if all its versions match the given PATTERN.
	   *
	   *  Fields: pattern.
	   */
	  all_versions,
	  /** \brief ?any-version(PATTERN)
	   *
	   *  Matches a package if any of its versions matches the given PATTERN.
	   *
	   *  Fields: pattern.
	   */
	  any_version,
          /** \brief ?architecture(SPECIFICATION)
           *
           *  Matches packages whose architecture meets the given
           *  SPECIFICATION string (see Debian Policy section 11.1).
           *
           *  Fields: arch_specification.
           */
          architecture,
	  /** \brief ?automatic
	   *
	   *  Matches packages that were automatically installed.
	   */
	  automatic,
	  /** \brief ?and(PATTERN, ...)
	   *
	   *  Matches packages if none of its patterns fail.
	   *
	   *  Fields: patterns.
	   */
	  and_tp,
	  /** \brief ?bind(X, PATTERN)
	   *
	   *  Matches if PATTERN matches the value of X.
	   *
	   *  Fields: pattern, variable_index.
	   */
	  bind,
	  /** \brief ?broken
	   *
	   *  Matches broken packages.
	   */
	  broken,
	  /** \brief ?broken-TYPE
	   *
	   *  Matches packages with a particular type of broken
	   *  dependency.
	   *
	   *  Fields: depends_type
	   */
	  broken_type,
	  /** \brief ?version(CANDIDATE)
	   *
	   *  Matches the candidate version of a package.
	   */
	  candidate_version,
	  /** \brief ?config-files
	   *
	   *  Matches packages that were removed but that still have
	   *  config files present.
	   */
	  config_files,
	  /** \brief ?version(CURRENT)
	   *
	   *  Matches the current version of a package.
	   */
	  current_version,
	  /** \brief ?DEPENDS-TYPE(PATTERN), ?broken-DEPENDS-TYPE(PATTERN)
	   *
	   *  Matches packages with a dependency of the type
	   *  DEPENDS-TYPE on a package matching PATTERN.  If "broken"
	   *  is true, only broken dependencies are considered.
	   *
	   *  Fields: depends_type, pattern, broken.
	   */
	  depends,
	  /** \brief ?description(PATTERN)
	   *
	   *  Matches packages by their Description field.
	   *
	   *  Fields: regex_info.
	   */
	  description,
	  /** \brief ?essential
	   *
	   *  Matches Essential packages.
	   */
	  essential,
	  /** \brief ?=X
	   *
	   *  Matches packages/versions that equal the given stack position.
	   *
	   *  Fields: stack_position.
	   */
	  equal,
	  /** \brief ?exact-name(NAME)
	   *
	   *  Matches packages whose name is exactly NAME.
	   *
	   *  Fields: name.
	   */
	  exact_name,
	  /** \brief ?false
	   *
	   *  Matches nothing.
	   */
	  false_tp,
	  /** \brief ?for X: PATTERN
	   *
	   *  Matches packages if PATTERN matches with "X" bound to that package.
	   *
	   *  Fields: variable_name, pattern.
	   */
	  for_tp,
          /** \brief ?architecture(foreign)
           *
           *  Matches packages of foreign architectures.
           */
          foreign_architecture,
	  /** \brief ?garbage
	   *
	   *  Matches packages that are not required by any manually
	   *  installed package.
	   */
	  garbage,
	  /** \brief ?version(TARGET)
	   *
	   *  Matches the to-be-installed version of a package.
	   */
	  install_version,
	  /** \brief ?installed
	   *
	   *  Matches installed packages/versions.
	   */
	  installed,
	  /** \brief ?maintainer(PATTERN)
	   *
	   *  Matches packages by their Maintainer field.
	   *
	   *  Fields: regex_info.
	   */
	  maintainer,
	  /** \brief ?multiarch(MULTIARCH)
	   *
	   *  Matches packages by their Multi-Arch field.
	   *
	   *  Fields: multiarch_type.
	   */
	  multiarch,
	  /** \brief ?name(PATTERN)
	   *
	   *  Matches packages by their name.
	   *
	   *  Fields: regex_info.
	   */
	  name,
	  /** \brief ?narrow(FILTER, PATTERN)
	   *
	   *  Matches packages if a version matching filter matches PATTERN.
	   *
	   *  Fields: filter, pattern
	   */
	  narrow,
          /** \brief ?architecture(native)
           *
           *  Matches packages of the native architecture.
           */
          native_architecture,
	  /** \brief ?new
	   *
	   *  Matches packages that are "new".
	   */
	  new_tp,
	  /** \brief ?not(PATTERN)
	   *
	   *  Matches packages if the given PATTERN fails to match.
	   *
	   *  Fields: pattern.
	   */
	  not_tp,
	  /** \brief ?obsolete
	   *
	   *  Matches packages that are installed but that are not
	   *  downloadable.
	   */
	  obsolete,
	  /** \brief ?or(PATTERN, ...)
	   *
	   *  Matches if at least one PATTERN matches.
	   *
	   *  Fields: patterns.
	   */
	  or_tp,
	  /** \brief ?origin(PATTERN)
	   *
	   *  Matches packages by their origin.
	   *
	   *  Fields: regex_info.
	   */
	  origin,
	  /** \brief ?priority(PRIORITY)
	   *
	   *  Matches packages by their priority.
	   *
	   *  Fields: priority, priority_name.
	   */
	  priority,
	  /** \brief ?provides(PATTERN)
	   *
	   *  Matches packages that provide a package matching the
	   *  given pattern.
	   *
	   *  Fields: pattern.
	   */
	  provides,
	  /** \brief ?reverse-TYPE(PATTERN), ?reverse-broken-TYPE(PATTERN)
	   *
	   *  Matches packages that have a reverse dependency of the
	   *  given TYPE on a package matching PATTERN.  If "broken"
	   *  is set, only dependencies that are broken by the
	   *  currently planned installation will be examined.
	   *
	   *  Fields: type, pattern, broken.
	   */
	  reverse_depends,
	  /** \brief ?reverse-provides(PATTERN), ?provided-by(PATTERN)
	   *
	   *  Matches packages that are provided by a package matching
	   *  PATTERN.
	   *
	   *  Fields: pattern.
	   */
	  reverse_provides,
	  /** \brief ?section(PATTERN)
	   *
	   *  Matches packages by their section.
	   *
	   *  Fields: regex_info.
	   */
	  section,
	  /** \brief ?source-package(PATTERN)
	   *
	   *  Matches packages by their source package.
	   *
	   *  Fields: regex_info.
	   */
	  source_package,
	  /** \brief ?source-version(PATTERN)
	   *
	   *  Matches packages by the version of their source package.
	   *
	   *  Fields: regex_info.
	   */
	  source_version,
	  /** \brief ?tag(PATTERN)
	   *
	   *  Matches packages using debtags.
	   *
	   *  Fields: regex_info.
	   */
	  tag,
	  /** \brief ?task(PATTERN)
	   *
	   *  Matches packages by their task.
	   *
	   *  Fields: regex_info.
	   */
	  task,
	  /** \brief ?term(TERM)
	   *
	   *  Matches a package using a full-text keyword search.
	   *
	   *  Fields: term.
	   */
	  term,
	  /** \brief ?term-prefix(TERM)
	   *
	   *  Matches a package using a full-text keyword search against
	   *  a prefix (so "apt" will match both "apt" and "aptitude").
	   */
	  term_prefix,
	  /** \brief ?true
	   *
	   *  Matches everything.
	   */
	  true_tp,
	  /** \brief ?upgradable
	   *
	   *  Matches packages that are upgradable.
	   */
	  upgradable,
	  /** \brief ?user-tag(PATTERN)
	   *
	   *  Matches packages by their user tags.
	   *
	   *  Fields: regex_info.
	   */
	  user_tag,
	  /** \brief ?version(PATTERN)
	   *
	   *  Matches packages by their version.
	   *
	   *  Fields: regex_info.
	   */
	  version,
	  /** \brief ?virtual
	   *
	   *  Matches package versions that are not associated with a
	   *  real package, or virtual packages, or package versions
	   *  that correspond to removing a package.
	   */
	  virtual_tp,
	  /** \brief ?widen(pattern)
	   *
	   *  Matches packages and package versions if any version of
	   *  the package matches PATTERN.
	   *
	   *  Fields: pattern.
	   */
	  widen
	};

      /** \brief Get the type of this term. */
      type get_type() const
      {
	return tp;
      }

      // @}

      /** \brief Represents information about the regular expression
       *  associated with a pattern.
       *
       *  Patterns that use regular expressions compile the regex
       *  twice, once with grouping enabled and once without (to allow
       *  quick matches in the case that we aren't retrieving group
       *  information).  In addition, the text of the regular
       *  expression that the user entered is preserved.
       */
      class regex_info
      {
	cwidget::util::ref_ptr<regex> regex_group;
	cwidget::util::ref_ptr<regex> regex_nogroup;

	std::string regex_string;

      public:
	/** \brief Create an empty regex_info structure.
	 *
	 *  The regex_group and regex_nogroup fields will be NULL, and
	 *  regex_string will be empty.
	 */
	regex_info()
	{
	}

	/** \brief Compile the given regular expression.
	 *
	 *  \param _regex_string  The text of the regular expression.
	 *                        If it is the empty string, the
	 *                        regular expression will match anything.
	 *
	 *  \throw MatchingException if the regular expression cannot be
	 *  compiled.
	 */
	regex_info(const std::string &_regex_string)
	  : regex_group(new regex(_regex_string.empty() ? ".*" : _regex_string, REG_ICASE|REG_EXTENDED)),
	    regex_nogroup(new regex(_regex_string.empty() ? ".*" : _regex_string, REG_ICASE|REG_EXTENDED|REG_NOSUB)),
	    regex_string(_regex_string)
	{
	}

	/** \brief Retrieve the regular expression, compiled with
	 *  grouping enabled.
	 */
	const cwidget::util::ref_ptr<regex> &get_regex_group() const
	{
	  return regex_group;
	}

	/** \brief Retrieve the regular expression, compiled without
	 *  grouping enabled.
	 */
	const cwidget::util::ref_ptr<regex> &get_regex_nogroup() const
	{
	  return regex_nogroup;
	}

	/** \brief Retrieve the original text of the regular expression. */
	const std::string &get_regex_string() const
	{
	  return regex_string;
	}
      };

      /** \brief The actions that can be matched against. */
      enum action_type
	{
	  /** \brief Match packages that are going to be installed. */
	  action_install,
	  /** \brief Match packages that are going to be upgraded. */
	  action_upgrade,
	  /** \brief Match packages that are going to be downgraded. */
	  action_downgrade,
	  /** \brief Match packages that are going to be removed (not purged). */
	  action_remove,
	  /** \brief Match packages that are going to be purged. */
	  action_purge,
	  /** \brief Match packages that are going to be reinstalled. */
	  action_reinstall,
	  /** \brief Match packages that are being held back. */
	  action_hold,
	  /** \brief Match packages that are not being modified. */
	  action_keep
	};

      /** \brief The Multi-Arch capabilities that can be matched against. */
      enum multiarch_type
	{
	  /** \brief Match packages that have no Multi-Arch capability. */
	  multiarch_none,
	  /** \brief Match packages that are Multi-Arch: foreign. */
	  multiarch_foreign,
	  /** \brief Match packages that are Multi-Arch: same. */
	  multiarch_same,
	  /** \brief Match packages that are Multi-Arch: allowed. */
	  multiarch_allowed
	};

    private:

      // The type of this node.
      type tp;

      regex_info regex_information;

      // The sub-patterns, if any.
      std::vector<cwidget::util::ref_ptr<pattern> > sub_patterns;

      // The variable name, if any, or the string corresponding
      // to the action match information.
      std::string string_info;

      cwidget::util::ref_ptr<arch_specification> arch_spec;

      // Groups several POD values that aren't used simultaneously.
      union
      {
	struct
	{
	  // The dependency type, if any.
	  pkgCache::Dep::DepType deptype;

	  // Whether to match broken packages (if applicable).
	  bool broken;
	} dep;

	// The stack position, if applicable.
	size_t stack_position;

	// The action being selected, if applicable.
	action_type action;

	// The priority, if applicable.
	pkgCache::State::VerPriority priority;

	// The multiarch type being selected, if applicable.
	multiarch_type multiarch;
      } info;

      // Disallow copy-construction.
      pattern(const pattern &other);

      // Allocate a pattern with no parameters.
      pattern(type _tp)
	: tp(_tp)
      {
      }

      // Allocate a pattern that has a regular expression.
      pattern(type _tp, const regex_info &_regex_information)
	: tp(_tp),
	  regex_information(_regex_information)
      {
      }

      pattern(type _tp, pkgCache::State::VerPriority priority)
	: tp(_tp)
      {
	info.priority = priority;
      }

      // Allocate a pattern that just has string information.
      pattern(type _tp,
	      const std::string &_string_info)
	: tp(_tp), string_info(_string_info)
      {
      }

      // Allocate a pattern that has sub-patterns.
      template<typename Iter>
      pattern(type _tp,
	      Iter sub_patterns_start, Iter sub_patterns_end,
	      const std::string &_string_info = std::string())
	: tp(_tp), sub_patterns(sub_patterns_start, sub_patterns_end),
	  string_info(_string_info)
      {
      }

      // Allocate a pattern that has a single sub-pattern.
      pattern(type _tp,
	      const cwidget::util::ref_ptr<pattern> &p,
	      const std::string &_string_info = std::string())
	: tp(_tp), sub_patterns(&p, (&p) + 1), string_info(_string_info)
      {
      }

      // Allocate a pattern that has two sub-patterns.
      pattern(type _tp,
	      const cwidget::util::ref_ptr<pattern> &p1,
	      const cwidget::util::ref_ptr<pattern> &p2,
	      const std::string &_string_info = std::string())
	: tp(_tp), string_info(_string_info)
      {
	sub_patterns.push_back(p1);
	sub_patterns.push_back(p2);
      }

      // Allocate a pattern that just has dependency info.
      pattern(type _tp,
	      pkgCache::Dep::DepType deptype, bool broken)
	: tp(_tp)
      {
	info.dep.deptype = deptype;
	info.dep.broken = broken;
      }

      // Allocate a pattern that has sub-patterns and dependency info.
      template<typename Iter>
      pattern(type _tp,
	      Iter sub_patterns_start, Iter sub_patterns_end,
	      pkgCache::Dep::DepType deptype, bool broken)
	: tp(_tp), sub_patterns(sub_patterns_start, sub_patterns_end)
      {
	info.dep.deptype = deptype;
	info.dep.broken = broken;
      }

      // Allocate a pattern that has stack position information.
      pattern(type _tp, size_t _stack_position)
	: tp(_tp)
      {
	info.stack_position = _stack_position;
      }

      // Allocate a pattern that has stack position information
      // and a pattern.
      pattern(type _tp, size_t _stack_position,
	      const cwidget::util::ref_ptr<pattern> &p)
	: tp(_tp)
      {
	info.stack_position = _stack_position;
	sub_patterns.push_back(p);
      }

      // Allocate a pattern that has package action information.
      pattern(type _tp, action_type action_type)
	: tp(_tp)
      {
	info.action = action_type;
      }

      // Allocate a pattern that has multi-arch info.
      pattern(type _tp,
	      multiarch_type multiarch_type)
	: tp(_tp)
      {
	info.multiarch = multiarch_type;
      }

      // Allocate a pattern that has an architecture specification.
      pattern(type _tp,
              const cwidget::util::ref_ptr<arch_specification> &spec)
        : tp(_tp),
          arch_spec(spec)
      {
      }

    public:

      /** \name archive term constructor and accessors. */

      // @{

      /** \brief Create an ?archive term.
       *
       *  \param s  The regular expression to match against.
       */
      static cwidget::util::ref_ptr<pattern>
      make_archive(const std::string &s)
      {
	return new pattern(archive, regex_info(s));
      }

      /** \brief Get the regex_info field of an ?archive term. */
      const regex_info &get_archive_regex_info() const
      {
	eassert(tp == archive);

	return regex_information;
      }

      // @}

      /** \name action term constructor and accessors. */
      // @{

      /** \brief Create an ?action term.
       *
       *  \param act  The action type to match.
       */
      static cwidget::util::ref_ptr<pattern>
      make_action(const action_type act);

      /** \brief Retrieve the information associated with an ?action
       *  term.
       */
      const action_type get_action_action_type() const
      {
	eassert(tp == action);

	return info.action;
      }

      // @}

      /** \name all_versions term constructor and accessors. */
      // @{

      /** \brief Create an ?all-versions term.
       *
       *  \param p  The sub-pattern.
       */
      static cwidget::util::ref_ptr<pattern>
      make_all_versions(const cwidget::util::ref_ptr<pattern> &p)
      {
	return new pattern(all_versions, p);
      }

      /** \brief Retrieve the sub-pattern of an ?all-versions term.
       */
      const cwidget::util::ref_ptr<pattern> &
      get_all_versions_pattern() const
      {
	eassert(tp == all_versions && sub_patterns.size() == 1);

	return sub_patterns.front();
      }

      // @}

      /** \name any_version term constructor and accessors. */
      // @{

      /** \brief Create an ?any-versions term.
       *
       *  \param p  The sub-pattern.
       */
      static cwidget::util::ref_ptr<pattern>
      make_any_version(const cwidget::util::ref_ptr<pattern> &p)
      {
	return new pattern(any_version, p);
      }

      /** \brief Retrieve the sub-pattern of an ?any-version term. */
      const cwidget::util::ref_ptr<pattern> &
      get_any_version_pattern() const
      {
	eassert(tp == any_version && sub_patterns.size() == 1);

	return sub_patterns.front();
      }

      // @}

      /** \name architecture term constructor and accessors. */

      // @{

      /** \brief Create an ?architecture term.
       *
       *  \param spec  The architecture specification string to match.
       */
      static cwidget::util::ref_ptr<pattern>
      make_architecture(const std::string &spec)
      {
        return new pattern(architecture,
                           new arch_specification(spec));
      }

      const cwidget::util::ref_ptr<arch_specification> &
      get_architecture_arch_specification() const
      {
        eassert(tp == architecture);

        return arch_spec;
      }

      // @}

      /** \name automatic term constructor. */

      // @{

      /** \brief Create an ?automatic term. */
      static cwidget::util::ref_ptr<pattern>
      make_automatic()
      {
	return new pattern(automatic);
      }

      // @}

      /** \name and_tp term constructor and accessors. */

      // @{

      /** \brief Create an ?and term.
       *
       *  \param sub_patterns_begin   The beginning of the
       *                              range of sub-patterns.
       *  \param sub_patterns_end     The end of the range of
       *                              sub-patterns.
       */
      template<typename Iter>
      static cwidget::util::ref_ptr<pattern>
      make_and(Iter sub_patterns_begin, Iter sub_patterns_end)
      {
	return new pattern(and_tp, sub_patterns_begin, sub_patterns_end);
      }

      /** \brief Create an ?and term.
       *
       *  \param container  An STL container holding the
       *                    sub-patterns of the new term.
       */
      template<typename C>
      static cwidget::util::ref_ptr<pattern>
      make_and(const C &container)
      {
	return new pattern(and_tp, container.begin(), container.end());
      }

      /** \brief Create a binary ?and term.
       *
       *  \param p1  The first sub-term of the ?and.
       *  \param p2  The second sub-term of the ?and.
       */
      static cwidget::util::ref_ptr<pattern>
      make_and(const cwidget::util::ref_ptr<pattern> &p1,
	       const cwidget::util::ref_ptr<pattern> &p2)
      {
	std::vector<cwidget::util::ref_ptr<pattern> > patterns;
	patterns.push_back(p1);
	patterns.push_back(p2);

	return new pattern(and_tp, patterns.begin(), patterns.end());
      }

      /** \brief Create a binary ?and term.
       *
       *  \param p1  The first sub-term of the ?and.
       *  \param p2  The second sub-term of the ?and.
       */
      static cwidget::util::ref_ptr<pattern>
      make_and(const cwidget::util::ref_ptr<pattern> &p1,
	       const cwidget::util::ref_ptr<pattern> &p2,
	       const cwidget::util::ref_ptr<pattern> &p3)
      {
	std::vector<cwidget::util::ref_ptr<pattern> > patterns;
	patterns.push_back(p1);
	patterns.push_back(p2);
	patterns.push_back(p3);

	return new pattern(and_tp, patterns.begin(), patterns.end());
      }

      /** \brief Retrieve the sub-patterns of an ?and term. */
      const std::vector<cwidget::util::ref_ptr<pattern> > &get_and_patterns() const
      {
	eassert(tp == and_tp);

	return sub_patterns;
      }

      // @}

      /** \name bind term constructor and accessors. */

      // @{

      /** \brief Create a ?bind term.
       *
       *  \param p                 The pattern in which to
       *                           bind this variable.
       *  \param variable_index    The index on the stack of the
       *                           bound variable.
       */
      static cwidget::util::ref_ptr<pattern>
      make_bind(size_t variable_index,
		const cwidget::util::ref_ptr<pattern> &p)
      {
	return new pattern(bind, variable_index, p);
      }

      /** \brief Retrieve the variable index of a ?bind term. */
      size_t get_bind_variable_index() const
      {
	eassert(tp == bind);

	return info.stack_position;
      }

      /** \brief Retrieve the sub-pattern of a ?bind term. */
      const cwidget::util::ref_ptr<pattern> &get_bind_pattern() const
      {
	eassert(tp == bind && sub_patterns.size() == 1);

	return sub_patterns.front();
      }

      // @}

      /** \name broken term constructor */

      // @{

      /** \brief Create a ?broken term. */
      static cwidget::util::ref_ptr<pattern>
      make_broken()
      {
	return new pattern(broken);
      }

      // @}

      /** \name broken_type term constructor and accessors */

      // @{

      /** \brief Create a ?broken-TYPE term.
       *
       *  \param deptype  The type of dependency to match.
       */
      static cwidget::util::ref_ptr<pattern>
      make_broken_type(pkgCache::Dep::DepType deptype)
      {
	return new pattern(broken_type, deptype, true);
      }

      /** \brief Retrieve the dependency type of a
       *  ?broken-TYPE matcher.
       */
      pkgCache::Dep::DepType get_broken_type_depends_type() const
      {
	eassert(tp == broken_type);

	return info.dep.deptype;
      }

      // @}

      /** \name candidate_version term constructor. */

      // @{

      /** \brief Create a ?version(CANDIDATE) term. */
      static cwidget::util::ref_ptr<pattern>
      make_candidate_version()
      {
	return new pattern(candidate_version);
      }

      // @}

      /** \name config_files term constructor */

      // @{

      /** \brief Create a ?config-files term. */
      static cwidget::util::ref_ptr<pattern>
      make_config_files()
      {
	return new pattern(config_files);
      }

      // @}

      /** \name current_version term constructor */

      // @{

      /** \brief Create a ?version(CURRENT) term. */

      static cwidget::util::ref_ptr<pattern>
      make_current_version()
      {
	return new pattern(current_version);
      }

      // @}

      /** \name depends term constructor and accessors */

      // @{

      /** \brief Create a ?depends or ?broken-depends term.
       *
       *  \param deptype  The type of dependency to match.
       *  \param broken   \b true to only match broken dependencies.
       *  \param p        The sub-pattern of ?depends.
       */
      static cwidget::util::ref_ptr<pattern>
      make_depends(pkgCache::Dep::DepType deptype,
		   bool broken,
		   const cwidget::util::ref_ptr<pattern> &p)
      {
	return new pattern(depends, &p, (&p) + 1, deptype, broken);
      }

      /** \brief Retrieve the depends_type field of a ?depends term. */
      pkgCache::Dep::DepType get_depends_depends_type() const
      {
	eassert(tp == depends);

	return info.dep.deptype;
      }

      /** \brief Retrieve the broken field of a ?depends term. */
      bool get_depends_broken() const
      {
	eassert(tp == depends);

	return info.dep.broken;
      }

      /** \brief Retrieve the sub-pattern of a ?depends term. */
      const cwidget::util::ref_ptr<pattern> &get_depends_pattern() const
      {
	eassert(tp == depends && sub_patterns.size() == 1);

	return sub_patterns.front();
      }

      // @}

      /** \name description term constructor and accessors */

      // @{

      /** \brief Create a ?description term.
       *
       *  \param s   The regular expression to match against the
       *             description.
       */
      static cwidget::util::ref_ptr<pattern> make_description(const std::string &s)
      {
	return new pattern(description, regex_info(s));
      }

      /** \brief Retrieve the regular expression info for a
       *  description term.
       */
      const regex_info &get_description_regex_info() const
      {
	eassert(tp == description);

	return regex_information;
      }

      // @}

      /** \name essential term constructor */

      // @{

      /** \brief Create an ?essential term. */
      static cwidget::util::ref_ptr<pattern> make_essential()
      {
	return new pattern(essential);
      }

      // @}

      /** \name equal term constructor and accessors */

      // @{

      /** \brief Create a ?= term. */
      static cwidget::util::ref_ptr<pattern> make_equal(size_t stack_position)
      {
	return new pattern(equal, stack_position);
      }

      /** \brief Retrieve the stack position of a ?= term. */
      size_t get_equal_stack_position() const
      {
	eassert(tp == equal);

	return info.stack_position;
      }

      // @}

      /** \name exact_name term constructor and accessors */

      // @{

      static cwidget::util::ref_ptr<pattern> make_exact_name(const std::string &name)
      {
	return new pattern(exact_name, name);
      }

      const std::string &get_exact_name_name() const
      {
	eassert(tp == exact_name);

	return string_info;
      }

      // @}

      /** \name false term constructor */

      // @{

      /** \brief Create a ?false term. */
      static cwidget::util::ref_ptr<pattern> make_false()
      {
	return new pattern(false_tp);
      }

      // @}

      /** \name for_tp term constructor and accessors. */

      // @{

      /** \brief Create a ?for term.
       *
       *  \param variable_name  The name of the variable introduced
       *                        by this term.
       *  \param p              The sub-pattern of this term.
       */
      static cwidget::util::ref_ptr<pattern> make_for(const std::string &variable_name,
						      const cwidget::util::ref_ptr<pattern> &p)
      {
	return new pattern(for_tp, p, variable_name);
      }

      std::string get_for_variable_name() const
      {
	eassert(tp == for_tp);

	return string_info;
      }

      const cwidget::util::ref_ptr<pattern> &get_for_pattern() const
      {
	eassert(tp == for_tp && sub_patterns.size() == 1);

	return sub_patterns.front();
      }

      // @}

      /** \name foreign_architecture term constructor */

      // @{

      /** \brief Create an ?architecture(foreign) term. */

      static cwidget::util::ref_ptr<pattern>
      make_foreign_architecture()
      {
	return new pattern(foreign_architecture);
      }

      // @}

      /** \name garbage term constructor. */

      // @{

      /** \brief Create a ?garbage term. */
      static cwidget::util::ref_ptr<pattern> make_garbage()
      {
	return new pattern(garbage);
      }

      // @}

      /** \name install_version term */

      // @{

      /** \brief Create a ?version(TARGET) term. */
      static cwidget::util::ref_ptr<pattern> make_install_version()
      {
	return new pattern(install_version);
      }

      // @}

      /** \name installed term constructor */

      // @{

      /** \brief Create a ?installed term. */
      static cwidget::util::ref_ptr<pattern> make_installed()
      {
	return new pattern(installed);
      }

      // @}

      /** \name maintainer term constructor and accessors */

      // @{

      /** \brief Create a ?maintainer term.
       *
       *  \param s   The regular expression to match against the
       *             package maintainer.
       */
      static cwidget::util::ref_ptr<pattern> make_maintainer(const std::string &s)
      {
	return new pattern(maintainer, regex_info(s));
      }

      /** \brief Retrieve the regular expression info for a
       *  maintainer term.
       */
      const regex_info &get_maintainer_regex_info() const
      {
	eassert(tp == maintainer);

	return regex_information;
      }

      // @}

      /** \name multiarch term constructor and accessors. */

      // @{

      /** \brief Create a ?multiarch term.
       *
       *  \param ma  The multiarch type to match.
       */
      static cwidget::util::ref_ptr<pattern>
      make_multiarch(const multiarch_type ma)
      {
	return new pattern(multiarch, ma);
      }

      /** \brief Retrieve the information associated with a ?multiarch
       *  term.
       */
      const multiarch_type get_multiarch_multiarch_type() const
      {
	eassert(tp == multiarch);

	return info.multiarch;
      }

      // @}

      /** \name name term constructor and accessors */

      // @{

      /** \brief Create a ?name term.
       *
       *  \param s   The regular expression to match against the
       *             package name.
       */
      static cwidget::util::ref_ptr<pattern> make_name(const std::string &s)
      {
	return new pattern(name, regex_info(s));
      }

      /** \brief Retrieve the regular expression info for a
       *  name term.
       */
      const regex_info &get_name_regex_info() const
      {
	eassert(tp == name);

	return regex_information;
      }

      // @}

      /** \name narrow term constructor and accessors */

      // @{

      /** \brief Create a ?narrow term.
       *
       *  \param filter The filter for this term.
       *  \param p      The sub-pattern of this term.
       */
      static cwidget::util::ref_ptr<pattern> make_narrow(const cwidget::util::ref_ptr<pattern> &filter,
							 const cwidget::util::ref_ptr<pattern> &p)
      {
	return new pattern(narrow, filter, p);
      }

      /** \brief Retrieve the filter of a ?narrow term. */
      const cwidget::util::ref_ptr<pattern> &get_narrow_filter() const
      {
	eassert(tp == narrow && sub_patterns.size() == 2);

	return sub_patterns[0];
      }

      /** \brief Retrieve the pattern of a ?narrow term. */
      const cwidget::util::ref_ptr<pattern> &get_narrow_pattern() const
      {
	eassert(tp == narrow && sub_patterns.size() == 2);

	return sub_patterns[1];
      }

      // @}

      /** \name native_architecture term constructor */

      // @{

      /** \brief Create an ?architecture(native) term. */

      static cwidget::util::ref_ptr<pattern>
      make_native_architecture()
      {
	return new pattern(native_architecture);
      }

      // @}

      /** \name new_tp term constructor */

      // @{

      /** \brief Create a ?new term. */
      static cwidget::util::ref_ptr<pattern> make_new()
      {
	return new pattern(new_tp);
      }

      // @}

      /** \name not_tp term constructor and accessors */

      // @{

      /** \brief Create a ?not term.
       *
       *  \param p   The sub-pattern of this term.
       */
      static cwidget::util::ref_ptr<pattern> make_not(const cwidget::util::ref_ptr<pattern> &p)
      {
	return new pattern(not_tp, p);
      }

      /** \brief Retrieve the pattern field of a ?not term. */
      const cwidget::util::ref_ptr<pattern> &get_not_pattern() const
      {
	eassert(tp == not_tp && sub_patterns.size() == 1);

	return sub_patterns.front();
      }

      // @}

      /** \name obsolete term constructor */

      // @{

      /** \brief Create an ?obsolete term. */
      static cwidget::util::ref_ptr<pattern> make_obsolete()
      {
	return new pattern(obsolete);
      }

      // @}

      /** \name or_tp term constructor and accessors */

      // @{

      /** \brief Create an ?or term.
       *
       *  \param begin   The left bound of the range with which
       *                 to construct the term.
       *  \param end     The right bound of the range with which
       *                 to construct the term.
       */
      template<typename Iter>
      static cwidget::util::ref_ptr<pattern> make_or(Iter begin,
						     Iter end)
      {
	return new pattern(or_tp, begin, end);
      }

      /** \brief Create a binary ?or term.
       *
       *  \param p1  The first term in the ?or.
       *  \param p2  The second term in the ?or.
       */
      static cwidget::util::ref_ptr<pattern>
      make_or(const cwidget::util::ref_ptr<pattern> &p1,
	      const cwidget::util::ref_ptr<pattern> &p2)
      {
	std::vector<cwidget::util::ref_ptr<pattern> > patterns;

	patterns.push_back(p1);
	patterns.push_back(p2);
	return make_or(patterns);
      }

      /** \brief Create a binary ?or term.
       *
       *  \param p1  The first term in the ?or.
       *  \param p2  The second term in the ?or.
       *  \param p3  The third term in the ?or.
       */
      static cwidget::util::ref_ptr<pattern>
      make_or(const cwidget::util::ref_ptr<pattern> &p1,
	      const cwidget::util::ref_ptr<pattern> &p2,
	      const cwidget::util::ref_ptr<pattern> &p3)
      {
	std::vector<cwidget::util::ref_ptr<pattern> > patterns;

	patterns.push_back(p1);
	patterns.push_back(p2);
	patterns.push_back(p3);
	return make_or(patterns);
      }

      /** \brief Create an ?or term.
       *
       *  \param container  An STL container holding the
       *                    sub-patterns of the new term.
       */
      template<typename C>
      static cwidget::util::ref_ptr<pattern>
      make_or(const C &container)
      {
	return new pattern(or_tp, container.begin(), container.end());
      }

      /** \brief Retrieve the sub-patterns of an ?or term. */
      const std::vector<cwidget::util::ref_ptr<pattern> > &
      get_or_patterns() const
      {
	eassert(tp == or_tp);

	return sub_patterns;
      }

      // @}

      /** \name origin term constructor and accessors */

      // @{

      /** \brief Create an ?origin term.
       *
       *  \param s   The regular expression to match against
       *             the package's origin.
       */
      static cwidget::util::ref_ptr<pattern> make_origin(const std::string &s)
      {
	return new pattern(origin, regex_info(s));
      }

      /** \brief Retrieve the regex_info field of an ?origin term. */
      const regex_info &get_origin_regex_info() const
      {
	eassert(tp == origin);

	return regex_information;
      }

      // @}

      /** \name priority term constructor and accessors */

      // @{

      /** \brief Create a ?priority term.
       *
       *  \param s  The name of the priority to select.
       */
      static cwidget::util::ref_ptr<pattern> make_priority(pkgCache::State::VerPriority priority)
      {
	return new pattern(pattern::priority, priority);
      }

      /** \brief Retrieve the priority field of a ?priority term. */
      pkgCache::State::VerPriority get_priority_priority() const
      {
	eassert(tp == priority);

	return info.priority;
      }

      // @}

      /** \name provides term constructor and accessors */

      // @{

      /** \brief Create a ?provides term.
       *
       *  \param p   The sub-pattern of the term.
       */
      static cwidget::util::ref_ptr<pattern>
      make_provides(const cwidget::util::ref_ptr<pattern> &p)
      {
	return new pattern(provides, p);
      }

      /** \brief Retrieve the pattern field of a ?provides term. */
      const cwidget::util::ref_ptr<pattern> &
      get_provides_pattern() const
      {
	eassert(tp == provides && sub_patterns.size() == 1);

	return sub_patterns.front();
      }

      // @}


      /** \name reverse_depends term constructor and accessors */

      // @{

      /** \brief Create a ?reverse-depends or ?broken-reverse-depends term.
       *
       *  \param deptype  The type of dependency to match.
       *  \param broken   \b true to only match broken dependencies.
       *  \param p        The sub-pattern of ?reverse-depends.
       */
      static cwidget::util::ref_ptr<pattern>
      make_reverse_depends(pkgCache::Dep::DepType deptype,
			   bool broken,
			   const cwidget::util::ref_ptr<pattern> &p)
      {
	return new pattern(reverse_depends, &p, (&p) + 1, deptype, broken);
      }

      /** \brief Retrieve the depends_type field of a ?reverse-depends term. */
      pkgCache::Dep::DepType get_reverse_depends_depends_type() const
      {
	eassert(tp == reverse_depends);

	return info.dep.deptype;
      }

      /** \brief Retrieve the broken field of a ?reverse-depends term. */
      bool get_reverse_depends_broken() const
      {
	eassert(tp == reverse_depends);

	return info.dep.broken;
      }

      /** \brief Retrieve the sub-pattern of a ?reverse-depends term. */
      const cwidget::util::ref_ptr<pattern> &get_reverse_depends_pattern() const
      {
	eassert(tp == reverse_depends && sub_patterns.size() == 1);

	return sub_patterns.front();
      }

      // @}

      /** \name reverse_provides term constructor and accessors */

      // @{

      /** \brief Create a ?reverse-provides term.
       *
       *  \param p   The sub-pattern of ?reverse-provides.
       */
      static cwidget::util::ref_ptr<pattern>
      make_reverse_provides(const cwidget::util::ref_ptr<pattern> &p)
      {
	return new pattern(reverse_provides, p);
      }

      /** \brief Retrieve the sub-pattern of a ?reverse-provides term. */
      const cwidget::util::ref_ptr<pattern> &
      get_reverse_provides_pattern() const
      {
	eassert(tp == reverse_provides && sub_patterns.size() == 1);

	return sub_patterns.front();
      }

      // @}

      /** \name section term constructor and accessors */

      // @{

      /** \brief Create a ?section term.
       *
       *  \param s   The regular expression to match against the
       *             package's section.
       */
      static cwidget::util::ref_ptr<pattern>
      make_section(const std::string &s)
      {
	return new pattern(section, regex_info(s));
      }

      /** \brief Retrieve the regex_info field of a ?section term. */
      const regex_info &get_section_regex_info() const
      {
	eassert(tp == section);

	return regex_information;
      }

      // @}

      /** \name source_package term constructor and accessors */

      // @{

      /** \brief Create a ?source-package term.
       *
       *  \param s   The regular expression to match against the
       *             source package.
       */
      static cwidget::util::ref_ptr<pattern>
      make_source_package(const std::string &s)
      {
	return new pattern(source_package, regex_info(s));
      }

      /** \brief Retrieve the regex_info field of a ?source-package term. */
      const regex_info &get_source_package_regex_info() const
      {
	eassert(tp == source_package);

	return regex_information;
      }

      // @}

      /** \name source_version term constructor and accessors */

      // @{

      /** \brief Create a ?source-version term.
       *
       *  \param s   The regular expression to match against the
       *             source package version.
       */
      static cwidget::util::ref_ptr<pattern>
      make_source_version(const std::string &s)
      {
	return new pattern(source_version, regex_info(s));
      }

      /** \brief Retrieve the regex_info field of a ?source-version term. */
      const regex_info &get_source_version_regex_info() const
      {
	eassert(tp == source_version);

	return regex_information;
      }

      // @}

      /** \name tag term constructor and accessors */

      // @{

      /** \brief Create a ?tag term.
       *
       *  \param s   The regular expression to match against the
       *             package's tags.
       */
      static cwidget::util::ref_ptr<pattern>
      make_tag(const std::string &s)
      {
	return new pattern(tag, regex_info(s));
      }

      /** \brief Retrieve the regex_info field of a ?tag term. */
      const regex_info &get_tag_regex_info() const
      {
	eassert(tp == tag);

	return regex_information;
      }

      // @}

      /** \name task term constructor and accessors */

      // @{

      /** \brief Create a ?task term.
       *
       *  \param s   The regular expression to match against the
       *             package's tasks.
       */
      static cwidget::util::ref_ptr<pattern>
      make_task(const std::string &s)
      {
	return new pattern(task, regex_info(s));
      }

      /** \brief Retrieve the regex_info field of a ?task term. */
      const regex_info &get_task_regex_info() const
      {
	eassert(tp == task);

	return regex_information;
      }

      // @}

      /** \name ?term term constructor and accessors */

      // @{

      /** \brief Create a ?term term.
       *
       *  \param s  The keyword to search for.
       */
      static cwidget::util::ref_ptr<pattern>
      make_term(const std::string &s)
      {
	return new pattern(term, s);
      }

      /** \brief Retrieve the term field of a ?term term. */
      const std::string &get_term_term() const
      {
	eassert(tp == term);

	return string_info;
      }

      // @}


      // @{

      /** \brief Create a ?term-prefix term.
       *
       *  \param s  The keyword to search for.
       */
      static cwidget::util::ref_ptr<pattern>
      make_term_prefix(const std::string &s)
      {
	return new pattern(term_prefix, s);
      }

      /** \brief Retrieve the term field of a ?term term. */
      const std::string &get_term_prefix_term() const
      {
	eassert(tp == term_prefix);

	return string_info;
      }

      // @}

      /** \name true term constructor */

      // @{

      /** \brief Create a ?true term. */
      static cwidget::util::ref_ptr<pattern> make_true()
      {
	return new pattern(true_tp);
      }

      // @}

      /** \name upgradable term constructor */

      // @{

      /** \brief Create an ?upgradable term. */
      static cwidget::util::ref_ptr<pattern> make_upgradable()
      {
	return new pattern(upgradable);
      }

      // @}

      /** \name user_tag term constructor and accessors */

      // @{

      /** \brief Create a ?user-tag term.
       *
       *  \param s   The regular expression to match against the
       *             package's user tags.
       */
      static cwidget::util::ref_ptr<pattern>
      make_user_tag(const std::string &s)
      {
	return new pattern(user_tag, regex_info(s));
      }

      /** \brief Retrieve the regex_info field of a ?user-tag term. */
      const regex_info &get_user_tag_regex_info() const
      {
	eassert(tp == user_tag);

	return regex_information;
      }

      // @}

      /** \name version term constructor and accessors */

      // @{

      /** \brief Create a ?version term.
       *
       *  \param s   The regular expression to match against the
       *             package's versions.
       */
      static cwidget::util::ref_ptr<pattern>
      make_version(const std::string &s)
      {
	return new pattern(version, regex_info(s));
      }

      /** \brief Retrieve the regex_info field of a ?version term. */
      const regex_info &get_version_regex_info() const
      {
	eassert(tp == version);

	return regex_information;
      }

      // @}

      /** \name virtual_tp term constructor and accessors */

      // @{

      /** \brief Create a ?virtual term. */

      static cwidget::util::ref_ptr<pattern> make_virtual()
      {
	return new pattern(virtual_tp);
      }

      // @}

      /** \name widen term constructor and accessors */

      // @{

      /** \brief Create a ?widen term.
       *
       *  \param p   The sub-pattern of the new term.
       */
      static cwidget::util::ref_ptr<pattern>
      make_widen(const cwidget::util::ref_ptr<pattern> &p)
      {
	return new pattern(widen, p);
      }

      /** \brief Retrieve the sub-pattern of a ?widen term. */
      const cwidget::util::ref_ptr<pattern> &get_widen_pattern()
      {
	eassert(tp == widen && sub_patterns.size() == 1);

	return sub_patterns.front();
      }

      // @}
    };

    /** \brief Test whether a string looks like a search pattern.
     *
     *  \param s  the string to test.
     *
     *  This is used in situations where it would be counterintuitive
     *  for all strings to be treated as search patterns, but where we
     *  want search patterns to be available.  Strings are considered
     *  to be seach patterns if they contain a tilde (~) or a question
     *  mark (?).
     *
     *  \return \b true if the string qualifies as a search pattern.
     */
    bool is_pattern(const std::string &s);
  }
}

#endif
