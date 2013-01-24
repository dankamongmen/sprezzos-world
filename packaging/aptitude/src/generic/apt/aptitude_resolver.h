// aptitude_resolver.h                  -*-c++-*-
//
// 
//   Copyright (C) 2005, 2008-2010 Daniel Burrows

//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.

//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.

//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.
//
// 
//


#ifndef APTITUDE_RESOLVER_H
#define APTITUDE_RESOLVER_H

#include "aptitude_resolver_cost_settings.h"
#include "aptitude_resolver_universe.h"

#include <generic/apt/matching/pattern.h>
#include <generic/problemresolver/problemresolver.h>

#include <generic/util/immset.h>

#include <iosfwd>

class pkgPolicy;

/** \brief Glue code to make the resolver talk to the core aptitude classes.
 *
 *  
 *  General comment on how the iterators are handled: basically the
 *  technique is (generally) to have a normalize() routine that
 *  advances the current iterator(s) to the next "interesting"
 *  iterator.  For instance, broken_dep_iterator::normalize() moves to
 *  the next broken dependency (sort of).  If the current iterator is
 *  already interesting, nothing happens.  This is used on
 *  initialization and in operator++ (after advancing the iterator a
 *  single step manually).
 * 
 *  \file aptitude_resolver.h
 */

namespace aptitude
{
  namespace matching
  {
    class pattern;
  }
}

class aptitude_resolver:public generic_problem_resolver<aptitude_universe>
{
  choice_set keep_all_solution;
  pkgPolicy *policy;

  aptitude_resolver_cost_settings cost_settings;

  void add_full_replacement_score(const pkgCache::VerIterator &src,
				  const pkgCache::PkgIterator &real_target,
				  const pkgCache::VerIterator &provider,
				  int full_replacement_score,
				  int undo_full_replacement_score);

  /** \brief Given the first dependency in an OR group, add scores to
   *  bias the resolver in favor of the default candidate that
   *  MarkInstall would pick.
   */
  void add_default_resolution_score(const pkgCache::DepIterator &dep,
				    int default_resolution_score);
public:
  class hint
  {
  public:
    /** \brief The type of hint represented by this object. */
    enum hint_type
      {
	/** \brief A hint indicating that a named component of the
	 *  cost tuple should have a number added to it.
	 */
	add_to_cost_component,
        /** \brief A hint indicating that the target should be
         *  discarded.
         */
        discard,
	/** \brief A hint indicating that a named component of the
	 *  cost tuple should be increased to an upper bound.
	 */
	raise_cost_component,
	/** \brief A hint that one or more package versions should be
	 *  rejected.
	 */
	reject,
	/** \brief A hint that one or more package versions should be
	 *   mandated.
	 */
	mandate,
	/** \brief A hint that one or more package versions should
	 *  have their scores adjusted by some amount.
	 */
	tweak_score
      };

    /** \brief Describes which versions are selected by a hint. */
    class version_selection
    {
    public:
      /** \brief Describes what sort of version selection is in use. */
      enum version_selection_type
	{
	  /** \brief All versions.
	   *
	   *  Matches any version.
	   */
	  select_all,

	  /** \brief Versions are selected by archive.
	   *
	   *  Any version contained in an archive that equals the
	   *  version selection string will be selected.
	   */
	  select_by_archive,

	  /** \brief All versions of a package except the
	   *  not-installed version will be matched.
	   *
	   *  This is equivalent to not providing a version string.
	   */
	  select_inst,

	  /** \brief The non-installed version of the package will be
	   *  matched.
	   */
	  select_uninst,

	  /** \brief Versions are selected by version string.
	   *
	   *  Any version contained in an archive that compares
	   *  correctly to the version selection string (according to
	   *  the comparison operator) will be selected.
	   */
	  select_by_version
	};

      /** \brief Lists the comparison operations that are allowed. */
      enum compare_op_type
	{
	  less_than,
	  less_than_or_equal_to,
	  equal_to,
	  not_equal_to,
	  greater_than,
	  greater_than_or_equal_to
	};

    private:
      version_selection_type type;
      compare_op_type compare_op;
      std::string version_selection_string;

      version_selection(version_selection_type _type,
			compare_op_type _compare_op,
			const std::string &_version_selection_string)
	: type(_type), compare_op(_compare_op),
	  version_selection_string(_version_selection_string)
      {
      }

    public:
      version_selection()
	: type((version_selection_type)-1),
	  compare_op((compare_op_type)-1),
	  version_selection_string()
      {
      }

      static version_selection make_all()
      {
	return version_selection(select_all, (compare_op_type)-1, std::string());
      }

      /** \brief Create a version selection that selects versions by
       *  their archive.
       *
       *  \param archive   The archive to match; only versions that are
       *                   contained in this archive will be selected.
       */
      static version_selection make_archive(const std::string &archive)
      {
	return version_selection(select_by_archive, (compare_op_type)-1, archive);
      }

      /** \brief Create a version selection that selects all versions
       *  except the not-installed version.
       */
      static version_selection make_inst()
      {
	return version_selection(select_inst, (compare_op_type)-1, std::string());
      }

      /** \brief Create a version selection that selects not-installed
       *  versions.
       */
      static version_selection make_uninst()
      {
	return version_selection(select_uninst, (compare_op_type)-1, std::string());
      }

      /** \brief Create a version selection that selects versions by
       *  version number.
       *
       *  \param   The version number to compare against.
       *  \param   The operation to use in comparison.  For instance,
       *           use pkgCache::Dep::Less to select only versions
       *           less than the given version.
       */
      static version_selection make_version(compare_op_type compare_op,
					    const std::string &version)
      {
	return version_selection(select_by_version, compare_op, version);
      }

      /** \brief Test a version against this selector.
       *
       *  \param v   The version to test.
       *
       *  \return \b true if v is matched by this selector.
       */
      bool matches(const aptitude_resolver_version &v) const;

      /** \brief Compare two version selectors.
       *
       *  \param other   The version selector to compare against.
       *
       *  Selectors are arbitrarily arranged in a total ordering.
       *
       *  \return a number less than zero if this selector is less
       *  than the other selector, a number greater than zero if the
       *  other selector is greater than this selector, and zero if
       *  the two selectors are equal.
       */
      int compare(const version_selection &other) const;

      bool operator<(const version_selection &other) const { return compare(other) < 0; }
      bool operator<=(const version_selection &other) const { return compare(other) <= 0; }
      bool operator==(const version_selection &other) const { return compare(other) == 0; }
      bool operator!=(const version_selection &other) const { return compare(other) != 0; }
      bool operator>=(const version_selection &other) const { return compare(other) >= 0; }
      bool operator>(const version_selection &other) const { return compare(other) > 0; }

      /** \brief Get the type of this selection. */
      version_selection_type get_type() const { return type; }

      /** \brief Get the version selection string of this selection.
       *
       *  Only valid for select_by_archive and select_by_version
       *  selections.
       */
      const std::string &get_version_selection_string() const
      {
	eassert(type == select_by_archive || type == select_by_version);

	return version_selection_string;
      }

      /** \brief Get the comparison operation of this selection.
       *
       *  Only valid for select_by_version selections.
       */
      compare_op_type get_version_comparison_operator() const
      {
	eassert(type == select_by_version);

	return compare_op;
      }
    };

  private:
    hint_type type;
    int amt;
    cwidget::util::ref_ptr<aptitude::matching::pattern> target;
    version_selection selection;
    std::string component_name;

    hint(hint_type _type, int _amt,
	 const cwidget::util::ref_ptr<aptitude::matching::pattern> &_target,
	 version_selection _selection,
         const std::string &_component_name)
      : type(_type), amt(_amt),
	target(_target), selection(_selection), component_name(_component_name)
    {
    }

  public:
    hint()
      : type((hint_type)-1), amt(-1), target(NULL), selection(), component_name()
    {
    }

    ~hint();

    /** \brief Create a hint that adds to a single component of the
     *  cost tuple.
     */
    static hint make_add_to_cost_component(const cwidget::util::ref_ptr<aptitude::matching::pattern> &target,
					   const version_selection &selection,
					   const std::string &component_name,
					   int amt)
    {
      return hint(add_to_cost_component, amt,
		  target, selection, component_name);
    }

    /** \brief Create a hint that discards solutions containing the target. */
    static hint make_discard(const cwidget::util::ref_ptr<aptitude::matching::pattern> &target,
                             const version_selection &selection)
    {
      return hint(discard, 0, target, selection, "");
    }

    /** \brief Create a hint that increases a single component of the
     *  cost level to the given value.
     */
    static hint make_raise_cost_component(const cwidget::util::ref_ptr<aptitude::matching::pattern> &target,
					  const version_selection &selection,
					  const std::string &component_name,
					  int amt)
    {
      return hint(raise_cost_component, amt,
		  target, selection, component_name);
    }

    /** \brief Create a hint that rejects a version or versions of a package. */
    static hint make_reject(const cwidget::util::ref_ptr<aptitude::matching::pattern> &target,
				     const version_selection &selection)
    {
      return hint(reject, 0, target, selection, "");
    }

    /** \brief Create a hint that mandates a version or versions of a package. */
    static hint make_mandate(const cwidget::util::ref_ptr<aptitude::matching::pattern> &target,
				      const version_selection &selection)
    {
      return hint(mandate, 0, target, selection, "");
    }

    /** \brief Create a hint that adjusts the score of a package. */
    static hint make_tweak_score(const cwidget::util::ref_ptr<aptitude::matching::pattern> &target,
					  const version_selection &selection,
					  int score)
    {
      return hint(tweak_score, score, target, selection, "");
    }

    /** \brief Parse a resolver hint definition.
     *
     *  Definitions have the form ACTION TARGET [VERSION].  ACTION is
     *  either a number (which will be added to the score of the
     *  selected version), "increase-tier N" where N is a number, or
     *  the special strings "reject" or "approve".  If TARGET is a
     *  match pattern (specifically, if the portion of the remaining
     *  string that parses as a match pattern includes a question mark
     *  or tilde), then it will be treated as such; otherwise it is
     *  the name of the package to match.  VERSION is the version of
     *  TARGET that is to be tweaked.  If VERSION is not present, all
     *  versions of the package (except the removal version) that
     *  match TARGET will be selected.  If VERSION has the form
     *  "/<archive>" then the version of the package from that archive
     *  will be selected.  If VERSION is ":UNINST" then the
     *  not-installed version of the package will be selected.
     *  Finally, VERSION may be ">VERSION2", "=VERSION2",
     *  ">=VERSION2", "<VERSION2", "<=VERSION2", or "<>VERSION2" to
     *  only apply the hint to versions of the package that compare
     *  accordingly to the version string.  (obviously "=VERSION2" is
     *  redundant, but it is included for completeness)
     *
     *  \param definition   The text of the hint definition.
     *  \param out  A location in which to store the parsed hint.
     *
     *  \return \b true if the hint was parsed successfully, \b false
     *  otherwise.
     */
    static bool parse(const std::string &definition, hint &out);

    /** \brief Compare this hint to another hint.
     *
     *  \param other  The hint to which this is to be compared.
     *
     *  \return -1 if this is less than other, 0 if the two hints are
     *  equal, and 1 if this is more than other.
     *
     *  Hints exist in an arbitrary total ordering.
     */
    int compare(const hint &other) const;

    bool operator<(const hint &other) const { return compare(other) < 0; }
    bool operator<=(const hint &other) const { return compare(other) <= 0; }
    bool operator==(const hint &other) const { return compare(other) == 0; }
    bool operator!=(const hint &other) const { return compare(other) != 0; }
    bool operator>=(const hint &other) const { return compare(other) >= 0; }
    bool operator>(const hint &other) const { return compare(other) > 0; }

    /** \brief Get the type of this hint.
     *
     *  \sa hint_type
     */
    hint_type get_type() const { return type; }

    /** \brief Retrieve the integer associated with this hint.
     *
     *  For score-tweaking hints, this is the number of points to be
     *  added to the version's score.  For cost-component-tweaking
     *  hints, this is the amount to increase the cost component by or
     *  the value to increase it to.
     */
    int get_amt() const { return amt; }

    /** \brief Retrieve the cost component name associated with this hint. */
    const std::string &get_component_name() const { return component_name; }

    /** \brief Return the pattern identifying the package or packages
     *  to be adjusted.
     */
    const cwidget::util::ref_ptr<aptitude::matching::pattern> &
    get_target() const { return target; }

    /** \brief Return the version selection rule for this hint. */
    const version_selection &get_version_selection() const { return selection; }
  };

  aptitude_resolver(int step_score, int broken_score,
		    int unfixed_soft_score,
		    int infinity,
		    int resolution_score,
                    const cost &unfixed_soft_cost,
		    int future_horizon,
		    const aptitude_resolver_cost_settings &_cost_settings,
		    const imm::map<aptitude_resolver_package, aptitude_resolver_version> &initial_installations,
		    aptitudeDepCache *cache,
		    pkgPolicy *_policy);

  virtual ~aptitude_resolver() {}

  /** \brief Return \b true if the given version will break a hold or
   *  install a forbidden version.
   */
  bool is_break_hold(const version &v) const;

  /** Assign scores to all packages and all package versions according
   *  to its arguments.  All scores are assigned with add_score, so
   *  this can be easily combined with other policies.
   *
   *  Note: hints are folded into this routine for efficiency
   *  (minimizing the number of passes over the cache.  We should
   *  probably fold everything into one enormous monster "set all the
   *  aptitude scores up" routine.
   *
   * \param preserve_score the score to assign to the version that the
   * user selected.
   *
   * \param auto_score the score to assign to automatically assigned
   * actions.  By making this smaller than preserve_score you can bias
   * the system towards overriding automatic decisions rather than
   * user actions.
   *
   * \param remove_score the score to assign to removing a package
   * against the user's wishes.
   *
   * \param keep_score the score to assign to cancelling actions on a
   * package against the user's wishes.
   *
   * \param install_score the score to assign to removing a package
   * against the user's wishes.
   *
   * \param upgrade_score the score to assign to upgrading a package
   * against the user's wishes.
   *
   * \param non_default_score the score to assign to installing a
   * non-default version of a package (such as a downgrade or an
   * experimental version).
   *
   * \param essential_remove an additional modification applied to the
   * removal of an essential package (typically used to deep-six such
   * solutions by, eg, setting it to -100000)
   *
   * \param full_replacement_score the score for removing a package p
   * and installing a package that fully replaces p (i.e., conflicts,
   * provides, and replaces it).
   *
   * \param undo_full_replacement_score the score for installing a
   * package p and removing a package that fully replaces p.
   *
   * \param break_hold_score an additional modification applied to
   * solutions that break a hold or violate a forbidding.
   *
   * \param allow_break_holds_and_forbids   if false, versions that
   * would break a package hold or install a forbidden version are
   * rejected up-front.
   *
   * \param default_resolution_score   the score for installing a
   * package and also resolving a dependency in the way that
   * MarkInstall would, if the dependency isn't current resolved.
   * (this is arguably not quite right: it ought to be cancelled
   * whenever the dependency is resolved by a partial solution)
   *
   * \param initial_state_manual_flags maps packages that have an
   * overridden initial state to "true" or "false" depending on
   * whether they should be considered to have a manually chosen
   * state.  The manual states of overridden packages default to
   * "true" if they do not have a mapping in this collection.
   */
  void add_action_scores(int preserve_score, int auto_score,
			 int remove_score, int keep_score,
			 int install_score, int upgrade_score,
			 int non_default_score, int essential_remove,
			 int full_replacement_score,
			 int undo_full_replacement_score,
			 int break_hold_score,
			 bool allow_break_holds_and_forbids,
			 int default_resolution_score,
			 const std::map<package, bool> &initial_state_manual_flags,
			 const std::vector<hint> &hints);

  /** Score packages/versions according to their priorities.  Normally
   *  you want important>=required>=standard>=optional>=extra.
   *
   *  \param important score modification for Important versions
   *  \param required score modification for Required versions
   *  \param standard score modification for Standard versions
   *  \param optional score modification for Optional versions
   *  \param extra score modification for Extra versions
   */
  void add_priority_scores(int important, int required, int standard,
			   int optional, int extra);

  /** \return the "keep-all" solution, the solution that cancels
   *  all of the user's planned actions.
   */
  choice_set get_keep_all_solution() const;
};

std::ostream &operator<<(std::ostream &out, const aptitude_resolver::hint &hint);

#endif
