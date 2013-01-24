// cmdline_util.h                                   -*-c++-*-
//
// Copyright (C) 2004, 2010 Daniel Burrows
// Copyright (C) 2012 Daniel Hartwig
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

#ifndef CMDLINE_UTIL_H
#define CMDLINE_UTIL_H

// Local includes:
#include "cmdline_common.h"

#include <pkg_sortpolicy.h>

// For download_manager::result
#include <generic/apt/download_manager.h>
#include <generic/apt/matching/pattern.h>
#include <generic/apt/matching/match.h> // For structural_match.
#include <generic/apt/tasks.h>

// System includes:
#include <apt-pkg/srcrecords.h>

#include <boost/shared_ptr.hpp>

// Ew: for column_definition_list.
#include <cwidget/config/column_definition.h>
#include <cwidget/generic/util/ref_ptr.h>

#include <string>

/** \file cmdline_util.h
 */

namespace aptitude
{
  namespace cmdline
  {
    class terminal_input;
    class terminal_locale;
    class terminal_metrics;
    class terminal_output;
  }
}

void cmdline_show_pkglist(pkgvector &items,
                          const boost::shared_ptr<aptitude::cmdline::terminal_metrics> &term_metrics);
void cmdline_show_stringlist(strvector &items,
                             const boost::shared_ptr<aptitude::cmdline::terminal_metrics> &term_metrics);

/** Finds a candidate version for the package using the given source.
 */
pkgCache::VerIterator cmdline_find_ver(pkgCache::PkgIterator pkg,
				       cmdline_version_source source,
				       string sourcestr,
                                       GlobalError::MsgType error_type = GlobalError::ERROR);

/** Starts up the visual UI in preview mode, and exits with status 0
 *  when the UI shuts down.
 */
void ui_preview();

/** Starts up the visual UI with the solution screen visible, exiting
 *  when the UI shuts down.
 */
void ui_solution_screen();

/** Splits the given input string into a package name/pattern and a
 *  version source.  If the input string is an output string, the
 *  function will still behave sanely.
 *
 *  \param input the string to be split
 *  \param source will be set to the type of source specified
 *  \param package will be set to the package name/pattern
 *  \param sourcestr will be set to the string associated with the source,
 *                   if any
 *
 *  \return \b true if the source was successfully parsed.
 */
bool cmdline_parse_source(const string &input,
			  cmdline_version_source &source,
			  string &package,
			  string &sourcestr);

/** Run the given download and post-download commands using the
 *  standard command-line UI.  Runs the preparation routine, the
 *  actual download, and the post-download commands.
 *
 *  \param m        the download process to run.
 *  \param verbose  the verbosity level; controls how many
 *                  stats are printed when the run completes.
 *  \param term_input   the object used to read from the terminal.
 *  \param term_metrics the object from which to read the terminal
 *                      dimensions.
 *  \param term_locale  the locale used for output to the terminal.
 *  \param term_output  the object used to write to the terminal.
 *
 *  \return the success status of the post-download commands, or
 *  failure if the process failed before they could be run.
 */
download_manager::result cmdline_do_download(download_manager *m,
					     int verbose,
                                             const boost::shared_ptr<aptitude::cmdline::terminal_input> &term_input,
                                             const boost::shared_ptr<aptitude::cmdline::terminal_locale> &term_locale,
                                             const boost::shared_ptr<aptitude::cmdline::terminal_metrics> &term_metrics,
                                             const boost::shared_ptr<aptitude::cmdline::terminal_output> &term_output);

namespace aptitude
{
  namespace cmdline
  {
    /** \brief Hack to handle memory management of apt source parsers.
     *
     *  apt parsers belong to the source-list class they are
     *  instantiated by.  This makes it tricky to safely return them
     *  from any routine.  Rather than play games with carefully
     *  managing their use, I just use this class: a safe,
     *  copy-constructable object holding all the information stored
     *  by a source package.
     */
    class source_package
    {
      std::string package;
      std::string version;
      std::string maintainer;
      std::string section;
      std::vector<std::string> binaries;
      std::vector<pkgSrcRecords::Parser::BuildDepRec> build_deps;

    public:
      source_package();
      source_package(pkgSrcRecords::Parser *parser);

      /** \return \b true if this represents a real source package.
       *
       *  Invalid packages are analogous to NULL returns.
       */
      bool valid() const { return !package.empty(); }

      const std::string &get_package() const { return package; }
      const std::string &get_version() const { return version; }
      const std::string &get_maintainer() const { return maintainer; }
      const std::string &get_section() const { return section; }
      const std::vector<std::string> &get_binaries() const { return binaries; }
      const std::vector<pkgSrcRecords::Parser::BuildDepRec> &get_build_deps() const { return build_deps; }
    };

    /** Find a source record in the given set of source records
     *  corresponding to the given package and archive.
     *
     *  \param records the source records object
     *  \param pkg the package name to match on
     *  \param ver the version string to match on
     *
     *  \return the source record, or an invalid package if none is
     *  found.
     */
    source_package find_source_by_archive(const std::string &pkg,
					  const std::string &archive);

    /** \brief Interpret a source-package specification from
     *  the command line.
     *
     *  \brief package   the name of the package to search for.
     *                   If this is a binary package, then that
     *                   package's source is used; otherwise
     *                   the list of source packages is searched.
     *  \brief version_source   how to find the source version
     *                          (implicit, candidate, or archive)
     *  \brief version_source_string    which source version
     *                                  to use.
     *
     *  \return the located source record, or an invalid package if
     *  unsuccessful.
     */
    source_package find_source_package(const std::string &package,
				       cmdline_version_source version_source,
				       const std::string &version_source_string);

    /** \brief Represents a user request to add or remove a user-tag.
     *
     *  This corresponds to the command-line arguments --add-user-tag,
     *  --remove-user-tag, --add-user-tag-to, and
     *  --remove-user-tag-from.
     */
    class tag_application
    {
      bool is_add;
      std::string tag;
      cwidget::util::ref_ptr<aptitude::matching::pattern> pattern; // or NULL for implicit patterns.

    public:
      tag_application(bool _is_add,
		      const std::string &_tag,
		      const cwidget::util::ref_ptr<aptitude::matching::pattern> &_pattern)
      {
	is_add = _is_add;
	tag = _tag;
	pattern = _pattern;
      }

      bool get_is_add() const { return is_add; }
      const std::string &get_tag() const { return tag; }
      const cwidget::util::ref_ptr<aptitude::matching::pattern> &
      get_pattern() const { return pattern; }
    };

    /** \brief Read user-tag application requests from a configuration tree.
     *
     *  \param user_tags The vector to which the applications will be added.
     *  \param config_item Name of the configuration item tree to read from.
     *  \param is_add \b true if these are add requests.
     *  \param implicit \b true if these are implicit requests.
     */
    bool read_user_tag_applications(std::vector<tag_application> &user_tags,
                                    const char *config_item,
                                    const bool is_add, const bool implicit);

    /** \brief Apply explicit and implicit user-tags to packages.
     *
     *  Explicit tags are applied where their associated pattern holds;
     *  implicit tags are applied to packages that the user requested (as
     *  indicated in to_installed et al) and for which the requested
     *  action is being performed.
     *
     *  \param tags The actions requested by the user; they will be
     *              applied in turn to each package, so later actions
     *              will override earlier ones.
     *
     *  \param to_upgrade The packages that the user asked to have upgraded.
     *  \param to_install The packages that the user asked to have installed.
     *  \param to_hold The package that the user asked to have held back.
     *  \param to_remove The packages that the user asked to have removed.
     *  \param to_purge The packages that the user asked to have purged.
     */
    void apply_user_tags(const std::vector<tag_application> &user_tags);

    /** \brief Render a cwidget column list without columns.
     *
     *  Static columns are rendered literally; variant columns are
     *  rendered with their natural width.
     *
     *  \param columns     the column definitions giving the output format
     *  \param columnizer  a thunk providing values for variant columns
     *  \param p           any side parameters (e.g., search results) for
     *                     the columnizer to use
     */
    std::wstring de_columnize(const cwidget::config::column_definition_list &columns,
			      cwidget::config::column_generator &columnizer,
			      cwidget::config::column_parameters &p);

    /** \brief Compare pairs according to their first element. */
    class lessthan_1st
    {
    public:
      lessthan_1st() { }

      template<typename T1, typename T2, typename U1, typename U2>
      bool operator()(const std::pair<T1, T2> &p1,
                      const std::pair<U1, U2> &p2) const
      {
        return p1.first < p2.first;
      }
    };

    /** \brief Compare pairs according to their first element. */
    class equalto_1st
    {
    public:
      equalto_1st() { }

      template<typename T1, typename T2, typename U1, typename U2>
      bool operator()(const std::pair<T1, T2> &p1,
                      const std::pair<U1, U2> &p2) const
      {
        return p1.first == p2.first;
      }
    };

    /** Compare pairs whose first elements are package iterators,
     *  according to a sorting policy.
     *
     *  When a version is needed to do a comparison, I arbitrarily decided
     *  to use the candidate version.
     */
    class package_results_lt
    {
      pkg_sortpolicy *s;

    public:
      package_results_lt(pkg_sortpolicy *_s):s(_s) {}

      template<typename T, typename U>
      bool operator()(const std::pair<pkgCache::PkgIterator, T> &a,
                      const std::pair<pkgCache::PkgIterator, U> &b)
      {
        pkgCache::VerIterator av = (*apt_cache_file)[a.first].CandidateVerIter(*apt_cache_file);
        pkgCache::VerIterator bv = (*apt_cache_file)[b.first].CandidateVerIter(*apt_cache_file);

        return s->compare(a.first, av, b.first, bv)<0;
      }
    };

    /** \brief Compare pairs whose first elements are package
     *  iterators for equality.
     */
    class package_results_eq
    {
      pkg_sortpolicy *s;
    public:
      package_results_eq(pkg_sortpolicy *_s):s(_s) {}

      template<typename T, typename U>
      bool operator()(const std::pair<pkgCache::PkgIterator, T> &a,
                      const std::pair<pkgCache::PkgIterator, U> &b)
      {
        pkgCache::VerIterator av =
          (*apt_cache_file)[a.first].CandidateVerIter(*apt_cache_file);
        pkgCache::VerIterator bv =
          (*apt_cache_file)[b.first].CandidateVerIter(*apt_cache_file);

        return s->compare(a.first, av, b.first, bv) == 0;
      }
    };

    /** \brief 3-way compare of version match results (pairs whose
     *  first element is a version).
     */
    class compare_version_results3
    {
      pkg_sortpolicy *sortpolicy;

    public:
      compare_version_results3(pkg_sortpolicy *_sortpolicy)
        : sortpolicy(_sortpolicy)
      {
      }

      template<typename T, typename U>
      int operator()(const std::pair<pkgCache::VerIterator, T> &a,
                     const std::pair<pkgCache::VerIterator, U> &b)
      {
        const pkgCache::VerIterator &av = a.first;
        const pkgCache::VerIterator &bv = b.first;

        return sortpolicy->compare(av.ParentPkg(), av, bv.ParentPkg(), bv);
      }
    };

    /** \brief less-than over version match results. */
    class version_results_lt
    {
      compare_version_results3 comparer;

    public:
      version_results_lt(pkg_sortpolicy *sortpolicy)
        : comparer(sortpolicy)
      {
      }

      template<typename T, typename U>
      bool operator()(const std::pair<pkgCache::VerIterator, T> &a,
                      const std::pair<pkgCache::VerIterator, U> &b)
      {
        return comparer(a, b) < 0;
      }
    };

    /** \brief equality over version match results.
     *
     *  Returns \b true if two match results are equivalent under the
     *  given sort order.
     */
    class version_results_eq
    {
      compare_version_results3 comparer;

    public:
      version_results_eq(pkg_sortpolicy *sortpolicy)
        : comparer(sortpolicy)
      {
      }

      template<typename T, typename U>
      bool operator()(const std::pair<pkgCache::VerIterator, T> &a,
                      const std::pair<pkgCache::VerIterator, U> &b)
      {
        return comparer(a, b) == 0;
      }
    };


    /** \brief Custom hash on packages. */
    class hash_pkgiterator
    {
    public:
      std::size_t operator()(const pkgCache::PkgIterator &pkg) const
      {
        return pkg->ID;
      }
    };

    /// \brief A helper object to generate columns from the result of
    /// a search.
    ///
    /// \todo search_result_parameters currently acts as an empty
    /// list; it should parse the match result and return the groups
    /// it contains.
    class search_result_column_parameters : public cwidget::config::column_parameters
    {
      cwidget::util::ref_ptr<aptitude::matching::structural_match> r;

    public:
      search_result_column_parameters(const cwidget::util::ref_ptr<aptitude::matching::structural_match> &_r)
        : r(_r)
      {
      }

      int param_count()
      {
        return 0;
      }

      std::wstring get_param(int n)
      {
        return std::wstring();
      }
    };

    bool pkgset_from_group(pkgset * const pkgset, string name,
                           GlobalError::MsgType error_type = GlobalError::ERROR);

    bool pkgset_from_task(pkgset * const pkgset, string pattern,
                          GlobalError::MsgType error_type = GlobalError::ERROR);

    /** \brief Fill a pkgset using the given regex to match package
     *  names.
     */
    bool pkgset_from_regex(pkgset * const pkgset, string pattern,
                           GlobalError::MsgType error_type = GlobalError::ERROR);

    /** \brief Fill a pkgset using the given matching pattern.
     *
     *  This does not try any string as a search pattern, only those
     *  which contain explicit search terms or regex characters.
     */
    bool pkgset_from_pattern(pkgset * const pkgset, string pattern,
                             GlobalError::MsgType error_type = GlobalError::ERROR);

    /** \brief Fill a pkgset using the given string.  If the
     *  string names exactly a package then insert that package,
     *  otherwise, if the string is a search pattern, add all matching
     *  packages.
     *
     *  Based on cacheset.cc(PackageContainerInterface::FromString).
     *  TODO: Should be replaced with cacheset functions once we have
     *  become more compatible with them.
     */
    bool pkgset_from_string(pkgset * const pkgset, string str,
                            GlobalError::MsgType error_type = GlobalError::ERROR,
                            GlobalError::MsgType pattern_error_type = GlobalError::NOTICE);
  }
}

#endif // CMDLINE_UTIL_H
