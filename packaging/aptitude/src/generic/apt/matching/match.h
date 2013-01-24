// match.h    -*-c++-*-
//
//   Copyright (C) 2008-2010 Daniel Burrows
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

#ifndef MATCH_H
#define MATCH_H

#include <cwidget/generic/util/ref_ptr.h>

#include <generic/util/refcounted_base.h>

#include <apt-pkg/pkgcache.h>

#include <sigc++/slot.h>

#include <vector>

#include <regex.h>
#include <sys/types.h>

#include "pattern.h"

class aptitudeDepCache;
class pkgRecords;

namespace aptitude
{
  namespace util
  {
    class progress_info;
  }

  namespace matching
  {
    class structural_match;
    typedef std::vector< std::pair< pkgCache::PkgIterator,
                                    cwidget::util::ref_ptr<structural_match> > > pkg_results_list;
    typedef std::vector< std::pair< pkgCache::VerIterator,
                                    cwidget::util::ref_ptr<structural_match> > > ver_results_list;

    /** \brief Represents the atomic values that are selected by search
     *  patterns.
     *
     *  Each matchable object is either a virtual package, or a
     *  particular version of a package.
     */
    class matchable
    {
      // The package and version; if the version is invalid, this
      // represents a virtual package.
      pkgCache::Package *pkg;
      pkgCache::Version *ver;

    public:
      matchable()
      {
      }

      matchable(pkgCache::Package *_pkg,
		pkgCache::Version *_ver)
	: pkg(_pkg), ver(_ver)
      {
      }

      matchable(pkgCache::PkgIterator _pkg,
		pkgCache::VerIterator _ver)
	: pkg(_pkg), ver(_ver)
      {
      }

      matchable(pkgCache::PkgIterator _pkg)
	: pkg(_pkg), ver(0)
      {
      }

      pkgCache::Package *get_pkg() const { return pkg; }
      pkgCache::Version *get_ver() const { return ver; }

      pkgCache::PkgIterator get_package_iterator(pkgCache &owner) const
      {
	return pkgCache::PkgIterator(owner, pkg);
      }

      bool get_has_version() const { return ver != NULL; }

      pkgCache::VerIterator get_version_iterator(pkgCache &owner) const
      {
	eassert(ver != NULL);

	return pkgCache::VerIterator(owner, ver);
      }

      /** \brief Compare two matchables.
       *
       *  Matchables are ordered first by their package, then by their
       *  version (if any).  Version ordering is arbitrary but
       *  well-defined.
       */
      bool operator<(const matchable &other) const
      {
	if(pkg < other.pkg)
	  return true;
	else if(pkg > other.pkg)
	  return false;
	else if(ver < other.ver)
	  return true;
	else if(ver > other.ver)
	  return false;
	else
	  return false;
      }
    };

    /** \brief Represents information about how a package was matched.
     *
     *  This only represents the information that can't be derived
     *  from the type of matcher.  For instance, it might contain the
     *  string region that was matched or the dependency that was
     *  followed, but it won't contain package status information,
     *  because that is available from the pkgDepCache.
     *
     *  This means that if the user of a match needs information that
     *  will last across depCache changes, it must be copied.
     */
    class match : public util::refcounted_base_threadsafe
    {
    public:
      class dependency_match_info
      {
	pkgCache::DepIterator dep;
	
      };

      /** \brief The type of a match. */
      enum type
	{
	  /** \brief A match that has no associated information.
	   *
	   *  This would represent, for instance, a match
	   *  corresponding to a ?action term.
	   */
	  atomic,

	  /** \brief A match using a regular expression.
	   *
	   *  The attached information consists of a list of ranges
	   *  within the input string that were matched.
	   */
	  regexp,

	  /** \brief A match with a sub-match and no other
	   *  information.
	   *
	   *  e.g., ?bind.
	   *
	   *  The attached information is the structural matcher
	   *  representing how the sub-expression matched.
	   */
	  with_sub_match,

	  /** \brief A match that was made via a dependency.
	   *
	   *  The attached information is a structural matcher
	   *  representing how the sub-expression matched, along with
	   *  the dependency that was followed.
	   */
	  dependency,

	  /** \brief A match that was made via a Provides.
	   *
	   *  The attached information is a structural matcher
	   *  representing how the sub-expression matched, along with
	   *  the Provides that was followed.
	   */
	  provides
	};

      /** \brief Represents a match of a regular expression against a string.
       *
       *  aptitude uses the POSIX regular expression engine, which
       *  uses narrow character strings.  Match starting and ending
       *  locations are *byte offsets* from the start of the string.
       */
      class regexp_match
      {
	// Starting and ending bytes of the match.
	int start, end;

      public:
	regexp_match(int _start, int _end)
	  : start(_start), end(_end)
	{
	}

	regexp_match(const regmatch_t &match)
	  : start(match.rm_so), end(match.rm_eo)
	{
	}

	int get_start() const { return start; }
	int get_end() const { return end; }
      };

    private:
      type tp;

      // The pattern that produced this particular match.
      cwidget::util::ref_ptr<pattern> p;

      // The dependency that was followed, if any.
      pkgCache::DepIterator dep;

      // The Provides that was followed, if any.
      pkgCache::PrvIterator prv;

      // The string that the regexp matched, if any.
      std::string match_string;

      // The string regions matched by the regular expression, if any.
      std::vector<regexp_match> regexp_matches;

      // The sub-match, if any.
      cwidget::util::ref_ptr<structural_match> sub_match;

      // NB: maybe I should avoid 
      template<typename RegexpMatchIter>
      match(type _tp,
	    const cwidget::util::ref_ptr<pattern> &_p,
	    const cwidget::util::ref_ptr<structural_match> &_sub_match,
	    const pkgCache::DepIterator &_dep,
	    const pkgCache::PrvIterator &_prv,
	    const std::string &_match_string,
	    RegexpMatchIter regexp_matches_begin,
	    RegexpMatchIter regexp_matches_end)
	: tp(_tp), p(_p),
	  dep(_dep), prv(_prv),
	  match_string(_match_string),
	  regexp_matches(regexp_matches_begin,
			 regexp_matches_end),
	  sub_match(_sub_match)
      {
      }

    public:
      /** \brief Create a new atomic match.
       *
       *  \param p  The pattern that produced this match.
       */
      static cwidget::util::ref_ptr<match> make_atomic(const cwidget::util::ref_ptr<pattern> &p)
      {
	return new match(atomic, p,
			 cwidget::util::ref_ptr<structural_match>(),
			 pkgCache::DepIterator(),
			 pkgCache::PrvIterator(),
			 std::string(),
			 (regexp_match *)0, (regexp_match *)0);
      }

      static cwidget::util::ref_ptr<match> make_atomic(const cwidget::util::ref_ptr<pattern> &p,
						       const std::string &match_string)
      {
	return new match(atomic, p,
			 cwidget::util::ref_ptr<structural_match>(),
			 pkgCache::DepIterator(),
			 pkgCache::PrvIterator(),
			 match_string,
			 (regexp_match *)0, (regexp_match *)0);
      }

      /** \brief Create a new regular expression match.
       *
       *  \param p  The pattern that produced this match.
       *  \param regexp_matches_begin  The beginning of the
       *                               range of regular
       *                               expression matches.
       *  \param regexp_matches_end    The end of the range
       *                               of regular expression
       *                               matches.
       */
      template<typename RegexpMatchIter>
      static cwidget::util::ref_ptr<match> make_regexp(const cwidget::util::ref_ptr<pattern> &p,
						       const std::string &match_string,
						       RegexpMatchIter regexp_matches_begin,
						       RegexpMatchIter regexp_matches_end)
      {
	return new match(regexp, p,
			 cwidget::util::ref_ptr<structural_match>(),
			 pkgCache::DepIterator(),
			 pkgCache::PrvIterator(),
			 match_string,
			 regexp_matches_begin, regexp_matches_end);
      }

      /** \brief Create a new match with a sub-match.
       *
       *  \param p   The pattern that producd this match.
       *  \param m   The sub-match.
       */
      static cwidget::util::ref_ptr<match> make_with_sub_match(const cwidget::util::ref_ptr<pattern> &p,
							  const cwidget::util::ref_ptr<structural_match> &m)
      {
	return new match(with_sub_match, p, m,
			 pkgCache::DepIterator(),
			 pkgCache::PrvIterator(),
			 std::string(),
			 (regexp_match *)0, (regexp_match *)0);
      }

      /** \brief Create a new match through a dependency.
       *
       *  \param p   The pattern that produced this match.
       *  \param m   The sub-match.
       *  \param dep The dependency that was followed.
       */
      static cwidget::util::ref_ptr<match> make_dependency(const cwidget::util::ref_ptr<pattern> &p,
							   const cwidget::util::ref_ptr<structural_match> &m,
							   const pkgCache::DepIterator &dep)
      {
	return new match(dependency, p,
			 m,
			 dep,
			 pkgCache::PrvIterator(),
			 std::string(),
			 (regexp_match *)0, (regexp_match *)0);
      }


      /** \brief Create a new match through a Provides.
       *
       *  \param p   The pattern that produced this match.
       *  \param m   The sub-match.
       *  \param prv The Provides that was followed.
       */
      static cwidget::util::ref_ptr<match> make_provides(const cwidget::util::ref_ptr<pattern> &p,
							 const cwidget::util::ref_ptr<structural_match> &m,
							 const pkgCache::PrvIterator &prv)
      {
	return new match(dependency, p,
			 m,
			 pkgCache::DepIterator(),
			 prv,
			 std::string(),
			 (regexp_match *)0, (regexp_match *)0);
      }


      /** \brief Retrieve the type of this match. */
      type get_type() const { return tp; }

      /** \brief Retrieve the pattern that produced
       *  this match.
       */
      const cwidget::util::ref_ptr<pattern> &get_pattern() const
      {
	return p;
      }

      /** \brief Retrieve the number of string groups in this match.
       */
      unsigned int get_num_groups() const;

      /** \brief Retrieve a single string group.
       *
       *  \param group_num The group number to retrieve; must be
       *  positive and less than this->get_num_groups().
       */
      std::string get_group(unsigned int group_num) const;

      /** \brief For dependency and provides matches, return the
       *  object describing the sub-match.
       */
      const cwidget::util::ref_ptr<structural_match> &get_sub_matches() const
      {
	eassert(tp == with_sub_match || tp == dependency || tp == provides);

	return sub_match;
      }

      /** \brief For regular expression matches, retrieve the list of
       *  locations in the string that were matched.
       */
      const std::vector<regexp_match> &get_regexp_matches() const
      {
	eassert(tp == regexp);

	return regexp_matches;
      }

      /** \brief For dependency matches, retrieve the dependency that
       *  was followed.
       */
      const pkgCache::DepIterator &get_dep() const
      {
	eassert(tp == dependency);

	return dep;
      }

      /** \brief For provides matches, retrieve the Provides
       *  that was followed.
       */
      const pkgCache::PrvIterator &get_prv() const
      {
	eassert(tp == provides);

	return prv;
      }
    };

    /** \brief A match for a node that is "atomic" at the level of
     *  matches against a particular pool of possibilities.
     *
     *  Representing matches is tricky because we have to handle
     *  version information in a sensible way.  Each pattern could
     *  match multiple versions, and this information has to be
     *  represented for the user of the match to make sense of it
     *  since, e.g., every version can have a different Maintainer
     *  field.
     *
     *  We can divide term types into two categories: some terms
     *  return true by testing a condition against the packages in the
     *  incoming version pool, while other terms simply delegate to
     *  their sub-parts.  We call the second category \e structural
     *  terms.  Note that terms such as ?depends, which \e ignore the
     *  incoming pool and begin a new match against a new pool, are in
     *  the second category, not the first category.  The structural
     *  terms are:
     *
     *   - ?and
     *   - ?or
     *   - ?not
     *   - ?any
     *   - ?all
     *   - ?for
     *   - ?widen
     *   - ?narrow
     *
     *  To represent a match, we use a heterogeneous tree.  The inner
     *  nodes represent the structural matchers that are encountered
     *  at the root of the tree, and the leaves represent the
     *  structually-atomic matchers (again, these may be matchers with
     *  sub-expressions, but their sub-expression represents a
     *  completely new search).
     *
     *  Structural matches either represent an internal node (i.e., a
     *  match using one of the above patterns) or an atomic node (a
     *  match to one of the other patterns).
     */
    class structural_match : public util::refcounted_base_threadsafe
    {
    public:
      /** \brief The type of this structural match node. */
      enum type
	{
	  /** \brief Am internal match node; it has a number
	   *  of sub-matches (get_sub_matches()).
	   */
	  internal,
	  /** \brief An atomic match node; it has an attached list of
	   *  pairs containing a matchable object and a match object
	   *  describing how it was matched.
	   */
	  atomic
	};

    private:
      type tp;

      // The corresponding pattern node.
      cwidget::util::ref_ptr<pattern> p;

      // The list of structural sub-matches, if any.  Each corresponds
      // to a different sub-pattern (e.g., the list of immediate
      // children of a ?and node).
      std::vector<cwidget::util::ref_ptr<structural_match> > sub_matches;

      // The atomic match values, if any.  Each corresponds to how a
      // single matchable object satisfied the search.
      std::vector<std::pair<matchable, cwidget::util::ref_ptr<match> > > atomic_matches;

      unsigned int num_groups;

      template<typename StructuralMatchIter,
	       typename AtomicMatchIter>
      structural_match(type _tp,
		       const cwidget::util::ref_ptr<pattern> &_p,
		       StructuralMatchIter _sub_matches_begin,
		       StructuralMatchIter _sub_matches_end,
		       AtomicMatchIter _atomic_matches_begin,
		       AtomicMatchIter _atomic_matches_end)
	: tp(_tp), p(_p),
	  sub_matches(_sub_matches_begin, _sub_matches_end),
	  atomic_matches(_atomic_matches_begin, _atomic_matches_end),
	  num_groups(0)
      {
	for(std::vector<cwidget::util::ref_ptr<structural_match> >::const_iterator
	      it = sub_matches.begin(); it != sub_matches.end(); ++it)
	  num_groups += (*it)->get_num_groups();

	for(std::vector<std::pair<matchable, cwidget::util::ref_ptr<match> > >::const_iterator
	      it = atomic_matches.begin(); it != atomic_matches.end(); ++it)
	  num_groups += it->second->get_num_groups();
      }

    public:
      /** \brief Create a new inner structural match node.
       *
       *  \typeparam StructuralMatchIter An input iterator returning
       *                                 values of type
       *                                 cwidget::util::ref_ptr<structural_match>.
       *
       *  \param pattern   The corresponding match pattern.
       *
       *  \param sub_matches_begin   The beginning of the range
       *                             of sub-nodes.
       *
       *  \param sub_matches_end     The end of the range of
       *                             sub-nodes.
       */
      template<typename StructuralMatchIter>
      static cwidget::util::ref_ptr<structural_match> make_branch(const cwidget::util::ref_ptr<pattern> &pattern,
						   StructuralMatchIter sub_matches_begin,
						   StructuralMatchIter sub_matches_end)
      {
	return new structural_match(internal,
				    pattern,
				    sub_matches_begin,
				    sub_matches_end,
				    (std::pair<matchable, cwidget::util::ref_ptr<match> > *)0,
				    (std::pair<matchable, cwidget::util::ref_ptr<match> > *)0);
      }

      /** \brief Create a new leaf structural match node.
       *
       *  \typeparam AtomicMatchIter An input iterator returning
       *                             values of type
       *                             std::pair<matchable, cwidget::util::ref_ptr<match> >.
       *
       *  \param pattern   The corresponding match pattern.
       *
       *  \param atomic_matches_begin   The beginning of the
       *                                range of atomic matches.
       *  \param atomic_matches_end     The end of the range
       *                                of atomic matches.
       */
      template<typename AtomicMatchIter>
      static cwidget::util::ref_ptr<structural_match> make_leaf(const cwidget::util::ref_ptr<pattern> &pattern,
								AtomicMatchIter atomic_matches_begin,
								AtomicMatchIter atomic_matches_end)
      {
	return new structural_match(atomic,
				    pattern,
				    (cwidget::util::ref_ptr<structural_match> *)0,
				    (cwidget::util::ref_ptr<structural_match> *)0,
				    atomic_matches_begin,
				    atomic_matches_end);
      }

      /** \brief Get the type of this node. */
      type get_type() const { return tp; }

      /** \brief Get the number of string matches in this node. */
      unsigned int get_num_groups() const { return num_groups; }

      /** \brief Retrieve a single group from this node.
       *
       *  \param group_num The group number; must be positive and less
       *  than this->get_num_groups().
       */
      std::string get_group(unsigned int group_num) const;

      /** \brief Get the corresponding pattern node. */
      const cwidget::util::ref_ptr<pattern> &get_pattern() const
      {
	return p;
      }

      /** \brief Get the list of sub-matches, if this is an internal
       *  match.
       */
      const std::vector<cwidget::util::ref_ptr<structural_match> > &get_sub_matches() const
      {
	eassert(tp == internal);

	return sub_matches;
      }

      /** \brief Get the atomic matches contained in this node, if
       *  this is an atomic match node.
       */
      const std::vector<std::pair<matchable, cwidget::util::ref_ptr<match> > > &get_atomic_matches() const
      {
	eassert(tp == atomic);

	return atomic_matches;
      }
    };

    /** \brief Used to attach information to individual patterns in
     *  the course of a search.
     *
     *  This is used to store information that should be computed
     *  only once during a search.
     */
    class search_cache : public util::refcounted_base_threadsafe
    {
      // The implementation details are hidden in
      // search_cache::implementation.  You can only create a
      // search_cache through create(), which really creates
      // search_cache::implementation.
      //
      // I assume implicitly in various places that all search_cache
      // pointers are search_cache_real pointers; this is enforced by
      // making sure that nothing else can construct them.

    public:
      class implementation;
      friend class implementation;

    private:
      search_cache();
      search_cache(const search_cache &other);

    public:
      /** \brief Construct a new search cache. */
      static cwidget::util::ref_ptr<search_cache> create();
    };

    /** \brief Test a version of a package against a pattern.
     *
     *  \param p   The pattern to execute.
     *  \param pkg The package to compare.
     *  \param ver The version of pkg to compare, or an end iterator to match the
     *             package itself.
     *  \param search_info  Where to store "side information"
     *                      associated with this search.
     *  \param cache   The cache in which to search.
     *  \param records The package records with which to perform the match.
     *  \param debug   If \b true, information about the search process
     *                 will be printed to standard output.
     *
     *  \return A match object describing the match, or \b NULL if the
     *  package does not match.
     */
    cwidget::util::ref_ptr<structural_match>
    get_match(const cwidget::util::ref_ptr<pattern> &p,
	      const pkgCache::PkgIterator &pkg,
	      const pkgCache::VerIterator &ver,
	      const cwidget::util::ref_ptr<search_cache> &search_info,
	      aptitudeDepCache &cache,
	      pkgRecords &records,
	      bool debug = false);

    /** \brief Test a package against a pattern.
     *
     *  This tests the package as a package, not as a version.
     *
     *  \param p   The pattern to execute.
     *  \param pkg The package to compare.
     *  \param search_info  Where to store "side information"
     *                      associated with this search.
     *  \param cache   The package cache in which to search.
     *  \param records The package records with which to perform the match.
     *  \param debug   If \b true, information about the search process
     *                 will be printed to standard output.
     *
     *  \return A match object describing the match, or \b NULL if the
     *  package does not match.
     */
    cwidget::util::ref_ptr<structural_match>
    get_match(const cwidget::util::ref_ptr<pattern> &p,
	      const pkgCache::PkgIterator &pkg,
	      const cwidget::util::ref_ptr<search_cache> &search_info,
	      aptitudeDepCache &cache,
	      pkgRecords &records,
	      bool debug = false);

    /** \brief Retrieve all the packages matching the given pattern.
     *
     *  This may use Xapian or other indices to accelerate the search
     *  at the cost of exactness, depending on the query that was
     *  passed in.
     *
     *  \param p            The pattern to match against.
     *  \param search_info  Where to store "side information"
     *                      associated with this search.
     *  \param cache        The package cache in which to search.
     *  \param records      The package records in which to perform the match.
     *  \param debug        If \b true, information about the search
     *                      process will be printed to standard output.
     *  \param progress_slot A slot used to report the progress of the search.
     */
    void search(const cwidget::util::ref_ptr<pattern> &p,
		const cwidget::util::ref_ptr<search_cache> &search_info,
		pkg_results_list &matches,
		aptitudeDepCache &cache,
		pkgRecords &records,
		bool debug = false,
                const sigc::slot<void, aptitude::util::progress_info> &progress_slot
                  = sigc::slot<void, aptitude::util::progress_info>());

    /** \brief Retrieve all the package versions matching the given pattern.
     *
     *  This may use Xapian or other indices to accelerate the search
     *  at the cost of exactness, depending on the query that was
     *  passed in.
     *
     *  \param p            The pattern to match against.
     *  \param search_info  Where to store "side information"
     *                      associated with this search.
     *  \param cache        The package cache in which to search.
     *  \param records      The package records in which to perform the match.
     *  \param debug        If \b true, information about the search
     *                      process will be printed to standard output.
     *  \param progress_slot A slot used to report the progress of the search.
     */
    void search_versions(const cwidget::util::ref_ptr<pattern> &p,
                         const cwidget::util::ref_ptr<search_cache> &search_info,
                         ver_results_list &matches,
                         aptitudeDepCache &cache,
                         pkgRecords &records,
                         bool debug = false,
                         const sigc::slot<void, aptitude::util::progress_info> &progress_slot =
                           sigc::slot<void, aptitude::util::progress_info>());
  }
}

#endif
