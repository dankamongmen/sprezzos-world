// match.cc
//
//   Copyright (C) 2008-2011 Daniel Burrows
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

#include "match.h"

#include <aptitude.h>

#include <generic/apt/apt.h>
#include <generic/apt/tags.h>
#include <generic/apt/tasks.h>
#include <generic/util/progress_info.h>
#include <generic/util/util.h>

#include <apt-pkg/error.h>
#include <apt-pkg/pkgrecords.h>
#include <apt-pkg/pkgsystem.h>
#include <apt-pkg/version.h>
#include <apt-pkg/cachefilter.h>

#include <cwidget/generic/util/transcode.h>

#include <xapian.h>

#include <algorithm>

#include <boost/scoped_ptr.hpp>
#include <boost/unordered_map.hpp>

#include "serialize.h"

using aptitude::util::progress_info;
using boost::unordered_map;
using cwidget::util::transcode;
using cwidget::util::ref_ptr;

namespace aptitude
{
  namespace matching
  {
    namespace
    {
      typedef Xapian::Database debtags_db;

      const Xapian::docid get_docid_by_name(const debtags_db &db,
                                            const char *name)
      {
        std::string term = "XP";
        term += name;

        Xapian::PostingIterator i = db.postlist_begin(term);
        if(i == db.postlist_end(term))
          return Xapian::docid();
        else
          return *i;
      }

      const Xapian::Database &get_xapian_db(const debtags_db &db)
      {
        return db;
      }

      /** \brief Evaluate any regular expression-based pattern.
       *
       *  \param p      The pattern to evaluate.
       *  \param inf    The regular expression to apply.
       *  \param s      The string to test the regular expression against.
       *  \param invert \b true if this match is inverted (i.e., in a NOT
       *                context).  For inverted matches, we only return
       *                a match if the regex does \e not match, and the
       *                match region is the whole string.
       *
       *  \return     A match object corresponding to the regexp,
       *              or \b NULL if the match failed.
       */
      ref_ptr<match> evaluate_regexp(const ref_ptr<pattern> &p,
				     const pattern::regex_info &inf,
				     const char *s,
				     bool debug)
      {
	// Unfortunately, regexec() seems to require a hard limit to
	// the number of matches that can be returned. :-(
	regmatch_t matches[30];
	const int num_matches = sizeof(matches) / sizeof(regmatch_t);

	bool matched = inf.get_regex_group()->exec(s,
						   matches,
						   num_matches);

	if(matched)
	  {
	    int matches_found = 0;
	    while(matches_found < 30 && matches[matches_found].rm_so >= 0)
	      ++matches_found;

	    return match::make_regexp(p, s, matches, matches + matches_found);
	  }
	else
	  return NULL;
      }

      // Information on the Xapian compilation of a top-level term.
      // Note that for correct results in the presence of variable
      // binding constructs, we rely on the fact that those constructs
      // never interact with the Xapian-level search (so it's safe to
      // search once and save the results).
      class xapian_info
      {
      private:
	// Read out the hits in the given MSet into the matched_terms
	// list.
	void record_hits(Xapian::Enquire enq,
			 Xapian::MSet mset);

	// If true, the term was compiled to a Xapian query whose
	// results are stored in matched_packages; only packages in
	// that set could possibly match the pattern.  If false, we'll
	// have to examine all possible packages to find a match.
	bool matched_packages_valid;

	// The actual match set.
	Xapian::MSet xapian_match;

	// The match set resolved to package iterators and sorted for
	// fast access.  (using a flat list instead of std::set
	// because this will be hit a lot during a search and is
	// potentially large)
	std::vector<Xapian::docid> matched_packages;

      public:
	/** \brief Return \b true if this pattern can be used to
	 *  constrain the set of packages to search.
	 */
	bool get_matched_packages_valid() const
	{
	  return matched_packages_valid;
	}

	const Xapian::MSet get_xapian_match() const
	{
	  return xapian_match;
	}

	/** \brief Return the set of packages matched by the top-level
	 *  search (makes no sense if get_matched_packages_valid() is
	 *  false).
	 *
	 *  \todo Should really use a vector that projects through a
	 *  "look up the docid" function.
	 */
	const std::vector<Xapian::docid> &get_matched_packages() const
	{
	  return matched_packages;
	}

	/** \brief Return \b true if the given package might be
	 *  matched by this pattern.
	 */
	bool maybe_contains_package(const pkgCache::PkgIterator &pkg,
				    const boost::scoped_ptr<debtags_db> &db) const
	{
	  if(!matched_packages_valid || !db)
	    return true;
	  else
	    return std::binary_search(matched_packages.begin(),
				      matched_packages.end(),
				      get_docid_by_name(*db, pkg.Name()));
	}

	xapian_info()
	  : matched_packages_valid(false)
	{
	}

	/** \brief Compile a Xapian query from the given
	 *  pattern and execute it.
	 */
	void setup(const Xapian::Database &db,
		   const ref_ptr<pattern> &pattern,
		   bool debug);
      };
    }

    std::string structural_match::get_group(unsigned int group_num) const
    {
      if(group_num >= num_groups)
	throw MatchingException("Can't retrieve match information: the group number is out of bounds.");

      for(std::vector<cwidget::util::ref_ptr<structural_match> >::const_iterator
	    it = sub_matches.begin(); it != sub_matches.end(); ++it)
	{
	  if(group_num < (*it)->get_num_groups())
	    return (*it)->get_group(group_num);
	  else
	    group_num -= (*it)->get_num_groups();
	}

      for(std::vector<std::pair<matchable, cwidget::util::ref_ptr<match> > >::const_iterator
	    it = atomic_matches.begin(); it != atomic_matches.end(); ++it)
	{
	  if(group_num < it->second->get_num_groups())
	    return it->second->get_group(group_num);
	  else
	    group_num -= it->second->get_num_groups();
	}

      throw MatchingException("Internal error: inconsistent group count.");
    }

    unsigned int match::get_num_groups() const
    {
      switch(tp)
	{
	case atomic:
	  // TODO: do something special based on the type, like we
	  // used to before the recent changes.
	  return match_string.empty() ? 0 : 1;

	case regexp:
	  return regexp_matches.size();

	case with_sub_match:
	  return sub_match->get_num_groups();

	case dependency:
	  // TODO: represent the dependency here somehow.
	  return 0;

	case provides:
	  // TODO: represent the provides here somehow.
	  return 0;

	default:
	  throw MatchingException("Internal error: bad match type.");
	}
    }

    std::string match::get_group(unsigned int group_num) const
    {
      switch(tp)
	{
	case atomic:
	  if(group_num >= (match_string.empty() ? 0 : 1))
	    throw MatchingException("Can't retrieve match information: the group number is out of bounds.");
	  return std::string(match_string);

	case dependency:
	case provides:
	  throw MatchingException("Can't retrieve match information: the group number is out of bounds.");

	case regexp:
	  if(group_num >= regexp_matches.size())
	    throw MatchingException("Can't retrieve match information: the group number is out of bounds.");
	  else
	    {
	      const regexp_match &m(regexp_matches[group_num]);
	      const int start(m.get_start());
	      const int end(m.get_end());
	      return std::string(match_string, start, end - start);
	    }

	case with_sub_match:
	  return sub_match->get_group(group_num);

	default:
	  throw MatchingException("Internal error: bad match type.");
	}
    }

    // We could try a fancy scheme where arbitrary values are attached
    // to each pattern and downcast using dynamic_cast, but I opted
    // for just explicitly listing all the possible caches in one
    // place.  This fits better with the architecture of the match
    // language and means that all the caching information is
    // collected in one place.
    class search_cache::implementation : public search_cache
    {
      typedef std::map<std::pair<ref_ptr<pattern>, aptitudeDepCache::user_tag>, ref_ptr<match> > user_tag_match_map;

      user_tag_match_map user_tag_matches;

      struct compare_user_tag_match_by_tag
      {
	bool operator()(const std::pair<aptitudeDepCache::user_tag, ref_ptr<match> > &p1,
			const std::pair<aptitudeDepCache::user_tag, ref_ptr<match> > &p2) const
	{
	  return p1.first < p2.first;
	}
      };

      // Either a pointer to the debtags database, or NULL if it
      // couldn't be initialized.
      boost::scoped_ptr<debtags_db> db;

      // Maps "top-level" patterns to their Xapian information (that
      // is, the corresponding query and/or query results).  Term hit
      // lists are stored in matched_terms.  The information for a
      // top-level term is filled in the first time it's encountered.
      std::map<ref_ptr<pattern>, xapian_info> toplevel_xapian_info;

      // Maps each term that has been looked up to a sorted list of
      // the packages it matches.
      std::map<std::string, std::vector<Xapian::docid> > matched_terms;

      // Maps each term that has been looked up as a prefix to a
      // sorted list of the packages it matches.
      std::map<std::string, std::vector<Xapian::docid> > matched_term_prefixes;

      // Stores compiled regular expressions that mimic Xapian's term
      // matching (specifically, they only match at word boundaries).
      // Only used if the Xapian database failed to load.
      unordered_map<std::string, ref_ptr<regex> > term_regexes;

      // Stores compiled regular expressions that mimic Xapian's term
      // matching (specifically, they only match at word boundaries).
      // Only used if the Xapian database failed to load.
      unordered_map<std::string, ref_ptr<regex> > term_prefix_regexes;

      /** \brief Get a regular expression that matches the given
       *  string as a "term".
       *
       *  Memoizes its return value in term_regexes.
       */
      const ref_ptr<regex> &get_term_regex(const std::string &term)
      {
        ref_ptr<regex> &result = term_regexes[term];

        if(!result.valid())
          {
            const std::string term_regex =
              term.empty()
                ? ".*"
                : "(^|[^[:alnum:]])" + backslash_escape_nonalnum(term) + "($|[^[:alnum:]])";

            result = ref_ptr<regex>(new regex(term_regex, REG_ICASE | REG_EXTENDED | REG_NOSUB));
          }

        return result;
      }

      /** \brief Get a regular expression that matches the given
       *  string as a "term prefix".
       *
       *  Memoizes its return value in term_prefix_regexes.
       */
      const ref_ptr<regex> &get_term_prefix_regex(const std::string &term)
      {
        ref_ptr<regex> &result = term_regexes[term];

        if(!result.valid())
          {
            const std::string term_regex =
              term.empty()
                ? ".*"
                : "(^|[^[:alnum:]])" + backslash_escape_nonalnum(term) + ".*";

            result = ref_ptr<regex>(new regex(term_regex, REG_ICASE | REG_EXTENDED | REG_NOSUB));
          }

        return result;
      }

    public:
      implementation()
      {
	try
	  {
            db.reset(new Xapian::Database("/var/lib/apt-xapian-index/index"));
	  }
	catch(...)
	  {
	    db.reset();
	  }
      }

      const boost::scoped_ptr<debtags_db> &get_db() const
      {
	return db;
      }

      // Return a match of the given user tag to the given pattern,
      // which must be a ?user-tag pattern.  If possible, this looks
      // the match up using the internal cache; otherwise, it creates
      // a new cache entry for the given pattern and tag.
      ref_ptr<match> find_user_tag_match(const ref_ptr<pattern> &p,
					 aptitudeDepCache::user_tag tag,
					 const aptitudeDepCache &cache,
					 bool debug)
      {
	std::pair<ref_ptr<pattern>, aptitudeDepCache::user_tag>
	  key(std::make_pair(p, tag));
	user_tag_match_map::iterator cached_match(user_tag_matches.find(key));

	if(cached_match == user_tag_matches.end())
	  {
	    ref_ptr<match> m = evaluate_regexp(p,
					       p->get_user_tag_regex_info(),
					       cache.deref_user_tag(tag).c_str(),
					       debug);

	    user_tag_matches[key] = m;
	    return m;
	  }
	else
	  return cached_match->second;
      }

      bool term_prefix_matches(const matchable &target,
                               const std::string &prefix,
                               aptitudeDepCache &cache,
                               pkgRecords &records,
                               bool debug)
      {
        pkgCache::PkgIterator pkg(target.get_package_iterator(cache));
        if(db.get() != NULL)
          return xapian_term_prefix_matches(pkg, prefix, debug);


        // If we don't have a Xapian database, fake it by checking the
        // package's name and description.
        //
        // Note that this is not a perfectly faithful representation
        // of what Xapian would do: most notably, it omits special
        // prefix handling (e.g., XP for package names).

        const ref_ptr<regex> &term_prefix_regex = get_term_prefix_regex(prefix);

        if(term_prefix_regex->exec(pkg.Name()))
          return true;
        else if(!target.get_has_version())
          return false;
        else
          {
            pkgCache::VerIterator ver = target.get_version_iterator(cache);

            return term_prefix_regex->exec(transcode(get_long_description(ver, &records)));
          }
      }

    private:
      bool xapian_term_prefix_matches(const pkgCache::PkgIterator &pkg,
                                      const std::string &prefix,
                                      bool debug)
      {
	if(debug)
	  std::cout << "Searching for " << prefix << " as a term pefix." << std::endl;

	Xapian::docid pkg_docid(get_docid_by_name(*db, pkg.Name()));
	const Xapian::Database xapian_db(get_xapian_db(*db));


	const std::map<std::string, std::vector<Xapian::docid> >::iterator
	  found = matched_term_prefixes.find(prefix);

	if(found != matched_term_prefixes.end())
	  return std::binary_search(found->second.begin(),
				    found->second.end(),
				    pkg_docid);
	else
	  {
	    if(debug)
	      std::cout << "Retrieving the prefix hits for " << prefix << std::endl;

	    const std::map<std::string, std::vector<Xapian::docid> >::iterator
	      inserted = matched_term_prefixes.insert(std::make_pair(prefix, std::vector<Xapian::docid>())).first;

	    std::vector<Xapian::docid> &matches(inserted->second);

	    Xapian::TermIterator prefix_list_end = xapian_db.allterms_end(prefix);
	    for(Xapian::TermIterator extensionIt = xapian_db.allterms_begin(prefix);
		extensionIt != prefix_list_end; ++extensionIt)
	      {
		// Both this string and its stemmed version are
		// possible continuations, so index both of them.
		const std::string extension(*extensionIt);
		const std::string extensionStemmed(Xapian::Stem("en")(extension));

		const std::string *terms[2] = { &extension, &extensionStemmed };
		const int numTerms = sizeof(terms) / sizeof(terms[0]);

		for(const std::string **termIt = terms;
		    termIt < terms + numTerms; ++termIt)
		  {
		    const std::string &currTerm(**termIt);

		    Xapian::PostingIterator
		      postingsBegin = xapian_db.postlist_begin(currTerm),
		      postingsEnd   = xapian_db.postlist_end(currTerm);

		    for(Xapian::PostingIterator it = postingsBegin;
			it != postingsEnd; ++it)
		      matches.push_back(*it);
		  }
	      }

	    std::sort(matches.begin(), matches.end());
	    matches.erase(std::unique(matches.begin(), matches.end()),
			  matches.end());

	    if(debug)
	      std::cout << "  (" << matches.size() << " hits)" << std::endl;

	    // Now that the hit list is initialized, we can search it
	    // for a match.
	    return std::binary_search(matches.begin(),
				      matches.end(),
				      pkg_docid);
	  }
      }

    public:
      bool term_matches(const matchable &target,
			const std::string &term,
                        aptitudeDepCache &cache,
                        pkgRecords &records,
			bool debug)
      {
        pkgCache::PkgIterator pkg(target.get_package_iterator(cache));
        if(db.get() != NULL)
          return xapian_term_matches(pkg, term, debug);

        // If we don't have a Xapian database, fake it by checking the
        // package's name and description.
        //
        // Note that this is not a perfectly faithful representation
        // of what Xapian would do: most notably, it omits special
        // prefix handling (e.g., XP for package names).

        const ref_ptr<regex> &term_regex = get_term_regex(term);

        if(term_regex->exec(pkg.Name()))
          return true;
        else if(!target.get_has_version())
          return false;
        else
          {
            pkgCache::VerIterator ver = target.get_version_iterator(cache);

            return term_regex->exec(transcode(get_long_description(ver, &records)));
          }
      }

    private:
      bool xapian_term_matches(const pkgCache::PkgIterator &pkg,
                               const std::string &term,
                               bool debug)
      {
	Xapian::docid pkg_docid(get_docid_by_name(*db, pkg.Name()));

	const std::map<std::string, std::vector<Xapian::docid> >::iterator
	  found = matched_terms.find(term);

	if(found != matched_terms.end())
	  return std::binary_search(found->second.begin(),
				    found->second.end(),
				    pkg_docid);
	else
	  {
	    if(debug)
	      std::cout << "Retrieving the hits for " << term << std::endl;

	    std::map<std::string, std::vector<Xapian::docid> >::iterator
	      inserted = matched_terms.insert(std::make_pair(term, std::vector<Xapian::docid>())).first;
	    std::vector<Xapian::docid> &matches(inserted->second);

	    // Index the stemmed version of the term too.
	    std::string termStemmed = Xapian::Stem("en")(term);
	    const std::string *terms[2] = { &term, &termStemmed };
	    const int numTerms = sizeof(terms) / sizeof(terms[0]);

	    for(const std::string **termIt = terms; termIt < terms + numTerms; ++termIt)
	      {
		const std::string &currTerm(**termIt);

		Xapian::Database xapian_db(get_xapian_db(*db));

		Xapian::PostingIterator
		  postingsBegin = xapian_db.postlist_begin(currTerm),
		  postingsEnd   = xapian_db.postlist_end(currTerm);

		for(Xapian::PostingIterator it = postingsBegin;
		    it != postingsEnd; ++it)
		  matches.push_back(*it);
	      }

	    std::sort(matches.begin(), matches.end());
	    matches.erase(std::unique(matches.begin(), matches.end()),
			  matches.end());

	    if(debug)
	      std::cout << "  (" << matches.size() << " hits)" << std::endl;

	    // Now that the hit list is initialized, we can search it
	    // for a match.
	    return std::binary_search(matches.begin(),
				      matches.end(),
				      pkg_docid);
	  }
      }

    public:

      const xapian_info &get_toplevel_xapian_info(const ref_ptr<pattern> &toplevel,
						  bool debug)
      {
	std::map<ref_ptr<pattern>, xapian_info>::iterator found =
	  toplevel_xapian_info.find(toplevel);

	if(found == toplevel_xapian_info.end())
	  {
	    std::map<ref_ptr<pattern>, xapian_info>::iterator inserted =
	      toplevel_xapian_info.insert(std::make_pair(toplevel, xapian_info())).first;

	    xapian_info &rval(inserted->second);
	    if(db.get() != NULL)
	      rval.setup(get_xapian_db(*db), toplevel, debug);

	    return rval;
	  }
	else
	  return found->second;
      }
    };
 
    search_cache::search_cache()
    {
    }

    ref_ptr<search_cache> search_cache::create()
    {
      return new implementation;
    }

    namespace
    {
      Xapian::Query stem_term(const std::string &term)
      {
	return Xapian::Query(Xapian::Query::OP_OR,
			     Xapian::Query(term),
			     Xapian::Stem("en")(term));
      }

      /** \brief Describes how version-by-version matching is carried
       *  out.
       */
      enum structural_eval_mode
	{
	  /** \brief All the versions in the current pool must match. */
	  structural_eval_all,

	  /** \brief Any one of the versions in the current pool can match. */
	  structural_eval_any
	};

      void print_matchable(std::ostream &out,
			   const matchable &matchable,
			   aptitudeDepCache &cache)
      {
	out << matchable.get_package_iterator(cache).Name();
	if(matchable.get_has_version())
	  {
	    out << " "
		<< matchable.get_version_iterator(cache).VerStr();
	  }
      }

      void print_pool(std::ostream &out,
		      const std::vector<matchable> &pool,
		      aptitudeDepCache &cache)
      {
	out << "{";
	for(std::vector<matchable>::const_iterator it =
	      pool.begin(); it != pool.end(); ++it)
	  {
	    if(it != pool.begin())
	      out << ", ";

	    print_matchable(out, *it, cache);
	  }
	out << "}";
      }

      // The evaluation stack holds references to pools (sorted lists
      // of matchables).
      //
      // NB: this is safe only because references to captured
      // variables can't escape (because you can't, e.g., get a handle
      // to a lambda and return it -- the only naming construct forces
      // the variables to be referred to in the dynamic scope of the
      // construct).  If this weren't the case, we'd need to
      // reference-count the values on the stack -- and if lambdas
      // could end up on the stack themselves, we'd have to fall back
      // to full garbage-collection (e.g., mark-and-sweep).
      typedef std::vector<const std::vector<matchable> *> stack;

      void print_stack(std::ostream &out,
		       const stack &stack,
		       aptitudeDepCache &cache)
      {
	out << "[";
	for(stack::const_reverse_iterator it =
	      stack.rbegin(); it != stack.rend(); ++it)
	  {
	    if(it != stack.rbegin())
	      out << " | ";

	    print_pool(out, **it, cache);
	  }
	out << "]";
      }

      /** \brief Like evaluate_structural, but filters the input using
       *  Xapian first.
       */
      ref_ptr<structural_match> evaluate_toplevel(structural_eval_mode mode,
						  const ref_ptr<pattern> &p,
						  stack &the_stack,
						  const ref_ptr<search_cache::implementation> &search_info,
						  const std::vector<matchable> &pool,
						  aptitudeDepCache &cache,
						  pkgRecords &records,
						  bool debug);

      // Match an atomic expression against one matchable.
      ref_ptr<match> evaluate_atomic(const ref_ptr<pattern> &p,
				     const matchable &target,
				     stack &the_stack,
				     const ref_ptr<search_cache::implementation> &search_info,
				     aptitudeDepCache &cache,
				     pkgRecords &records,
				     bool debug)
      {
	if(debug)
	  {
	    std::cout << "Matching " << serialize_pattern(p)
		      << " against the target ";
	    print_matchable(std::cout, target, cache);
	    std::cout << " with stack ";
	    print_stack(std::cout, the_stack, cache);
	    std::cout << std::endl;
	  }

	switch(p->get_type())
	  {
	    // Structural matchers:

	  case pattern::all_versions:
	  case pattern::and_tp:
	  case pattern::any_version:
	  case pattern::for_tp:
	  case pattern::narrow:
	  case pattern::not_tp:
	  case pattern::or_tp:
	  case pattern::widen:
	    throw MatchingException("Internal error: evaluate_atomic() invoked on a non-leaf node.");
	    break;

	    // Atomic matchers:
	  case pattern::archive:
	    if(!target.get_has_version())
	      return NULL;

	    {
	      pkgCache::VerIterator ver(target.get_version_iterator(cache));


	      for(pkgCache::VerFileIterator f = ver.FileList(); !f.end(); ++f)
		{
		  pkgCache::PkgFileIterator cur = f.File();

		  if(!cur.end() && cur.Archive())
		    {
		      ref_ptr<match> m = evaluate_regexp(p,
							 p->get_archive_regex_info(),
							 cur.Archive(),
							 debug);

		      if(m.valid())
			return m;
		    }
		}
	    }

	    return NULL;
	    break;

	  case pattern::action:
	    {
	      bool matches = false;
	      pattern::action_type type = p->get_action_action_type();
	      pkgCache::PkgIterator pkg = target.get_package_iterator(cache);

	      // Install, purge, and remove states all match more than
	      // one find_pkg_state return value.
	      switch(type)
		{
		case pattern::action_install:
		  {
		    pkg_action_state thetype = find_pkg_state(pkg, cache);
		    matches = thetype == pkg_install || thetype == pkg_auto_install;
		  }
		  break;

		case pattern::action_purge:
		  if((cache[pkg].iFlags & pkgDepCache::Purge) == 0)
		    matches = false;
		  else
		    {
		      pkg_action_state thetype = find_pkg_state(pkg, cache);
		      matches = thetype == pkg_remove || thetype == pkg_auto_remove || thetype == pkg_unused_remove;
		    }
		  break;

		case pattern::action_remove:
		  {
		    pkg_action_state thetype = find_pkg_state(pkg, cache);

		    matches = thetype == pkg_remove || thetype == pkg_auto_remove || thetype == pkg_unused_remove;
		  }
		  break;

		case pattern::action_hold:
		  matches = !pkg.CurrentVer().end() && cache.get_ext_state(pkg).selection_state == pkgCache::State::Hold;
		  break;

		  // The rest correspond directly to find_pkg_state() return values.
		case pattern::action_reinstall:
		  matches = find_pkg_state(pkg, cache) == pkg_reinstall;
		  break;

		case pattern::action_upgrade:
		  matches = find_pkg_state(pkg, cache) == pkg_upgrade;
		  break;

		case pattern::action_downgrade:
		  matches = find_pkg_state(pkg, cache) == pkg_downgrade;
		  break;

		case pattern::action_keep:
		  matches = cache[pkg].Keep();
		  break;

		default:
		  throw MatchingException("Internal error: bad action-type flag.");
		}

	      if(matches)
		return match::make_atomic(p);
	      else
		return NULL;
	    }

	    break;

	  case pattern::architecture:
	    if(!target.get_has_version())
	      return NULL;

	    {
	      pkgCache::VerIterator ver(target.get_version_iterator(cache));
              const ref_ptr<arch_specification> spec(p->get_architecture_arch_specification());

              if(spec->matches(ver.Arch()) == true)
                return match::make_atomic(p, ver.Arch());
              else
                return NULL;
	    }
	    break;

	  case pattern::automatic:
	    {
	      pkgCache::PkgIterator pkg(target.get_package_iterator(cache));

	      if(  (!pkg.CurrentVer().end() || cache[pkg].Install()) &&
		   (cache[pkg].Flags & pkgCache::Flag::Auto)  )
		return match::make_atomic(p);
	      else
		return NULL;
	    }
	    break;

	  case pattern::bind:
	    // If this assert fails, something went wrong internally.
	    {
	      const std::size_t variable_index = p->get_bind_variable_index();
	      eassert(variable_index >= 0 && variable_index < the_stack.size());

	      ref_ptr<structural_match>
		sub_match(evaluate_toplevel(structural_eval_any,
					    p->get_bind_pattern(),
					    the_stack,
					    search_info,
					    *the_stack[variable_index],
					    cache,
					    records,
					    debug));

	      if(sub_match.valid())
		return match::make_with_sub_match(p, sub_match);
	      else
		return NULL;
	    }
	    break;

	  case pattern::broken:
	    if(!target.get_has_version())
	      return NULL;
	    else
	      {
		pkgCache::PkgIterator pkg(target.get_package_iterator(cache));
		aptitudeDepCache::StateCache &state = cache[pkg];

		if(state.NowBroken() || state.InstBroken())
		  return match::make_atomic(p);
		else
		  return NULL;
	      }
	    break;

	  case pattern::broken_type:
	    if(!target.get_has_version())
	      return NULL;
	    else
	      {
		pkgCache::VerIterator ver(target.get_version_iterator(cache));
		pkgCache::DepIterator dep(ver.DependsList());

		while(!dep.end())
		  {
		    while(dep->CompareOp & pkgCache::Dep::Or)
		      ++dep;

		    if(dep->Type == p->get_broken_type_depends_type() &&
		       !(cache[dep] & pkgDepCache::DepGInstall))
		      // Oops, it's broken..
		      return match::make_atomic(p);

		    ++dep;
		  }

		return NULL;
	      }
	    break;

	  case pattern::candidate_version:
	    if(!target.get_has_version())
	      return NULL;
	    else
	      {
		pkgCache::PkgIterator pkg(target.get_package_iterator(cache));
		pkgCache::VerIterator ver(target.get_version_iterator(cache));

		if(ver == cache[pkg].CandidateVerIter(cache))
		  return match::make_atomic(p);
		else
		  return NULL;
	      }
	    break;

	  case pattern::config_files:
	    if(target.get_pkg()->CurrentState == pkgCache::State::ConfigFiles)
	      return match::make_atomic(p);
	    else
	      return NULL;
	    break;

	  case pattern::current_version:
	    if(!target.get_has_version())
	      return NULL;
	    else
	      {
		pkgCache::PkgIterator pkg(target.get_package_iterator(cache));

		pkgCache::VerIterator ver(target.get_version_iterator(cache));

		if(ver == pkg.CurrentVer())
		  return match::make_atomic(p);
		else
		  return NULL;
	      }
	    break;

	  case pattern::depends:
	    if(!target.get_has_version())
	      return NULL;
	    else
	      {
		const pkgCache::VerIterator ver(target.get_version_iterator(cache));

		const pkgCache::Dep::DepType depends_type = p->get_depends_depends_type();
		const bool broken = p->get_depends_broken();

		pkgCache::DepIterator dep = ver.DependsList();
		while(!dep.end())
		  {
		    pkgCache::DepIterator or_group_start = dep;

		    if( (depends_type == dep->Type) ||
			(depends_type == pkgCache::Dep::Depends &&
			 dep->Type == pkgCache::Dep::PreDepends))
		      {
			if(broken)
			  {
			    pkgCache::DepIterator d2(cache, &*dep);
			    while(d2->CompareOp & pkgCache::Dep::Or)
			      ++d2;
			    if(cache[d2] & pkgDepCache::DepGInstall)
			      {
				dep = d2;
				++dep;
				continue;
			      }
			  }

			std::vector<matchable> new_pool;

			// See if a versionless match works.
			while(1)
			  {
			    pkgCache::PkgIterator pkg(dep.TargetPkg());
			    if(pkg.VersionList().end())
			      new_pool.push_back(matchable(pkg));
			    else
			      {
				for(pkgCache::VerIterator i=pkg.VersionList(); !i.end(); i++)
				  if(_system->VS->CheckDep(i.VerStr(), dep->CompareOp, dep.TargetVer()))
				    new_pool.push_back(matchable(pkg, i));
			      }

			    if((dep->CompareOp & pkgCache::Dep::Or) == 0)
			      break;
			    else
			      ++dep;
			  }

			if(!new_pool.empty())
			  {
			    std::sort(new_pool.begin(), new_pool.end());

			    ref_ptr<structural_match> m =
			      evaluate_toplevel(structural_eval_any,
						p->get_depends_pattern(),
						the_stack,
						search_info,
						new_pool,
						cache,
						records,
						debug);

			    // Note: the dependency that we return is
			    // just the head of the OR group.
			    if(m.valid())
			      return match::make_dependency(p, m,
							    or_group_start);
			  }
		      }

		    ++dep;
		  }

		return NULL;
	      }
	    break;

	  case pattern::description:
	    if(!target.get_has_version())
	      return NULL;
	    else
	      {
		pkgCache::VerIterator ver(target.get_version_iterator(cache));
		return evaluate_regexp(p,
				       p->get_description_regex_info(),
				       transcode(get_long_description(ver, &records)).c_str(),
				       debug);
	      }
	    break;

	  case pattern::essential:
	    {
	      pkgCache::PkgIterator pkg(target.get_package_iterator(cache));

	      if(  ((pkg->Flags & pkgCache::Flag::Essential) == pkgCache::Flag::Essential) ||
		   ((pkg->Flags & pkgCache::Flag::Important) == pkgCache::Flag::Important)  )
		  return match::make_atomic(p);
	      else
		return NULL;
	    }
	    break;

	  case pattern::equal:
	    {
	      const std::size_t variable_index = p->get_equal_stack_position();
	      eassert(variable_index >= 0 && variable_index < the_stack.size());

	      // Search for the incoming package/version in the pool
	      // referenced by this pattern.
	      const std::vector<matchable> &pool(*the_stack[variable_index]);

	      if(std::binary_search(pool.begin(), pool.end(),
				    target))
		return match::make_atomic(p);
	      else
		return NULL;
	    }

	    break;

	  case pattern::exact_name:
	    if(p->get_exact_name_name() == target.get_package_iterator(cache).Name())
	      return match::make_atomic(p);
	    else
	      return NULL;

	  case pattern::false_tp:
	    return NULL;
	    break;

	  case pattern::foreign_architecture:
	    if(!target.get_has_version())
	      return NULL;

	    {
	      pkgCache::VerIterator ver(target.get_version_iterator(cache));
	      if(aptitude::apt::is_foreign_arch(ver))
		return match::make_atomic(p, ver.Arch());
	      else
		return NULL;
	    }

	  case pattern::garbage:
	    if(!target.get_has_version())
	      return NULL;
	    else if(!cache[target.get_package_iterator(cache)].Garbage)
	      return NULL;
	    else
	      return match::make_atomic(p);
	    break;

	  case pattern::install_version:
	    if(target.get_has_version() &&
	       target.get_ver() == cache[target.get_package_iterator(cache)].InstallVer)
	      return match::make_atomic(p);
	    else
	      return NULL;
	    break;

	  case pattern::installed:
	    if(target.get_has_version() &&
	       target.get_version_iterator(cache) == target.get_package_iterator(cache).CurrentVer())
	      return match::make_atomic(p);
	    else
	      return NULL;
	    break;

	  case pattern::maintainer:
	    if(!target.get_has_version())
	      return NULL;
	    else
	      {
		pkgCache::VerIterator ver(target.get_version_iterator(cache));
		pkgRecords::Parser &rec(records.Lookup(ver.FileList()));

		return evaluate_regexp(p,
				       p->get_maintainer_regex_info(),
				       rec.Maintainer().c_str(),
				       debug);
	      }
	    break;

	  case pattern::multiarch:
	    if(!target.get_has_version())
	      return NULL;
	    else
	      {
		bool matches = false;
                const pattern::multiarch_type type = p->get_multiarch_multiarch_type();

                switch(target.get_ver()->MultiArch)
                  {
                  case pkgCache::Version::Foreign:
                  case pkgCache::Version::AllForeign:
                    matches = (type == pattern::multiarch_foreign);
                    break;
                  case pkgCache::Version::Same:
                    matches = (type == pattern::multiarch_same);
                    break;
                  case pkgCache::Version::Allowed:
                  case pkgCache::Version::AllAllowed:
                    matches = (type == pattern::multiarch_allowed);
                    break;
                  default:
                    matches = (type == pattern::multiarch_none);
                  }

		if(matches)
		  return match::make_atomic(p);
		else
		  return NULL;
	      }
	    break;

	  case pattern::name:
	    return evaluate_regexp(p,
				   p->get_name_regex_info(),
				   target.get_package_iterator(cache).Name(),
				   debug);
	    break;

	  case pattern::native_architecture:
	    if(!target.get_has_version())
	      return NULL;

	    {
	      pkgCache::VerIterator ver(target.get_version_iterator(cache));
	      if(aptitude::apt::is_native_arch(ver))
		return match::make_atomic(p, ver.Arch());
	      else
		return NULL;
	    }

	  case pattern::new_tp:
	    if(!target.get_has_version())
	      return NULL;
	    else if(!cache.get_ext_state(target.get_package_iterator(cache)).new_package)
	      return NULL;
	    else
	      return match::make_atomic(p);
	    break;

	  case pattern::obsolete:
	    if(pkg_obsolete(target.get_package_iterator(cache)))
	      return match::make_atomic(p);
	    else
	      return NULL;
	    break;

	  case pattern::origin:
	    if(!target.get_has_version())
	      return NULL;
	    {
	      pkgCache::PkgIterator pkg(target.get_package_iterator(cache));
	      pkgCache::VerIterator ver(target.get_version_iterator(cache));

	      for(pkgCache::VerFileIterator f = ver.FileList(); !f.end(); ++f)
		{
		  pkgCache::PkgFileIterator cur = f.File();
		  const char *origin = cur.Origin();

		  if(!cur.end() && origin != NULL)
		    {
		      ref_ptr<match>
			m(evaluate_regexp(p,
					  p->get_origin_regex_info(),
					  origin,
					  debug));

		      if(m.valid())
			return m;
		    }
		}

	      return NULL;
	    }

	    break;

	  case pattern::priority:
	    if(!target.get_has_version())
	      return NULL;
	    else if(target.get_ver()->Priority != p->get_priority_priority())
	      return NULL;
	    else
	      return match::make_atomic(p);
	    break;

	  case pattern::provides:
	    if(!target.get_has_version())
	      return NULL;
	    else
	      {
		pkgCache::VerIterator ver(target.get_version_iterator(cache));

		std::vector<matchable> new_pool;

		for(pkgCache::PrvIterator prv = ver.ProvidesList();
		    !prv.end(); ++prv)
		  {
		    // Add all versions of each provided package to
		    // the pool.  I chose this because it seems least
		    // surprising (?provides(?description(blah))
		    // should behave as expected), but if versioned
		    // Provides happen this might cause problems with
		    // matching only the versions that are actually
		    // provided.
		    pkgCache::PkgIterator provided_pkg = prv.ParentPkg();
		    if(provided_pkg.VersionList().end())
		      new_pool.push_back(matchable(provided_pkg));
		    else
		      for(pkgCache::VerIterator ver = provided_pkg.VersionList();
			  !ver.end(); ++ver)
			new_pool.push_back(matchable(provided_pkg, ver));

		    ref_ptr<structural_match>
		      m(evaluate_toplevel(structural_eval_any,
					  p->get_provides_pattern(),
					  the_stack,
					  search_info,
					  new_pool,
					  cache,
					  records,
					  debug));

		    if(m.valid())
		      return match::make_provides(p, m, prv);
		  }
	      }

	    return NULL;
	    break;

	  case pattern::reverse_depends:
	    {
	      pkgCache::PkgIterator pkg = target.get_package_iterator(cache);
	      pkgCache::VerIterator ver;
	      if(target.get_has_version())
		ver = target.get_version_iterator(cache);
	      const bool broken = p->get_reverse_depends_broken();
	      pkgCache::Dep::DepType type = p->get_reverse_depends_depends_type();

	      std::vector<matchable> revdep_pool;

	      for(pkgCache::DepIterator d = pkg.RevDependsList();
		  !d.end(); ++d)
		{
		  if(broken)
		    {
		      // Find the corresponding forward dependency and
		      // check whether it's broken.
		      pkgCache::DepIterator d2(cache, &*d);
		      while(d2->CompareOp & pkgCache::Dep::Or)
			++d2;
		      if(cache[d2] & pkgDepCache::DepGInstall)
			continue;
		    }

		  if(  (d->Type == type ||
			(type == pkgCache::Dep::Depends && d->Type == pkgCache::Dep::PreDepends)) &&
		       (!d.TargetVer() || (target.get_has_version() &&
					   _system->VS->CheckDep(ver.VerStr(), d->CompareOp, d.TargetVer())))   )
		    {
		      matchable m(d.ParentPkg(), d.ParentVer());
		      if(revdep_pool.empty())
			revdep_pool.push_back(m);
		      else
			revdep_pool[0] = m;


		      ref_ptr<structural_match>
			rval(evaluate_toplevel(structural_eval_any,
					       p->get_reverse_depends_pattern(),
					       the_stack,
					       search_info,
					       revdep_pool,
					       cache,
					       records,
					       debug));

		      if(rval.valid())
			return match::make_dependency(p, rval, d);
		    }
		}

	      // Check dependencies through virtual packages.
	      if(target.get_has_version())
		{
		  for(pkgCache::PrvIterator prv = ver.ProvidesList();
		      !prv.end(); ++prv)
		    {
		      for(pkgCache::DepIterator d = prv.ParentPkg().RevDependsList();
			  !d.end(); ++d)
			{
			  if(broken)
			    {
			      pkgCache::DepIterator d2(cache, &*d);
			      while(d2->CompareOp & pkgCache::Dep::Or)
				++d2;
			      if(cache[d2] & pkgDepCache::DepGInstall)
				continue;
			    }

			  if(d->Type == type &&
			     (d.TargetVer() == NULL ||
			      (  prv.ProvideVersion() != NULL &&
			         _system->VS->CheckDep(ver.VerStr(), d->CompareOp, d.TargetVer())  )))
			    {
			      matchable m(d.ParentPkg(), d.ParentVer());
			      if(revdep_pool.empty())
				revdep_pool.push_back(m);
			      else
				revdep_pool[0] = m;


			      ref_ptr<structural_match>
				rval(evaluate_toplevel(structural_eval_any,
						       p->get_reverse_depends_pattern(),
						       the_stack,
						       search_info,
						       revdep_pool,
						       cache,
						       records,
						       debug));

			      if(rval.valid())
				return match::make_dependency(p, rval, d);
			    }
			}
		    }
		}

	      return NULL;
	    }
	    break;

	  case pattern::reverse_provides:
	    {
	      std::vector<matchable> revprv_pool;
	      pkgCache::PkgIterator pkg = target.get_package_iterator(cache);

	      // Hm, it would be nice if we could do this in a way that
	      // allowed users to ask to match all the reverse provides.
	      for(pkgCache::PrvIterator prv = pkg.ProvidesList();
		  !prv.end(); ++prv)
		{
		  if(revprv_pool.empty())
		    revprv_pool.push_back(matchable(prv.OwnerPkg(), prv.OwnerVer()));
		  else
		    revprv_pool[0] = matchable(prv.OwnerPkg(), prv.OwnerVer());

		  ref_ptr<structural_match>
		    m(evaluate_toplevel(structural_eval_any,
					p->get_reverse_provides_pattern(),
					the_stack,
					search_info,
					revprv_pool,
					cache,
					records,
					debug));

		  if(m.valid())
		    return match::make_provides(p, m, prv);
		}

	      return NULL;
	    }

	    break;

	  case pattern::section:
	    if(target.get_has_version())
	      {
		pkgCache::VerIterator ver(target.get_version_iterator(cache));
		const char *ver_section = ver.Section();
		if(ver_section != NULL)
		  {
		    ref_ptr<match>
		      m(evaluate_regexp(p,
					p->get_section_regex_info(),
					ver_section,
					debug));

		    if(m.valid())
		      return m;
		  }
	      }

	    {
	      pkgCache::PkgIterator pkg(target.get_package_iterator(cache));
	      const char *pkg_section = pkg.Section();

	      if(pkg_section != NULL)
		return evaluate_regexp(p,
				       p->get_section_regex_info(),
				       pkg_section,
				       debug);
	      else
		return NULL;
	    }
	    break;

	  case pattern::source_package:
	    {
	      if(!target.get_has_version())
		return NULL;

	      bool checked_real_package = false;

	      pkgCache::PkgIterator pkg(target.get_package_iterator(cache));
	      pkgCache::VerIterator ver(target.get_version_iterator(cache));

	      for(pkgCache::VerFileIterator vf = ver.FileList();
		  !vf.end(); ++vf)
		{
		  pkgRecords::Parser &rec = records.Lookup(vf);

		  if(rec.SourcePkg().empty())
		    {
		      if(!checked_real_package)
			{
			  ref_ptr<match> rval =
			    evaluate_regexp(p,
					    p->get_source_package_regex_info(),
					    pkg.Name(),
					    debug);

			  if(rval.valid())
			    return rval;
			}
		    }
		  else
		    {
		      ref_ptr<match> rval =
			evaluate_regexp(p,
					p->get_source_package_regex_info(),
					rec.SourcePkg().c_str(),
					debug);

		      if(rval.valid())
			return rval;
		    }
		}

	      return NULL;
	    }
	    break;

	  case pattern::source_version:
	    {
	      if(!target.get_has_version())
		return NULL;

	      bool checked_real_package = false;

	      pkgCache::PkgIterator pkg(target.get_package_iterator(cache));
	      pkgCache::VerIterator ver(target.get_version_iterator(cache));

	      for(pkgCache::VerFileIterator vf = ver.FileList();
		  !vf.end(); ++vf)
		{
		  pkgRecords::Parser &rec = records.Lookup(vf);

		  if(rec.SourceVer().empty())
		    {
		      if(!checked_real_package)
			{
			  ref_ptr<match> rval =
			    evaluate_regexp(p,
					    p->get_source_version_regex_info(),
					    ver.VerStr(),
					    debug);

			  if(rval.valid())
			    return rval;
			}
		    }
		  else
		    {
		      ref_ptr<match> rval =
			evaluate_regexp(p,
					p->get_source_version_regex_info(),
					rec.SourceVer().c_str(),
					debug);

		      if(rval.valid())
			return rval;
		    }
		}

	      return NULL;
	    }
	    break;

	  case pattern::tag:
	    {
	      pkgCache::PkgIterator pkg(target.get_package_iterator(cache));

              using aptitude::apt::get_fullname;
	      using aptitude::apt::get_tags;
              using aptitude::apt::tag;

	      const std::set<tag> tags(get_tags(pkg));

	      if(tags.empty() == true)
		return NULL;

	      for(std::set<tag>::const_iterator i = tags.begin();
                  i != tags.end();
                  ++i)
		{
		  std::string name(get_fullname(*i));
		  ref_ptr<match> rval =
		    evaluate_regexp(p,
				    p->get_tag_regex_info(),
				    name.c_str(),
				    debug);

		  if(rval.valid())
		    return rval;
		}

	      return NULL;
	    }
	    break;

	  case pattern::task:
	    {
	      pkgCache::PkgIterator pkg(target.get_package_iterator(cache));

	      std::set<string> *l = aptitude::apt::get_tasks(pkg);

	      if(!l)
		return NULL;

	      for(std::set<string>::iterator i = l->begin();
		  i != l->end();
		  ++i)
		{
		  ref_ptr<match> m =
		    evaluate_regexp(p,
				    p->get_task_regex_info(),
				    i->c_str(),
				    debug);

		  if(m.valid())
		    return m;
		}

	      return NULL;
	    }
	    break;

	  case pattern::term:
	    {
	      if(search_info->term_matches(target,
                                           p->get_term_term(),
                                           cache,
                                           records,
                                           debug))
		return match::make_atomic(p);
	      else
		return NULL;
	    }
	    break;

	  case pattern::term_prefix:
	    {
	      if(search_info->term_prefix_matches(target,
                                                  p->get_term_prefix_term(),
                                                  cache,
                                                  records,
                                                  debug))
		return match::make_atomic(p);
	      else
		return NULL;
	    }
	    break;

	  case pattern::true_tp:
	    return match::make_atomic(p);
	    break;

	  case pattern::upgradable:
	    {
	      pkgCache::PkgIterator pkg =
		target.get_package_iterator(cache);

	      if(!pkg.CurrentVer().end() &&
		 cache[pkg].CandidateVer != NULL &&
		 cache[pkg].Upgradable())
		return match::make_atomic(p);
	      else
		return NULL;
	    }
	    break;

	  case pattern::user_tag:
	    {
	      pkgCache::PkgIterator pkg =
		target.get_package_iterator(cache);

	      const std::set<aptitudeDepCache::user_tag> &user_tags =
		cache.get_ext_state(pkg).user_tags;

	      for(std::set<aptitudeDepCache::user_tag>::const_iterator it =
		    user_tags.begin(); it != user_tags.end(); ++it)
		{
		  aptitudeDepCache::user_tag tag(*it);

		  ref_ptr<match> m(search_info->find_user_tag_match(p,
								    tag,
								    cache,
								    debug));

		  // NB: this currently short-circuits (as does, e.g.,
		  // ?task); for highlighting purposes we might want
		  // to return all matches.
		  if(m.valid())
		    return m;
		}

	      return NULL;
	    }
	    break;

	  case pattern::version:
	    if(!target.get_has_version())
	      return NULL;

	    return evaluate_regexp(p,
				   p->get_version_regex_info(),
				   target.get_version_iterator(cache).VerStr(),
				   debug);
	    break;

	  case pattern::virtual_tp:
	    if(!target.get_package_iterator(cache).VersionList().end())
	      return NULL;
	    else
	      return match::make_atomic(p);
	    break;

	  default:
	    throw MatchingException("Internal error: unhandled pattern type in evaluate()");
	  }
      }

      ref_ptr<structural_match> evaluate_structural(structural_eval_mode mode,
						    const ref_ptr<pattern> &p,
						    stack &the_stack,
						    const ref_ptr<search_cache::implementation> &search_info,
						    const std::vector<matchable> &pool,
						    aptitudeDepCache &cache,
						    pkgRecords &records,
						    bool debug)
      {
	if(debug)
	  {
	    std::cout << "Matching " << serialize_pattern(p)
		      << " against the pool ";
	    print_pool(std::cout, pool, cache);
	    std::cout << " with stack ";
	    print_stack(std::cout, the_stack, cache);
	    std::cout << " (mode=";
	    switch(mode)
	      {
	      case structural_eval_all:
		std::cout << "all";
		break;

	      case structural_eval_any:
		std::cout << "any";
		break;
	      }
	    std::cout << ")" << std::endl;
	  }

	switch(p->get_type())
	  {
	    // Structural matchers:

	  case pattern::all_versions:
	    {
	      ref_ptr<structural_match>
		m(evaluate_structural(structural_eval_all,
				      p->get_all_versions_pattern(),
				      the_stack,
				      search_info,
				      pool,
				      cache,
				      records,
				      debug));

	      if(!m.valid())
		return NULL;
	      else
		return structural_match::make_branch(p, &m, (&m) + 1);
	    }
	    break;

	  case pattern::and_tp:
	    {
	      const std::vector<ref_ptr<pattern> > &sub_patterns(p->get_and_patterns());
	      std::vector<ref_ptr<structural_match> > sub_matches;

	      for(std::vector<ref_ptr<pattern> >::const_iterator it =
		    sub_patterns.begin(); it != sub_patterns.end(); ++it)
		{
		  ref_ptr<structural_match> m(evaluate_structural(mode,
								  (*it),
								  the_stack,
								  search_info,
								  pool,
								  cache,
								  records,
								  debug));

		  if(!m.valid())
		    return NULL;

		  sub_matches.push_back(m);
		}

	      return structural_match::make_branch(p, sub_matches.begin(), sub_matches.end());
	    }
	    break;

	  case pattern::any_version:
	    {
	      std::vector<matchable> new_pool;
	      new_pool.push_back(matchable());

	      std::vector<ref_ptr<structural_match> > sub_matches;
	      for(std::vector<matchable>::const_iterator it =
		    pool.begin(); it != pool.end(); ++it)
		{
		  new_pool[0] = *it;

		  ref_ptr<structural_match>
		    m(evaluate_structural(mode,
					  p->get_any_version_pattern(),
					  the_stack,
					  search_info,
					  new_pool,
					  cache,
					  records,
					  debug));

		  if(m.valid())
		    sub_matches.push_back(m);
		}

	      if(sub_matches.empty())
		return NULL;
	      else
		return structural_match::make_branch(p, sub_matches.begin(), sub_matches.end());
	    }
	    break;

	  case pattern::for_tp:
	    {
	      the_stack.push_back(&pool);

	      const ref_ptr<structural_match>
		m(evaluate_structural(mode,
				      p->get_for_pattern(),
				      the_stack,
				      search_info,
				      pool,
				      cache,
				      records,
				      debug));

	      if(m.valid())
		return structural_match::make_branch(p, &m, (&m) + 1);
	      else
		return NULL;
	    }
	    break;

	  case pattern::narrow:
	    // Match each entry in the pool against the filter
	    // separately.  Then match the main pattern against a
	    // pool formed from values that passed the filter.
	    {
	      std::vector<matchable> singleton_pool;
	      std::vector<matchable> new_pool;
	      singleton_pool.push_back(matchable());

	      // \todo we should perhaps store the filter matches in a
	      // separate list.
	      for(std::vector<matchable>::const_iterator it =
		    pool.begin(); it != pool.end(); ++it)
		{
		  singleton_pool[0] = *it;

		  if(evaluate_structural(mode,
					 p->get_narrow_filter(),
					 the_stack,
					 search_info,
					 singleton_pool,
					 cache,
					 records,
					 debug).valid())
		    new_pool.push_back(*it);
		}

	      if(new_pool.empty())
		return NULL;
	      else
		{
		  ref_ptr<structural_match>
		    m(evaluate_structural(mode,
					  p->get_narrow_pattern(),
					  the_stack,
					  search_info,
					  new_pool,
					  cache,
					  records,
					  debug));

		  if(!m.valid())
		    return NULL;
		  else
		    return structural_match::make_branch(p, &m, (&m) + 1);
		}
	    }
	    break;

	  case pattern::not_tp:
	    {
	      ref_ptr<structural_match> m(evaluate_structural(mode,
							      p->get_not_pattern(),
							      the_stack,
							      search_info,
							      pool,
							      cache,
							      records,
							      debug));

	      if(!m.valid())
		// Report a structural match with no sub-parts.  This
		// will lose doubly-negated information.  For now that's
		// just too bad; we can try to recover it later.
		return structural_match::make_branch(p,
						     (ref_ptr<structural_match> *)0,
						     (ref_ptr<structural_match> *)0);
	      else
		return NULL;
	    }

	    break;

	  case pattern::or_tp:
	    {
	      const std::vector<ref_ptr<pattern> > &sub_patterns(p->get_or_patterns());
	      std::vector<ref_ptr<structural_match> > sub_matches;

	      // Note: we do *not* short-circuit, in order to allow
	      // the caller to see as much information as possible
	      // about the match.
	      for(std::vector<ref_ptr<pattern> >::const_iterator it =
		    sub_patterns.begin(); it != sub_patterns.end(); ++it)
		{
		  ref_ptr<structural_match> m(evaluate_structural(mode,
								  (*it),
								  the_stack,
								  search_info,
								  pool,
								  cache,
								  records,
								  debug));

		  if(m.valid())
		    sub_matches.push_back(m);
		}

	      if(sub_matches.empty())
		return NULL;
	      else
		return structural_match::make_branch(p, sub_matches.begin(), sub_matches.end());
	    }
	    break;

	  case pattern::widen:
	    // NB: to make this fast I rely on the sort order of
	    // matchables.
	    //
	    // \todo Perhaps this pattern should be redefined to allow
	    // us to inject arbitrary stuff into the pool?  Right now
	    // it just expands the pool to include all the versions of
	    // each package that it includes.
	    {
	      std::vector<matchable> new_pool;

	      for(std::vector<matchable>::const_iterator it
		    = pool.begin(); it != pool.end(); ++it)
		{
		  // If we've already seen this package it'll be at
		  // the back of the new pool (due to processing
		  // inputs in the pool in sort order).
		  if(!new_pool.empty() &&
		     new_pool.back().get_pkg() == it->get_pkg())
		    continue;

		  // Virtual packages aren't touched by ?widen.
		  if(!it->get_has_version())
		    {
		      new_pool.push_back(*it);
		      continue;
		    }

		  pkgCache::PkgIterator pkg =
		    it->get_package_iterator(cache);
		  if(pkg.VersionList().end())
		    new_pool.push_back(*it);
		  else
		    {
		      for(pkgCache::VerIterator ver = pkg.VersionList();
			  !ver.end(); ++ver)
			{
			  new_pool.push_back(matchable(pkg, ver));
			}
		    }
		}

	      std::sort(new_pool.begin(), new_pool.end());
	      ref_ptr<structural_match>
		m(evaluate_structural(mode,
				      p->get_widen_pattern(),
				      the_stack,
				      search_info,
				      new_pool,
				      cache,
				      records,
				      debug));
	      if(!m.valid())
		return NULL;
	      else
		return structural_match::make_branch(p, &m, (&m) + 1);
	    }
	    break;

	    // Atomic matchers:

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
	  case pattern::exact_name:
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
	  case pattern::term_prefix:
	  case pattern::true_tp:
	  case pattern::upgradable:
	  case pattern::user_tag:
	  case pattern::version:
	  case pattern::virtual_tp:
	    switch(mode)
	      {
	      case structural_eval_all:
		{
		  std::vector<std::pair<matchable, ref_ptr<match> > > matches;
		  for(std::vector<matchable>::const_iterator it =
			pool.begin(); it != pool.end(); ++it)
		    {
		      cwidget::util::ref_ptr<match> m(evaluate_atomic(p, *it, the_stack, search_info, cache, records, debug));
		      if(!m.valid())
			{
			  if(debug)
			    {
			      std::cout << "Failed to match: ";
			      print_matchable(std::cout, *it, cache);
			      std::cout << std::endl;
			    }
			  return NULL;
			}
		      else
			matches.push_back(std::make_pair(*it, m));
		    }

		  if(matches.size() == 0)
		    return NULL;
		  else
		    return structural_match::make_leaf(p, matches.begin(), matches.end());
		}
		break;

	      case structural_eval_any:
		{
		  std::vector<std::pair<matchable, ref_ptr<match> > > matches;
		  for(std::vector<matchable>::const_iterator it =
			pool.begin(); it != pool.end(); ++it)
		    {
		      cwidget::util::ref_ptr<match> m(evaluate_atomic(p, *it, the_stack, search_info, cache, records, debug));
		      if(m.valid())
			{
			  if(debug)
			    {
			      std::cout << "Matched: ";
			      print_matchable(std::cout, *it, cache);
			      std::cout << std::endl;
			    }
			  // TODO: short-circuit?
			  matches.push_back(std::make_pair(*it, m));
			}
		    }

		  if(matches.size() == 0)
		    return NULL;
		  else
		    return structural_match::make_leaf(p, matches.begin(), matches.end());
		}
		break;

	      default:
		throw MatchingException("Internal error: unhandled structural match mode.");
	      }

	    break;

	  default:
	    throw MatchingException("Internal error: unhandled pattern type in evaluate()");
	  }
      }

      ref_ptr<structural_match> evaluate_toplevel(structural_eval_mode mode,
						  const ref_ptr<pattern> &p,
						  stack &the_stack,
						  const ref_ptr<search_cache::implementation> &search_info,
						  const std::vector<matchable> &pool,
						  aptitudeDepCache &cache,
						  pkgRecords &records,
						  bool debug)
      {
	const xapian_info &xapian_match(search_info->get_toplevel_xapian_info(p, debug));

	// This may be an efficiency problem.
	std::vector<matchable> filtered_pool;
	filtered_pool.reserve(pool.size());
	for(std::vector<matchable>::const_iterator it = pool.begin();
	    it != pool.end(); ++it)
	  {
	    if(xapian_match.maybe_contains_package(it->get_package_iterator(cache), search_info->get_db()))
	      filtered_pool.push_back(*it);
	  }

	return evaluate_structural(mode, p, the_stack, search_info,
				   filtered_pool, cache, records, debug);
      }

      // is_pure_xapian returns "true" if we can identify a Xapian
      // term that matches if AND ONLY IF the pattern matches.  In
      // other words: it contains only structural patterns combined
      // with Boolean operators (and version-manipulation operators,
      // which we ignore).
      bool is_pure_xapian(const ref_ptr<pattern> &p)
      {
	switch(p->get_type())
	  {
	  case pattern::all_versions:
	    return is_pure_xapian(p->get_all_versions_pattern());

	  case pattern::and_tp:
	    {
	      const std::vector<ref_ptr<pattern> > &
		sub_patterns(p->get_and_patterns());

	      if(sub_patterns.size() == 0)
		return true;

	      for(std::vector<ref_ptr<pattern> >::const_iterator it =
		    sub_patterns.begin(); it != sub_patterns.end(); ++it)
		{
		  if(!is_pure_xapian(*it))
		    return false;
		}

	      return true;
	    }

	  case pattern::any_version:
	    return is_pure_xapian(p->get_any_version_pattern());

	  case pattern::exact_name:
	    // Names are indexed by apt-xapian-index under the prefix
	    // "XP".
	    return true;

	  case pattern::for_tp:
	    return is_pure_xapian(p->get_for_pattern());

	  case pattern::narrow:
	    return
	      is_pure_xapian(p->get_narrow_filter()) &&
	      is_pure_xapian(p->get_narrow_pattern());

	  case pattern::not_tp:
	    return is_pure_xapian(p->get_not_pattern());

	  case pattern::or_tp:
	    {
	      const std::vector<ref_ptr<pattern> > &
		sub_patterns(p->get_or_patterns());

	      if(sub_patterns.size() == 0)
		return true;

	      for(std::vector<ref_ptr<pattern> >::const_iterator it =
		    sub_patterns.begin(); it != sub_patterns.end(); ++it)
		{
		  if(!is_pure_xapian(*it))
		    return false;
		}

	      return true;
	    }

	  case pattern::widen:
	    return is_pure_xapian(p->get_widen_pattern());

	  case pattern::term:
	    return true;

	  case pattern::term_prefix:
	    return true;

	    // Various non-Xapian terms.  All of these return false.

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
	  case pattern::true_tp:
	  case pattern::upgradable:
	  case pattern::user_tag:
	  case pattern::version:
	  case pattern::virtual_tp:
	    return false;
	  default:
	    throw MatchingException("Internal error: unhandled pattern type in is_pure_xapian()");
	  }
      }

      // is_xapian_dependent returns "true" if we can always identify
      // a Xapian term that must match for the pattern to match.
      bool is_xapian_dependent(const ref_ptr<pattern> &p)
      {
	switch(p->get_type())
	  {
	  case pattern::all_versions:
	    return is_xapian_dependent(p->get_all_versions_pattern());

	  case pattern::and_tp:
	    {
	      const std::vector<ref_ptr<pattern> > &
		sub_patterns(p->get_and_patterns());

	      if(sub_patterns.size() == 0)
		return false;

	      // The AND is fine if it has at least one positive
	      // Xapian-dependent term.  NB: since negative terms are
	      // not Xapian-dependent, checking both conditions is
	      // redundant, so we just check that each sub-term is
	      // Xapian-dependent.
	      for(std::vector<ref_ptr<pattern> >::const_iterator it =
		    sub_patterns.begin(); it != sub_patterns.end(); ++it)
		{
		  if(is_xapian_dependent(*it))
		    return true;
		}

	      return false;
	    }

	  case pattern::any_version:
	    return is_xapian_dependent(p->get_any_version_pattern());

	  case pattern::for_tp:
	    return is_xapian_dependent(p->get_for_pattern());

	  case pattern::narrow:
	    return is_xapian_dependent(p->get_narrow_pattern());

	  case pattern::not_tp:
	    return false;

	  case pattern::or_tp:
	    // OR terms are Xapian-dependent if all of their sub-terms
	    // are.
	    {
	      const std::vector<ref_ptr<pattern> > &
		sub_patterns(p->get_or_patterns());

	      if(sub_patterns.size() == 0)
		return false;

	      for(std::vector<ref_ptr<pattern> >::const_iterator it =
		    sub_patterns.begin(); it != sub_patterns.end(); ++it)
		{
		  if(!is_xapian_dependent(*it))
		    return false;
		}

	      return true;
	    }

	  case pattern::widen:
	    return is_xapian_dependent(p->get_widen_pattern());

	  case pattern::exact_name:
	    return true;

	  case pattern::term:
	    return true;

	  case pattern::term_prefix:
	    return true;

	    // Various non-dependent terms.  All of these return
	    // false.  Some have internal matchers, but they are
	    // separate searches.

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
	  case pattern::true_tp:
	  case pattern::upgradable:
	  case pattern::user_tag:
	  case pattern::version:
	  case pattern::virtual_tp:
	    return false;
	  default:
	    throw MatchingException("Internal error: unhandled pattern type in is_xapian_dependent()");
	  }
      }

      ref_ptr<pattern> negate_pattern(const ref_ptr<pattern> &p)
      {
	if(p->get_type() == pattern::not_tp)
	  return p->get_not_pattern();
	else
	  return pattern::make_not(p);
      }

      // Adjusts the incoming pattern for the purposes of computing
      // the Xapian query.
      //
      // Specifically, this pushes NOTs out of OR expressions to the
      // top structural level (the top-level of the pattern, or the
      // top-level of a term like ?depends that has a sub-pattern).
      // Also throws away some structural patterns that are irrelevant
      // for Xapian, like all_versions.
      cwidget::util::ref_ptr<pattern> normalize_pattern(const cwidget::util::ref_ptr<pattern> &p)
      {
	switch(p->get_type())
	  {
	  case pattern::all_versions:
	    return normalize_pattern(p->get_all_versions_pattern());

	  case pattern::and_tp:
	    {
	      const std::vector<ref_ptr<pattern> > &
		sub_patterns(p->get_and_patterns());

	      // First, normalize the sub-parts:
	      std::vector<ref_ptr<pattern> >
		normalized_sub_patterns;
	      normalized_sub_patterns.reserve(sub_patterns.size());

	      // The AND is fine if it has at least one positive
	      // Xapian-dependent term.  If it isn't, but it has a
	      // Xapian-dependent term (i.e., all the Xapian terms are
	      // negative), we invert it and turn it into a
	      // Xapian-dependent OR.  If that fails too, it's not
	      // Xapian-dependent.
	      bool has_xapian_dependent_sub_term = false;
	      bool has_positive_xapian_dependent_sub_term = false;
	      for(std::vector<ref_ptr<pattern> >::const_iterator it =
		    sub_patterns.begin(); it != sub_patterns.end(); ++it)
		{
		  ref_ptr<pattern> sub_normalized(normalize_pattern(*it));

		  if(is_xapian_dependent(sub_normalized))
		    {
		      has_xapian_dependent_sub_term = true;
		      if(sub_normalized->get_type() != pattern::not_tp)
			has_positive_xapian_dependent_sub_term = true;
		    }

		  normalized_sub_patterns.push_back(sub_normalized);
		}

	      if(has_positive_xapian_dependent_sub_term)
		return pattern::make_and(normalized_sub_patterns);
	      else if(has_xapian_dependent_sub_term)
		{
		  std::vector<ref_ptr<pattern> > negative_sub_normalized;
		  negative_sub_normalized.reserve(normalized_sub_patterns.size());

		  for(std::vector<ref_ptr<pattern> >::const_iterator it =
			normalized_sub_patterns.begin(); it != normalized_sub_patterns.end(); ++it)
		    negative_sub_normalized.push_back(negate_pattern(*it));

		  return pattern::make_not(pattern::make_or(negative_sub_normalized));
		}
	      else
		// No Xapian-dependent terms at all, so
		// just muddle through.
		return pattern::make_and(normalized_sub_patterns);
	    }

	  case pattern::any_version:
	    return normalize_pattern(p->get_any_version_pattern());

	  case pattern::for_tp:
	    // Don't eliminate the ?for or its captured variables
	    // won't make sense.  (useful for printing purposes)
	    //
	    // However, we need to reach through it and lift the ?not,
	    // if any, out so that it's visible at top-level.
	    {
	      ref_ptr<pattern> sub_normalized =
		normalize_pattern(p->get_for_pattern());

	      // Note: because the normalization lifts ?not terms to
	      // the top level and eliminates double ?nots, we don't
	      // need to handle double ?nots here.
	      if(sub_normalized->get_type() == pattern::not_tp)
		return pattern::make_not(pattern::make_for(p->get_for_variable_name(),
							   sub_normalized->get_not_pattern()));
	      else
		return pattern::make_for(p->get_for_variable_name(),
					 sub_normalized);
	    }

	  case pattern::narrow:
	    return normalize_pattern(p->get_narrow_pattern());

	  case pattern::not_tp:
	    {
	      ref_ptr<pattern> sub_normalized =
		normalize_pattern(p->get_not_pattern());

	      // Eliminate double ?nots.
	      if(sub_normalized->get_type() == pattern::not_tp)
		return sub_normalized->get_not_pattern();
	      else
		return pattern::make_not(sub_normalized);
	    }

	  case pattern::or_tp:
	    {
	      const std::vector<ref_ptr<pattern> > &
		sub_patterns(p->get_or_patterns());

	      std::vector<ref_ptr<pattern> >
		normalized_sub_patterns;
	      normalized_sub_patterns.reserve(sub_patterns.size());

	      bool has_negative_xapian_dependent_sub_term = false;
	      for(std::vector<ref_ptr<pattern> >::const_iterator it =
		    sub_patterns.begin(); it != sub_patterns.end(); ++it)
		{
		  ref_ptr<pattern> sub_normalized(normalize_pattern(*it));

		  if(is_xapian_dependent(sub_normalized) &&
		     sub_normalized->get_type() == pattern::not_tp)
		    has_negative_xapian_dependent_sub_term = true;

		  normalized_sub_patterns.push_back(sub_normalized);
		}

	      if(has_negative_xapian_dependent_sub_term)
		{
		  std::vector<ref_ptr<pattern> > negative_sub_normalized;
		  for(std::vector<ref_ptr<pattern> >::const_iterator it =
		      normalized_sub_patterns.begin(); it != normalized_sub_patterns.end(); ++it)
		    negative_sub_normalized.push_back(negate_pattern(*it));

		  return pattern::make_not(pattern::make_and(negative_sub_normalized));
		}
	      else
		return pattern::make_or(normalized_sub_patterns);
	    }

	  case pattern::widen:
	    return normalize_pattern(p->get_widen_pattern());

	    // Various non-Xapian patterns, along with ?term.
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
	  case pattern::exact_name:
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
	  case pattern::term_prefix:
	  case pattern::true_tp:
	  case pattern::upgradable:
	  case pattern::user_tag:
	  case pattern::version:
	  case pattern::virtual_tp:
	    return p;
	  default:
	    throw MatchingException("Internal error: unhandled pattern type in normalize_pattern()");
	  }
      }

      // Note: the incoming expression must be Xapian-normalized as
      // with normalize_pattern().
      //
      // Builds a query for the Xapian-relevant part of the search.
      //
      // Analyzes the incoming expression in an attempt to prove that
      // any matching expression will match at least one Xapian term.
      // If this can be proven, builds a Xapian query that
      // overapproximates the match set.  Otherwise, bails and returns
      // the empty query.
      //
      // Returns the new query, or an empty query if there's no
      // Xapian-dependence.
      Xapian::Query build_xapian_query(const cwidget::util::ref_ptr<pattern> &p,
				       const Xapian::Database &db)
      {
	switch(p->get_type())
	  {
	  case pattern::all_versions:
	    return build_xapian_query(p->get_all_versions_pattern(),
				      db);

	  case pattern::and_tp:
	    {
	      // We make ?and right-associative:
	      // ?and(a, b, c) => a AND (b AND c).
	      //
	      // Hm, maybe this should be the other way around?  That
	      // would make handling AND_NOT and AND_MAYBE cleaner.

	      const std::vector<ref_ptr<pattern> > &sub_patterns =
		p->get_and_patterns();

	      Xapian::Query and_not_tail;
	      // First build all the negative terms for insertion into
	      // an AND_NOT.  (of course, to collect them under a
	      // single ?not, we need to make them use OR and we need
	      // to drop the leading ?not)
	      for(std::vector<ref_ptr<pattern> >::const_reverse_iterator it =
		    sub_patterns.rbegin(); it != sub_patterns.rend(); ++it)
		{
		  // If the sub-pattern has non-Xapian pieces, we need
		  // to just throw it away (which results in the
		  // overestimate we promise) because the
		  // transformation will be an overestimate, so
		  // negating it produces an UNDERestimate and the
		  // world explodes.  This shouldn't affect scores or
		  // our ability to index, so I think it's just fine.
		  if((*it)->get_type() == pattern::not_tp &&
		     is_pure_xapian((*it)->get_not_pattern()))
		    {
		      Xapian::Query q(build_xapian_query((*it)->get_not_pattern(),
							 db));

		      // Ignore empty queries; they signify that the
		      // entry should be pruned from the generated
		      // tree.
		      if(!q.empty())
			{
			  if(and_not_tail.empty())
			    and_not_tail = q;
			  else
			    and_not_tail = Xapian::Query(Xapian::Query::OP_OR,
							 q,
							 and_not_tail);
			}
		    }
		}

	      Xapian::Query and_maybe_tail;
	      // Build in positive, non-Xapian-dependent terms.  These
	      // are added to the expression using AND_MAYBE to make
	      // sure that the terms are indexed and their scores
	      // considered, but they don't constrain the search
	      // (because the Xapian query might be false when the
	      // aptitude one is true).  This would be wrong inside a
	      // NOT, but we avoid that with the check above for pure
	      // Xapian expressions.
	      for(std::vector<ref_ptr<pattern> >::const_reverse_iterator it =
		    sub_patterns.rbegin(); it != sub_patterns.rend(); ++it)
		{
		  if((*it)->get_type() != pattern::not_tp &&
		     !is_xapian_dependent(*it))
		    {
		      Xapian::Query q(build_xapian_query(*it,
							 db));

		      if(and_maybe_tail.empty())
			and_maybe_tail = q;
		      else
			and_maybe_tail = Xapian::Query(Xapian::Query::OP_AND_MAYBE,
						       q,
						       and_maybe_tail);
		    }
		}

	      Xapian::Query and_tail;
	      // Now build in the positive, Xapian-dependent terms.
	      for(std::vector<ref_ptr<pattern> >::const_reverse_iterator it =
		    sub_patterns.rbegin(); it != sub_patterns.rend(); ++it)
		{
		  if((*it)->get_type() != pattern::not_tp &&
		     is_xapian_dependent(*it))
		    {
		      Xapian::Query q(build_xapian_query(*it,
							 db));

		      if(!q.empty())
			{
			  if(and_tail.empty())
			    and_tail = q;
			  else
			    and_tail = Xapian::Query(Xapian::Query::OP_AND,
						     q,
						     and_tail);
			}
		    }
		}

	      if(and_tail.empty())
		return and_tail;
	      else
		{
		  // If we have negative and independent terms, we
		  // don't want this:
		  //
		  //   (a AND NOT (b AND MAYBE c))
		  //
		  // or this:
		  //
		  //   (a AND MAYBE (c AND NOT b))
		  //
		  // What we want is this:
		  //
		  //   (a AND NOT b) AND MAYBE c
		  const bool and_not_empty = and_not_tail.empty();
		  const bool and_maybe_empty = and_maybe_tail.empty();

		  if(and_not_empty && and_maybe_empty)
		    return and_tail;
		  else if(!and_not_empty && and_maybe_empty)
		    return Xapian::Query(Xapian::Query::OP_AND_NOT,
					 and_tail,
					 and_not_tail);
		  else if(and_not_empty && !and_maybe_empty)
		    return Xapian::Query(Xapian::Query::OP_AND_MAYBE,
					 and_tail,
					 and_maybe_tail);
		  else // if(!and_not_empty && !and_maybe_empty)
		    return Xapian::Query(Xapian::Query::OP_AND_MAYBE,
					 Xapian::Query(Xapian::Query::OP_AND_NOT,
						       and_tail,
						       and_not_tail),
					 and_maybe_tail);
		}
	    }

	  case pattern::any_version:
	    return build_xapian_query(p->get_any_version_pattern(),
				      db);

	  case pattern::for_tp:
	    return build_xapian_query(p->get_for_pattern(),
				      db);

	  case pattern::narrow:
	    return build_xapian_query(p->get_narrow_pattern(),
				      db);

	  case pattern::not_tp:
	    // D'oh!  Should I assert-fail here?
	    //
	    // "not" is handled very specially; normalization should
	    // mean it only occurs inside "and" or at the top level.
	    // So I assume that this is a top-level expression; if
	    // "not" occurs at the top-level, we just can't do
	    // anything.
	    return Xapian::Query();

	  case pattern::or_tp:
	    {
	      const std::vector<ref_ptr<pattern> > &sub_patterns =
		p->get_or_patterns();

	      Xapian::Query tail;

	      for(std::vector<ref_ptr<pattern> >::const_reverse_iterator it =
		    sub_patterns.rbegin(); it != sub_patterns.rend(); ++it)
		{
		  if((*it)->get_type() != pattern::not_tp)
		    {
		      Xapian::Query q(build_xapian_query(*it,
							 db));

		      if(!q.empty())
			{
			  if(tail.empty())
			    tail = q;
			  else
			    tail = Xapian::Query(Xapian::Query::OP_OR,
						 q,
						 tail);
			}
		    }
		}

	      return tail;
	    }

	  case pattern::widen:
	    return build_xapian_query(p->get_widen_pattern(),
				      db);

	  case pattern::exact_name:
	    return Xapian::Query("XP" + p->get_exact_name_name());

	  case pattern::term:
	    // We try stemming everything as if it were English.
	    //
	    // TODO: guess which language to use for stemming somehow
	    // (how? the locale isn't reliable; we care about the
	    // language of the package descriptions).
	    return stem_term(p->get_term_term());

	  case pattern::term_prefix:
	    {
	      Xapian::Stem stemmer("en");
	      std::vector<Xapian::Query> subqueries;

	      Xapian::TermIterator prefix_list_end = db.allterms_end(p->get_term_prefix_term());
	      for(Xapian::TermIterator extensionIt = db.allterms_begin(p->get_term_prefix_term());
		  extensionIt != prefix_list_end; ++extensionIt)
		{
		  const std::string extension(*extensionIt);
		  subqueries.push_back(Xapian::Query(extension));
		  subqueries.push_back(Xapian::Query(stemmer(extension)));
		}

	      return Xapian::Query(Xapian::Query::OP_OR,
				   subqueries.begin(),
				   subqueries.end());
	    }

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
	  case pattern::true_tp:
	  case pattern::upgradable:
	  case pattern::user_tag:
	  case pattern::version:
	  case pattern::virtual_tp:
	    return Xapian::Query();
	  default:
	    throw MatchingException("Internal error: unhandled pattern type in build_xapian_query()");
	  }
      }
    }

    ref_ptr<structural_match>
    get_match(const ref_ptr<pattern> &p,
	      const pkgCache::PkgIterator &pkg,
	      const pkgCache::VerIterator &ver,
	      const cwidget::util::ref_ptr<search_cache> &search_info,
	      aptitudeDepCache &cache,
	      pkgRecords &records,
	      bool debug)
    {
      eassert(p.valid());
      eassert(search_info.valid());

      std::vector<matchable> initial_pool;

      if(pkg.VersionList().end())
	initial_pool.push_back(matchable(pkg));
      else if(ver.end())
	{
	  for(pkgCache::VerIterator ver2 = pkg.VersionList();
	      !ver2.end(); ++ver2)
	    {
	      initial_pool.push_back(matchable(pkg, ver2));
	    }
	}
      else
	{
	  eassert(ver.ParentPkg() == pkg);

	  initial_pool.push_back(matchable(pkg, ver));
	}

      std::sort(initial_pool.begin(), initial_pool.end());

      stack st;
      st.push_back(&initial_pool);

      ref_ptr<search_cache::implementation> search_info_imp =
	search_info.dyn_downcast<search_cache::implementation>();
      eassert(search_info_imp.valid());

      return evaluate_structural(structural_eval_any,
				 p,
				 st,
				 search_info_imp,
				 initial_pool,
				 cache,
				 records,
				 debug);
    }

    ref_ptr<structural_match>
    get_match(const ref_ptr<pattern> &p,
	      const pkgCache::PkgIterator &pkg,
	      const cwidget::util::ref_ptr<search_cache> &search_info,
	      aptitudeDepCache &cache,
	      pkgRecords &records,
	      bool debug)
    {
      return get_match(p, pkg,
		       pkgCache::VerIterator(cache),
		       search_info, cache, records, debug);
    }

    void xapian_info::setup(const Xapian::Database &db,
			    const ref_ptr<pattern> &p,
			    bool debug)
    {
      matched_packages_valid = false;

      if(debug)
	std::cout << "Finding Xapian hits for " << serialize_pattern(p) << std::endl;

      ref_ptr<pattern> normalized(normalize_pattern(p));

      if(debug)
	std::cout << "Pattern Xapian-normalized to: " << serialize_pattern(normalized)
		  << (is_xapian_dependent(normalized) ? " [Xapian-dependent]" : " [not Xapian-dependent]")
		  << std::endl;


      Xapian::Query q(build_xapian_query(normalized, db));

      if(q.empty())
	{
	  if(debug)
	    std::cout << "Can't build a Xapian query for this search." << std::endl
		      << "Each incoming package will be tested separately." << std::endl;
	}
      else
	{
	  if(debug)
	    std::cout << "Xapian query built: " << q.get_description() << std::endl;

	  Xapian::Enquire enq(db);
	  enq.set_query(q);

	  xapian_match = enq.get_mset(0, 100000);

	  if(debug)
	    std::cout << "  (" << xapian_match.size() << " hits)"
		      << std::endl;

	  // Read off the matches and stuff them into the list of
	  // matched packages for future reference.
	  for(Xapian::MSetIterator it = xapian_match.begin();
	      it != xapian_match.end(); ++it)
	    matched_packages.push_back(*it);

	  std::sort(matched_packages.begin(), matched_packages.end());

	  matched_packages_valid = true;
	}
    }

    void search(const ref_ptr<pattern> &p,
		const ref_ptr<search_cache> &search_info,
		pkg_results_list &matches,
		aptitudeDepCache &cache,
		pkgRecords &records,
                bool debug,
                const sigc::slot<void, progress_info> &progress_slot)
    {
      try
	{
          progress_slot(progress_info::pulse(_("Accessing index")));

	  eassert(p.valid());
	  eassert(search_info.valid());

	  const ref_ptr<search_cache::implementation> info = search_info.dyn_downcast<search_cache::implementation>();
	  eassert(info.valid());

	  const xapian_info &xapian_results(info->get_toplevel_xapian_info(p, debug));

          const std::string filter_msg = _("Filtering packages");
	  if(!xapian_results.get_matched_packages_valid())
	    {
	      if(debug)
		std::cout << "Failed to build a Xapian query for this search." << std::endl
			  << "Falling back to testing each package." << std::endl;

              progress_info progress = progress_info::bar(0, filter_msg);
              progress_slot(progress);

              int i = 0;
	      for(pkgCache::PkgIterator pkg = cache.PkgBegin();
		  !pkg.end(); ++pkg)
		{
		  if(pkg.VersionList().end() && pkg.ProvidesList().end())
		    continue;

		  // TODO: how do I make sure the sub-patterns are
		  // searched using the right xapian_info?  I could thread
		  // the current top-level or the current xapian_info
		  // through, I suppose.  Or I could use a global list of
		  // term postings and only store match sets on a
		  // per-toplevel basis (that might work, actually?).

		  ref_ptr<structural_match> m(get_match(p, pkg,
							info,
							cache,
							records,
							debug));

		  if(m.valid())
		    matches.push_back(std::make_pair(pkg, m));

                  ++i;
                  progress.set_progress_fraction(((double)i) / ((double)cache.Head().PackageCount));
                  progress_slot(progress);
		}
	    }
	  else
	    {
              // Xapian doesn't allow us to present meaningful
              // progress information.
              progress_slot(progress_info::pulse(filter_msg));

	      Xapian::MSet mset(xapian_results.get_xapian_match());
	      for(Xapian::MSetIterator it = mset.begin();
		  it != mset.end(); ++it)
		{
		  std::string name(it.get_document().get_data());

		  if(debug)
		    std::cout << "HIT: " << name
			      << " (score " << it.get_weight() << ")" << std::endl;

		  pkgCache::PkgIterator pkg(cache.FindPkg(name));
		  if(pkg.end())
		    {
		      if(debug)
			std::cout << "W: unable to find the package " << name
				  << std::endl;
		    }
		  else if(!(pkg.VersionList().end() && pkg.ProvidesList().end()))
		    {
		      ref_ptr<structural_match> m(get_match(p, pkg,
							    info,
							    cache,
							    records,
							    debug));

		      if(m.valid())
			matches.push_back(std::make_pair(pkg, m));
		    }
		}
	    }

          progress_slot(progress_info::none());
	}
      catch(cwidget::util::Exception &e)
	{
	  _error->Error("%s", e.errmsg().c_str());
	}
      catch(std::exception &e)
	{
	  _error->Error("%s", e.what());
	}
      catch(Xapian::Error &e)
	{
	  _error->Error("%s", e.get_msg().c_str());
	}
    }

    void search_versions(const ref_ptr<pattern> &p,
                         const ref_ptr<search_cache> &search_info,
                         ver_results_list &matches,
                         aptitudeDepCache &cache,
                         pkgRecords &records,
                         bool debug,
                         const sigc::slot<void, progress_info> &progress_slot)
    {
      // It's a bit ugly that this is separate from search(), but it's
      // not obvious how to merge them given their different looping
      // requirements.
      try
	{
	  eassert(p.valid());
	  eassert(search_info.valid());

          progress_slot(progress_info::pulse(_("Accessing index")));

	  const ref_ptr<search_cache::implementation> info = search_info.dyn_downcast<search_cache::implementation>();
	  eassert(info.valid());

	  const xapian_info &xapian_results(info->get_toplevel_xapian_info(p, debug));

          const std::string filter_msg = _("Filtering packages");

	  if(!xapian_results.get_matched_packages_valid())
	    {
	      if(debug)
		std::cout << "Failed to build a Xapian query for this search." << std::endl
			  << "Falling back to testing each package." << std::endl;

              progress_info progress = progress_info::bar(0, filter_msg);
              progress_slot(progress);


              int i = 0;
	      for(pkgCache::PkgIterator pkg = cache.PkgBegin();
		  !pkg.end(); ++pkg)
                {
                  for(pkgCache::VerIterator ver = pkg.VersionList();
                      !ver.end(); ++ver)
                    {
                      ref_ptr<structural_match> m(get_match(p,
                                                            pkg, ver,
                                                            info,
                                                            cache,
                                                            records,
                                                            debug));

                      if(m.valid())
                        matches.push_back(std::make_pair(ver, m));
                    }

                  ++i;
                  progress.set_progress_fraction(((double)i) / ((double)cache.Head().PackageCount));
                  progress_slot(progress);
                }
	    }
	  else
	    {
              // Xapian doesn't allow us to present meaningful
              // progress information.
              progress_slot(progress_info::pulse(filter_msg));

	      Xapian::MSet mset(xapian_results.get_xapian_match());
	      for(Xapian::MSetIterator it = mset.begin();
		  it != mset.end(); ++it)
		{
		  std::string name(it.get_document().get_data());

		  if(debug)
		    std::cout << "HIT: " << name
			      << " (score " << it.get_weight() << ")" << std::endl;

		  pkgCache::PkgIterator pkg(cache.FindPkg(name));
		  if(pkg.end())
		    {
		      if(debug)
			std::cout << "W: unable to find the package " << name
				  << std::endl;
		    }
		  else
                    for(pkgCache::VerIterator ver = pkg.VersionList();
                        !ver.end(); ++ver)
                      {
                        ref_ptr<structural_match> m(get_match(p,
                                                              pkg, ver,
                                                              info,
                                                              cache,
                                                              records,
                                                              debug));

                        if(m.valid())
                          matches.push_back(std::make_pair(ver, m));
                      }
		}
	    }

          progress_slot(progress_info::none());
	}
      catch(cwidget::util::Exception &e)
	{
	  _error->Error("%s", e.errmsg().c_str());
	}
      catch(std::exception &e)
	{
	  _error->Error("%s", e.what());
	}
      catch(Xapian::Error &e)
	{
	  _error->Error("%s", e.get_msg().c_str());
	}
    }
  }
}

