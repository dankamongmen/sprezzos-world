// cmdline_why.cc                                -*-c++-*-
//
//   Copyright (C) 2007-2010 Daniel Burrows
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

// g++ generates a spurious warning in a call to std::sort below;
// downgrade it to a non-error.
#pragma GCC diagnostic warning "-Wuninitialized"


// Local includes:
#include "cmdline_why.h"

#include "cmdline_common.h"
#include "cmdline_show.h"
#include "cmdline_util.h"
#include "terminal.h"

#include <aptitude.h>
#include <pkg_item.h>
#include <pkg_ver_item.h> // For column formats.
#include <solution_fragment.h> // For dep_targets()

#include <generic/apt/apt.h>
#include <generic/apt/matching/match.h>
#include <generic/apt/matching/parse.h>
#include <generic/apt/matching/pattern.h>

#include <generic/util/immset.h>
#include <generic/util/util.h>

// System includes:
#include <apt-pkg/depcache.h>
#include <apt-pkg/error.h>
#include <apt-pkg/pkgcache.h>
#include <apt-pkg/pkgsystem.h>
#include <apt-pkg/version.h>

#include <boost/make_shared.hpp>

#include <cwidget/fragment.h>

#include <algorithm>
#include <deque>
#include <set>

namespace cw = cwidget;
using aptitude::cmdline::create_terminal;
using aptitude::cmdline::terminal_io;
using aptitude::cmdline::terminal_metrics;
using aptitude::why::make_cmdline_why_callbacks;
using aptitude::why::why_callbacks;
using boost::make_shared;
using boost::shared_ptr;
using cwidget::fragf;
using cwidget::fragment;

using namespace aptitude::matching;

namespace
{
  cw::fragment *print_dep(pkgCache::DepIterator dep)
  {
    if(dep.TargetVer() != NULL)
      return cw::fragf("%s %s %s (%s %s)",
		   dep.ParentPkg().FullName(true).c_str(),
		   dep.DepType(),
		   dep.TargetPkg().FullName(true).c_str(),
		   dep.CompType(),
		   dep.TargetVer());
    else
      return cw::fragf("%s %s %s",
		   dep.ParentPkg().FullName(true).c_str(),
		   dep.DepType(),
		   dep.TargetPkg().FullName(true).c_str());
  }

  // Place weaker dependencies first, then order alphabetically.
  struct compare_pair_by_deptype
  {
    deptype_lt base;
  public:
    bool operator()(const std::pair<std::string, pkgCache::Dep::DepType> &p1,
		    const std::pair<std::string, pkgCache::Dep::DepType> &p2) const
    {
      if(p1.second != p2.second)
	return base(p2.second, p1.second);
      else
	return p1.first < p2.first;
    }
  };
}

namespace aptitude
{
  namespace why
  {
    // Represents a full forward or reverse justification for the
    // installation of the given package/version.  The justification
    // may terminate on either a package version, a provided package
    // name, or the removal of a package.
    class justification
    {
      target the_target;
      imm::set<action> actions;

      justification(const target &_the_target,
		    const imm::set<action> &_actions)
	: the_target(_the_target), actions(_actions)
      {
      }
    public:
      // Create a node with an empty history rooted at the given target.
      justification(const target &_the_target)
	: the_target(_the_target)
      {
      }

      const target &get_target() const
      {
	return the_target;
      }

      const imm::set<action> &get_actions() const
      {
	return actions;
      }

      // Generate all the successors of this node (Q: could I lift
      // justify_target::generate_successors into this class?  It feels
      // like it makes more sense to have the smarts in here)
      void generate_successors(std::deque<justification> &output,
			       const search_params &params,
			       int verbosity,
                               const shared_ptr<why_callbacks> &callbacks) const
      {
	the_target.generate_successors(*this, output, params, verbosity, callbacks);
      }

      // Build a successor of this node.
      justification successor(const target &new_target,
			      const pkgCache::DepIterator &dep) const
      {
	imm::set<action> new_actions(actions);
	new_actions.insert(action(dep, new_actions.size()));

	return justification(new_target, new_actions);
      }

      justification successor(const target &new_target,
			      const pkgCache::PrvIterator &prv) const
      {
	imm::set<action> new_actions(actions);
	new_actions.insert(action(prv, new_actions.size()));

	return justification(new_target, new_actions);
      }

      cwidget::fragment *description() const;
    };

    cw::style action::get_style() const
    {
      pkgCache::PkgIterator pkg;
      if(!dep.end())
	pkg = const_cast<pkgCache::DepIterator &>(dep).ParentPkg();
      else
	pkg = const_cast<pkgCache::PrvIterator &>(prv).OwnerPkg();

      return pkg_item::pkg_style(pkg, false);
    }

    cw::fragment *action::description_column1_fragment() const
    {
      pkgCache::VerIterator ver;
      if(!dep.end())
	ver = const_cast<pkgCache::DepIterator &>(dep).ParentVer();
      else
	ver = const_cast<pkgCache::PrvIterator &>(prv).OwnerVer();

      pkgCache::PkgIterator pkg = ver.ParentPkg();

      // Copy flags from "aptitude search" et al.
      //
      // This is a little crufty; the column formatting needs an overhaul.
      cw::column_disposition flag1 =
	pkg_ver_columnizer::setup_column(ver, true, 0, pkg_item::pkg_columnizer::stateflag);
      cw::column_disposition flag2 =
	pkg_ver_columnizer::setup_column(ver, true, 0, pkg_item::pkg_columnizer::actionflag);
      cw::column_disposition flag3 =
	pkg_ver_columnizer::setup_column(ver, true, 0, pkg_item::pkg_columnizer::autoset);

      std::wstring rval;
      if(flag1.text.size() < 1)
	rval += L' ';
      else
	rval += flag1.text;
      if(flag2.text.size() < 1)
	rval += L' ';
      else
	rval += flag2.text;
      if(flag3.text.size() < 1)
	rval += L' ';
      else
	rval += flag3.text;
      rval += L' ';
      rval += cw::util::transcode(pkg.FullName(true));
      return cw::text_fragment(rval);
    }

    cw::fragment *action::description_column2_fragment() const
    {
      if(!dep.end())
	return cw::text_fragment(const_cast<pkgCache::DepIterator &>(dep).DepType());
      else
	return cw::text_fragment(_("Provides"));
    }

    cw::fragment *action::description_column3_fragment() const
    {
      // Q: can I use a std::string and cw::util::transcode on the way out instead?
      if(!dep.end())
	{
	  pkgCache::DepIterator start, end;
	  surrounding_or(dep, start, end);
	  return cw::text_fragment(dep_targets(start));
	}
      else
	return cw::text_fragment(const_cast<pkgCache::PrvIterator &>(prv).ParentPkg().FullName(true));
    }

    cw::fragment *action::description_fragment() const
    {
      return cw::fragf("%F %F %F",
		       description_column1_fragment(),
		       description_column2_fragment(),
		       description_column3_fragment());
    }

    bool action::operator<(const action &other) const
    {
      typedef pkgCache::Dependency Dependency;
      typedef pkgCache::Provides Provides;

      if(id > other.id)
	return true;
      else if(other.id > id)
	return false;
      else if(dep.end() && !other.dep.end())
	return true;
      else if(other.dep.end() && !dep.end())
	return false;
      else if(!dep.end() && !other.dep.end() &&
	      (const Dependency *)dep < (const Dependency *)other.dep)
	return true;
      else if(!dep.end() && !other.dep.end() &&
	      (const Dependency *)other.dep < (const Dependency *)dep)
	return false;
      else if(prv.end() && !other.prv.end())
	return true;
      else if(other.prv.end() && !prv.end())
	return false;
      else if(!prv.end() && !other.prv.end() &&
	      (const Provides *)prv < (const Provides *)other.prv)
	return true;
      else if(!prv.end() && !other.prv.end() &&
	      (const Provides *)other.prv < (const Provides *)prv)
	return false;
      else
	return false;
    }

    std::wstring search_params::description() const
    {
      std::wstring rval(L"{ ");
      rval += W_("dep_level");
      rval += L" = ";

      switch(dep_level)
	{
	case DependsOnly:
	  rval += W_("DependsOnly");
	  break;
	case Recommends:
	  rval += W_("Recommends");
	  break;
	case Suggests:
	  rval += W_("Suggests");
	  break;
	default:
	  rval += L"???";
	  break;
	}

      rval += L", ";
      rval += W_("version_selection");
      rval += L" = ";
      switch(version_selection)
	{
	case Current:
	  rval += W_("Current");
	  break;
	case Candidate:
	  rval += W_("Candidate");
	  break;
	case Install:
	  rval += W_("Install");
	  break;
	case InstallNotCurrent:
	  rval += W_("InstallNotCurrent");
	  break;
	default:
	  rval += L"???";
	  break;
	}

      rval += L", ";
      rval += W_("allow_choices");
      rval += L" = ";
      if(allow_choices)
	rval += W_("true");
      else
	rval += W_("false");

      rval += L" }";
      return rval;
    }

    cw::fragment *justification_description(const target &t,
                                            const imm::set<action> &actions)
    {
      std::vector<cw::fragment *> rval;
      rval.push_back(cw::fragf("%F\n", t.description()));
      std::vector<cw::fragment *> col1_entries, col2_entries, col3_entries;
      for(imm::set<action>::const_iterator it = actions.begin();
	  it != actions.end(); ++it)
	{
	  col1_entries.push_back(cw::hardwrapbox(cw::fragf("%F | \n", it->description_column1_fragment())));
	  col2_entries.push_back(cw::hardwrapbox(cw::fragf("%F\n", it->description_column2_fragment())));
	  col3_entries.push_back(cw::hardwrapbox(cw::fragf("%F\n", it->description_column3_fragment())));
	}

      using cw::fragment_column_entry;

      std::vector<fragment_column_entry> columns;
      columns.push_back(fragment_column_entry(false, true, 0, fragment_column_entry::top, col1_entries));
      columns.push_back(fragment_column_entry(false, false, 1, fragment_column_entry::top, NULL)),
	columns.push_back(fragment_column_entry(false, true, 0, fragment_column_entry::top, col2_entries));
      columns.push_back(fragment_column_entry(false, false, 1, fragment_column_entry::top, NULL)),
	columns.push_back(fragment_column_entry(false, true, 0, fragment_column_entry::top, col3_entries));
      rval.push_back(fragment_columns(columns));
      return cw::sequence_fragment(rval);
    }

    cw::fragment *justification::description() const
    {
      return justification_description(the_target, actions);
    }

  cw::fragment *target::description() const
  {
    pkgCache::PkgIterator &mpkg = const_cast<pkgCache::PkgIterator &>(pkg);

    switch(type)
      {
      case InstallType:
	return cw::fragf(_("Install(%s)"), mpkg.FullName(true).c_str());
      case RemoveType:
	return cw::fragf(_("Remove(%s)"), mpkg.FullName(true).c_str());
      case ProvidesInstall:
	return cw::fragf(_("Install(%s provides %s)"),
			 const_cast<pkgCache::PrvIterator &>(prv).OwnerPkg().FullName(true).c_str(),
			 mpkg.FullName(true).c_str());
      case ProvidesRemove:
	return cw::fragf(_("Remove(%s provides %s)"),
			 const_cast<pkgCache::PrvIterator &>(prv).OwnerPkg().FullName(true).c_str(),
			 mpkg.FullName(true).c_str());
      default:
	return cw::text_fragment("Internal error: bad target node type.");
      }
  }

  void target::generate_successors(const justification &parent,
				   std::deque<justification> &output,
				   const search_params &params,
				   int verbosity,
                                   const shared_ptr<why_callbacks> &callbacks) const
  {
    why_callbacks * const callbacks_bare = callbacks.get();

    // The reverse successors of an install node are all the revdeps
    // of the package, minus conflicts and deps from versions that
    // aren't selected by the params, plus paths passing through
    // Provides nodes.
    //
    // The successors of a provides node are the same as for an
    // install node, except that we don't look for provided names.
    //
    // The successors of a remove node are the same as for an install
    // node, except that we ONLY take conflicts and we use the
    // candidate version regardless of what params says.
    //
    // Note that there's no need to broaden ORs here; I just care
    // about reaching backwards until I find a leaf node.
    for(pkgCache::DepIterator dep = pkg.RevDependsList(); !dep.end(); ++dep)
      {
	// If we walked through a Provides, we can only look at conflicts.
	if(!params.get_allow_choices() &&
	   !is_conflict(dep->Type) &&
	   is_provides())
	  continue;

	{
	  pkgCache::DepIterator start, end;
	  // Drop ORs if choices are disallowed.  Note that ORs are
	  // meaningless for conflicts, so we ignore them there.
	  if(!params.get_allow_choices() &&
	     !is_conflict(dep->Type))
	    {
	      // Check if we're in an OR by checking whether either
	      // (a) the OR flag is set, or (b) this isn't the first
	      // element of its OR group.  (if the OR flag isn't set,
	      // then either we're the last element of an OR group, or
	      // we aren't part of an OR group)
	      if(dep->CompareOp & pkgCache::Dep::Or)
		continue;

	      surrounding_or(dep, start, end);
	      if(start != dep)
		continue;
	    }
	}

	if(callbacks_bare != NULL)
          callbacks_bare->examining_dep(dep);

	if(is_remove())
	  {
	    // Remove, ProvidesRemove nodes take this.
	    if(!is_conflict(dep->Type))
	      {
                if(callbacks_bare != NULL)
                  callbacks_bare->skip_because_not_a_conflict(dep);
		continue;
	      }
	  }
	else
	  {
	    // Install, ProvidesInstall nodes take this.
	    if(is_conflict(dep->Type))
	      {
                if(callbacks_bare != NULL)
                  callbacks_bare->skip_because_is_a_conflict(dep);
		continue;
	      }
	  }

	if(!params.should_follow_dep(dep))
	  {
            if(callbacks_bare != NULL)
              callbacks_bare->skip_according_to_parameters(dep);
	    continue;
	  }

	if(dep.ParentVer() != params.selected_version(dep.ParentPkg()))
	  {
            if(callbacks_bare != NULL)
              callbacks_bare->skip_because_not_from_selected_version(dep);
	    continue;
	  }

	if(params.get_only_not_current())
	  {
	    bool satisfied_by_current = false;

	    // Skip this dep if it's satisfied by the package's
	    // current version.
	    if(is_provides())
	      {
		pkgCache::VerIterator provider_current = get_provides().OwnerPkg().CurrentVer();

		if(!provider_current.end())
		  {
		    for(pkgCache::PrvIterator prv = provider_current.ProvidesList();
		    !satisfied_by_current && !prv.end(); ++prv)
		      {
			if(dep.TargetVer() == NULL ||
			   (prv.ProvideVersion() != NULL &&
			    _system->VS->CheckDep(prv.ProvideVersion(),
						  dep->CompareOp,
						  dep.TargetVer())))
			  satisfied_by_current = true;
		      }
		  }
	      }
	    else
	      {
		if((*apt_cache_file)[dep.TargetPkg()].Status != 2)
		  {
		    pkgCache::VerIterator current = dep.TargetPkg().CurrentVer();
		    if(!current.end())
		      {
			if(dep.TargetVer() == NULL ||
			   _system->VS->CheckDep(current.VerStr(),
						 dep->CompareOp,
						 dep.TargetVer()))
			  satisfied_by_current = true;
		      }
		  }
	      }

	    if(satisfied_by_current)
	      {
                if(callbacks_bare != NULL)
                  callbacks_bare->skip_because_satisfied_by_current_version(dep);
		continue;
	      }
	  }

	const char *ver_to_check;
	if(is_provides())
	  ver_to_check = get_provides().ProvideVersion();
	else if(is_remove())
	  {
	    pkgCache::VerIterator candver =
	      (*apt_cache_file)[pkg].CandidateVerIter(*apt_cache_file);
	    if(candver.end())
	      ver_to_check = NULL;
	    else
	      ver_to_check = candver.VerStr();
	  }
	else
	  {
	    pkgCache::VerIterator ver = params.selected_version(pkg);
	    if(ver.end())
	      ver_to_check = "";
	    else
	      ver_to_check = params.selected_version(pkg).VerStr();
	  }

	if(dep.TargetVer() == NULL ||
	   (ver_to_check != NULL &&
	    _system->VS->CheckDep(ver_to_check,
				  dep->CompareOp,
				  dep.TargetVer())))
	  {
            if(callbacks_bare != NULL)
              callbacks_bare->enqueued(dep.ParentPkg());
	    target the_target(Install(dep.ParentPkg()));
	    output.push_back(parent.successor(the_target, dep));
	  }
	else
	  {
            if(callbacks_bare != NULL)
              callbacks_bare->skip_because_version_check_failed(dep);
	  }
      }

    if(!is_provides())
      {
	pkgCache::VerIterator ver;
	if(is_remove())
	  ver = (*apt_cache_file)[pkg].CandidateVerIter(*apt_cache_file);
	else
	  ver = params.selected_version(pkg);

	// Walk over the provides declared by the version wrapped by this node.
	if(!ver.end())
	  {
	    for(pkgCache::PrvIterator prv = ver.ProvidesList(); !prv.end(); ++prv)
	      {
                if(callbacks_bare != NULL)
                  callbacks_bare->enqueued(prv);
		target the_target(Provide(prv.ParentPkg(), prv, is_remove()));
		output.push_back(parent.successor(the_target, prv));
	      }
	  }
      }
  }

    namespace
    {
  class justification_search
  {
    // The central queue.  Nodes are inserted at the back and removed
    // from the front.
    std::deque<justification> q;

    std::vector<cwidget::util::ref_ptr<pattern> > leaves;

    cwidget::util::ref_ptr<search_cache> search_info;

    search_params params;

    // Array of flags indicating which packages have been visited.
    bool *seen_packages;

    // Used for debug output.
    bool first_iteration;

    int verbosity;

  public:
    /** \brief Initialize a search for justifications.
     *
     *  \param leaves the point at which to stop searching and signal
     *                success.
     *                These pointers are NOT owner by the search; they
     *                must live as long as the search does, and will
     *                not be freed when it is destroyed.
     *
     *  \param root the root package of the search.
     *
     *  \param search_for_removal if true, the root note is the removal
     *                            of root; otherwise, it is the installation
     *                            of root.
     *
     *  \param params the search parameters: whether to use the current
     *                or the inst ver, and whether to consider
     *                suggests/recommends to be important.
     */
    justification_search(const std::vector<cwidget::util::ref_ptr<pattern> > &_leaves,
			 const target &root,
			 const search_params &_params,
			 int _verbosity)
      : leaves(_leaves),
	search_info(aptitude::matching::search_cache::create()),
	params(_params),
	seen_packages(NULL),
	first_iteration(true),
	verbosity(_verbosity)
    {
      // Prime the pump.
      q.push_back(justification(root));
    }

    justification_search(const justification_search &other)
      : q(other.q),
	leaves(other.leaves),
	params(other.params),
	first_iteration(other.first_iteration),
	verbosity(other.verbosity)
    {
      if(other.seen_packages == NULL)
	seen_packages = NULL;
      else
	{
	  int count = (*apt_cache_file)->Head().PackageCount;
	  seen_packages = new bool[count];
	  for(int i = 0; i < count; ++i)
	    seen_packages[i] = other.seen_packages[i];
	}
    }

    justification_search &operator=(const justification_search &other)
    {
      q = other.q;
      leaves = other.leaves;
      params = other.params;
      if(other.seen_packages == NULL)
	seen_packages = NULL;
      else
	{
	  int count = (*apt_cache_file)->Head().PackageCount;
	  seen_packages = new bool[count];
	  for(int i = 0; i < count; ++i)
	    seen_packages[i] = other.seen_packages[i];
	}
      first_iteration = other.first_iteration;
      verbosity = other.verbosity;
      return *this;
    }

    ~justification_search()
    {
      delete[] seen_packages;
    }

    /** \brief Compute the next output of this search.
     *
     *  \param output a vector whose contents will be replaced with the
     *                results of the search (expressed as a sequence
     *                of actions in the order in which they were
     *                performed).  If no justification is found,
     *                output will be set to an empty list.
     *
     *  \param callbacks  Callbacks to invoke as the search progresses,
     *                    or \b null to invoke nothing.
     *
     *  \return true if a justification was found, false otherwise.
     */
    bool next(std::vector<action> &output,
              const boost::shared_ptr<why_callbacks> &callbacks)
    {
      why_callbacks * const callbacks_bare = callbacks.get();


      std::vector<action> tmp;
      bool reached_leaf = false;

      if(seen_packages == NULL)
	{
	  int count = (*apt_cache_file)->Head().PackageCount;
	  seen_packages = new bool[count];
	  for(int i = 0; i < count; ++i)
	    seen_packages[i] = false;
	}

      if(first_iteration)
	{
          if(callbacks_bare != NULL)
            callbacks_bare->begin(params);
	  first_iteration = false;
	}

      while(!q.empty() && !reached_leaf)
	{
	  using cwidget::util::ref_ptr;

	  // NB: could avoid this copy, but I'd have to move the
	  // pop_front() into all execution branches.  Not worth it.
	  const justification front(q.front());
	  q.pop_front();


          if(callbacks_bare != NULL)
            callbacks_bare->start_target(front.get_target(),
                                         front.get_actions());

	  // If we visited this package already, skip it.  Otherwise,
	  // flag it as visited.
	  pkgCache::PkgIterator frontpkg = front.get_target().get_visited_package();
	  int frontid = frontpkg->ID;
	  bool &package_is_seen = seen_packages[frontid];
	  if(package_is_seen)
	    continue;
	  // Don't flag the starting package as "seen", since we want
	  // to be able to find self-loops.
	  if(!front.get_actions().empty())
	    package_is_seen = true;

	  // If we've stepped at least once, test whether the front
	  // node is a leaf; if it is, return it and quit.
	  //
	  // Checking that we stepped at least once (verified by
	  // checking whether we have any actions in our result list)
	  // ensures that we always return nontrivial answers (i.e.,
	  // even if the target of the search matches a leaf pattern,
	  // we'll keep looking past it).
	  pkgCache::VerIterator frontver = params.selected_version(frontpkg);
	  if(!frontver.end() && !front.get_actions().empty())
	    for(std::vector<ref_ptr<pattern> >::const_iterator it = leaves.begin();
		!reached_leaf && it != leaves.end(); ++it)
	      {
		if(get_match((*it),
			     frontpkg, frontver,
			     search_info,
			     *apt_cache_file,
			     *apt_package_records).valid())
		  reached_leaf = true;
	      }
	  if(reached_leaf)
	    {
	      tmp.insert(tmp.begin(),
			 front.get_actions().begin(),
			 front.get_actions().end());
	    }
	  else
	    // Since this isn't a leaf, stick its successors on the
	    // queue and carry on.
	    front.generate_successors(q,
                                      params,
                                      verbosity,
                                      callbacks);
	}

      output.swap(tmp);
      return reached_leaf;
    }
  };
    }

    bool find_justification(const target &target,
			    const std::vector<cwidget::util::ref_ptr<pattern> > leaves,
			    const search_params &params,
			    bool find_all,
                            const boost::shared_ptr<why_callbacks> &callbacks,
			    std::vector<std::vector<action> > &output)
    {
      justification_search search(leaves, target, params, 0);

      std::vector<std::vector<action> > rval;
      std::vector<action> tmp;

      int i = 0;

      while((i == 0 || find_all) && search.next(tmp, callbacks))
	{
	  rval.push_back(std::vector<action>());
	  rval.back().swap(tmp);
	}

      if(rval.size() > 0)
	{
	  output.swap(rval);
	  return true;
	}
      else
	return false;
    }


    void find_best_justification(const std::vector<cwidget::util::ref_ptr<pattern> > &leaves,
				 const target &goal,
				 bool find_all,
				 int verbosity,
                                 const shared_ptr<why_callbacks> &callbacks,
				 std::vector<std::vector<action> > &output)
    {
      std::vector<search_params> searches;

      // The priority of searches goes like this:
      // (1) install version, depends only
      // (2) current version, depends only
      // (3) install version, recommends or depends
      // (4) current version, recommends or depends
      // (5) install version, recommends or depends or suggests
      // (6) current version, recommends or depends or suggests
      searches.push_back(search_params(search_params::Install,
				       search_params::DependsOnly,
				       false));
      searches.push_back(search_params(search_params::Current,
				       search_params::DependsOnly,
				       false));

      searches.push_back(search_params(search_params::Install,
				       search_params::DependsOnly,
				       true));
      searches.push_back(search_params(search_params::Current,
				       search_params::DependsOnly,
				       true));



      searches.push_back(search_params(search_params::Install,
				       search_params::Recommends,
				       false));
      searches.push_back(search_params(search_params::Current,
				       search_params::Recommends,
				       false));

      searches.push_back(search_params(search_params::Install,
				       search_params::Recommends,
				       true));
      searches.push_back(search_params(search_params::Current,
				       search_params::Recommends,
				       true));





      searches.push_back(search_params(search_params::Install,
				       search_params::Suggests,
				       false));

      searches.push_back(search_params(search_params::Current,
				       search_params::Suggests,
				       false));

      searches.push_back(search_params(search_params::Install,
				       search_params::Suggests,
				       true));

      searches.push_back(search_params(search_params::Current,
				       search_params::Suggests,
				       true));




      // As a last-ditch thing, run searches against candidate versions.
      // We prefer *any* match that sticks to current/future installed versions
      // to this, though.
      searches.push_back(search_params(search_params::Candidate,
				       search_params::DependsOnly,
				       false));
      searches.push_back(search_params(search_params::Candidate,
				       search_params::DependsOnly,
				       true));


      searches.push_back(search_params(search_params::Candidate,
				       search_params::Recommends,
				       false));
      searches.push_back(search_params(search_params::Candidate,
				       search_params::Recommends,
				       true));

      searches.push_back(search_params(search_params::Candidate,
				       search_params::Suggests,
				       false));
      searches.push_back(search_params(search_params::Candidate,
				       search_params::Suggests,
				       true));


      // Throw out completely identical search results.  (note that this
      // might not perfectly eliminate results that appear identical if
      // multiple versions of something are available; needs more work to
      // do that)
      std::set<std::vector<action> > seen_results;
      std::vector<action> results;

      for(std::vector<search_params>::const_iterator it = searches.begin();
	  it != searches.end(); ++it)
	{
	  if(!output.empty() && !find_all)
	    return;

	  justification_search search(leaves, goal, *it, verbosity);

	  while(search.next(results, callbacks))
	    {
	      if(seen_results.find(results) != seen_results.end())
		{
                  if(callbacks.get() != NULL)
                    callbacks->skip_because_already_seen(results);
		}
	      else
		{
		  seen_results.insert(results);

		  if(!results.empty())
		    output.push_back(results);
		}

	      if(!output.empty() && !find_all)
		return;
	    }
	}
    }

    namespace
    {
      cw::fragment *render_reason_columns(const std::vector<std::vector<action> > &solutions,
					  bool show_all)
      {
	std::vector<cw::fragment *> rval;
	bool first = true;
	for(std::vector<std::vector<action> >::const_iterator solutionIt = solutions.begin();
	    solutionIt != solutions.end(); ++solutionIt)
	  {
	    const std::vector<action> &results(*solutionIt);

	    for(std::vector<action>::const_iterator actionIt = results.begin();
		actionIt != results.end(); ++actionIt)
	      {
		if(first)
		  first = false;
		else
		  rval.push_back(cw::newline_fragment());

		int col1_width = 0;
		int col2_width = 0;
		std::vector<cw::fragment *> col1_entries, col2_entries, col3_entries;
		for(std::vector<action>::const_iterator it = results.begin();
		    it != results.end(); ++it)
		  {
		    cw::fragment *col1_fragment = it->description_column1_fragment();
		    cw::fragment *col2_fragment = it->description_column2_fragment();
		    cw::fragment *col3_fragment = it->description_column3_fragment();

		    int this_col1_width = col1_fragment->max_width(0, 0);
		    int this_col2_width = col2_fragment->max_width(0, 0);

		    col1_width = std::max(col1_width, this_col1_width);
		    col2_width = std::max(col2_width, this_col2_width);

		    col1_entries.push_back(cw::hardwrapbox(cw::style_fragment(cw::fragf("%F\n", col1_fragment),
									      it->get_style())));
		    col2_entries.push_back(cw::hardwrapbox(cw::style_fragment(cw::fragf("%F\n", col2_fragment),
									      it->get_style())));
		    col3_entries.push_back(cw::hardwrapbox(cw::style_fragment(cw::fragf("%F\n", col3_fragment),
									      it->get_style())));
		  }

		using cw::fragment_column_entry;

		std::vector<fragment_column_entry> columns;
		columns.push_back(fragment_column_entry(false,
							false,
							col1_width,
							fragment_column_entry::top,
							col1_entries));
		columns.push_back(fragment_column_entry(false,
							false,
							1,
							fragment_column_entry::top,
							NULL));
		columns.push_back(fragment_column_entry(false,
							false,
							col2_width,
							fragment_column_entry::top,
							col2_entries));
		columns.push_back(fragment_column_entry(false,
							false,
							1,
							fragment_column_entry::top,
							NULL));
		columns.push_back(fragment_column_entry(false,
							true,
							0,
							fragment_column_entry::top,
							col3_entries));
		cw::fragment *solution_fragment(cw::fragment_columns(columns));

		if(!show_all)
		  return solution_fragment;
		else
		  rval.push_back(solution_fragment);
	      }
	  }

	return cw::sequence_fragment(rval);
      }
    }
  }
}

namespace aptitude
{
  namespace why
  {
    why_callbacks::~why_callbacks()
    {
    }

    namespace
    {
      class cmdline_why_callbacks : public why_callbacks
      {
        const shared_ptr<terminal_metrics> term_metrics;
        const int verbosity;
        const unsigned int screen_width;

      public:
        cmdline_why_callbacks(const shared_ptr<terminal_metrics> &_term_metrics,
                              const int _verbosity)
          : term_metrics(_term_metrics),
            verbosity(_verbosity),
            screen_width(_term_metrics->get_screen_width())
        {
        }

        void examining_dep(const pkgCache::DepIterator &dep)
        {
          if(verbosity > 1)
            {
              std::auto_ptr<cw::fragment> tmp(cw::fragf(_("    ++ Examining %F\n"), print_dep(dep)));
              std::cout << tmp->layout(screen_width, screen_width, cw::style());
            }
        }

        void skip_because_not_a_conflict(const pkgCache::DepIterator &dep)
        {
          if(verbosity > 1)
            std::cout << _("    ++   --> skipping, not a conflict\n");
        }

        void skip_because_is_a_conflict(const pkgCache::DepIterator &dep)
        {
          if(verbosity > 1)
            std::cout << _("    ++   --> skipping conflict\n");
        }

        void skip_according_to_parameters(const pkgCache::DepIterator &dep)
        {
          if(verbosity > 1)
            std::cout << _("    ++   --> skipping, not relevant according to params\n");
        }

        void skip_because_not_from_selected_version(const pkgCache::DepIterator &dep)
        {
          if(verbosity > 1)
            std::cout << _("    ++   --> skipping, parent is not the selected version\n");
        }

        void skip_because_satisfied_by_current_version(const pkgCache::DepIterator &dep)
        {
          if(verbosity > 1)
            std::cout << _("    ++   --> skipping, the dep is satisfied by the current version\n");
        }

        void skip_because_already_seen(const std::vector<action> &results)
        {
          if(verbosity > 1)
            std::cout << ssprintf(_("Skipping this solution, I've already seen it.\n"));
        }

        void skip_because_version_check_failed(const pkgCache::DepIterator &dep)
        {
          if(verbosity > 1)
            std::cout << _("    ++   --> skipping, version check failed\n");
        }

        void enqueued(const pkgCache::PkgIterator &pkg)
        {
          if(verbosity > 1)
            std::cout << _("    ++   --> ENQUEUING\n");
        }

        void enqueued(const pkgCache::PrvIterator &prv)
        {
          if(verbosity > 1)
            std::cout << ssprintf(_("    ++   --> ENQUEUING %s Provides %s\n"),
                                  const_cast<pkgCache::PrvIterator &>(prv).OwnerPkg().FullName(true).c_str(),
                                  const_cast<pkgCache::PrvIterator &>(prv).ParentPkg().FullName(true).c_str());
        }

        void begin(const search_params &params)
        {
          if(verbosity > 1)
            std::cout << ssprintf(_("Starting search with parameters %ls\n"),
                                  params.description().c_str());
        }

        void start_target(const target &target,
                          const imm::set<action> &actions)
        {
          if(verbosity > 1)
            {
              std::auto_ptr<cw::fragment> desc(justification_description(target, actions));

              std::auto_ptr<cw::fragment> f(cw::fragf("Search for %F\n",
                                                      desc.release()));
              std::cout << f->layout(screen_width,
                                     screen_width,
                                     cw::style());
            }
        }
      };
    }

    shared_ptr<why_callbacks>
    make_cmdline_why_callbacks(const int verbosity,
                               const shared_ptr<terminal_metrics> &term_metrics)
    {
      return make_shared<cmdline_why_callbacks>(term_metrics, verbosity);
    }
  }
}

cw::fragment *do_why(const std::vector<cwidget::util::ref_ptr<pattern> > &leaves,
		 const pkgCache::PkgIterator &root,
		     aptitude::why::roots_string_mode display_mode,
		 int verbosity,
		 bool root_is_removal,
                     const shared_ptr<why_callbacks> &callbacks,
		 bool &success)
{
  using namespace aptitude::why;

  success = true;

  std::vector<std::vector<action> > solutions;
  target goal = root_is_removal ? target::Remove(root) : target::Install(root);

  find_best_justification(leaves, goal,
			  verbosity >= 1,
			  verbosity,
                          callbacks,
			  solutions);

  if(solutions.empty())
    {
      success = false;

      if(root_is_removal)
	return cw::fragf(_("Unable to find a reason to remove %s.\n"), root.FullName(true).c_str());
      else
	return cw::fragf(_("Unable to find a reason to install %s.\n"), root.FullName(true).c_str());
    }
  else if(display_mode == aptitude::why::no_summary)
    return render_reason_columns(solutions, verbosity >= 1);
  else
    {
      // HACK: drop all chains that include a dependency that's less
      // strict than Recommends.  (ideally we should let the user set
      // a level of strictness and conform to that here AND in the
      // search, rather than generating a lot of junk we then throw
      // away)
      std::vector<std::vector<action> > strong_solutions;
      for(std::vector<std::vector<action> >::const_iterator it = solutions.begin();
	  it != solutions.end(); ++it)
	{
	  bool keeper = true;
	  for(std::vector<action>::const_iterator act_it = it->begin();
	      keeper && act_it != it->end(); ++act_it)
	    {
	      if(!act_it->get_dep().end())
		{
		  pkgCache::Dep::DepType type = (pkgCache::Dep::DepType)act_it->get_dep()->Type;

		  if(!(type == pkgCache::Dep::Depends ||
		       type == pkgCache::Dep::PreDepends ||
		       type == pkgCache::Dep::Recommends ||
		       type == pkgCache::Dep::Conflicts))
		    keeper = false;
		}
	    }

	  if(keeper)
	    strong_solutions.push_back(*it);
	}

      std::vector<std::string> lines;
      aptitude::why::summarize_reasons(strong_solutions, display_mode, lines);

      std::vector<cw::fragment *> fragments;
      fragments.push_back(cw::fragf(_("Packages requiring %s:"), root.FullName(true).c_str()));
      fragments.push_back(cw::newline_fragment());

      for(std::vector<std::string>::const_iterator it = lines.begin();
	  it != lines.end(); ++it)
	fragments.push_back(cw::fragf("  %s\n", it->c_str()));

      return sequence_fragment(fragments);
    }
}

int do_why(const std::vector<cwidget::util::ref_ptr<pattern> > &leaves,
	   const pkgCache::PkgIterator &root,
	   aptitude::why::roots_string_mode display_mode,
	   int verbosity,
	   bool root_is_removal,
           const shared_ptr<terminal_metrics> &term_metrics)
{
  bool success = false;
  const shared_ptr<why_callbacks> callbacks =
    make_cmdline_why_callbacks(verbosity, term_metrics);
  std::auto_ptr<cw::fragment> f(do_why(leaves, root, display_mode,
				       verbosity, root_is_removal,
				       callbacks,
                                       success));
  const unsigned int screen_width = term_metrics->get_screen_width();
  // TODO: display each result as we find it.
  std::cout << f->layout(screen_width, screen_width, cw::style());

  return success ? 0 : 1;
}

cw::fragment *do_why(const std::vector<cwidget::util::ref_ptr<pattern> > &leaves,
		 const pkgCache::PkgIterator &root,
		     aptitude::why::roots_string_mode display_mode,
		 bool find_all,
		 bool root_is_removal,
                     const shared_ptr<why_callbacks> &callbacks,
		 bool &success)
{
  const int verbosity = find_all ? 1 : 0;
  return do_why(leaves,
                root,
                display_mode,
                verbosity,
                root_is_removal,
                callbacks,
                success);
}

bool interpret_why_args(const std::vector<std::string> &args,
			std::vector<cwidget::util::ref_ptr<pattern> > &output)
{
  bool parsing_arguments_failed = false;

  for(std::vector<std::string>::const_iterator it = args.begin();
      it != args.end(); ++it)
    {
      cwidget::util::ref_ptr<pattern> p;
      pkgCache::PkgIterator pkg = (*apt_cache_file)->FindPkg(*it);
      if(pkg.end() == false)
        p = pattern::make_exact_name(pkg.Name());
      else if(aptitude::matching::is_pattern(*it) == true)
        p = parse(*it);
      else
        _error->Error(_("Unable to locate package %s"), (*it).c_str());

      if(!p.valid())
	parsing_arguments_failed = true;
      else
	output.push_back(p);
    }

  return !parsing_arguments_failed;
}

cw::fragment *do_why(const std::vector<std::string> &arguments,
		 const std::string &root,
		     aptitude::why::roots_string_mode display_mode,
		 int verbosity,
		 bool root_is_removal,
                     const shared_ptr<why_callbacks> &callbacks,
		 bool &success)
{
  success = false;

  pkgCache::PkgIterator pkg((*apt_cache_file)->FindPkg(root.c_str()));
  if(pkg.end())
    return cw::fragf(_("No package named \"%s\" exists."), root.c_str());

  std::vector<cwidget::util::ref_ptr<pattern> > matchers;
  if(!interpret_why_args(arguments, matchers))
    return cw::text_fragment(_("Unable to parse some match patterns."));

  cw::fragment *rval = do_why(matchers, pkg, display_mode,
			      verbosity, root_is_removal,
                              callbacks,
                              success);

  return rval;
}

cw::fragment *do_why(const std::vector<std::string> &leaves,
		 const std::string &root,
		     aptitude::why::roots_string_mode display_mode,
		 bool find_all,
		 bool root_is_removal,
                     const shared_ptr<why_callbacks> &callbacks,
		 bool &success)
{
  return do_why(leaves, root, display_mode, find_all ? 1 : 0,
		root_is_removal,
                callbacks,
                success);
}

int cmdline_why(int argc, char *argv[],
		const char *status_fname, int verbosity,
		aptitude::why::roots_string_mode display_mode,
		bool is_why_not)
{
  const shared_ptr<terminal_io> term = create_terminal();

  consume_errors();

  if(argc < 2)
    {
      _error->Error(_("%s: this command requires at least one argument (the"
                      " package to query)"),
                    argv[0]);
      return 100;
    }

  OpProgress progress;

  apt_init(&progress, true, status_fname);

  if(_error->PendingError())
    return 100;

  // Keep track of whether any argument couldn't be parsed, but
  // don't bail until we finish parsing, so we can display all
  // the errors we find.
  bool parsing_arguments_failed = false;

  const char *pkgname = argv[argc - 1];
  bool is_removal = is_why_not;
  pkgCache::PkgIterator pkg = (*apt_cache_file)->FindPkg(pkgname);
  if(pkg.end())
    {
      _error->Error(_("Unable to locate package %s"), pkgname);
      parsing_arguments_failed = true;
    }

  std::vector<std::string> arguments;
  for(int i = 1; i + 1 < argc; ++i)
    arguments.push_back(argv[i]);
  std::vector<cwidget::util::ref_ptr<pattern> > matchers;
  if(!interpret_why_args(arguments, matchers))
    parsing_arguments_failed = true;

  if(matchers.empty())
    {
      cwidget::util::ref_ptr<pattern> p =
	pattern::make_and(pattern::make_installed(),
			  pattern::make_not(pattern::make_automatic()));
      if(!p.valid())
	parsing_arguments_failed = true;
      else
	matchers.push_back(p);
    }

  int rval;
  if(parsing_arguments_failed)
    rval = 100;
  else
    rval = do_why(matchers,
                  pkg,
                  display_mode,
                  verbosity,
                  is_removal,
                  term);

  return rval;
}

namespace aptitude
{
  namespace why
  {
    namespace
    {
      // Sort action vectors by the name of the package in the first
      // action and by the chain strength.
      struct compare_first_action
      {
      private:
	/** \return \b true if all the dependencies in the given chain are
	 *  Depends.
	 */
	static bool is_depends_chain(const std::vector<action> &reasons)
	{
	  for(std::vector<action>::const_iterator it = reasons.begin();
	      it != reasons.end(); ++it)
	    {
	      if(!it->get_dep().end() &&
		 it->get_dep()->Type == pkgCache::Dep::Recommends)
		return false;
	    }

	  return true;
	}

	static const char *first_package_name(const std::vector<action> &reasons)
	{
	  for(std::vector<action>::const_iterator it = reasons.begin();
	      it != reasons.end(); ++it)
	    {
	      if(!it->get_dep().end())
		return it->get_dep().ParentPkg().Name();
	    }

	  return "";
	}

      public:
	bool operator()(const std::vector<action> &reason1,
			const std::vector<action> &reason2)
	{
	  const bool first_is_depends(is_depends_chain(reason1));
	  const bool second_is_depends(is_depends_chain(reason2));

	  const char * const package_name1(first_package_name(reason1));
	  const char * const package_name2(first_package_name(reason2));

	  if(!first_is_depends && second_is_depends)
	    return true;
	  else if(first_is_depends && !second_is_depends)
	    return false;
	  else if(reason1.empty())
	    return !reason2.empty();
	  else if(reason2.empty())
	    return false;
	  else
	    return strcmp(package_name1, package_name2) < 0;
	}
      };
    }

    void summarize_reasons(const std::vector<std::vector<action> > &reasons,
			   roots_string_mode mode,
			   std::vector<std::string> &output)
    {
      eassert(mode != no_summary);

      if(mode == show_requiring_packages ||
	 mode == show_requiring_packages_and_strength)
	{
	  deptype_lt deptype_less_than;
	  // Maps root names to strongest dependency type.
	  std::map<std::string, pkgCache::Dep::DepType> roots;
	  for(std::vector<std::vector<action> >::const_iterator it =
		reasons.begin(); it != reasons.end(); ++it)
	    {
	      // Shouldn't happen, but deal anyway.
	      if(it->empty())
		continue; // Generate an internal error here?

	      const action &act(it->front());

	      // This can happen in the case of an impure virtual
	      // package.  e.g., A is manually installed and B provides
	      // A.
	      if(act.get_dep().end())
		continue;

	      if(!act.get_dep().end())
		{
		  const std::string name(act.get_dep().ParentPkg().FullName(true));
		  std::map<std::string, pkgCache::Dep::DepType>::iterator found =
		    roots.find(name);

		  // Find the strongest dependency on the chain.
		  pkgCache::Dep::DepType type = pkgCache::Dep::Suggests;
		  for(std::vector<action>::const_iterator act_it = it->begin();
		      act_it != it->end(); ++act_it)
		    {
		      if(act_it->get_dep().end())
			continue;
		      else
			{
			  pkgCache::Dep::DepType current_type =
			    (pkgCache::Dep::DepType)act_it->get_dep()->Type;
			  if(deptype_less_than(type, current_type))
			    type = current_type;
			}
		    }

		  if(found == roots.end())
		    roots.insert(found, std::make_pair(name, type));
		  else if(deptype_less_than(found->second, type))
		    found->second = type;
		}
	    }

	  if(mode == show_requiring_packages)
	    for(std::map<std::string, pkgCache::Dep::DepType>::const_iterator it = roots.begin();
		it != roots.end(); ++it)
	      output.push_back(it->first);
	  else // if(mode == show_requiring_packages_and_strength)
	    {
	      std::vector<std::pair<std::string, pkgCache::Dep::DepType> >
		packages_by_dep_strength(roots.begin(), roots.end());
              // g++ emits a spurious error here.  Don't know why:
	      std::sort(packages_by_dep_strength.begin(),
			packages_by_dep_strength.end(),
			compare_pair_by_deptype());

	      for(std::vector<std::pair<std::string, pkgCache::Dep::DepType> >::const_iterator it =
		    packages_by_dep_strength.begin();
		  it != packages_by_dep_strength.end(); ++it)
		{
		  if(mode == show_requiring_packages_and_strength)
		    output.push_back(ssprintf("[%s] %s",
					      pkgCache::DepType(it->second),
					      it->first.c_str()));
		  else
		    output.push_back(it->first);
		}
	    }
	}
      else
	{
	  std::vector<std::vector<action> > reasons_copy(reasons);
	  std::sort(reasons_copy.begin(), reasons_copy.end(), compare_first_action());
	  for(std::vector<std::vector<action> >::const_iterator it =
		reasons_copy.begin(); it != reasons_copy.end(); ++it)
	
	    {
	      if(it->empty())
		continue;

	      std::string entry;

	      bool first_action = true;
	      for(std::vector<action>::const_iterator aIt = it->begin();
		  aIt != it->end(); ++aIt)
		{
		  if(!first_action)
		    entry += " ";

		  if(!aIt->get_dep().end())
		    {
		      const pkgCache::DepIterator &dep(aIt->get_dep());

		      if(first_action)
			{
			  entry += const_cast<pkgCache::DepIterator &>(dep).ParentPkg().FullName(true);
			  entry += " ";
			}

		      std::string dep_type = const_cast<pkgCache::DepIterator &>(dep).DepType();
		      entry += cw::util::transcode(cw::util::transcode(dep_type).substr(0, 1));
		      entry += ": ";

		      entry += const_cast<pkgCache::DepIterator &>(dep).TargetPkg().FullName(true);

		      // Show version information if we were asked for it.
		      if(mode == show_chain_with_versions && ((dep->CompareOp & ~pkgCache::Dep::Or) != pkgCache::Dep::NoOp))
			{
			  entry += " (";
			  entry += const_cast<pkgCache::DepIterator &>(dep).CompType();
			  entry += " ";
			  entry += dep.TargetVer();
			  entry += ")";
			}
		    }
		  else
		    {
		      const pkgCache::PrvIterator &prv(aIt->get_prv());
		      eassert(!prv.end());

		      if(first_action)
			{
			  entry += const_cast<pkgCache::PrvIterator &>(prv).OwnerPkg().FullName(true);
			}

		      entry += cw::util::transcode(cw::util::transcode(_("Provides")).substr(0, 1));
		      entry += "<- ";

		      entry += const_cast<pkgCache::PrvIterator &>(prv).ParentPkg().FullName(true);
		      if(mode == show_chain_with_versions && prv.ProvideVersion() != NULL)
			{
			  entry += " (";
			  entry += prv.ProvideVersion();
			  entry += ")";
			}
		    }

		  first_action = false;
		}

	      output.push_back(entry);
	    }
	}
    }
  }
}
