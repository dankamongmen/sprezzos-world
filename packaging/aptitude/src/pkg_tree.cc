// pkg_tree.cc
//
//  Copyright 1999-2005, 2007-2008 Daniel Burrows
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
//  A package-tree displayer.

#include "pkg_tree.h"

#include "aptitude.h"
#include "load_grouppolicy.h"
#include "load_sortpolicy.h"
#include "pkg_columnizer.h"
#include "pkg_grouppolicy.h"
#include "pkg_node.h"
#include "pkg_sortpolicy.h"
#include "pkg_subtree.h"
#include "ui.h"
#include "progress.h"

#include <cwidget/columnify.h>
#include <cwidget/generic/util/transcode.h>
#include <cwidget/toplevel.h>
#include <cwidget/widgets/treeitem.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/matching/match.h>
#include <generic/apt/matching/parse.h>
#include <generic/apt/matching/pattern.h>

#include <apt-pkg/progress.h>
#include <apt-pkg/configuration.h>
#include <apt-pkg/algorithms.h>

#include <sigc++/adaptors/retype_return.h>
#include <sigc++/functors/mem_fun.h>

namespace cw = cwidget;
namespace cwidget
{
  using namespace widgets;
}

namespace matching = aptitude::matching;
using cw::util::ref_ptr;

cw::config::keybindings *pkg_tree::bindings=NULL;

cw::editline::history_list pkg_tree::limit_history, pkg_tree::grouping_history,
  pkg_tree::sorting_history;

void pkg_tree::init_bindings()
{
  bindings=new cw::config::keybindings(cw::tree::bindings);
}

pkg_tree::pkg_tree(const std::string &def_grouping,
		   pkg_grouppolicy_factory *_grouping,
		   const std::wstring &def_limit)
  :initialized(false),
   grouping(_grouping), groupingstr(def_grouping),
   sorting(parse_sortpolicy(aptcfg->Find(PACKAGE "::UI::Default-Sorting",
					 "name"))),
   limit(NULL),
   limitstr(def_limit)
{
  if(!limitstr.empty())
    limit = matching::parse(cw::util::transcode(limitstr));
}

pkg_tree::pkg_tree(const std::string &def_grouping,
		   pkg_grouppolicy_factory *_grouping)
  :initialized(false),
   grouping(_grouping), groupingstr(def_grouping),
   sorting(parse_sortpolicy(aptcfg->Find(PACKAGE "::UI::Default-Sorting",
					 "name"))),
   limit(NULL),
   limitstr(cw::util::transcode(aptcfg->Find(PACKAGE "::Pkg-Display-Limit", "")))
{
  if(!limitstr.empty())
    limit = matching::parse(cw::util::transcode(limitstr));
}

void pkg_tree::handle_cache_close()
{
  set_root(NULL);
}

pkg_tree::~pkg_tree()
{
  delete sorting;
}

void pkg_tree::set_grouping(pkg_grouppolicy_factory *_grouping)
{
  // Reject NULL groupings (should never happen!)
  if(!_grouping)
    return;

  pkg_grouppolicy_factory *oldgrouping=grouping;

  grouping=_grouping;

  build_tree();

  delete oldgrouping;
}

void pkg_tree::set_grouping(const std::wstring &s)
{
  // FIXME: push wstrings down into the parsing code.
  groupingstr=cw::util::transcode(s);

  pkg_grouppolicy_factory *grp=parse_grouppolicy(groupingstr);

  if(grp)
    set_grouping(grp);
}

void pkg_tree::set_sorting(pkg_sortpolicy *_sorting)
{
  delete sorting;

  sorting=_sorting;

  // ummmm
  if(grouping)
    build_tree();
}

void pkg_tree::set_sorting(const std::wstring &s)
{
  // FIXME: push wstrings down into the parsing code.
  pkg_sortpolicy *policy=parse_sortpolicy(cw::util::transcode(s));

  if(policy)
    set_sorting(policy);
}

bool pkg_tree::build_tree(OpProgress &progress)
{
  bool rval;

  if(!initialized)
    {
      cache_closed.connect(sigc::mem_fun(*this, &pkg_tree::handle_cache_close));

      cache_reloaded.connect(sigc::hide_return(sigc::mem_fun<bool, pkg_tree, pkg_tree>(*this, &pkg_tree::build_tree)));

      initialized=true;
    }

  reset_incsearch();

  set_root(NULL);

  reset_incsearch();

  if(grouping && apt_cache_file)
    {
      bool empty=true, cache_empty=true;

      pkg_subtree *mytree=new pkg_subtree(W_("All Packages"), true);
      pkg_grouppolicy *grouper=grouping->instantiate(&selected_signal,
						     &selected_desc_signal);

      mytree->set_depth(-1);

      if(limit.valid())
	{
	  ref_ptr<matching::search_cache> search_info(matching::search_cache::create());

	  std::vector<std::pair<pkgCache::PkgIterator, cwidget::util::ref_ptr<matching::structural_match> > > matches;
	  matching::search(limit, search_info,
			   matches,
			   *apt_cache_file,
			   *apt_package_records);

	  int num = 0;
	  int total = matches.size();

	  for(std::vector<std::pair<pkgCache::PkgIterator, cwidget::util::ref_ptr<matching::structural_match> > >::const_iterator
		it = matches.begin(); it != matches.end(); ++it)
	    {
	      pkgCache::PkgIterator pkg(it->first);

	      cache_empty = false;

	      progress.OverallProgress(num, total, 1, _("Building view"));
	      ++num;

	      // Filter useless packages up-front.
	      if(pkg.VersionList().end() && pkg.ProvidesList().end())
		continue;

	      empty = false;
	      grouper->add_package(pkg, mytree);
	    }

	  progress.OverallProgress(total, total, 1, _("Building view"));
	}
      else
	{
	  int num = 0;
	  int total = (*apt_cache_file)->Head().PackageCount;

	  for(pkgCache::PkgIterator pkg = (*apt_cache_file)->PkgBegin(); !pkg.end(); ++pkg)
	    {
	      cache_empty = false;

	      progress.OverallProgress(num, total, 1, _("Building view"));

	      ++num;

	      // Filter useless packages up-front.
	      if(pkg.VersionList().end() && pkg.ProvidesList().end())
		continue;

	      empty = false;
	      grouper->add_package(pkg, mytree);
	    }

	  progress.OverallProgress(total, total, 1, _("Building view"));
	}

      pkg_sortpolicy_wrapper sorter(sorting);
      mytree->sort(sorter);

      set_root(mytree);

      delete grouper;

      rval=cache_empty || !empty;
    }
  else
    rval=true;

  cw::toplevel::update();

  return rval;
}

bool pkg_tree::build_tree()
{
  progress_ref p=gen_progress_bar();
  bool rval=build_tree(*p->get_progress().unsafe_get_ref());
  p->destroy();

  return rval;
}

void pkg_tree::set_limit(const std::wstring &_limit)
{
  ref_ptr<matching::pattern> old_limit(limit);
  std::wstring old_limitstr(limitstr);

  ref_ptr<matching::pattern> new_limit(matching::parse(cw::util::transcode(_limit)));
  if(_limit.empty() || new_limit.valid())
    {
      limit=new_limit;
      limitstr=_limit;

      if(!build_tree() && !aptcfg->FindB(PACKAGE "::UI::Allow-Unmatched-Limit", false))
	{
	  wchar_t buf[512];

	  swprintf(buf, 512, W_("No packages matched the pattern \"%ls\".").c_str(),
		   _limit.c_str());

	  show_message(buf);
	  limit=old_limit;
	  limitstr=old_limitstr;

	  build_tree();
	}
    }
}

bool pkg_tree::find_limit_enabled()
{
  return get_visible();
}

bool pkg_tree::find_limit()
{
  prompt_string(W_("Enter the new package tree limit: "),
		limitstr,
		cw::util::arg(sigc::mem_fun(*this, &pkg_tree::set_limit)),
		NULL,
		NULL,
		&limit_history);

  return true;
}

bool pkg_tree::find_reset_limit_enabled()
{
  return get_visible() && limit.valid();
}

bool pkg_tree::find_reset_limit()
{
  if(!get_visible())
    return false;

  limit=NULL;
  limitstr=L"";

  build_tree();

  return true;
}

bool pkg_tree::handle_key(const cw::config::key &k)
{
  if(bindings->key_matches(k, "ChangePkgTreeLimit"))
    find_limit();
  else if(bindings->key_matches(k, "ChangePkgTreeGrouping"))
    prompt_string(_("Enter the new package grouping mechanism for this display: "),
		  groupingstr,
		  cw::util::arg(sigc::mem_fun(*this,
				    (void (pkg_tree::*) (const std::wstring &)) &pkg_tree::set_grouping)),
		  NULL,
		  NULL,
		  &grouping_history);
  else if(bindings->key_matches(k, "ChangePkgTreeSorting"))
    prompt_string(_("Enter the new package sorting mechanism for this display: "),
		  "",
		  cw::util::arg(sigc::mem_fun(*this,
				    (void (pkg_tree::*) (const std::wstring &)) &pkg_tree::set_sorting)),
		  NULL,
		  NULL,
		  &sorting_history);
  else
    return menu_tree::handle_key(k);

  return true;
}
