// menu_tree.cc
//
//   Copyright (C) 2005, 2007-2008 Daniel Burrows
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

#include "menu_tree.h"

#include "aptitude.h"

#include "pkg_item.h"
#include "pkg_tree.h" // For pkg_tree::bindings(?!?!)
#include "pkg_ver_item.h"
#include "solution_item.h"
#include "ui.h"
#include "view_changelog.h"

#include <generic/apt/apt.h>
#include <generic/apt/apt_undo_group.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/matching/match.h>
#include <generic/apt/matching/parse.h>
#include <generic/apt/matching/pattern.h>

#include <generic/util/undo.h>

#include <sigc++/adaptors/bind.h>

namespace cw = cwidget;
namespace cwidget
{
  using namespace widgets;
}
namespace matching = aptitude::matching;
using cwidget::util::ref_ptr;

cw::editline::history_list menu_tree::search_history;

class pattern_search:public cw::tree_search_func
{
  ref_ptr<matching::pattern> pattern;
  ref_ptr<matching::search_cache> cache;
public:
  pattern_search(const ref_ptr<matching::pattern> &_pattern,
		 const ref_ptr<matching::search_cache> &_cache)
    : pattern(_pattern),
      cache(matching::search_cache::create())
  {
  }

  bool operator()(const cw::treeitem &item)
  {
    // EWW
    const pkg_item *pitem=dynamic_cast<const pkg_item *>(&item);
    if(pitem)
      return matching::get_match(pattern,
				 pitem->get_package(),
				 cache,
				 *apt_cache_file,
				 *apt_package_records).valid();
    else {
      const pkg_ver_item *pvitem=dynamic_cast<const pkg_ver_item *>(&item);

      if(pvitem)
	return matching::get_match(pattern,
				   pvitem->get_package(),
				   pvitem->get_version(),
				   cache,
				   *apt_cache_file,
				   *apt_package_records).valid();
      else
	return false;
    }
  }
};

menu_tree::menu_tree()
  :last_search_pattern(), doing_incsearch(false),
   pre_incsearch_selected(get_end())
{
  aptcfg->connect(PACKAGE "::UI::Incremental-Search",
		  sigc::mem_fun(*this, &menu_tree::do_cancel_incsearch));
}

menu_tree::~menu_tree()
{
}

bool menu_tree::proxy_redirect(bool (menu_redirect::*call)())
{
  if(!get_visible())
    return false;

  cw::treeiterator curr = get_selection();
  if(curr == get_end())
    return false;

  menu_redirect *proxied = dynamic_cast<menu_redirect *>(&*curr);
  if(proxied == NULL)
    return false;

  return (proxied->*call)();
}

bool menu_tree::package_enabled()
{
  return proxy_redirect(&menu_redirect::package_enabled);
}

bool menu_tree::package_install()
{
  return proxy_redirect(&menu_redirect::package_install);
}

bool menu_tree::package_remove()
{
  return proxy_redirect(&menu_redirect::package_remove);
}

bool menu_tree::package_purge()
{
  return proxy_redirect(&menu_redirect::package_purge);
}

bool menu_tree::package_keep()
{
  return proxy_redirect(&menu_redirect::package_keep);
}

bool menu_tree::package_hold()
{
  return proxy_redirect(&menu_redirect::package_hold);
}

bool menu_tree::package_mark_auto()
{
  return proxy_redirect(&menu_redirect::package_mark_auto);
}

bool menu_tree::package_unmark_auto()
{
  return proxy_redirect(&menu_redirect::package_unmark_auto);
}

bool menu_tree::package_forbid_enabled()
{
  return proxy_redirect(&menu_redirect::package_forbid_enabled);
}

bool menu_tree::package_forbid()
{
  return proxy_redirect(&menu_redirect::package_forbid);
}

bool menu_tree::package_changelog_enabled()
{
  return proxy_redirect(&menu_redirect::package_changelog_enabled);
}

bool menu_tree::package_changelog()
{
  return proxy_redirect(&menu_redirect::package_changelog);
}

bool menu_tree::package_information_enabled()
{
  return proxy_redirect(&menu_redirect::package_information_enabled);
}

bool menu_tree::package_information()
{
  return proxy_redirect(&menu_redirect::package_information);
}

bool menu_tree::resolver_toggle_rejected()
{
  return proxy_redirect(&menu_redirect::resolver_toggle_rejected);
}

bool menu_tree::resolver_toggle_rejected_enabled()
{
  return proxy_redirect(&menu_redirect::resolver_toggle_rejected_enabled);
}

bool menu_tree::resolver_toggle_approved()
{
  return proxy_redirect(&menu_redirect::resolver_toggle_approved);
}

bool menu_tree::resolver_toggle_approved_enabled()
{
  return proxy_redirect(&menu_redirect::resolver_toggle_approved_enabled);
}

bool menu_tree::resolver_view_target()
{
  return proxy_redirect(&menu_redirect::resolver_view_target);
}

bool menu_tree::resolver_view_target_enabled()
{
  return proxy_redirect(&menu_redirect::resolver_view_target_enabled);
}



bool menu_tree::find_search_enabled()
{
  return get_visible();
}

bool menu_tree::find_search()
{
  prompt_string(W_("Search for: "),
		last_search_term,
		cw::util::arg(sigc::bind(sigc::mem_fun(*this, &menu_tree::do_search), false)),
		cw::util::arg(sigc::mem_fun(*this, &menu_tree::do_cancel_incsearch)),
		cw::util::arg(sigc::bind(sigc::mem_fun(*this, &menu_tree::do_incsearch), false)),
		&search_history);

  return true;
}

bool menu_tree::find_search_back_enabled()
{
  return get_visible();
}

bool menu_tree::find_search_back()
{
  prompt_string(W_("Search backwards for: "),
		last_search_term,
		cw::util::arg(sigc::bind(sigc::mem_fun(*this, &menu_tree::do_search), true)),
		cw::util::arg(sigc::mem_fun(*this, &menu_tree::do_cancel_incsearch)),
		cw::util::arg(sigc::bind(sigc::mem_fun(*this, &menu_tree::do_incsearch), true)),
		&search_history);

  return true;
}

bool menu_tree::find_research_enabled()
{
  return last_search_pattern.valid();
}

bool menu_tree::find_research()
{
  if(last_search_pattern.valid())
    {
      pattern_search searcher(last_search_pattern,
			      matching::search_cache::create());
      if(last_search_backwards)
	search_back_for(searcher);
      else
	search_for(searcher);

      return true;
    }
  else
    {
      beep();
      return true;
    }

}


bool menu_tree::find_repeat_search_back_enabled()
{
  return last_search_pattern.valid();
}

bool menu_tree::find_repeat_search_back()
{
  if(last_search_pattern.valid())
    {
      pattern_search searcher(last_search_pattern,
			      matching::search_cache::create());
      if(!last_search_backwards)
	search_back_for(searcher);
      else
	search_for(searcher);

      return true;
    }
  else
    {
      beep();
      return true;
    }
}

bool menu_tree::find_limit_enabled()
{
  return false;
}

bool menu_tree::find_limit()
{
  return false;
}

bool menu_tree::find_reset_limit_enabled()
{
  return false;
}

bool menu_tree::find_reset_limit()
{
  return false;
}

bool menu_tree::find_broken_enabled()
{
  return get_visible();
}

bool menu_tree::find_broken()
{
  if(!get_visible())
    return false;

  do_search(L"~b", false);

  return true;
}

void menu_tree::do_search(std::wstring s, bool backward)
{
  if(s.size()!=0)
    {
      last_search_term=s;
      last_search_pattern = matching::parse(cw::util::transcode(s));
    }

  if(doing_incsearch)
    doing_incsearch=false;
  else
    {
      if(last_search_term.size() != 0 && last_search_pattern.valid())
	{
	  last_search_backwards = backward;

	  pattern_search searcher(last_search_pattern,
				  matching::search_cache::create());
	  if(backward)
	    search_back_for(searcher);
	  else
	    search_for(searcher);
	}
      else
	beep();
    }
}

void menu_tree::do_incsearch(std::wstring s, bool backward)
{
  if(!aptcfg->FindB(PACKAGE "::UI::Incremental-Search", true))
    return;

  if(!doing_incsearch)
    {
      doing_incsearch=true;
      pre_incsearch_selected=get_selection();
    }

  ref_ptr<matching::pattern> p = matching::parse(cw::util::transcode(s), false, true, true);

  set_selection(pre_incsearch_selected);

  if(p.valid())
    {
      pattern_search searcher(p,
			      matching::search_cache::create());
      last_search_backwards = backward;
      if(backward)
	search_back_for(searcher);
      else
	search_for(searcher);
    }
}

void menu_tree::do_cancel_incsearch()
{
  if(doing_incsearch)
    {
      set_selection(pre_incsearch_selected);
      doing_incsearch=false;
    }
}

void menu_tree::reset_incsearch()
{
  doing_incsearch=false;
  pre_incsearch_selected=get_end();
}

bool menu_tree::handle_key(const cw::config::key &k)
{
  // ick -- but having our own bindings is also ugly. hm.
  if(pkg_tree::bindings->key_matches(k, "Search"))
    find_search();
  else if(pkg_tree::bindings->key_matches(k, "SearchBack"))
    find_search_back();
  else if(pkg_tree::bindings->key_matches(k, "ReSearch"))
    find_research();
  else if(pkg_tree::bindings->key_matches(k, "RepeatSearchBack"))
    find_repeat_search_back();
  else if(pkg_tree::bindings->key_matches(k, "SearchBroken"))
    find_broken();
  else
    return cw::tree::handle_key(k);

  return true;
}
