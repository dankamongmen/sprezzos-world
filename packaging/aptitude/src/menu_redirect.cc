// menu_redirect.cc                                   -*-c++-*-
//
//   Copyright (C) 2005, 2007 Daniel Burrows
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

#include "menu_redirect.h"

#include "ui.h"

#include <sigc++/bind.h>

#include <cwidget/generic/util/ref_ptr.h>
#include <cwidget/widgets/container.h>

namespace cw = cwidget;
namespace cwidget
{
  using namespace widgets;
}

bool menu_redirect::undo_undo_enabled() { return false; }
bool menu_redirect::undo_undo() { return false; }
bool menu_redirect::package_enabled() { return false; }
bool menu_redirect::package_install() { return false; }
bool menu_redirect::package_remove() { return false; }
bool menu_redirect::package_purge() { return false; }
bool menu_redirect::package_hold() { return false; }
bool menu_redirect::package_keep() { return false; }
bool menu_redirect::package_mark_auto() { return false; }
bool menu_redirect::package_unmark_auto() { return false; }
bool menu_redirect::package_forbid_enabled() { return false; }
bool menu_redirect::package_forbid() { return false; }
bool menu_redirect::package_information_enabled() { return false; }
bool menu_redirect::package_information() { return false; }
bool menu_redirect::package_changelog_enabled() { return false; }
bool menu_redirect::package_changelog() { return false; }
bool menu_redirect::resolver_toggle_approved() { return false;}
bool menu_redirect::resolver_toggle_approved_enabled() { return false; }
bool menu_redirect::resolver_toggle_rejected() { return false;}
bool menu_redirect::resolver_toggle_rejected_enabled() { return false; }
bool menu_redirect::resolver_view_target() { return false; }
bool menu_redirect::resolver_view_target_enabled() { return false; }
bool menu_redirect::find_search_enabled() { return false; }
bool menu_redirect::find_search() { return false; }
bool menu_redirect::find_search_back_enabled() { return false; }
bool menu_redirect::find_search_back() { return false; }
bool menu_redirect::find_research_enabled() { return false; }
bool menu_redirect::find_research() { return false; }
bool menu_redirect::find_repeat_search_back_enabled() { return false; }
bool menu_redirect::find_repeat_search_back() { return false; }
bool menu_redirect::find_limit_enabled() { return false; }
bool menu_redirect::find_limit() { return false; }
bool menu_redirect::find_reset_limit_enabled() { return false; }
bool menu_redirect::find_reset_limit() { return false; }
bool menu_redirect::find_broken_enabled() { return false; }
bool menu_redirect::find_broken() { return false; }

static bool do_menu_callback(cw::widget &viewBare,
			     menu_redirect *redirect,
			     bool (menu_redirect::* action)())
{
  cw::widget_ref view(&viewBare);
  cw::util::ref_ptr<cw::container> owner = view->get_owner();

  while(owner.valid())
    {
      if(owner->get_active_widget() != view)
	return false;

      view = owner;
      owner = view->get_owner();
    }

  return (redirect->*action)();
}

void create_menu_bindings(menu_redirect *menu_handler,
			  const cw::util::ref_ptr<cw::widget> &valve)
{
  undo_undo_enabled.connect(sigc::bind(ptr_fun(do_menu_callback),
				       valve.weak_ref(),
				       menu_handler,
				       &menu_redirect::undo_undo_enabled));

  undo_undo.connect(sigc::bind(ptr_fun(do_menu_callback),
			       valve.weak_ref(),
			       menu_handler,
			       &menu_redirect::undo_undo));

  package_menu_enabled.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
					  valve.weak_ref(),
					  menu_handler,
					  &menu_redirect::package_enabled));

  package_install.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
				     valve.weak_ref(),
				     menu_handler,
				     &menu_redirect::package_install));

  package_remove.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
				    valve.weak_ref(),
				    menu_handler,
				    &menu_redirect::package_remove));

  package_purge.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
				   valve.weak_ref(),
				   menu_handler,
				   &menu_redirect::package_purge));

  package_hold.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
				  valve.weak_ref(),
				  menu_handler,
				  &menu_redirect::package_hold));

  package_keep.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
				  valve.weak_ref(),
				  menu_handler,
				  &menu_redirect::package_keep));

  package_mark_auto.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
				       valve.weak_ref(),
				       menu_handler,
				       &menu_redirect::package_mark_auto));

  package_unmark_auto.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
					 valve.weak_ref(),
					 menu_handler,
					 &menu_redirect::package_unmark_auto));

  package_forbid_enabled.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
					    valve.weak_ref(),
					    menu_handler,
					    &menu_redirect::package_forbid_enabled));

  package_forbid.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
				    valve.weak_ref(),
				    menu_handler,
				    &menu_redirect::package_forbid));

  package_information_enabled.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
						 valve.weak_ref(),
						 menu_handler,
						 &menu_redirect::package_information_enabled));

  package_information.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
					 valve.weak_ref(),
					 menu_handler,
					 &menu_redirect::package_information));

  package_changelog_enabled.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
					       valve.weak_ref(),
					       menu_handler,
					       &menu_redirect::package_changelog_enabled));

  package_changelog.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
				       valve.weak_ref(),
				       menu_handler,
				       &menu_redirect::package_changelog));


  resolver_toggle_rejected_enabled.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
						      valve.weak_ref(),
						      menu_handler,
						      &menu_redirect::resolver_toggle_rejected_enabled));

  resolver_toggle_rejected.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
					      valve.weak_ref(),
					      menu_handler,
					      &menu_redirect::resolver_toggle_rejected));

  resolver_toggle_approved_enabled.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
						      valve.weak_ref(),
						      menu_handler,
						      &menu_redirect::resolver_toggle_approved_enabled));

  resolver_toggle_approved.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
					      valve.weak_ref(),
					      menu_handler,
					      &menu_redirect::resolver_toggle_approved));

  resolver_view_target_enabled.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
						  valve.weak_ref(),
						  menu_handler,
						  &menu_redirect::resolver_view_target_enabled));

  resolver_view_target.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
					  valve.weak_ref(),
					  menu_handler,
					  &menu_redirect::resolver_view_target));


  find_search_enabled.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
					 valve.weak_ref(),
					 menu_handler,
					 &menu_redirect::find_search_enabled));

  find_search_back_enabled.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
					      valve.weak_ref(),
					      menu_handler,
					      &menu_redirect::find_search_back_enabled));

  find_search.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
				 valve.weak_ref(),
				 menu_handler,
				 &menu_redirect::find_search));

  find_search_back.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
				      valve.weak_ref(),
				      menu_handler,
				      &menu_redirect::find_search_back));

  find_research_enabled.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
					   valve.weak_ref(),
					   menu_handler,
					   &menu_redirect::find_research_enabled));

  find_research.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
				   valve.weak_ref(),
				   menu_handler,
				   &menu_redirect::find_research));

  find_limit_enabled.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
					valve.weak_ref(),
					menu_handler,
					&menu_redirect::find_limit_enabled));

  find_limit.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
				valve.weak_ref(),
				menu_handler,
				&menu_redirect::find_limit));

  find_cancel_limit_enabled.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
					       valve.weak_ref(),
					       menu_handler,
					       &menu_redirect::find_reset_limit_enabled));

  find_cancel_limit.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
				       valve.weak_ref(),
				       menu_handler,
				       &menu_redirect::find_reset_limit));

  find_broken_enabled.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
					 valve.weak_ref(),
					 menu_handler,
					 &menu_redirect::find_broken_enabled));

  find_broken.connect(sigc::bind(sigc::ptr_fun(do_menu_callback),
				 valve.weak_ref(),
				 menu_handler,
				 &menu_redirect::find_broken));
}
