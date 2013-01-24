// menu_tree.h                                      -*-c++-*-
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
//

#ifndef MENU_TREE
#define MENU_TREE

#include "menu_redirect.h"

#include <cwidget/widgets/editline.h>

#include <cwidget/widgets/tree.h>

#include <generic/apt/matching/pattern.h>

/** \brief A cwidget::widgets::tree augmented with the ability to act as a menu redirector.
 * 
 *  \file menu_tree.h
 */

class pkg_tree_node;
class solution_item;
class undo_group;

/** A cwidget::widgets::tree that can be generically used as a menu redirector.  All
 *  the menu redirection routines are filled in; case analysis on the
 *  currently selected tree item is used to implement them.  In
 *  addition, the tree will pop up a search dialog in response to
 *  either the Search menu command or the corresponding keybinding.
 *
 *  \todo eliminate some of the dynamic_casting by making
 *  pkg_tree_node and solution_item subclasses of menu_redirect; then
 *  you can just cast the active item to a menu_redirect and proxy for
 *  it.
 */
class menu_tree:public cwidget::widgets::tree, public menu_redirect
{
  /** Proxy the given call to the currently selected item, if it
   *  implements the menu_redirect interface.
   */
  bool proxy_redirect(bool (menu_redirect::*)());

  /** Return the selected node, if any, or \b NULL if no node is selected. */
  pkg_tree_node *pkg_node_selection();

  /** Return the selected solution item, if any, or \b NULL if no
   *  solution is selected.
   */
  solution_item *solution_selection();

  /** A precompiled pattern representing the last search that was performed. */
  cwidget::util::ref_ptr<aptitude::matching::pattern> last_search_pattern;

  /** The string that was compiled to produce the above pattern. */
  std::wstring last_search_term;

  /** If \b true, the last search was a backwards search. */
  bool last_search_backwards;

  /** \b true if an incremental search is in progress. */
  bool doing_incsearch;

  /** The iterator that was selected prior to the incremental search. */
  cwidget::widgets::treeiterator pre_incsearch_selected;

  void do_search(std::wstring s, bool backward);
  void do_incsearch(std::wstring s, bool backward);
  void do_cancel_incsearch();

  /** Execute the given action; this is needed because some "wrapper"
   *  code is used to handle undo and so on.
   */
  bool package_action(void (pkg_tree_node::* action)(undo_group *));

  /** \return \b true if a package or a package version is selected. */
  bool pkg_or_ver_selected();

  static cwidget::widgets::editline::history_list search_history;
protected:
  /** Reset all information about the incremental search.  This must be
   *  performed whenever the root is changed.
   */
  void reset_incsearch();

  menu_tree();
public:
  static cwidget::util::ref_ptr<menu_tree> create()
  {
    cwidget::util::ref_ptr<menu_tree> rval(new menu_tree);
    rval->decref();
    return rval;
  }

  ~menu_tree();

  /** \return \b true iff a pkg_node descendant is currently selected. */
  bool package_enabled();

  /** If a pkg_node is currently selected, execute its "install" operation. */
  bool package_install();

  /** If a pkg_node is currently selected, execute its "remove" operation. */
  bool package_remove();

  /** If a pkg_node is currently selected, execute its "purge" operation. */
  bool package_purge();

  /** If a pkg_node is currently selected, execute its "keep" operation. */
  bool package_keep();

  /** If a pkg_node is currently selected, execute its "hold" operation. */
  bool package_hold();

  /** If a pkg_node is currently selected, execute its "set auto" operation. */
  bool package_mark_auto();

  /** If a pkg_node is currently selected, execute its "set manual" operation. */
  bool package_unmark_auto();

  /** \return \b true if a package or a package version is selected. */
  bool package_forbid_enabled();

  /** If a package or a version is selected, perform a "forbid"
   *  operation on it.
   */
  bool package_forbid();

  /** \return \b true if a package or a package version is selected. */
  bool package_changelog_enabled();

  /** If a package or version is selected, show its changelog. */
  bool package_changelog();

  /** \return \b true if a package or a package version is selected. */
  bool package_information_enabled();

  /** If a package or version is selected, show its information. */
  bool package_information();


  /** If a solution item is selected, toggle whether it is rejected. */
  bool resolver_toggle_rejected();

  /** \return \b true if a solution item is selected. */
  bool resolver_toggle_rejected_enabled();

  /** If a solution item is selected, toggle whether it is approved. */
  bool resolver_toggle_approved();

  /** \return \b true if a solution item is selected. */
  bool resolver_toggle_approved_enabled();

  /** If a solution item is selected, view its target. */
  bool resolver_view_target();

  /** \return \b true if a solution item is selected. */
  bool resolver_view_target_enabled();


  /** \return \b true; all package trees know how to search. */
  bool find_search_enabled();

  /** \return \b true; all package trees know how to search. */
  bool find_search_back_enabled();

  /** Execute the 'search' menu command. */
  bool find_search();

  /** Execute the 'search backwards' menu command. */
  bool find_search_back();

  /** \return \b true if there is a "previous search". */
  bool find_research_enabled();

  /** Execute the 're-search' menu command. */
  bool find_research();

  /** \return \b true if there is a "previous search". */
  bool find_repeat_search_back_enabled();

  /** Execute the 'repeat search in opposite direction' menu command. */
  bool find_repeat_search_back();

  /** \return \b false. */
  bool find_limit_enabled();

  /** Does nothing. */
  bool find_limit();

  /** \return \b false. */
  bool find_reset_limit_enabled();

  /** Does nothing. */
  bool find_reset_limit();

  /** \return \b true if this view is active. */
  bool find_broken_enabled();

  /** Find the next broken package (searches for '~b'). */
  bool find_broken();

  bool handle_key(const cwidget::config::key &k);
};

typedef cwidget::util::ref_ptr<menu_tree> menu_tree_ref;

#endif
