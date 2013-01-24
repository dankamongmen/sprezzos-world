// pkg_tree.h      -*-c++-*-
//
//  Copyright 1999-2002, 2004-2005, 2008 Daniel Burrows
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


#ifndef PKG_TREE_H
#define PKG_TREE_H

#include "apt_undo_tree.h"

#include <apt-pkg/pkgcache.h>

#include <generic/apt/matching/pattern.h>

/** \brief Uses the cwidget::widgets::tree classes to display a tree containing packages
 *
 * 
 *  Uses the cwidget::widgets::tree classes to display a tree containing packages.  A generic
 *  version of this, suitable only for displaying the current state of the
 *  package cache, is provided; it can be extended as needed.
 * 
 *  \file pkg_tree.h
 */

class OpProgress;

class pkg_grouppolicy;
class pkg_grouppolicy_factory;
class pkg_sortpolicy;
class pkg_tree_node;
class undo_group;

class pkg_tree:public apt_undo_tree
{
  /** If \b true, the tree is fully initialized: in particular,
   *  the cache-reload signals are connected up.
   */
  bool initialized;

  pkg_grouppolicy_factory *grouping;
  std::string groupingstr;
  pkg_sortpolicy *sorting;

  cwidget::util::ref_ptr<aptitude::matching::pattern> limit;
  std::wstring limitstr;
  // Defines the limits on the display (what packages will be allowed
  // to be displayed)  This could be a grouping policy, but hardcoding the
  // filter here makes it easier to alter from the UI.

  static cwidget::widgets::editline::history_list limit_history, grouping_history,
    sorting_history;

  void handle_cache_close();

  /** Set up the limit and handle a few other things. */
  void init(const char *limitstr);
protected:
  virtual bool handle_key(const cwidget::config::key &k);

  pkg_tree(const std::string &groupingstr,
	   pkg_grouppolicy_factory *_grouping,
	   const std::wstring &limitstr);

  pkg_tree(const std::string &groupingstr,
	   pkg_grouppolicy_factory *_grouping);
public:
  /** Initialize a package tree, but don't build it.  The caller
   *  should call build_tree().  The main reason to do this is so you
   *  can connect up the tree's signals before building it.
   */
  static cwidget::util::ref_ptr<pkg_tree>
  create(const std::string &groupingstr,
	 pkg_grouppolicy_factory *grouping,
	 const std::wstring &limitstr)
  {
    cwidget::util::ref_ptr<pkg_tree> rval(new pkg_tree(groupingstr, grouping, limitstr));
    rval->decref();
    return rval;
  }

  /** Initialize a package tree, but don't build it.  The caller
   *  should call build_tree().  The main reason to do this is so you
   *  can connect up the tree's signals before building it.
   *
   *  The default tree limit is used.
   */
  static cwidget::util::ref_ptr<pkg_tree>
  create(const std::string &groupingstr,
	 pkg_grouppolicy_factory *grouping)
  {
    cwidget::util::ref_ptr<pkg_tree> rval(new pkg_tree(groupingstr, grouping));
    rval->decref();
    return rval;
  }

  // Should you be able to just pass in a string (?)
  ~pkg_tree();

  /** Rebuilds the tree.  If the new tree would be empty, keeps the
   *  current tree and returns \b false.
   *
   *  \param progress a progress bar with which to show the progress
   *  of the rebuild.
   *
   *  \return true iff the rebuild was successful.
   */
  bool build_tree(OpProgress &progress);

  /** Rebuilds the tree.  If the new tree would be empty, keeps the
   *  current tree and returns \b false.  Generates a new progress bar
   *  using gen_progress_bar().
   *
   *  \return true iff the rebuild was successful.
   */
  bool build_tree();

  void set_grouping(pkg_grouppolicy_factory *_grouping);
  void set_grouping(const std::wstring &s);
  void set_sorting(pkg_sortpolicy *_sorting);
  void set_sorting(const std::wstring &s);
  void set_limit(const std::wstring &_limit);
  // Selects a new limit and rebuilds the tree.
  std::wstring get_limit_str() {return limitstr;}

  /** Return \b true. */
  bool find_limit_enabled();

  /** Pop up the limit selection dialog. */
  bool find_limit();

  /** Return \b true if a limit is already set. */
  bool find_reset_limit_enabled();

  /** Reset the limit. */
  bool find_reset_limit();

  // Local bindings:
  static cwidget::config::keybindings *bindings;
  static void init_bindings();

  sigc::signal2<void, const pkgCache::PkgIterator &, const pkgCache::VerIterator &> selected_signal;
  sigc::signal1<void, std::wstring> selected_desc_signal;
};

typedef cwidget::util::ref_ptr<pkg_tree> pkg_tree_ref;

#endif
