// pkg_subtree.h (this is -*-c++-*-)
//
//  Copyright 1999-2002, 2004-2005, 2007-2008 Daniel Burrows
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

#ifndef PKG_SUBTREE_H
#define PKG_SUBTREE_H

#include <cwidget/widgets/subtree.h>

#include "pkg_node.h"

/** \brief A subtree which contains packages (and other subtrees)
 * 
 *  \file pkg_subtree.h
 */

class pkg_subtree:public cwidget::widgets::subtree<pkg_tree_node>,
		  public pkg_tree_node
{
  std::wstring name;
  std::wstring description; // This is like a Description: field.

  sigc::signal1<void, std::wstring> *info_signal;
  pkg_subtree *num_packages_parent;

  bool num_packages_known;
  int num_packages;

  void do_highlighted_changed(bool highlighted);
protected:
  void set_label(const std::wstring &_name) {name=_name;}
public:
  pkg_subtree(std::wstring _name, std::wstring _description=L"",
	      sigc::signal1<void, std::wstring> *_info_signal=NULL,
	      bool _expanded=false):
    cwidget::widgets::subtree<pkg_tree_node>(_expanded), name(_name),
    description(_description), info_signal(_info_signal),
    num_packages_parent(NULL),
    num_packages_known(true), num_packages(0)
  {
    highlighted_changed.connect(sigc::mem_fun(this, &pkg_subtree::do_highlighted_changed));
  }

  pkg_subtree(std::wstring _name, bool _expanded):
    cwidget::widgets::subtree<pkg_tree_node>(_expanded), name(_name),
    description(L""), info_signal(NULL),
    num_packages_parent(NULL),
    num_packages_known(true), num_packages(0)
  {
    highlighted_changed.connect(sigc::mem_fun(this, &pkg_subtree::do_highlighted_changed));
  }

  virtual void paint(cwidget::widgets::tree *win, int y, bool hierarchical,
		     const cwidget::style &st);
  virtual const wchar_t *tag();
  virtual const wchar_t *label();

  virtual void select(undo_group *undo);
  virtual void hold(undo_group *undo);
  virtual void keep(undo_group *undo);
  virtual void remove(undo_group *undo);
  virtual void purge(undo_group *undo);
  virtual void reinstall(undo_group *undo);
  virtual void set_auto(bool isauto, undo_group *undo);

  /** \brief Set the parent of this tree for the purposes of package counting.
   *
   *  When inc_num_packages() is called on this tree, it's also called
   *  on the parent.
   */
  void set_num_packages_parent(pkg_subtree *new_parent)
  {
    num_packages_parent = new_parent;
  }

  /** \brief Increment the number of packages in this tree and
   *  in the parent (if any).
   *
   *  Has no effect if num_packages_known is false.
   */
  void inc_num_packages();
  /** \brief Discard all information about how many packages
   *  this subtree contains.
   */
  void clear_num_packages();
  /** \brief Set the number of packages that this subtree
   *  contains.
   */
  void set_num_packages(int num);
  bool get_num_packages_known() const { return num_packages_known; }
  int get_num_packages() const { return num_packages; }
  std::wstring get_name() {return name;}
  std::wstring get_description() {return description;}

  bool dispatch_key(const cwidget::config::key &k, cwidget::widgets::tree *owner);
};

#endif
