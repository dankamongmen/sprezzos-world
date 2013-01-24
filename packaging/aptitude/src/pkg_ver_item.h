// pkg_ver_item.h (This is -*-c++-*-)
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

#ifndef PKG_VER_ITEM_H
#define PKG_VER_ITEM_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "apt_info_tree.h"

#include "pkg_columnizer.h"
#include "pkg_node.h"
#include "pkg_grouppolicy.h"
#include "pkg_item_with_subtree.h"

#include <apt-pkg/version.h>
#include <apt-pkg/depcache.h>
#include <apt-pkg/pkgsystem.h>

/** \brief Let you create tree nodes which refer to a particular version of a package.
 * 
 *  \file pkg_ver_item.h
 */

class pkg_ver_columnizer:public pkg_item::pkg_columnizer
{
  bool show_pkg_name;

public:
  static cwidget::column_disposition setup_column(const pkgCache::VerIterator &ver,
						  bool show_pkg_name,
						  int basex,
						  int type);
  cwidget::column_disposition setup_column(int type);


  pkg_ver_columnizer(const pkgCache::VerIterator &_ver,
		     bool _show_pkg_name,
		     const cwidget::config::column_definition_list &_columns,
		     int _basex):
    pkg_item::pkg_columnizer(_ver.ParentPkg(), _ver, _columns, _basex),
    show_pkg_name(_show_pkg_name)
  {
  }
};

class pkg_ver_item:public pkg_tree_node
{
  pkgCache::VerIterator version;
  /** Stores the name of this version to avoid problems that can arise
   *  with cwidget::util::transcode().
   */
  std::wstring version_name;

  bool show_pkg_name;

  pkg_signal *sig;

  void do_highlighted_changed(bool highlighted);
public:
  pkg_ver_item(const pkgCache::VerIterator &_version, pkg_signal *_sig,
	       bool _show_pkg_name=false);

  pkgCache::PkgIterator get_package() const {return version.ParentPkg();}
  const pkgCache::VerIterator &get_version() const {return version;}

  virtual cwidget::style get_normal_style();
  virtual cwidget::style get_highlighted_style();
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

  virtual void forbid_version(undo_group *undo);

  void show_information();

  bool dispatch_key(const cwidget::config::key &k, cwidget::widgets::tree *owner);

  pkg_ver_item *get_sig();

  static cwidget::style ver_style(pkgCache::VerIterator version,
				  bool highlighted);


  // Menu redirections:
  bool package_forbid_enabled();
  bool package_forbid();
  bool package_changelog_enabled();
  bool package_changelog();
  bool package_information_enabled();
  bool package_information();
};

class versort:public cwidget::widgets::sortpolicy
{
  pkg_name_lt plt;
public:
  bool operator()(cwidget::widgets::treeitem *item1, cwidget::widgets::treeitem *item2)
  {
    // FIXME: this is horrible.
    const pkg_ver_item *pitem1=dynamic_cast<const pkg_ver_item *>(item1);
    const pkg_ver_item *pitem2=dynamic_cast<const pkg_ver_item *>(item2);

    if(pitem1 && pitem2)
      {
        if(pitem1->get_version().ParentPkg()!=pitem2->get_version().ParentPkg())
          return plt(pitem1->get_version().ParentPkg(),
                     pitem2->get_version().ParentPkg());

        return _system->VS->CmpVersion(pitem1->get_version().VerStr(),
                                       pitem2->get_version().VerStr()) < 0;
      }
    else
      return false; // we shouldn't get here!
  }
};

typedef pkg_item_with_subtree<pkg_ver_item, versort> pkg_vertree;
class pkg_vertree_generic:public cwidget::widgets::subtree<pkg_ver_item, versort>
{
  std::wstring name;
  int num_versions;
public:
  pkg_vertree_generic(std::wstring _name, bool _expanded)
    : cwidget::widgets::subtree<pkg_ver_item, versort>(_expanded),
      name(_name),
      num_versions(0)
  {
  }
  void paint(cwidget::widgets::tree *win, int y, bool hierarchical, const cwidget::style &st);
  const wchar_t *tag() {return name.c_str();}
  const wchar_t *label() {return name.c_str();}
  void inc_num_versions() { ++num_versions; }
};

// The following policy will make each package given to it into a
// subtree containing all versions of the package known to apt-pkg.
class pkg_grouppolicy_ver_factory:public pkg_grouppolicy_factory
{
public:
  virtual pkg_grouppolicy *instantiate(pkg_signal *sig, desc_signal *desc_sig);
};

// The next class displays the available versions of a single package.
// It takes over the screen when instantiated and returns to the previous
// screen when finished.
class pkg_ver_screen:public apt_info_tree
{
protected:
  virtual cwidget::widgets::treeitem *setup_new_root(const pkgCache::PkgIterator &pkg,
				      const pkgCache::VerIterator &ver);
  pkg_ver_screen(const pkgCache::PkgIterator &pkg);
public:
  static cwidget::util::ref_ptr<pkg_ver_screen> create(const pkgCache::PkgIterator &pkg)
  {
    cwidget::util::ref_ptr<pkg_ver_screen> rval(new pkg_ver_screen(pkg));
    rval->decref();
    return rval;
  }
};

typedef cwidget::util::ref_ptr<pkg_ver_screen> pkg_ver_screen_ref;

void setup_package_versions(const pkgCache::PkgIterator &pkg, pkg_vertree *tree, pkg_signal *sig);
void setup_package_versions(const pkgCache::PkgIterator &pkg, pkg_vertree_generic *tree, pkg_signal *sig);
// Adds all versions of a package to the given tree.

#endif
