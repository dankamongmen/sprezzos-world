// dep_item.h (This is -*-c++-*-)
//
//  Copyright 1999, 2000, 2001, 2005 Daniel Burrows
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

#ifndef DEP_ITEM_H
#define DEP_ITEM_H

#include <cwidget/widgets/subtree.h>
#include "apt_info_tree.h"

#include "pkg_grouppolicy.h"
#include "pkg_item.h"
#include "pkg_subtree.h"
#include "pkg_item_with_subtree.h"

/** \brief A dependency chart for a package.
 * 
 *  \file dep_item.h
 */

// Inserts subtrees ("Depends", "Recommends", etc) for the package's
// dependencies into the given tree.  You can template it to use either
// pkg_tree or pkg_item_with_subtree.
template<class treetype>
void setup_package_deps(const pkgCache::PkgIterator &pkg,
			const pkgCache::VerIterator &ver,
			treetype *tree,
			pkg_signal *sig,
			bool reverse=false);

extern template void setup_package_deps<pkg_subtree>(const pkgCache::PkgIterator &pkg, const pkgCache::VerIterator &ver, pkg_subtree *tree, pkg_signal *sig, bool reverse);

// Each package added to the following policy will expand to a tree listing its
// dependencies.
class pkg_grouppolicy_dep_factory:public pkg_grouppolicy_factory
{
public:
  virtual pkg_grouppolicy *instantiate(pkg_signal *sig, desc_signal *desc_sig);
};

class pkg_dep_screen:public apt_info_tree
{
  bool reverse;
protected:
  cwidget::widgets::treeitem *setup_new_root(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver);

  pkg_dep_screen(const pkgCache::PkgIterator &pkg,
		 const pkgCache::VerIterator &ver,
		 bool _reverse=false);
public:
  static cwidget::util::ref_ptr<pkg_dep_screen>
  create(const pkgCache::PkgIterator &pkg,
	 const pkgCache::VerIterator &ver,
	 bool reverse = false)
  {
    cwidget::util::ref_ptr<pkg_dep_screen> rval(new pkg_dep_screen(pkg, ver, reverse));
    rval->decref();
    return rval;
  }

  virtual ~pkg_dep_screen() {}
};

typedef cwidget::util::ref_ptr<pkg_dep_screen> pkg_dep_screen_ref;

#endif
