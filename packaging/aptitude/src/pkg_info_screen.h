// pkg_info_screen.h     -*-c++-*-
//
//  Copyright 2000, 2005 Daniel Burrows
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

#ifndef PKG_INFO_SCREEN_H
#define PKG_INFO_SCREEN_H

#include "pkg_grouppolicy.h"

#include "apt_info_tree.h"

#include <apt-pkg/cacheiterators.h>
#include <string>

/** \brief Gathers package information and provides code to display it
 *
 * 
 *  Gathers information about a package into one
 *  spot (pkg_grouppolicy_info_factory) and provides dedicated code to display
 *  it (pkg_info_screen)
 * 
 *  \file pkg_info_screen.h
 */

class pkg_grouppolicy_info_factory:public pkg_grouppolicy_factory
{
public:
  pkg_grouppolicy *instantiate();
};

class pkg_info_screen:public apt_info_tree
{
protected:
  cwidget::widgets::treeitem *setup_new_root(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver);
  pkg_info_screen(const pkgCache::PkgIterator &pkg, const pkgCache::VerIterator &ver);
public:
  static cwidget::util::ref_ptr<pkg_info_screen>
  create(const pkgCache::PkgIterator &pkg,
	 const pkgCache::VerIterator &ver)
  {
    cwidget::util::ref_ptr<pkg_info_screen> rval(new pkg_info_screen(pkg, ver));
    rval->decref();
    return rval;
  }
};

typedef cwidget::util::ref_ptr<pkg_info_screen> pkg_info_screen_ref;

#endif
