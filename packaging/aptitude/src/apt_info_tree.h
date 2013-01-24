// apt_info_tree.h    -*-c++-*-
//
//  Copyright 2000-2005 Daniel Burrows
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

#ifndef APT_INFO_TREE_H
#define APT_INFO_TREE_H

#include "apt_undo_tree.h"

#include <apt-pkg/pkgcache.h>
#include <string>

/** \brief Abstracts a tree which displays information about a particular package
 *
 * 
 *  Abstracts a tree which displays information about a particular package (and
 *  optionally version).  The only thing that's special about this class is
 *  that it can sanely restore its state after the apt cache has been flushed.
 *
 *  IMPORTANT NOTE: this assumes that simply deleting its root will be enough
 *  to avoid segfaults while the cache is not valid.  Children that need more
 *  assurances should connect to cache_closed themselves.  (FIXME: add a hook
 *  in this class to avoid spiralling numbers of signal connections?  Good or bad
 *  idea?)
 * 
 *  \file apt_info_tree.h
 */

class apt_info_tree:public apt_undo_tree
{
  std::string package, version;
  // The package that should be used as the root, and the version to use
  // Stored so we can cleanly handle list updates

  sigc::signal2<void, const pkgCache::PkgIterator &, const pkgCache::VerIterator &> sig;
  sigc::signal1<void, std::wstring> desc_sig;

  void handle_cache_close();
protected:
  void restore_state();
  virtual cwidget::widgets::treeitem *setup_new_root(const pkgCache::PkgIterator &pkg,
				      const pkgCache::VerIterator &ver)=0;
public:
  sigc::signal2<void, const pkgCache::PkgIterator &, const pkgCache::VerIterator &> *get_sig() {return &sig;}
  sigc::signal1<void, std::wstring> *get_desc_sig() {return &desc_sig;}

  apt_info_tree(const std::string &_package, const std::string &_version);

  /** Repeats the 'highlighted' signal of the currently selected item.
   *  Call this after connecting the two signals above.
   */
  void repeat_signal();
};

#endif
