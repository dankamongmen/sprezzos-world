// edit_pkg_hier.h           -*-c++-*-
//
//  Copyright 2001, 2005 Daniel Burrows
//
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

#ifndef EDIT_PKG_HIER
#define EDIT_PKG_HIER

#include <generic/apt/pkg_hier.h>

#include <apt-pkg/pkgcache.h>

#include <cwidget/widgets/tree.h>

/** \brief Provides a convenient way to tweak the package hierarchy
 *
 * 
 *  This provides a convenient way to tweak the package hierarchy.  It lets
 *  the user select which 'groups' a package appears in, then allows them to
 *  save the changes to a file.
 *  
 *  Changes to the hierarchy are stored in GLOBAL VARIABLES.  Specifically,
 *  the user_pkg_hier global variable.
 * 
 *  \file edit_pkg_hier.h
 */

class hier_editor : public cwidget::widgets::tree
{
  class hier_item;

  // Ugh..track all the children we created..
  std::vector<hier_item *> items;

  pkg_hier::item *item;

  // Used for the "lazily load package hierarchies" behavior -- if we
  // get a "set package" command and this widget is invisible, it gets
  // deferred until the widget is shown.  This connection is used to
  // avoid connecting this command more than once.
  sigc::connection shown_conn;

  void save_hier(std::string file);

  void handle_reload();
protected:
  virtual bool handle_key(const cwidget::config::key &k);

  void paint(const cwidget::style &st);

  hier_editor();
public:
  static cwidget::util::ref_ptr<hier_editor> create()
  {
    cwidget::util::ref_ptr<hier_editor> rval(new hier_editor);
    rval->decref();
    return rval;
  }

  bool get_cursorvisible();

  void set_package(const pkgCache::PkgIterator &pkg);

  void set_package(const pkgCache::PkgIterator &pkg,
		   const pkgCache::VerIterator &ver);

  void set_package(std::string name);

  sigc::signal0<void> commit_changes;
};

typedef cwidget::util::ref_ptr<hier_editor> hier_editor_ref;

#endif
