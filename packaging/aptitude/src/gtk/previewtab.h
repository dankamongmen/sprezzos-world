// -*-c++-*-

// previewtab.h
//
//  Copyright 1999-2009 Daniel Burrows
//  Copyright 2008 Obey Arthur Liu
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

#ifndef PREVIEWTAB_H_
#define PREVIEWTAB_H_

#undef OK
#include <gtkmm.h>

#include <apt-pkg/pkgcache.h>

#include <gtk/tab.h>

#include <cwidget/generic/util/ref_ptr.h>

#include "pkgview.h"

namespace gui
{
  class Notification;
  class PackageSearchEntry;

  /** \brief A tree-view that displays a preview of which actions are
   *  to be performed.
   */
  class PreviewView : public PkgViewBase
  {
  public:
    class Generator : public PkgTreeModelGenerator
    {
      Glib::RefPtr<Gtk::TreeStore> store;
      const EntityColumns *entity_columns;

      // \todo Swiped from pkg_grouppolicy_mode; should be pushed into
      // low-level code.
      const static char * const child_names[];

      std::map<int, Gtk::TreeStore::iterator> state_trees;
    public:
      Generator(const EntityColumns *columns);
      static Generator *create(const EntityColumns *columns);

      void add(const pkgCache::PkgIterator &pkg);
      void finish();
      Glib::RefPtr<Gtk::TreeModel> get_model();
    };

    PreviewView(const Glib::RefPtr<Gnome::Glade::Xml> &refGlade,
		const Glib::ustring &gladename,
		const Glib::ustring &limit,
		const sigc::slot<cwidget::util::ref_ptr<refcounted_progress> > &build_progress_k);
  };

  class PreviewTab : public Tab
  {
    private:
      cwidget::util::ref_ptr<PreviewView> pPkgView;
      Gtk::TextView * pPackagesTextView;

      cwidget::util::ref_ptr<PackageSearchEntry> pSearchEntry;

    void limit_changed(const cwidget::util::ref_ptr<aptitude::matching::pattern> &limit);

    public:
      PreviewTab(const Glib::ustring &label);

      void activated_package_handler();
      void display_desc(const cwidget::util::ref_ptr<Entity> &ent);
      const cwidget::util::ref_ptr<PreviewView> &get_pkg_view() const { return pPkgView; };
  };

}

#endif /* PREVIEWTAB_H_ */
