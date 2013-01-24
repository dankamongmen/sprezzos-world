// filesview.h             -*-c++-*-
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

#ifndef FILESVIEW_H_
#define FILESVIEW_H_

#undef OK
#include <gtkmm.h>
#include <libglademm/xml.h>

#include <apt-pkg/pkgcache.h>
#include <apt-pkg/acquire.h>

#include <cwidget/generic/util/ref_ptr.h>

#include <generic/util/refcounted_base.h>
#include <generic/util/temp.h>
#include <generic/util/util.h>
#include <set>
using namespace std;

namespace gui
{
  enum FilesAction
  {
    ViewFile, ViewDirectory
  };

  class FilesColumns : public Gtk::TreeModel::ColumnRecord
  {
    public:
      Gtk::TreeModelColumn<Glib::ustring> Type;
      Gtk::TreeModelColumn<Glib::ustring> File;

      FilesColumns();
  };

  class FilesTreeView : public Gtk::TreeView
  {
    public:
      FilesTreeView(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade);
      bool on_button_press_event(GdkEventButton* event);
      sigc::signal<void, GdkEventButton*> signal_context_menu;
      sigc::signal<void> signal_selection;
  };

  class FilesView : public aptitude::util::refcounted_base_threadsafe
  {
    private:
      Glib::RefPtr<Gtk::ListStore> store;
      FilesTreeView * tree;
      FilesColumns cols;

      void init(Glib::RefPtr<Gnome::Glade::Xml> refGlade,
                Glib::ustring gladename);

      Gtk::TreeViewColumn * Type;
      Gtk::TreeViewColumn * File;

      /** \brief Construct a new files view.
       *
       *  \param treeview   The tree-view to attach to.
       */
      FilesView(Gtk::TreeView *treeview);

      /** \brief Build a menu of file actions. */
      Gtk::Menu * get_menu(const std::set<FilesAction> &actions, const sigc::slot1<void, FilesAction> &callback) const;

      /** \brief Apply the given action to all the currently selected files. */
      void apply_action_to_selected(FilesAction action);

      void dispatch_action(Glib::ustring filename, FilesAction action);
      void add_action(Glib::ustring type, Glib::ustring filename, std::set<FilesAction> &actions);

      void context_menu_handler(GdkEventButton * event);
      /** \brief Enforces constraints on column order. */
      bool column_drop_handler(Gtk::TreeView *self, Gtk::TreeViewColumn *column,
                               Gtk::TreeViewColumn *prev_column,
                               Gtk::TreeViewColumn *next_column);
      void row_activated_handler(const Gtk::TreeModel::Path &, Gtk::TreeViewColumn*);

    public:
      static cwidget::util::ref_ptr<FilesView> create(Gtk::TreeView *treeview)
      {
        return new FilesView(treeview);
      }

      void load_version(pkgCache::VerIterator ver);

      /** \brief Construct a new files view.
       *
       *  \param refGlade    The XML tree containing
       *                     the widgets for this view.
       *  \param gladename   The Glade name of the widget.
       */
      FilesView(Glib::RefPtr<Gnome::Glade::Xml> refGlade,
                               Glib::ustring gladename);


      FilesTreeView * get_treeview() const { return tree; };
      const FilesColumns * get_columns() const { return &cols; };
      Glib::RefPtr<Gtk::TreeModel> get_model() const { return get_treeview()->get_model(); };
  };

}

#endif /* FILESVIEW_H_ */
