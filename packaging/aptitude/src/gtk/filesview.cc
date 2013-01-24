// -*-c++-*-

// filesview.cpp
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

#include "filesview.h"
#include "aptitude.h"

#include <fstream>
#include <sstream>
//#include <string>

#undef OK
#include <gtkmm.h>

#include <apt-pkg/fileutl.h>
#include <apt-pkg/pkgsystem.h>
#include <apt-pkg/tagfile.h>
#include <apt-pkg/version.h>

#include <cwidget/generic/util/ssprintf.h>

#include <generic/apt/changelog_parse.h>
#include <generic/apt/download_manager.h>
#include <generic/apt/pkg_changelog.h>

#include <gtk/gui.h>
#include <gtk/progress.h>

namespace gui
{
  FilesColumns::FilesColumns()
  {
    add(Type);
    add(File);
  }

  FilesTreeView::FilesTreeView(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade)
  : Gtk::TreeView(cobject)
  {
    ;;
  }

  bool FilesTreeView::on_button_press_event(GdkEventButton* event)
  {
    bool return_value = true;

    if ((event->type == GDK_BUTTON_PRESS) && (event->button == 3))
    {
      Gtk::TreeModel::Path path;
      Gtk::TreeViewColumn *column;
      int cell_x;
      int cell_y;

      if(get_path_at_pos(round(event->x), round(event->y),
                         path, column,
                         cell_x, cell_y))
        {
          // If this row isn't already selected, change the selection
          // to just it.
          Glib::RefPtr<Gtk::TreeView::Selection> selection = get_selection();

          // We could try letting the user expand the selection by
          // holding a shift key down.  I decided not to because I
          // couldn't figure out an obviously right semantics for it,
          // so I figured behaving the same way all the time was the
          // best shot at following the Principle of Least Surprise.
          if(!selection->is_selected(path))
            {
              selection->unselect_all();
              selection->select(path);
            }

          signal_context_menu(event);
        }
    }
    else if ((event->type == GDK_BUTTON_PRESS) && (event->button == 1))
    {
      //Call base class, to allow normal handling,
      //such as allowing the row to be selected by the right-click:
      return_value = Gtk::TreeView::on_button_press_event(event);
      signal_selection();
    }
    else if ((event->type == GDK_2BUTTON_PRESS) && (event->button == 1))
    {
      //Call base class, to allow normal handling,
      //such as allowing the regular signals to be emitted:
      return_value = Gtk::TreeView::on_button_press_event(event);
    }
    return return_value;
  }

  void FilesView::init(Glib::RefPtr<Gnome::Glade::Xml> refGlade,
                        Glib::ustring gladename)
  {
    refGlade->get_widget_derived(gladename, tree);

    tree->signal_context_menu.connect(sigc::mem_fun(*this, &FilesView::context_menu_handler));
    tree->signal_row_activated().connect(sigc::mem_fun(*this, &FilesView::row_activated_handler));
    tree->set_column_drag_function(sigc::mem_fun(*this, &FilesView::column_drop_handler));

    tree->set_search_column(cols.File);

    // TODO: There should be a way to do this in Glade maybe.
    tree->get_selection()->set_mode(Gtk::SELECTION_MULTIPLE);

    // FIXME: Should we be doing this here ?
    store = Gtk::ListStore::create(cols);
    // FIXME: There's an issue here when the cache is reloaded (eg. after an update).
    //        A duplicate "Files" column is appended and the TreeView is wrecked.
    tree->append_column("Type", cols.Type);
    tree->append_column("Files", cols.File);
    tree->set_model(store);
  }

  void FilesView::apply_action_to_selected(FilesAction action)
  {
    Glib::RefPtr<Gtk::TreeModel> model = get_model();
    Glib::RefPtr<Gtk::TreeView::Selection> refSelection = get_treeview()->get_selection();
    if(refSelection)
    {
      Gtk::TreeSelection::ListHandle_Path path_list = refSelection->get_selected_rows();
      std::list<Gtk::TreeModel::iterator> iter_list;
      for (Gtk::TreeSelection::ListHandle_Path::iterator path = path_list.begin();
        path != path_list.end(); path++)
      {
        iter_list.push_back(model->get_iter(*path));
      }
      while (!iter_list.empty())
        {
          Gtk::TreeModel::iterator iter = iter_list.front();
          Glib::ustring filename = (*iter)[cols.File];
          dispatch_action(filename, action);

          iter_list.pop_front();
        }
    }
  }

  void FilesView::dispatch_action(Glib::ustring filename, FilesAction action)
  {
    switch(action)
    {
    case ViewFile:
      // FIXME: do stuff
      break;
    case ViewDirectory:
      // FIXME: do other stuff
      break;
    default:
      break;
    }
  }

  void FilesView::add_action(Glib::ustring type, Glib::ustring filename, std::set<FilesAction> &actions)
  {
    if (type == "d")
    {
      actions.insert(ViewDirectory);
    }
    else
    {
      actions.insert(ViewFile);
    }
  }

  FilesView::FilesView(Glib::RefPtr<Gnome::Glade::Xml> refGlade,
                         Glib::ustring gladename)
  {
    init(refGlade, gladename);
  }

  namespace
  {
    void add_menu_item(Gtk::Menu *menu,
                       Glib::ustring label,
                       Gtk::StockID icon,
                       sigc::slot0<void> callback,
                       bool sensitive = true)
    {
      Gtk::Image *image = manage(new Gtk::Image(icon, Gtk::ICON_SIZE_MENU));
      Gtk::MenuItem *item = manage(new Gtk::ImageMenuItem(*image, label));
      menu->append(*item);

      if (sensitive)
        item->signal_activate().connect(callback);
      else
        item->set_sensitive(false);

      item->show_all();
    }

    // for convenience
    void add_menu_item(Gtk::Menu *menu,
                       Glib::ustring label,
                       Gtk::StockID icon)
    {
      add_menu_item(menu, label, icon, sigc::slot0<void>(), false);
    }
  }

  Gtk::Menu *
  FilesView::get_menu(const std::set<FilesAction> &actions,
                       const sigc::slot1<void, FilesAction> &callback) const
  {
    Gtk::Menu *rval(manage(new Gtk::Menu));

    add_menu_item(rval, "View file", Gtk::Stock::OPEN,
                  sigc::bind(callback, ViewFile));

    add_menu_item(rval, "View directory", Gtk::Stock::OPEN,
                  sigc::bind(callback, ViewDirectory));

    return rval;
  }

  void FilesView::context_menu_handler(GdkEventButton * event)
  {
    Glib::RefPtr<Gtk::TreeModel> model = get_model();
    Glib::RefPtr<Gtk::TreeView::Selection> selected = tree->get_selection();
    if(selected)
      {
        std::set<FilesAction> actions;

        Gtk::TreeSelection::ListHandle_Path selected_rows = selected->get_selected_rows();
        for (Gtk::TreeSelection::ListHandle_Path::iterator path = selected_rows.begin();
             path != selected_rows.end(); ++path)
          {
            Gtk::TreeModel::iterator iter = model->get_iter(*path);
            Glib::ustring type = (*iter)[cols.Type];
            Glib::ustring filename = (*iter)[cols.File];
            add_action(type, filename, actions);
          }

        if(!actions.empty())
          {
            get_menu(actions, sigc::mem_fun(this, &FilesView::apply_action_to_selected))
              ->popup(event->button, event->time);
          }
      }
  }

  bool FilesView::column_drop_handler(Gtk::TreeView *self, Gtk::TreeViewColumn *column,
                                       Gtk::TreeViewColumn *prev_column,
                                       Gtk::TreeViewColumn *next_column)
  {
    return true;
  }

  void FilesView::row_activated_handler(const Gtk::TreeModel::Path & path, Gtk::TreeViewColumn* column)
  {
      //Gtk::TreeModel::iterator iter = get_model()->get_iter(path);
      //Glib::ustring filename = (*iter)[cols.File];
      //activated(path, column, this);
      // TODO: either do something useful, or remove the row activated stuff altogether
  }

  FilesView::FilesView(Gtk::TreeView *_treeview)
  {
  }

  void FilesView::load_version(pkgCache::VerIterator ver)
  {
    store->clear();

    if(ver.end())
       // Assume this means, e.g. a virtual package with no files.
      return;

    if(ver != ver.ParentPkg().CurrentVer())
      {
	Gtk::TreeModel::iterator iter = store->append();
	Gtk::TreeModel::Row row = *iter;
	using cwidget::util::ssprintf;

	if(!ver.ParentPkg().CurrentVer().end())
	  row[cols.File] =
	    ssprintf(_("This file list pertains to the currently installed version \"%s\", not the selected version \"%s\"."),
		     ver.ParentPkg().CurrentVer().VerStr(),
		     ver.VerStr());
      }

    Glib::ustring fileslistname = Glib::ustring("/var/lib/dpkg/info/")
    + Glib::ustring(ver.ParentPkg().Name()) + Glib::ustring(".list");

    std::ifstream fileslist(fileslistname.c_str());

    if(fileslist.fail())
    {
      Gtk::TreeModel::iterator iter = store->append();
      Gtk::TreeModel::Row row = *iter;
      row[cols.File] = _("Files list is only available for installed packages.");
      return;
    }

    std::string filename;

    while(std::getline(fileslist, filename))
    {
      Gtk::TreeModel::iterator iter = store->append();
      Gtk::TreeModel::Row row = *iter;
      if (Glib::file_test(filename, Glib::FILE_TEST_IS_DIR))
      {
        row[cols.Type] = "d";
      }
      else
      {
        row[cols.Type] = "f";
      }
      row[cols.File] = filename;
    }
  }

}
