// errortab.cc
//
// Copyright 2008 Daniel Burrows
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

#include "errortab.h"

#include <generic/apt/apt.h>

#include <apt-pkg/error.h>

#include <generic/util/util.h>

#include "gui.h"

namespace gui
{
  ErrorStore::ErrorStore()
  {
    Gtk::TreeModelColumnRecord columns;
    columns.add(icon_column);
    columns.add(datetime_column);
    columns.add(text_column);
    store = Gtk::ListStore::create(columns);

    consume_apt_errors();

    consume_errors.connect(sigc::mem_fun(*this, &ErrorStore::consume_apt_errors));
    Glib::signal_timeout().connect(sigc::bind_return(sigc::mem_fun(*this, &ErrorStore::consume_apt_errors),
						     true),
				   5000);
  }

  void ErrorStore::consume_apt_errors()
  {
    bool new_error = false;

    while(!_error->empty())
      {
	std::string msg;
	bool is_error = _error->PopMessage(msg);
	new_error = new_error && is_error;

	Glib::ustring icon = is_error ? Gtk::Stock::DIALOG_ERROR.id
	                              : Gtk::Stock::DIALOG_WARNING.id;

	time_t curtime = time(NULL);
	tm ltime;
	std::string now;
	if(localtime_r(&curtime, &ltime) != NULL)
	  now = sstrftime("%c", &ltime);

	Gtk::TreeModel::iterator it = store->append();
	Gtk::TreeModel::Row row = *it;

	row[icon_column] = icon;
	row[datetime_column] = now;
	row[text_column] = msg;
      }

    if(new_error)
      error_added();
  }

  ErrorTab::ErrorTab(Glib::ustring label,
		     const ErrorStore &store)
    : Tab(Error, label,
	  Gnome::Glade::Xml::create(glade_main_file, "errors_scrolledwindow"),
	  "errors_scrolledwindow")
  {
    get_widget()->set_manage();

    Gtk::TreeView *errors_treeview = NULL;
    get_xml()->get_widget("errors_treeview", errors_treeview);

    {
      Gtk::CellRendererPixbuf *icon_renderer = manage(new Gtk::CellRendererPixbuf);
      Gtk::TreeViewColumn *icon_column = manage(new Gtk::TreeViewColumn("", *icon_renderer));
      icon_column->add_attribute(icon_renderer->property_stock_id(),
				 store.get_icon_column());
      errors_treeview->append_column(*icon_column);
    }

    errors_treeview->append_column("Time", store.get_datetime_column());

    {
      Gtk::CellRendererText *text_renderer = manage(new Gtk::CellRendererText);
      text_renderer->property_ellipsize() = Pango::ELLIPSIZE_END;

      Gtk::TreeViewColumn *error_column = manage(new Gtk::TreeViewColumn("Error", *text_renderer));
      error_column->add_attribute(text_renderer->property_text(), store.get_text_column());
      errors_treeview->append_column(*error_column);
    }
    errors_treeview->set_tooltip_column(store.get_text_column().index());

    errors_treeview->set_model(store.get_model());

    get_widget()->show();
  }

  void ErrorTab::show()
  {
    get_widget()->show_all();
  }
}
