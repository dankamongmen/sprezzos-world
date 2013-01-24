// tab.cc
//
//  Copyright 1999-2009 Daniel Burrows
//  Copyright 2008-2009 Obey Arthur Liu
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

#include "tab.h"
#include "aptitude.h"
#include "loggers.h"

#undef OK
#include <gtkmm.h>
#include <libglademm/xml.h>

#include <gtk/gui.h>
#include <gtk/notify.h>
#include <gtk/util/property.h>

#include <iostream>

using aptitude::Loggers;

namespace gui
{
  namespace
  {
    /** \brief The named property we use to attach a tab pointer to
     *  its main widget for inspection or deletion.
     */
    property<Tab *> tab_property("aptitude-tab-manager-tab-object");
  }

  Tab::Tab(TabType _type, const Glib::ustring &_label,
           const Glib::RefPtr<Gnome::Glade::Xml> &_xml, const std::string &widgetName,
	   bool _autodestroy)
    : type(_type), label(_label),
      xml(_xml), widget(NULL),
      autodestroy(_autodestroy)
  {
    // This is a hack to reparent the tab widget into a VBox along with a NotifyView
    Gtk::VBox * vbox = manage(new Gtk::VBox());
    xml->get_widget(widgetName, widget);
    vbox->pack_start(*widget, true, true);
    Glib::RefPtr<Gnome::Glade::Xml> glade_notify = Gnome::Glade::Xml::create(glade_main_file, "main_notify_rows");
    glade_notify->get_widget_derived("main_notify_rows", notifyview);
    vbox->pack_start(*notifyview, false, true);
    widget->show();
    widget = vbox;

    label_widget = manage(new Gtk::HBox());
    label_widget->set_spacing(6);
    label_widget->show();

    label_label = manage(new Gtk::Label(_("Download")));
    label_label->show();
    label_widget->pack_start(*label_label);

    label_button = manage(new Gtk::Button());
    {
      Gtk::Image *label_button_image = manage(new Gtk::Image(Gtk::Stock::CLOSE,
							     Gtk::ICON_SIZE_MENU));
      label_button_image->show();
      label_button->add(*label_button_image);
    }

    label_button->set_name("notebook_close_button");
    label_button->property_can_focus() = true;
    label_button->property_receives_default() = true;
    label_button->set_relief(Gtk::RELIEF_NONE);
    label_button->show();
    label_widget->pack_start(*label_button);

    // Maybe we should create a close() method on the Tab so it can clean itself up or make a destructor.
    label_button->signal_clicked().connect(close_clicked.make_slot());

    tab_property.set_on(get_widget(), this);

    if (_label != "")
    {
      label_label->set_text(_label);
    }
    else
    {
      label_label->set_text("generic tab: " + label);
    }
  }

  Tab::~Tab()
  {
  }

  void Tab::set_active(bool now_active)
  {
    active = now_active;
    active_changed();
  }

  void Tab::set_label(Glib::ustring label)
  {
    this->label_label->set_text(label);
  }

  int TabsManager::next_position(TabType type)
  {
    // TODO: implement something more elaborate and workflow-wise intuitive
    return get_n_pages();
  }

  int TabsManager::number_of(TabType type)
  {
    int count = 0;

    for(int i = 0; i < get_n_pages(); ++i)
      {
	Gtk::Widget *page = get_nth_page(i);
	Tab *tab = tab_property.get_from(page);

	if(tab != NULL && tab->get_type() == type)
	  ++count;
      }

    return count;
  }

  std::set<PackagesAction> Tab::get_package_menu_actions()
  {
    return std::set<PackagesAction>();
  }

  void Tab::dispatch_package_menu_action(PackagesAction action)
  {
  }

  bool Tab::get_undo_available()
  {
    return false;
  }

  void Tab::dispatch_undo()
  {
  }

  bool Tab::get_edit_columns_available()
  {
    return false;
  }

  void Tab::dispatch_edit_columns()
  {
  }

  TabsManager::TabsManager(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade) :
    Gtk::Notebook(cobject)
  {
    signal_page_removed().connect(sigc::mem_fun(this, &TabsManager::page_removed));
    signal_switch_page().connect(sigc::mem_fun(this, &TabsManager::do_switch_page));

    last_active_page = get_current_page();
  }

  int TabsManager::append_page(Tab &tab)
  {
    {
      Tab *claimed_tab = tab_property.get_from(tab.get_widget());
      LOG_DEBUG(Loggers::getAptitudeGtkTabs(),
		"Appending page: tab object " << (&tab)
		<< ", tab property " << (&claimed_tab)
		<< ", tab widget " << tab.get_widget());
    }

    int rval = 0;
    switch (tab.get_type())
      {
    case Dashboard:
      // No more than one Dashboard at once
      if (number_of(Dashboard) == 0)
      {
        rval = insert_page(*(tab.get_widget()), *(tab.get_label_widget()), 0);
      }
      break;
      // TODO: handle other kinds of tabs
    default:
      rval = insert_page(*(tab.get_widget()), *(tab.get_label_widget()), next_position(tab.get_type()));
      }

    tab.close_clicked.connect(sigc::bind(sigc::mem_fun(*this, &TabsManager::maybe_close_page),
					 sigc::ref(tab)));

    return rval;
  }

  void TabsManager::maybe_close_page(Tab &tab)
  {
    if(tab.get_autodestroy())
      {
	LOG_TRACE(Loggers::getAptitudeGtkTabs(),
		  "Removing the autodestroy page " << (&tab));
	remove_page(tab);
      }
    else
      {
	LOG_TRACE(Loggers::getAptitudeGtkTabs(),
		  "Hiding the non-autodestroy page " << (&tab));
	tab.get_widget()->hide();
      }
  }

  void TabsManager::remove_page(Tab &tab)
  {
    LOG_DEBUG(Loggers::getAptitudeGtkTabs(),
	      "Removing the tab " << (&tab));
    Gtk::Notebook::remove_page(*(tab.get_widget()));
  }

  void TabsManager::maybe_close_current_page()
  {
    Gtk::Widget *current = get_nth_page(get_current_page());
    if(current != NULL)
    {
      Tab * tab = tab_property.get_from(current);
      if(tab != NULL)
	{
	  if(tab->get_autodestroy())
	    {
	      LOG_TRACE(Loggers::getAptitudeGtkTabs(),
			"Closing the current tab: widget pointer is "
			<< current << ", tab pointer is "
			<< tab);
	      remove_page(*tab);
	    }
	  else
	    {
	      LOG_TRACE(Loggers::getAptitudeGtkTabs(),
			"Hiding the current tab: widget pointer is "
			<< current << ", tab pointer is "
			<< tab);
	      current->hide();
	    }
	}
      else
	LOG_TRACE(Loggers::getAptitudeGtkTabs(),
		  "Not closing the current page: it has no associated tab object.");
    }
    else
      LOG_TRACE(Loggers::getAptitudeGtkTabs(),
		"Not closing the current page: it is NULL.");
  }

  void TabsManager::page_removed(Gtk::Widget *widget, int page)
  {
    LOG_TRACE(Loggers::getAptitudeGtkTabs(),
	      "Tab closed: page number " << page
	      << ", widget " << widget);
    if(widget != NULL)
      {
	Tab *tab = tab_property.get_from(widget);

	LOG_DEBUG(Loggers::getAptitudeGtkTabs(),
		  "The tab " << tab << " has been closed.");
	if(tab != NULL)
	  tab->closed();

	LOG_TRACE(Loggers::getAptitudeGtkTabs(),
		  "Deleting the tab " << tab);

	delete tab;
      }

    last_active_page = get_current_page();
  }

  Tab *TabsManager::get_current_tab()
  {
    Gtk::Widget *current = get_nth_page(get_current_page());
    if(current != NULL)
      return tab_property.get_from(current);
    else
      return NULL;
  }

  void TabsManager::do_switch_page(GtkNotebookPage *page, guint page_idx)
  {
    const int previous_page = last_active_page;
    last_active_page = page_idx;

    LOG_TRACE(Loggers::getAptitudeGtkTabs(),
	      "Switching from tab " << previous_page
	      << " to tab " << page_idx);

    package_menu_actions_changed_connection.disconnect();
    undo_available_changed_connection.disconnect();
    edit_columns_available_changed_connection.disconnect();

    LOG_TRACE(Loggers::getAptitudeGtkTabs(), "Making the old tab inactive.");

    Gtk::Widget *current_widget = get_nth_page(previous_page);
    if (current_widget != NULL)
    {
      Tab *current = tab_property.get_from(current_widget);
      if(current != NULL)
        current->set_active(false);
    }

    LOG_TRACE(Loggers::getAptitudeGtkTabs(),
	      "Setting up connections for the new tab.");

    Tab *tab = NULL;
    Widget *next = get_nth_page(page_idx);
    if(next != NULL)
      {
	tab = tab_property.get_from(next);
	package_menu_actions_changed_connection =
	  tab->package_menu_actions_changed.connect(package_menu_actions_changed.make_slot());
	undo_available_changed_connection =
	  tab->undo_available_changed.connect(undo_available_changed.make_slot());
	edit_columns_available_changed_connection =
	  tab->edit_columns_available_changed.connect(edit_columns_available_changed.make_slot());
      }

    LOG_TRACE(Loggers::getAptitudeGtkTabs(), "Making the new tab active.");

    if(tab != NULL)
      tab->set_active(true);
    tab_status_button_changed(tab);

    LOG_TRACE(Loggers::getAptitudeGtkTabs(), "Updating menu sensitivity.");

    package_menu_actions_changed();
    undo_available_changed();
    edit_columns_available_changed();
  }
}
