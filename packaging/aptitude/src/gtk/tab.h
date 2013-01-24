// -*-c++-*-

// tab.h
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

#ifndef TAB_H_
#define TAB_H_

#undef OK
#include <gtkmm.h>
#include <libglademm/xml.h>

#include "constants.h" // For PackagesAction

#include <set>

namespace gui
{
  class NotifyView;

  /**
   * This is a list of tab types.
   */
  enum TabType
  {
    /** \brief A tab that asks the user to view the difference between
     *  two versions of a conffile.
     */
    ConffileDiff,
    Dashboard,
    /** \brief A tab that provides support for finding dependency
     *  chains linking two groups of packages.
     */
    DependencyChains,
    Download, Packages, Info, Preview, Resolver, InstallRemove,
    /** \brief A tab that shows the terminal output produced by a dpkg
     *	invocation.
     */
    DpkgTerminalTabType,
    Error
  };

  class TabsManager;
  /**
   * \brief A Tab contains a widget and some metadata for inserting into the notebook.
   *
   *  \todo To delete tabs, we rely on the clicked() signal from the
   *  close button.  This is not ideal, because the underlying widget
   *  could be deleted some other way.
   */
  class Tab : public sigc::trackable
  {
    private:
      TabType type;
      Glib::ustring label;
      Glib::RefPtr<Gnome::Glade::Xml> xml;
      Gtk::Label * label_label;
      Gtk::Button * label_button;
      Gtk::HBox * label_widget;
      Gtk::Widget * widget;
      NotifyView * notifyview;
      bool autodestroy;
      // True if this is the currently active tab.
      bool active;

      /** \brief Tabs are not copy-constructible.
       *
       *  Copy-constructing a tab could lead to confusion when it's deleted.
       */
      Tab(const Tab &);

      friend class TabsManager; // So it can activate us.
      /** \brief Change whether this tab is active. */
      void set_active(bool now_active);
    public:
      /** \brief Construct a new tab.
       *
       *  \param _type The type of the new tab.
       *  \param _label The label of the new tab.
       *  \param _xml  The XML object from which to take the widget
       *               of the new tab.
       *  \param widgetName  The name of the new tab's associated
       *                     widget within the given XML tree.
       *  \param _autodestroy If \b true, the tab will be destroyed
       *                      when it is closed; otherwise it will
       *                      be hidden.
       */
      Tab(TabType _type, const Glib::ustring &_label,
          const Glib::RefPtr<Gnome::Glade::Xml> &_xml, const std::string &widgetName,
	  bool _autodestroy = true);
      virtual ~Tab();
      Glib::ustring get_label() { return label; }
      Gtk::Widget * get_label_widget() { return label_widget; }
      Gtk::Button * get_label_button() { return label_button; }
      bool get_autodestroy() const { return autodestroy; }
      void set_label(Glib::ustring);
      TabType get_type() { return type; }
      Gtk::Widget * get_widget() const { return widget; }
      NotifyView * get_notifyview() const { return notifyview; }
      const Glib::RefPtr<Gnome::Glade::Xml> &get_xml() { return xml; }
      bool get_active() const { return active; }

      /** \brief Get the actions that should be activated in the Package menu.
       *
       *  The default implementation returns an empty set.
       */
      virtual std::set<PackagesAction> get_package_menu_actions();

      /** \brief Perform a Package menu action.
       *
       *  The default implementation does nothing.
       *
       *  \param action   The action to perform.
       */
      virtual void dispatch_package_menu_action(PackagesAction action);

      /** \brief Returns \b true if the Undo action is available on this tab.
       *
       *  The default implementation always returns \b false.
       */
      virtual bool get_undo_available();

      /** \brief Invoke the Undo menu action on this tab.
       *
       *  The default implementation does nothing.
       */
      virtual void dispatch_undo();

      /** \brief Returns \b true if the "Edit Columns..." action is available on this tab.
       *
       *  The default implementation always returns \b false.
       */
      virtual bool get_edit_columns_available();

      /** \brief Invoke the "Edit Columns..." action on this tab.
       *
       *  The default implementation does nothing.
       */
      virtual void dispatch_edit_columns();

      /** \brief A signal invoked when the tab becomes or ceases to be the active tab. */
      sigc::signal0<void> active_changed;

      /** \brief A signal invoked when the tab's "close" button is clicked.
       *
       *  The TabManager uses this to actually close the tab.
       */
      sigc::signal0<void> close_clicked;

      /** \brief A signal invoked when the tab is closed in the notebook. */
      sigc::signal0<void> closed;

      /** \brief A signal invoked when the packages menu of the tab
       *  (the result of get_package_menu_actions()) changes.
       */
      sigc::signal0<void> package_menu_actions_changed;

      /** \brief A signal invoked when whether undo is available changes. */
      sigc::signal0<void> undo_available_changed;

      /** \brief A signal invoked when whether "Edit Columns..." is available changes. */
      sigc::signal0<void> edit_columns_available_changed;
  };

  /**
   * This is a custom widget that handles placement of tabs
   */
  class TabsManager : public Gtk::Notebook
  {
    private:
      /** \brief The connection, if any, that listens to the currently
       *  active tab's Tab::package_menu_actions_changed signal.
       *
       *  This is disconnected when we switch away from the tab.
       */
      sigc::connection package_menu_actions_changed_connection;

      /** \brief The connection, if any, that listens to the currently
       *  active tab's Tab::undo_available_changed signal.
       *
       *  This is disconnected when we switch away from the tab.
       */
      sigc::connection undo_available_changed_connection;

      /** \brief The connection, if any, that listens to the currently
       *  active tab's Tab::edit_columns_available_changed signal.
       *
       *  This is disconnected when we switch away from the tab.
       */
      sigc::connection edit_columns_available_changed_connection;

      /** \brief Hack to allow us to change the state of a page when we
       *  switch *away* from it.
       *
       *  When switch_page() is called, the current page is already modified.
       *  To find out what page we switched away from, I store the index of the
       *  last page and update it whenever we switch pages. (EWW)
       */
      int last_active_page;

      /** \brief Use the currently displayed tab to update
       *  the package menu.
       */
      void update_package_menu();

      /**
       * Gives the position for the next tab of given type
       * @param type type of tab
       * @return position of the next tab of this type
       */
      int next_position(TabType type);
      /**
       * Gives the number of tabs of given type
       * @param type type of tab
       * @return number of tabs of this type
       */
      int number_of(TabType type);

      /** Called when a page is removed from the notebook.
       *
       *  Technically this doesn't need to be a member, but
       *  it might be useful in some future expansions.
       */
      void page_removed(Gtk::Widget *widget, int page);

      void do_switch_page(GtkNotebookPage *page, guint page_idx);

      void do_status_button_changed(Tab *tab);

      void maybe_close_page(Tab &tab);
    public:
      /**
       * Glade::Xml derived widget constructor.
       */
      TabsManager(BaseObjectType* cobject, const Glib::RefPtr<Gnome::Glade::Xml>& refGlade);
      /**
       * Appends a tab to the notebook
       * @param tab tab to append
       * @return position of the appended tab
       */
      int append_page(Tab &tab);
      /**
       * Remove a tab from the notebook
       * @param tab tab to remove
       */
      void remove_page(Tab &tab);

      /**
       * Remove the current tab from the notebook (maybe)
       * Fires Tab::close_clicked().
       */
      void maybe_close_current_page();

      /** \brief Get the currently active tab. */
      Tab *get_current_tab();

      /** \brief Emitted when a new tab is selected or when the
       *  current tab's status button changes.
       */
      sigc::signal1<void, Tab *> tab_status_button_changed;


      /** \brief Emitted when the Package menu contents of the active
       *  tab might have changed.
       */
      sigc::signal0<void> package_menu_actions_changed;

      /** \brief Emitted when whether undo is available might have
       *  changed.
       */
      sigc::signal0<void> undo_available_changed;

      /** \brief Emitted when whether "Edit Columns..." is available
       *  might have changed.
       */
      sigc::signal0<void> edit_columns_available_changed;
  };

}

#endif /* TAB_H_ */
