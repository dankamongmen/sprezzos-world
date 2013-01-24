// -*-c++-*-

// packagestab.h
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

#ifndef PACKAGESTAB_H_
#define PACKAGESTAB_H_

#undef OK
#include <gtkmm.h>

#include <apt-pkg/pkgcache.h>

#include <gtk/tab.h>

#include <cwidget/generic/util/ref_ptr.h>

#include <generic/apt/matching/pattern.h>
#include <generic/util/refcounted_base.h>

namespace gui
{
  class PkgView;
  class Entity;

  /** \brief A class that handles managing a collection of widgets
   *  where package searches are entered.
   *
   *  This sets up signals to provide user feedback about search
   *  syntax, and to perform a search when the user finishes entering
   *  it.  It isn't a GTK+ container to provide more flexibility to
   *  client code: the widgets can be arranged whatever way is
   *  convenient.
   */
  class PackageSearchEntry : public aptitude::util::refcounted_base_threadsafe
  {
    Gtk::Entry *search_entry;
    Gtk::Label *error_label;
    Gtk::Button *find_button;
    Gtk::ToggleButton *incremental_toggle_button;

    Glib::ustring last_delayed_search_term;

    // Parse the current entry and emit the activated() signal if it's
    // valid (otherwise show the error).
    void do_search();

    // Will compare the current search term with the stored value and fire if unchanged,
    // but will not fire if same as last_delayed_search_term.
    // This is used to trigger a search, say, 200ms in the future if the search term
    // hasn't changed since but has since the last delayed search.
    bool do_delayed_search(Glib::ustring search_term);

    // Affect the search button according to the incremental toggle button:
    // if toggled, then disable manual search.
    void toggled_incremental();

    // Invoked when the search entry's text changes.
    void search_entry_changed();

    PackageSearchEntry(Gtk::Entry *_search_entry,
		       Gtk::Label *_error_label,
                       Gtk::Button *_find_button,
                       Gtk::ToggleButton *_incremental_toggle_button);

  public:
    /** \brief Create a new PackageSearchEntry.
     *
     *  \param search_entry               The text entry to manage.
     *  \param error_label                The label in which to display error messages.
     *  \param find_button                A button that the user can use to perform a search.
     *  \param incremental_toggle_button  A togglebutton to toggle incremental search.
     *
     *  \return A reference-counting wrapper around the new package
     *  search entry.
     */
    static cwidget::util::ref_ptr<PackageSearchEntry>
    create(Gtk::Entry *search_entry,
	   Gtk::Label *error_label,
	   Gtk::Button *find_button,
           Gtk::ToggleButton *incremental_toggle_button = NULL)
    {
      return new PackageSearchEntry(search_entry, error_label, find_button, incremental_toggle_button);
    }

    Glib::ustring get_text() const
    {
      return search_entry->get_text();
    }

    /** \brief Set the text of the entry and act as if the user had
     *	pressed Enter.
     */
    void set_text(const Glib::ustring &text);

    /** \brief A signal emitted when the user searches for a package. */
    sigc::signal<void, cwidget::util::ref_ptr<aptitude::matching::pattern> > activated;
  };

  /** \brief A searchable list of packages. */
  class PackageSearchList : public aptitude::util::refcounted_base_threadsafe
  {
    /** \brief The columns used in the filter combobox. */
    class filter_combobox_columns : public Gtk::TreeModel::ColumnRecord
    {
    public:
      Gtk::TreeModelColumn<Glib::ustring> name;
      Gtk::TreeModelColumn<cwidget::util::ref_ptr<aptitude::matching::pattern> > pattern;

      filter_combobox_columns();
    };

    // The last search that was successfully entered into the search
    // entry.  This is combined with the filter to produce the actual
    // search.
    cwidget::util::ref_ptr<aptitude::matching::pattern> current_search;

    // The search-entry aggregate associated with this view.
    cwidget::util::ref_ptr<PackageSearchEntry> search_entry;

    // The drop-down menu used to pick a filter.
    Gtk::ComboBox *filter_combobox;

    filter_combobox_columns filter_columns;

    // The actual package list that we encapsulate.
    cwidget::util::ref_ptr<PkgView> package_list;

    // A hook invoked whenever the model is rebuilt for whatever
    // reason.
    sigc::slot<void> after_repopulate_hook;

    // Builds the search list from the current selections.
    void repopulate();

    // Invoked when the user edits the pattern in the search entry.
    void do_search_entry_activated(const cwidget::util::ref_ptr<aptitude::matching::pattern> &p);

    // Used to test whether a row in the filter combobox is a
    // separator.
    bool filter_row_is_separator(const Glib::RefPtr<Gtk::TreeModel> &model,
				 const Gtk::TreeModel::iterator &iterator);

    PackageSearchList(const cwidget::util::ref_ptr<PackageSearchEntry> &_search_entry,
		      Gtk::ComboBox *_filter_combobox,
		      const cwidget::util::ref_ptr<PkgView> &_package_list,
		      const sigc::slot<void> &_after_repopulate_hook);
  public:
    /** \brief Create a new PackageSearchlist.
     *
     *  \param search_entry       The search-entry-widget aggregate
     *                            used to enter searches for this package list.
     *  \param filter_combobox    The drop-down box for picking
     *                            a filter for the package list.
     *  \param package_list       The package list managed by the new
     *                            object.
     *  \param after_repopulate_hook   A slot to invoke every time the
     *                                 list is rebuilt.
     */
    static cwidget::util::ref_ptr<PackageSearchList>
    create(const cwidget::util::ref_ptr<PackageSearchEntry> &search_entry,
						     Gtk::ComboBox *filter_combobox,
						     const cwidget::util::ref_ptr<PkgView> &package_list,
						     const sigc::slot<void> &after_repopulate_hook)
    {
      return new PackageSearchList(search_entry,
				   filter_combobox,
				   package_list,
				   after_repopulate_hook);
    }

    const cwidget::util::ref_ptr<PackageSearchEntry> &get_search_entry() const
    {
      return search_entry;
    }

    const cwidget::util::ref_ptr<PkgView> &get_package_list() const
    {
      return package_list;
    }
  };

  class PackagesTab : public Tab
  {
    private:
      cwidget::util::ref_ptr<PackageSearchList> pSearchList;
      Gtk::TextView * pPackagesTextView;

      void after_repopulate_model();
    public:
      PackagesTab(const Glib::ustring &label);
      void activated_package_handler();
      void display_desc(const cwidget::util::ref_ptr<Entity> &ent);
      const cwidget::util::ref_ptr<PackageSearchList> &get_search_list() const { return pSearchList; }

      std::set<PackagesAction> get_package_menu_actions();
      void dispatch_package_menu_action(PackagesAction action);
      bool get_undo_available();
      void dispatch_undo();
      void set_limit(const std::string &limit);

      bool get_edit_columns_available();
      void dispatch_edit_columns();
  };
}


#endif /* PACKAGESTAB_H_ */
