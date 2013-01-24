// packagestab.cc
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

#include "packagestab.h"

#include "aptitude.h"
#include "info.h"

#undef OK
#include <gtkmm.h>

#include <apt-pkg/strutl.h>

#include <generic/apt/config_signal.h>
#include <generic/apt/matching/compare_patterns.h>
#include <generic/apt/matching/parse.h>
#include <generic/apt/matching/pattern.h>
#include <generic/util/undo.h>
#include <generic/util/util.h>

#include <gtk/entitysummary.h>
#include <gtk/gui.h>
#include <gtk/pkgview.h>
#include <gtk/progress.h>
#include <gtk/util/property.h>

#include <cwidget/generic/util/ssprintf.h>

namespace gui
{
  void PackageSearchEntry::do_search()
  {
    std::string search_term(search_entry->get_text());
    cwidget::util::ref_ptr<aptitude::matching::pattern> p;
    try
      {
        p = aptitude::matching::parse_with_errors(search_term);
      }
    catch(aptitude::matching::MatchingException &ex)
      {
        // Show the user what's wrong and don't update the list.
        std::string markup(cwidget::util::ssprintf("<span size=\"smaller\" color=\"red\">%s: %s</span>",
                                                   Glib::Markup::escape_text(_("Parse error")).c_str(),
                                                   Glib::Markup::escape_text(ex.errmsg()).c_str()));
        error_label->set_markup(markup);
        error_label->show();
        return;
      }

    error_label->hide();
    activated(p);
  }

  bool PackageSearchEntry::do_delayed_search(Glib::ustring search_term)
  {
    Glib::ustring current_search_term(search_entry->get_text());
    if(current_search_term == search_term && current_search_term != last_delayed_search_term)
    {
      // FIXME: This sensitivity/position dance is a temporary workaround
      //        because the searching lag messes up badly with
      //        the text entry editing (caret position and selection).
      //        It looks like the caret is only moved after the newly typed character
      //        only after Editable::signal_changed() is fired and handled but a new
      //        character can be type (at the previous position) in the meantime.
      // NOTE:  This should now be mostly mitigated by the 200ms timeout but can still
      //        happen with very length searches.
      int position;
      position = search_entry->get_position();
      search_entry->set_sensitive(false);
      last_delayed_search_term = current_search_term;
      do_search();
      search_entry->set_sensitive(true);
      search_entry->grab_focus();
      search_entry->set_position(position);
    }
    return false;
  }

  void PackageSearchEntry::toggled_incremental()
  {
    if(incremental_toggle_button->get_active())
    {
      find_button->set_sensitive(false);
      if(search_entry->get_text() != "")
      {
        do_search();
      }
      else
      {
        search_entry->grab_focus();
      }
    }
    else
    {
      find_button->set_sensitive(true);
      search_entry->grab_focus();
    }
  }

  void PackageSearchEntry::search_entry_changed()
  {
    const Glib::ustring limit(search_entry->get_text());
    bool valid;
    try
      {
	aptitude::matching::parse_with_errors(limit);
	valid = true;
      }
    catch(aptitude::matching::MatchingException &)
      {
	valid = false;
      }

    if(valid)
    {
      search_entry->unset_base(Gtk::STATE_NORMAL);
      if(incremental_toggle_button != NULL && incremental_toggle_button->get_active())
      {
        Glib::signal_timeout().connect(sigc::bind(sigc::mem_fun(*this, &PackageSearchEntry::do_delayed_search), limit), 200);
      }
    }
    else
      search_entry->modify_base(Gtk::STATE_NORMAL, Gdk::Color("#FFD0D0"));
  }

  void PackageSearchEntry::set_text(const Glib::ustring &text)
  {
    search_entry->set_text(text);
    do_search();
  }

  PackageSearchEntry::PackageSearchEntry(Gtk::Entry *_search_entry,
					 Gtk::Label *_error_label,
					 Gtk::Button *_find_button,
			                 Gtk::ToggleButton *_incremental_toggle_button)
    : search_entry(_search_entry),
      error_label(_error_label),
      find_button(_find_button),
      incremental_toggle_button(_incremental_toggle_button)
  {
    search_entry->signal_activate().connect(sigc::mem_fun(*this, &PackageSearchEntry::do_search));
    find_button->signal_clicked().connect(sigc::mem_fun(*this, &PackageSearchEntry::do_search));
    if(incremental_toggle_button != NULL)
      incremental_toggle_button->signal_toggled().connect(sigc::mem_fun(*this, &PackageSearchEntry::toggled_incremental));
    search_entry->signal_changed().connect(sigc::mem_fun(*this, &PackageSearchEntry::search_entry_changed));
  }




  PackageSearchList::filter_combobox_columns::filter_combobox_columns()
  {
    add(name);
    add(pattern);
  }

  void PackageSearchList::repopulate()
  {
    using cwidget::util::ref_ptr;

    cwidget::util::ref_ptr<aptitude::matching::pattern> final_pattern;

    Gtk::TreeModel::const_iterator active_filter_iter = filter_combobox->get_active();
    if(!active_filter_iter)
      final_pattern = current_search;
    else if(current_search.valid())
      {
	Gtk::TreeModel::Row active_filter_row = *active_filter_iter;
	cwidget::util::ref_ptr<aptitude::matching::pattern> active_filter_pattern =
	  active_filter_row[filter_columns.pattern];

	if(active_filter_pattern.valid())
	  final_pattern = aptitude::matching::pattern::make_and(current_search, active_filter_pattern);
	else
	  final_pattern = current_search;
      }
    package_list->set_limit(final_pattern);

    if(package_list->get_model()->children().size() == 0)
      {
	Glib::RefPtr<Gtk::ListStore> store = Gtk::ListStore::create(*package_list->get_columns());

	Gtk::TreeModel::iterator iter = store->append();
	Gtk::TreeModel::Row row = *iter;
	Glib::ustring search_term(search_entry->get_text());
	(new HeaderEntity(ssprintf(_("No packages matched \"%s\"."), search_term.c_str())))->fill_row(package_list->get_columns(), row);

	package_list->set_model(store);
      }

    after_repopulate_hook();
  }

  void PackageSearchList::do_search_entry_activated(const cwidget::util::ref_ptr<aptitude::matching::pattern> &p)
  {
    current_search = p;
    repopulate();
  }

  // By convention, a blank string for a name means that a row is a
  // separator.
  bool PackageSearchList::filter_row_is_separator(const Glib::RefPtr<Gtk::TreeModel> &model,
						  const Gtk::TreeModel::iterator &iterator)
  {
    Gtk::TreeModel::Row row = *iterator;
    Glib::ustring name = row[filter_columns.name];
    return name.empty();
  }


  PackageSearchList::PackageSearchList(const cwidget::util::ref_ptr<PackageSearchEntry> &_search_entry,
				       Gtk::ComboBox *_filter_combobox,
				       const cwidget::util::ref_ptr<PkgView> &_package_list,
				       const sigc::slot<void> &_after_repopulate_hook)
    : search_entry(_search_entry),
      filter_combobox(_filter_combobox),
      package_list(_package_list),
      after_repopulate_hook(_after_repopulate_hook)
  {
    using aptitude::matching::pattern;
    using cwidget::util::ref_ptr;

    search_entry->activated.connect(sigc::mem_fun(*this, &PackageSearchList::do_search_entry_activated));

    Glib::RefPtr<Gtk::ListStore> filter_model = Gtk::ListStore::create(filter_columns);
    // The "All Packages" option is always there.
    {
      Gtk::TreeModel::iterator all_packages_iter = filter_model->append();
      Gtk::TreeModel::Row all_packages_row = *all_packages_iter;
      all_packages_row[filter_columns.name] = "All Packages";
      all_packages_row[filter_columns.pattern] = pattern::make_true();
    }

    std::map<std::string, ref_ptr<pattern> > filters;
    // Patterns that exist by default.
    //
    // \todo This is a mess for i18n.  How can I do this so that it
    // gets translated by default, but can still be overridden?  Maybe
    // I should make the name a property of the group rather than the
    // tag?
    filters["Installed Packages"] = aptitude::matching::parse("?installed");
    filters["Not Installed Packages"] = aptitude::matching::parse("?not(?installed)");
    filters["New Packages"] = aptitude::matching::parse("?new");
    filters["Virtual Packages"] = aptitude::matching::parse("?virtual");
    // Add in all the entries in the "Aptitude::Filters" group.
    for(const Configuration::Item *it = aptcfg->Tree(PACKAGE "::Filters");
	it != NULL; it = it->Next)
      {
	ref_ptr<pattern> p =
	  aptitude::matching::parse(it->Tag);

	filters[it->Tag] = p;
      }

    for(std::map<std::string, ref_ptr<pattern> >::const_iterator
	  it = filters.begin(); it != filters.end(); ++it)
      {
	if(it->second.valid())
	  {
	    // Output an hrule to separate the filters from "All
	    // Packages" if this is the first filter.
	    if(it == filters.begin())
	      {
		Gtk::TreeModel::iterator pattern_iter = filter_model->append();
		Gtk::TreeModel::Row pattern_row = *pattern_iter;
		pattern_row[filter_columns.name] = "";
		pattern_row[filter_columns.pattern] = ref_ptr<pattern>();
	      }

	    Gtk::TreeModel::iterator pattern_iter = filter_model->append();
	    Gtk::TreeModel::Row pattern_row = *pattern_iter;
	    pattern_row[filter_columns.name] = it->first;
	    pattern_row[filter_columns.pattern] = it->second;
	  }
      }

    filter_combobox->set_row_separator_func(sigc::mem_fun(*this, &PackageSearchList::filter_row_is_separator));
    filter_combobox->pack_start(filter_columns.name);
    if(filter_combobox->get_cells().begin() != filter_combobox->get_cells().end())
    {
      Gtk::CellRenderer *renderer = *filter_combobox->get_cells().begin();
      Gtk::CellRendererText *renderer_text = dynamic_cast<Gtk::CellRendererText *>(renderer);
      renderer_text->property_ellipsize() = Pango::ELLIPSIZE_END;
    }
    filter_combobox->set_model(filter_model);
    filter_combobox->set_active(0);

    filter_combobox->signal_changed().connect(sigc::mem_fun(*this, &PackageSearchList::repopulate));

    // Ask the user to enter a search pattern.
    {
      Glib::RefPtr<Gtk::ListStore> store = Gtk::ListStore::create(*package_list->get_columns());

      Gtk::TreeModel::iterator iter = store->append();
      Gtk::TreeModel::Row row = *iter;
      (new HeaderEntity(_("Enter a search and click \"Find\" to display packages.")))->fill_row(package_list->get_columns(), row);

      package_list->set_model(store);
    }
  }

  PackagesTab::PackagesTab(const Glib::ustring &label) :
    Tab(Packages, label, Gnome::Glade::Xml::create(glade_main_file, "main_packages_hpaned"), "main_packages_hpaned")
  {
    Gtk::Entry *pLimitEntry;
    Gtk::Label *pLimitErrorLabel;
    Gtk::ComboBox *pLimitComboBox;
    Gtk::Button *pLimitButton;
    Gtk::ToggleButton *pIncrementalToggleButton;

    get_xml()->get_widget("main_packages_textview", pPackagesTextView);
    get_xml()->get_widget("main_notebook_packages_limit_entry", pLimitEntry);
    get_xml()->get_widget("main_notebook_packages_limit_errors", pLimitErrorLabel);
    get_xml()->get_widget("main_notebook_packages_limit_button", pLimitButton);
    get_xml()->get_widget("main_notebook_packages_incremental_toggle_button", pIncrementalToggleButton);
    get_xml()->get_widget("main_notebook_packages_show_only_combo_box", pLimitComboBox);

    using cwidget::util::ref_ptr;
    ref_ptr<PkgView> pPkgView(new PkgView(get_xml(), "main_packages_treeview",
					  get_label(), "",
					  sigc::bind(sigc::ptr_fun(&gtkEntryOpProgress::create),
						     sigc::ref(*pLimitEntry))));

    // TODO: We prevent the tab from closing itself, but we should rather make closing
    //       the tab gracefully stop the generator from doing whatever it's doing
    pPkgView->store_reloading.connect(sigc::bind(sigc::mem_fun(*get_label_button(), &Gtk::Widget::set_sensitive), false));
    pPkgView->store_reloaded.connect(sigc::bind(sigc::mem_fun(*get_label_button(), &Gtk::Widget::set_sensitive), true));

    pSearchList = PackageSearchList::create(PackageSearchEntry::create(pLimitEntry, pLimitErrorLabel, pLimitButton, pIncrementalToggleButton),
					    pLimitComboBox, pPkgView,
					    sigc::mem_fun(this, &PackagesTab::after_repopulate_model));

    pPkgView->get_treeview()->signal_selection.connect(sigc::mem_fun(*this, &PackagesTab::activated_package_handler));
    pPkgView->get_treeview()->signal_cursor_changed().connect(sigc::mem_fun(*this, &PackagesTab::activated_package_handler));

    pPkgView->package_menu_actions_changed.connect(package_menu_actions_changed.make_slot());
    apt_undos->changed.connect(undo_available_changed.make_slot());

    Glib::RefPtr<Gtk::SizeGroup> size_group = Gtk::SizeGroup::create(Gtk::SIZE_GROUP_HORIZONTAL);
    Gtk::Label *path_label;
    get_xml()->get_widget("main_notebook_packages_limit_label", path_label);
    size_group->add_widget (*path_label);
    get_xml()->get_widget("main_notebook_packages_show_only_label", path_label);
    size_group->add_widget (*path_label);

    get_widget()->show();
  }

  bool PackagesTab::get_undo_available()
  {
    return apt_undos->size() > 0;
  }

  void PackagesTab::dispatch_undo()
  {
    apt_undos->undo();
  }

  void PackagesTab::set_limit(const std::string &limit)
  {
    pSearchList->get_search_entry()->set_text(limit);
  }

  bool PackagesTab::get_edit_columns_available()
  {
    return true;
  }

  void PackagesTab::dispatch_edit_columns()
  {
    pSearchList->get_package_list()->show_edit_columns_dialog();
  }

  // TODO: Should be moved into PackagesView for use with PackagesView::signal_on_package_selection.
  void PackagesTab::activated_package_handler()
  {
    Gtk::TreeModel::Path path;
    Gtk::TreeViewColumn * focus_column;
    const cwidget::util::ref_ptr<PkgView>
      package_list(pSearchList->get_package_list());
    package_list->get_treeview()->get_cursor(path, focus_column);
    if (package_list->get_treeview()->get_selection()->is_selected(path))
    {
      Gtk::TreeModel::iterator iter = package_list->get_model()->get_iter(path);
      using cwidget::util::ref_ptr;
      ref_ptr<Entity> ent = (*iter)[package_list->get_columns()->EntObject];
      ref_ptr<PkgEntity> pkg_ent = ent.dyn_downcast<PkgEntity>();
      display_desc(ent);
    }
    else
    {
      pPackagesTextView->get_buffer()->set_text("");
    }
  }

  void PackagesTab::after_repopulate_model()
  {
    const std::string title = _("Packages: ") + pSearchList->get_search_entry()->get_text();
    set_label(title);
    pSearchList->get_package_list()->edit_columns_dialog_parent_title_changed(get_label());
  }

  void PackagesTab::display_desc(const cwidget::util::ref_ptr<Entity> &ent)
  {
    show_entity_summary(ent, pPackagesTextView);
  }

  std::set<PackagesAction> PackagesTab::get_package_menu_actions()
  {
    return pSearchList->get_package_list()->get_package_menu_actions();
  }

  void PackagesTab::dispatch_package_menu_action(PackagesAction action)
  {
    pSearchList->get_package_list()->apply_action_to_selected(action);
  }
}
