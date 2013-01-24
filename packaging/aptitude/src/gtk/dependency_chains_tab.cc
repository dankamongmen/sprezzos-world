// dependency_chains_tab.cc
//
//   Copyright (C) 2008-2010 Daniel Burrows
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

/** \file dependency_chains_tab.cc */

#include "dependency_chains_tab.h"

#include "aptitude.h"

#include "entityview.h"
#include "gui.h"
#include "packagestab.h"
#include "pkgview.h"
#include "progress.h"

#include <cmdline/cmdline_why.h>

#include <cwidget/generic/util/ssprintf.h>

namespace gui
{
  // \todo let the user adjust the parameters of the search
  // interactively.  First we have to figure out what the best axes of
  // twiddling are...
  DependencyChainsTab::DependencyChainsTab(const Glib::ustring &label)
    : Tab(DependencyChains, label, Gnome::Glade::Xml::create(glade_main_file, "dependency_path_main"), "dependency_path_main")
  {
    Gtk::Entry *start_package_entry;
    Gtk::Entry *end_package_entry;
    get_xml()->get_widget("dependency_path_start_search_pattern",
			  start_package_entry);
    get_xml()->get_widget("dependency_path_end_search_pattern",
			  end_package_entry);

    Gtk::Label *start_errors;
    Gtk::Label *end_errors;
    get_xml()->get_widget("dependency_path_start_search_errors",
			  start_errors);
    get_xml()->get_widget("dependency_path_end_search_errors",
			  end_errors);

    get_widget()->show();

    Gtk::Button *start_search_button;
    Gtk::Button *end_search_button;

    get_xml()->get_widget("dependency_path_start_find_button",
			  start_search_button);
    get_xml()->get_widget("dependency_path_end_find_button",
			  end_search_button);

    Gtk::ComboBox *start_limit_combo_box;
    Gtk::ComboBox *end_limit_combo_box;

    get_xml()->get_widget("dependency_path_start_show_only_combo_box",
			  start_limit_combo_box);
    get_xml()->get_widget("dependency_path_end_show_only_combo_box",
			  end_limit_combo_box);

    using cwidget::util::ref_ptr;
    ref_ptr<PkgView> start_package_view(new PkgView(get_xml(), "dependency_path_start_packages_treeview",
						    _("Find dependency chains: start"), "",
						    sigc::bind(sigc::ptr_fun(&gtkEntryOpProgress::create),
							       sigc::ref(*start_package_entry))));
    ref_ptr<PkgView> end_package_view(new PkgView(get_xml(), "dependency_path_end_packages_treeview",
						  _("Find dependency chains: end"), "",
						  sigc::bind(sigc::ptr_fun(&gtkEntryOpProgress::create),
							     sigc::ref(*end_package_entry))));
    results_view = ref_ptr<EntityView>(new EntityView(get_xml(), "dependency_path_results_treeview",
						      _("Find dependency chains: results")));

    end_package_view->get_treeview()->get_selection()->set_mode(Gtk::SELECTION_BROWSE);

    start_search_list =
      PackageSearchList::create(PackageSearchEntry::create(start_package_entry,
							   start_errors,
							   start_search_button),
				start_limit_combo_box,
				start_package_view,
				sigc::slot<void>());
    end_search_list =
      PackageSearchList::create(PackageSearchEntry::create(end_package_entry,
							   end_errors,
							   end_search_button),
				end_limit_combo_box,
				end_package_view,
				sigc::slot<void>());

    start_package_view->get_treeview()->get_selection()->signal_changed()
      .connect(sigc::mem_fun(this, &DependencyChainsTab::selection_changed));
    end_package_view->get_treeview()->get_selection()->signal_changed()
      .connect(sigc::mem_fun(this, &DependencyChainsTab::selection_changed));

    results_view->get_version_column()->set_visible(false);
    selection_changed();

    Glib::RefPtr<Gtk::SizeGroup> size_group = Gtk::SizeGroup::create(Gtk::SIZE_GROUP_HORIZONTAL);
    Gtk::Label *path_label;
    get_xml()->get_widget("dependency_path_start_limit_label", path_label);
    size_group->add_widget (*path_label);
    get_xml()->get_widget("dependency_path_start_show_only_label", path_label);
    size_group->add_widget (*path_label);
    get_xml()->get_widget("dependency_path_end_limit_label", path_label);
    size_group->add_widget (*path_label);
    get_xml()->get_widget("dependency_path_end_show_only_label", path_label);
    size_group->add_widget (*path_label);

    // \todo do something sensible on "closed".
    cache_reloaded.connect(sigc::mem_fun(this, &DependencyChainsTab::selection_changed));
  }

  namespace
  {
    pkgCache::PkgIterator get_path_package(const Glib::RefPtr<Gtk::TreeModel> &model,
					   const EntityColumns &columns,
					   const Gtk::TreeModel::Path &path)
    {
      pkgCache::PkgIterator rval(*apt_cache_file, 0);
      Gtk::TreeModel::iterator iter = model->get_iter(path);
      if(iter != model->children().end())
	{
	  cwidget::util::ref_ptr<Entity> ent = (*iter)[columns.EntObject];
	  cwidget::util::ref_ptr<PkgEntity> pkg_ent = ent.dyn_downcast<PkgEntity>();
	  if(pkg_ent.valid())
	    rval = pkg_ent->get_pkg();
	}

      return rval;
    }
  }

  /** \brief An entity class that's responsible for displaying rows in
   *  a dependency chain view.
   *
   *  \todo Each row represents a single package.  Will this make
   *  sense to the user?  Should I just ditch the PkgView entirely and
   *  use a straight TreeView?
   */
  class DependencyChainActionEntity : public PkgEntity
  {
    aptitude::why::action act;
  public:
    DependencyChainActionEntity(const aptitude::why::action &_act)
      : PkgEntity(!_act.get_dep().end()
		    ? _act.get_dep().ParentPkg()
		    : _act.get_prv().OwnerPkg()),
	act(_act)
    {
    }

    void fill_row(const EntityColumns *cols,
		  Gtk::TreeModel::Row &row)
    {
      PkgEntity::fill_row(cols, row);

      Glib::ustring text;
      if(!act.get_dep().end())
	{
	  pkgCache::DepIterator dep = act.get_dep();

	  pkgCache::DepIterator start, end;
	  surrounding_or(dep, start, end);

	  text = dep.ParentPkg().Name();
	  text += " ";
	  text += dep.ParentVer().VerStr();
	  text += " ";
	  text += dep.DepType();
	  text += " ";
	  text += dep.TargetPkg().Name();

	  if(dep.TargetVer() != NULL)
	    {
	      text += " (";
	      text += dep.CompType();
	      text += " ";
	      text += dep.TargetVer();
	      text += ")";
	    }
	}
      else
	{
	  pkgCache::PrvIterator prv = act.get_prv();

	  text = prv.OwnerPkg().Name();
	  text += " ";
	  text += prv.OwnerVer().VerStr();
	  text += " ";
	  text += _("Provides");
	  text += " ";
	  text += prv.ParentPkg().Name();
	}

      row[cols->NameMarkup] = Glib::Markup::escape_text(text);
    }
  };

  Glib::RefPtr<Gtk::TreeModel> DependencyChainsTab::get_results()
  {
    using namespace aptitude;
    using cwidget::util::ref_ptr;

    Glib::RefPtr<Gtk::ListStore> store = Gtk::ListStore::create(*results_view->get_columns());

    // Just add the error row up-front so we know it always appears if
    // we break out.
    {
      Gtk::TreeModel::iterator iter = store->append();
      Gtk::TreeModel::Row row = *iter;
      (new HeaderEntity(_("Select one or more starting packages and an ending package to search.")))->fill_row(results_view->get_columns(), row);
    }

    Glib::RefPtr<Gtk::TreeView::Selection> start_selection =
      start_search_list->get_package_list()->get_treeview()->get_selection();
    Glib::RefPtr<Gtk::TreeView::Selection> end_selection =
      end_search_list->get_package_list()->get_treeview()->get_selection();

    if(!start_selection || !end_selection)
      return store;

    Gtk::TreeSelection::ListHandle_Path start_rows = start_selection->get_selected_rows();
    Gtk::TreeSelection::ListHandle_Path end_rows = end_selection->get_selected_rows();

    if(start_rows.empty())
      return store;

    const Gtk::TreeSelection::ListHandle_Path::const_iterator end_begin =
      end_rows.begin();
    if(end_begin == end_rows.end())
      return store;
    const pkgCache::PkgIterator target = get_path_package(end_search_list->get_package_list()->get_model(),
							  *end_search_list->get_package_list()->get_columns(),
							  *end_begin);
    if(target.end())
      return store;

    std::vector<ref_ptr<matching::pattern> > leaves;
    for(Gtk::TreeSelection::ListHandle_Path::const_iterator
	  it = start_rows.begin(); it != start_rows.end(); ++it)
      {
	const pkgCache::PkgIterator leaf = get_path_package(start_search_list->get_package_list()->get_model(),
							    *start_search_list->get_package_list()->get_columns(),
							    *it);
	if(!leaf.end())
	  leaves.push_back(matching::pattern::make_name(cwidget::util::ssprintf("^%s$", leaf.Name())));
      }

    if(leaves.empty())
      return store;

    // Clear the error message.
    store->clear();

    // Now we run the "why" algorithm.
    std::vector<std::vector<why::action> > results;
    why::find_best_justification(leaves,
				 why::target::Install(target),
				 false,
				 0,
                                 boost::shared_ptr<aptitude::why::why_callbacks>(),
				 results);
    if(results.empty() || results.front().empty())
      {
	Gtk::TreeModel::iterator iter = store->append();
	Gtk::TreeModel::Row row = *iter;
	(new HeaderEntity(_("No dependency chain found.")))->fill_row(results_view->get_columns(), row);
      }
    else
      {
	for(std::vector<why::action>::const_iterator act_it = results[0].begin();
	    act_it != results[0].end(); ++act_it)
	  {
	    // TODO: add support for viewing more than one
	    // result. (left/right arrow buttons?)
	    Gtk::TreeModel::iterator iter = store->append();
	    Gtk::TreeModel::Row row = *iter;
	    (new DependencyChainActionEntity(*act_it))->fill_row(results_view->get_columns(),
								 row);
	  }
      }

    return store;
  }

  void DependencyChainsTab::selection_changed()
  {
    results_view->set_model(get_results());
  }
}
