// previewtab.cc
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

#include "previewtab.h"
#include "aptitude.h"

#undef OK
#include <gtkmm.h>

#include <apt-pkg/strutl.h>

#include <generic/util/util.h>

#include <gtk/hyperlink.h>
#include <gtk/gui.h>
#include <gtk/info.h>
#include <gtk/packageinformation.h>
#include <gtk/packagestab.h> // For PackageSearchEntry.
#include <gtk/pkgview.h>
#include <gtk/progress.h>
#include <gtk/notify.h>

namespace gui
{
  PreviewView::Generator::Generator(const EntityColumns *_entity_columns)
  {
    entity_columns = _entity_columns;
    store = Gtk::TreeStore::create(*entity_columns);
  }

  PreviewView::Generator *PreviewView::Generator::create(const EntityColumns *entity_columns)
  {
    return new Generator(entity_columns);
  }

  void PreviewView::Generator::add(const pkgCache::PkgIterator &pkg)
  {
    int group = find_pkg_state(pkg, *apt_cache_file);
    if(group != pkg_unchanged)
      {
	const std::map<int, Gtk::TreeModel::iterator>::const_iterator found =
	  state_trees.find(group);

	Gtk::TreeModel::iterator tree;
	if(found == state_trees.end())
	  {
	    tree = store->append();
	    Gtk::TreeModel::Row tree_row = *tree;
	    (new HeaderEntity(_(child_names[group])))->fill_row(entity_columns, tree_row);
	    state_trees[group] = tree;
	  }
	else
	  tree = found->second;

	Gtk::TreeModel::iterator iter = store->append(tree->children());
	Gtk::TreeModel::Row row = *iter;

	PkgEntity *entity = new PkgEntity(pkg);
	entity->fill_row(entity_columns, row);
      }
  }

  void PreviewView::Generator::finish()
  {
    store->set_sort_column(entity_columns->Name, Gtk::SORT_ASCENDING);
    // FIXME: Hack while finding a nonblocking thread join.
    finished = true;
  }

  Glib::RefPtr<Gtk::TreeModel> PreviewView::Generator::get_model()
  {
    return store;
  }


  // \todo This is proof-of-concept only; the child_names list should
  // be in common code.
  const char * const PreviewView::Generator::child_names[num_pkg_action_states]=
    {
      N_("Packages with unsatisfied dependencies\n The dependency requirements of these packages will be unmet after the install is complete.\n .\n The presence of this tree probably indicates that something is broken, either on your system or in the Debian archive."),
      N_("Packages being removed because they are no longer used\n These packages are being deleted because they were automatically installed to fulfill dependencies, and the planned action will result in no installed package declaring an 'important' dependency on them.\n"),
      N_("Packages being automatically held in their current state\n These packages could be upgraded, but they have been kept in their current state to avoid breaking dependencies."),
      N_("Packages being automatically installed to satisfy dependencies\n These packages are being installed because they are required by another package you have chosen for installation."),
      N_("Packages being deleted due to unsatisfied dependencies\n These packages are being deleted because one or more of their dependencies is no longer available, or because another package conflicts with them."),
      N_("Packages to be downgraded\n An older version of these packages than is currently installed will be installed."),
      N_("Packages being held back\n These packages could be upgraded, but you have asked for them to be held at their current version."),
      N_("Packages to be reinstalled\n These packages will be reinstalled."),
      N_("Packages to be installed\n These packages have been manually selected for installation on your computer."),
      N_("Packages to be removed\n These packages have been manually selected for removal."),
      N_("Packages to be upgraded\n These packages will be upgraded to a newer version."),
      N_("Packages that are partially installed\n These packages are not fully installed and configured; an attempt will be made to complete their installation."),
    };

  PreviewView::PreviewView(const Glib::RefPtr<Gnome::Glade::Xml> &refGlade,
			   const Glib::ustring &gladename,
			   const Glib::ustring &limit,
			   const sigc::slot<cwidget::util::ref_ptr<refcounted_progress> > &build_progress_k)
    : PkgViewBase(sigc::ptr_fun(&Generator::create),
		  refGlade,
		  gladename,
		  _("Preview"),
		  limit,
		  build_progress_k)
  {
  }

  PreviewTab::PreviewTab(const Glib::ustring &label) :
    Tab(Preview, label, Gnome::Glade::Xml::create(glade_main_file, "main_packages_hpaned"), "main_packages_hpaned")
  {
    Gtk::Entry *pLimitEntry;
    Gtk::Button *pLimitButton;
    Gtk::Label *pLimitErrors;

    get_xml()->get_widget("main_packages_textview", pPackagesTextView);
    get_xml()->get_widget("main_notebook_packages_limit_entry", pLimitEntry);
    get_xml()->get_widget("main_notebook_packages_limit_errors", pLimitErrors);
    get_xml()->get_widget("main_notebook_packages_limit_button", pLimitButton);

    pSearchEntry = PackageSearchEntry::create(pLimitEntry,
					      pLimitErrors,
					      pLimitButton);
    pSearchEntry->activated.connect(sigc::mem_fun(*this, &PreviewTab::limit_changed));

    using cwidget::util::ref_ptr;
    pPkgView = ref_ptr<PreviewView>(new PreviewView(get_xml(), "main_packages_treeview", "",
						    sigc::bind(sigc::ptr_fun(&gtkEntryOpProgress::create),
							       sigc::ref(*pLimitEntry))));

    pPkgView->get_treeview()->signal_selection.connect(sigc::mem_fun(*this, &PreviewTab::activated_package_handler));

    // Start out with no limit.
    limit_changed(aptitude::matching::pattern::make_true());

    pPkgView->get_treeview()->expand_all();

    get_widget()->show();

  }

  // TODO: Should be moved into PackagesView for use with PackagesView::signal_on_package_selection.
  void PreviewTab::activated_package_handler()
  {
    Gtk::TreeModel::Path path;
    Gtk::TreeViewColumn * focus_column;
    pPkgView->get_treeview()->get_cursor(path, focus_column);
    if (pPkgView->get_treeview()->get_selection()->is_selected(path))
    {
      Gtk::TreeModel::iterator iter = pPkgView->get_model()->get_iter(path);
      cwidget::util::ref_ptr<Entity> ent = (*iter)[pPkgView->get_columns()->EntObject];
      display_desc(ent);
    }
    else
    {
      pPackagesTextView->get_buffer()->set_text("");
    }
  }

  void PreviewTab::limit_changed(const cwidget::util::ref_ptr<aptitude::matching::pattern> &limit)
  {
    pPkgView->set_limit(limit);
    pPkgView->get_treeview()->expand_all();
    set_label(_("Preview: ") + pSearchEntry->get_text());
  }

  void PreviewTab::display_desc(const cwidget::util::ref_ptr<Entity> &ent)
  {
    cwidget::util::ref_ptr<PkgEntity> pkg_ent = ent.dyn_downcast<PkgEntity>();

    pkgCache::PkgIterator pkg;
    pkgCache::VerIterator ver;

    if(pkg_ent.valid())
      {
	pkg = pkg_ent->get_pkg();
	ver = pkg_ent->get_ver();
      }

    Glib::RefPtr<Gtk::TextBuffer> textBuffer = Gtk::TextBuffer::create();

    if(!pkg_ent.valid() || pkg.end())
      {
        textBuffer->set_text("");
      }
    else
      {
        PackageInformation info(pkg, ver);

        Glib::RefPtr<Gtk::TextBuffer::Tag> nameTag = textBuffer->create_tag();
        nameTag->property_size() = 20 * Pango::SCALE;

        Glib::RefPtr<Gtk::TextBuffer::Tag> fieldNameTag = textBuffer->create_tag();
        fieldNameTag->property_weight() = 2 * Pango::SCALE;

        textBuffer->insert_with_tag(textBuffer->end(),
                                    info.Name(),
                                    nameTag);
        textBuffer->insert(textBuffer->end(), " ");
        add_hyperlink(textBuffer, textBuffer->end(), _("(more info...)"),
                      sigc::bind(sigc::ptr_fun(&InfoTab::show_tab),
                                 pkg, ver));
        textBuffer->insert(textBuffer->end(), "\n");
        textBuffer->insert(textBuffer->end(), info.ShortDescription());
        textBuffer->insert(textBuffer->end(), "\n");

        // TODO: insert a horizontal rule here (how?)

        textBuffer->insert(textBuffer->end(), "\n");

        if (ver)
          {
            //pkgRecords::Parser &rec=apt_package_records->Lookup(ver.FileList());

            textBuffer->insert_with_tag(textBuffer->end(), _("Version: "), fieldNameTag);
            textBuffer->insert(textBuffer->end(), info.Version());

            textBuffer->insert(textBuffer->end(), "\n");
            textBuffer->insert(textBuffer->end(), "\n");

            std::wstring longdesc = get_long_description(ver, apt_package_records);

            textBuffer->insert_with_tag(textBuffer->end(), _("Description: "), fieldNameTag);

            textBuffer->insert(textBuffer->end(), info.LongDescription());
          }
        else
          {
          }
      }

    pPackagesTextView->set_buffer(textBuffer);
  }

}
