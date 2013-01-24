// -*-c++-*-

// dependency_chains_tab.h
//
//   Copyright (C) 2008 Daniel Burrows
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

/** \file dependency_chains_tab.h
 *
 *  Code related to searching for dependency chains in the GUI.
 */

#include <cwidget/generic/util/ref_ptr.h>

#include "tab.h"

#include "packagestab.h" // For PackageSearchList.

#include <gtkmm/entry.h>
#include <gtkmm/treemodel.h>

namespace gui
{
  class PkgView;
  class EntityView;

  class DependencyChainsTab : public Tab
  {
  private:
    cwidget::util::ref_ptr<PackageSearchList> start_search_list;
    cwidget::util::ref_ptr<PackageSearchList> end_search_list;

    cwidget::util::ref_ptr<EntityView> results_view;

    // Gets the TreeModel for the results view.
    Glib::RefPtr<Gtk::TreeModel> get_results();

    // Invoked when either view's selection changes.
    void selection_changed();

  public:
    /** \brief Create a new dependency-chains tab.
     *
     *  \param label  The label of the new tab.
     */
    DependencyChainsTab(const Glib::ustring &label);
  };
}
