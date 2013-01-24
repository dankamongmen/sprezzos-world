/** \file search_input_entry.h */     // -*-c++-*-

// Copyright (C) 2010 Daniel Burrows
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

#ifndef APTITUDE_GTK_VIEW_IMPLS_SEARCH_INPUT_ENTRY_H
#define APTITUDE_GTK_VIEW_IMPLS_SEARCH_INPUT_ENTRY_H

#include <boost/shared_ptr.hpp>

#include <gtkmm/button.h>
#include <gtkmm/entry.h>
#include <gtkmm/label.h>

namespace aptitude
{
  namespace views
  {
    class search_input;
  }
}

namespace gui
{
  namespace view_impls
  {
    /** \brief Create a search input view based on a text entry
     *  object.
     *
     *  \param search_entry               The text entry to manage.
     *  \param error_label                The label in which to display error messages.
     *  \param find_button                A button that the user can use to perform a search.
     */
    boost::shared_ptr<aptitude::views::search_input>
    create_search_input_entry(Gtk::Entry *search_entry,
                              Gtk::Label *error_label,
                              Gtk::Button *find_button);
  }
}

#endif // APTITUDE_GTK_VIEW_IMPLS_SEARCH_ENTRY_H

