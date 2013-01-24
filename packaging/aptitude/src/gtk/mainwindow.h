/** \file mainwindow.h */ // -*-c++-*-

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

#ifndef APTITUDE_MAINWINDOW_H
#define APTITUDE_MAINWINDOW_H

#include <boost/shared_ptr.hpp>

#include <glibmm/refptr.h>

namespace Gtk
{
  class Window;
}

namespace Gnome
{
  namespace Glade
  {
    class Xml;
  }
}

namespace gui
{
  namespace toplevel
  {
    class view;
  }

  class areas;

  /** \brief Abstracted interface to the main window.
   *
   *  Decouples code that needs to access the main window from the
   *  class definition of the main window.
   */
  class main_window
  {
  public:
    virtual ~main_window();

    /** \brief Retrieve the main window's GTK+ object. */
    virtual Gtk::Window *get_window() = 0;

    /** \brief Retrieve the window's model. */
    virtual boost::shared_ptr<areas> get_areas() = 0;
  };

  /** \brief Create a main-window object.
   *
   *  \param glade   The glade file to load the main window from.
   *  \param view    The view to display as the top-level widget.
   *  \param areas   The model of this main window.  Should not be
   *                 shared with any other main window; should be
   *                 shared with the view.
   */
  boost::shared_ptr<main_window>
  create_mainwindow(const Glib::RefPtr<Gnome::Glade::Xml> &glade,
                    const boost::shared_ptr<toplevel::view> &view,
                    const boost::shared_ptr<areas> &areas);
}

#endif // APTITUDE_MAINWINDOW_H
