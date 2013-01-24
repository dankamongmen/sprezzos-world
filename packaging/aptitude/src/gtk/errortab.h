// errortab.h                -*-c++-*-
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

#ifndef ERROR_TAB_H
#define ERROR_TAB_H

#include <gtkmm.h>

#include <apt-pkg/pkgcache.h>

#include "tab.h"

namespace gui
{
  class AptitudeWindow;

  /** \brief Responsible for managing a tree model containing the
   *  errors that have occurred during execution.
   *
   *  This class is effectively a singleton: only the main window is
   *  permitted to create one, and it creates the one and only during
   *  its own initialization.
   */
  class ErrorStore : public sigc::trackable
  {
    Glib::RefPtr<Gtk::ListStore> store;

    Gtk::TreeModelColumn<Glib::ustring> icon_column;
    Gtk::TreeModelColumn<Glib::ustring> datetime_column;
    Gtk::TreeModelColumn<Glib::ustring> text_column;

    friend class AptitudeWindow;

    /** \brief Immediately consume any pending errors from apt. */
    void consume_apt_errors();

    /** \brief Create an error tab store and register it as the
     *  global error handler.
     */
    ErrorStore();
  public:
    const Gtk::TreeModelColumn<Glib::ustring> &get_icon_column() const { return icon_column; }
    const Gtk::TreeModelColumn<Glib::ustring> &get_datetime_column() const { return datetime_column; }
    const Gtk::TreeModelColumn<Glib::ustring> &get_text_column() const { return text_column; }

    const Glib::RefPtr<Gtk::TreeModel> get_model() const { return store; }


    /** \brief Emitted when a new error is added to the store.
     *
     *  For instance, a listener on this signal could display the
     *  error tab.
     */
    sigc::signal0<void> error_added;
  };

  /** \brief Displays a log of the apt errors that have occurred
   * during execution.
   *
   *  Like ErrorStore, this is effectively a singleton and
   *  conceptually part of AptitudeWindow.
   */
  class ErrorTab : public Tab
  {
    friend class AptitudeWindow;
    ErrorTab(Glib::ustring label, const ErrorStore &store);
  public:
    void show();
  };
}

#endif // ERROR_TAB_H
