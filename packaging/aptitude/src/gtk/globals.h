/** \file globals.h */       // -*-c++-*-

// Copyright (C) 2010 Daniel Burrows
// Copyright 2008-2009 Obey Arthur Liu
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

#ifndef APTITUDE_GTK_GLOBALS_H
#define APTITUDE_GTK_GLOBALS_H

#include <libglademm/xml.h>

#include <string>

namespace gui
{
  /** \brief All the global variables used by the GTK+ gui go in this
   *  namespace.
   *
   *  Please justify why each one needs to be global.  If possible,
   *  define accessor functions instead of directly exposing the
   *  variables.
   */
  namespace globals
  {
    /** \brief Find and load aptitude's glade file.
     *
     *  The first time this is called, it saves the file name that
     *  worked (if one did) so it will be returned by future calls to
     *  get_glade_filename().  Each subsequent time, the same glade
     *  file is reloaded.
     *
     *  This is global because all parts of aptitude should use the
     *  same glade file; it encapsulates the logic necessary to deal
     *  with that.
     *
     *  \param argv0   The first entry in argv; used to try to find
     *                 a local glade file relative to the aptitude
     *                 executable.
     *
     *  \return the newly loaded file, or a \b NULL pointer if the
     *  glade file couldn't be found.
     */
    Glib::RefPtr<Gnome::Glade::Xml> load_glade(const std::string &argv0);

    /** \brief Find and load aptitude's previously loaded glade file.
     *
     *  This routine will fail if load_glade(const std::string &) was
     *  not previously called.
     */
    Glib::RefPtr<Gnome::Glade::Xml> load_glade();

    /** \brief Ensure that the main loop is created.
     *
     *  Global because aptitude has only one main loop.
     *
     *  \param argc  The number of command-line arguments.
     *  \param argv  The array of command-line arguments.
     */
    void init_main_loop(int argc, char *argv[]);

    /** \brief Exit the main loop (and thus the program).
     *
     *  Global because aptitude has only one main loop.
     */
    void main_quit();

    /** \brief Run the main loop.
     *
     *  Should only be invoked from the initialization code.
     *
     *  Global because aptitude has only one main loop.
     */
    void run_main_loop();

    /** \brief Check whether a download is currently active.
     *
     *  Used to ensure that only one download runs at once; global
     *  because downloads should be mutually exclusive across the
     *  entire program.
     */
    bool is_a_download_active();

    /** \brief Set the flag that indicates that a download is active.
     *
     *  Global for the same reason that is_a_download_active is
     *  global.
     *
     *  \param value  The new value of the flag.
     */
    void set_is_a_download_active(bool value);
  }
}

#endif // APTITUDE_GTK_GLOBALS_H
