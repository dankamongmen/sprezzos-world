/** \file globals.cc */


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

#include "globals.h"

#include <loggers.h>

#include <boost/make_shared.hpp>
#include <boost/ref.hpp>
#include <boost/shared_ptr.hpp>

#include <glibmm/init.h>
#include <gtkmm/main.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

using aptitude::Loggers;

namespace gui
{
  namespace globals
  {
    // The globals that have to be shared between several routines.
    namespace
    {
      boost::shared_ptr<Gtk::Main> main_loop;
      bool download_active;
    }

    Glib::RefPtr<Gnome::Glade::Xml> load_glade(const std::string &argv0)
    {
      // The global filename:
      static std::string glade_filename;

      if(!glade_filename.empty())
        {
          try
            {
              return Gnome::Glade::Xml::create(glade_filename);
            }
          catch(Gnome::Glade::XmlError &err)
            {
              LOG_ERROR(Loggers::getAptitudeGtkGlobals(),
                        "Can't load " << glade_filename << ": " << err.what());
              // Try to find the glade file again anyway.
            }
        }

      Glib::RefPtr<Gnome::Glade::Xml> rval;

#ifndef DISABLE_PRIVATE_GLADE_FILE
      if(!argv0.empty())
      {
        // Use the basename of argv0 to find the Glade file.
        std::string argv0_path;
        std::string::size_type last_slash = argv0.rfind('/');
        if(last_slash != std::string::npos)
          {
            while(last_slash > 0 && argv0[last_slash - 1] == '/')
              --last_slash;
            argv0_path = std::string(argv0, 0, last_slash);
          }
        else
          argv0_path = '.';

        //Loading the .glade file and widgets
        const std::string glade_main_file = argv0_path + "/gtk/aptitude.glade";
        try
          {
            rval = Gnome::Glade::Xml::create(glade_main_file);
          }
        catch(Gnome::Glade::XmlError &err)
          {
            LOG_DEBUG(Loggers::getAptitudeGtkGlobals(),
                      "Can't load " << glade_main_file << ": " << err.what());
          }
      }
#endif

      if(!rval)
        {
          const std::string glade_main_file =
            std::string(PKGDATADIR) + "/aptitude.glade";

          try
            {
              rval = Gnome::Glade::Xml::create(glade_main_file);
            }
          catch(Gnome::Glade::XmlError &err)
            {
              LOG_ERROR(Loggers::getAptitudeGtkGlobals(),
                        "Can't load " << glade_main_file << ": " << err.what());
            }
        }

      if(rval)
        {
          LOG_INFO(Loggers::getAptitudeGtkGlobals(),
                   "UI definition loaded from " << glade_filename);
          glade_filename = rval->get_filename();
        }

      return rval;
    }

    Glib::RefPtr<Gnome::Glade::Xml> load_glade()
    {
      return load_glade(std::string());
    }

    void init_main_loop(int argc, char *argv[])
    {
      main_loop = boost::make_shared<Gtk::Main>(boost::ref(argc),
                                                boost::ref(argv));
    }

    void main_quit()
    {
      if(main_loop.get() != NULL)
        {
          LOG_TRACE(Loggers::getAptitudeGtkGlobals(),
                    "Telling the main loop to quit.");

          main_loop->quit();
        }
      else
        LOG_ERROR(Loggers::getAptitudeGtkGlobals(),
                  "main_quit() invoked with no main loop.");
    }

    void run_main_loop()
    {
      LOG_TRACE(Loggers::getAptitudeGtkGlobals(),
                "Entering main loop.");

      Gtk::Main::run();
    }

    bool is_a_download_active()
    {
      return download_active;
    }

    void set_is_a_download_active(bool value)
    {
      if(value == download_active)
        // Something wants to start a download when one is already
        // running, or stop when one isn't running.
        LOG_ERROR(Loggers::getAptitudeGtkGlobals(),
                  "Attempt to set the download_active flag to its current value ("
                  << download_active << ").");
      else
        download_active = value;
    }
  }
}
