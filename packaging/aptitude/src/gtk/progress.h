// -*-c++-*-

// progress.h
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

#ifndef PROGRESS_H_
#define PROGRESS_H_

#include <apt-pkg/progress.h>

#include "../ui_download_manager.h" // For refcounted_progress.

namespace Gtk
{
  class Entry;
}

namespace gui
{
  class guiOpProgress : public refcounted_progress
  { // must derive to read protected member..
  private:
    bool destroyed;

    guiOpProgress();
  public:
    ~guiOpProgress();

    static cwidget::util::ref_ptr<guiOpProgress> create()
    {
      return new guiOpProgress;
    }

    void Update();

    // Clear out the progress bar.
    void destroy();
  };

  class gtkEntryOpProgress : public refcounted_progress
  {
    // A slot is used to ensure that we don't have any problems if the
    // entry is destroyed.
    sigc::slot<void, double> set_progress_fraction;
    bool destroyed;

    gtkEntryOpProgress(Gtk::Entry &entry);

  public:
    ~gtkEntryOpProgress();

    static cwidget::util::ref_ptr<gtkEntryOpProgress> create(Gtk::Entry &entry)
    {
      return new gtkEntryOpProgress(entry);
    }

    void Update();
    void destroy();
  };
}

#endif /* PROGRESS_H_ */
