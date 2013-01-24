// progress.cc
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

#include "gtk/progress.h"

#include "gui.h"

namespace gui
{
  static double sanitizePercentFraction(float percent)
  {
    double rval = (double)percent / 100;
    if (rval < 0)
      rval = 0;
    if (rval > 1)
      rval = 1;
    return rval;
  }

  guiOpProgress::guiOpProgress()
    : destroyed(false)
  {
    pMainWindow->get_progress_bar()->show();
  }

  guiOpProgress::~guiOpProgress()
  {
    destroy();
  }

  void guiOpProgress::Update()
  {
    if (CheckChange(0.1))
    {
      pMainWindow->get_progress_bar()->set_text(Op);
      pMainWindow->get_progress_bar()->set_fraction(sanitizePercentFraction(Percent));
      gtk_update();
    }
  }

  void guiOpProgress::destroy()
  {
    if(!destroyed)
      {
	destroyed = true;
	pMainWindow->get_progress_bar()->set_text("");
	pMainWindow->get_progress_bar()->set_fraction(0);
	pMainWindow->get_progress_bar()->hide();
      }
  }


  gtkEntryOpProgress::gtkEntryOpProgress(Gtk::Entry &entry)
    : set_progress_fraction(sigc::mem_fun(entry, &Gtk::Entry::set_progress_fraction)),
      destroyed(false)
  {
  }

  gtkEntryOpProgress::~gtkEntryOpProgress()
  {
    destroy();
  }

  void gtkEntryOpProgress::Update()
  {
    if(!destroyed)
      {
	if(CheckChange(0.1))
	  {
	    set_progress_fraction(sanitizePercentFraction(Percent));
	    gtk_update();
	  }
      }
  }

  void gtkEntryOpProgress::destroy()
  {
    if(!destroyed)
      {
	destroyed = true;
	set_progress_fraction(0);
      }
  }
}
