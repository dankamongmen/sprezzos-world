/** \file view.h */   // -*-c++-*-

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


#ifndef APTITUDE_GTK_TOPLEVEL_VIEW_H
#define APTITUDE_GTK_TOPLEVEL_VIEW_H

#include "model.h"

#include <boost/shared_ptr.hpp>

#include <sigc++/slot.h>

namespace Gtk
{
  class Widget;
}

namespace gui
{
  namespace toplevel
  {
    class tab_info;

    /** \brief Interface providing information about a multiplexing
     *  view onto a set of tabs, where only one tab can be displayed
     *  at a time.
     */
    class view
    {
    public:
      /** \brief Retrieve the top-level widget of the view.
       *
       *  The view object does not own this widget; the widget could
       *  be destroyed while the view is still alive.
       */
      virtual Gtk::Widget *get_widget() = 0;

      /** \brief Retrieve the currently displayed tab.
       *
       *  \return the currently displayed tab, or \b NULL if no tab is
       *  being displayed.
       */
      virtual boost::shared_ptr<tab_display_info> get_active_tab() = 0;

      /** \brief Register a slot to be invoked when the currently
       *  displayed tab changes.
       *
       *  The tab_info passed to the slot is the newly active tab, or
       *  \b NULL if no tab is being shown now.
       */
      virtual sigc::connection connect_active_tab_changed(const sigc::slot<void, boost::shared_ptr<tab_display_info> > &slot) = 0;
    };
  }
}

#endif // APTITUDE_GTK_TOPLEVEL_VIEW_H
