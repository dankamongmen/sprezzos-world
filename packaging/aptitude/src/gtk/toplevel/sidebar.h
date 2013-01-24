/** \file sidebar.h */   // -*-c++-*-

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

#ifndef TOPLEVEL_SIDEBAR_H
#define TOPLEVEL_SIDEBAR_H

#include "area.h"

namespace gui
{
  namespace toplevel
  {
    /** \brief Displays a list of areas and the tabs that can be
     *  selected in each area.
     *
     *  Notifications will appear in line with the tabs.
     *
     *  Display of the actual tabs is left to another widget.
     */
    class sidebar
    {
    public:
      /** \brief Create a new sidebar.
       *
       *  \param The list of areas whose contents will appear in this
       *  sidebar.
       */
      sidebar(const boost::shared_ptr<area_list> &areas);
    };
  }
}

#endif // TOPLEVEL_SIDEBAR_H

