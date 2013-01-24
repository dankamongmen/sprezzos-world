/** \file tabs_notebook.h */  // -*-c++-*-


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

#ifndef TOPLEVEL_TABS_NOTEBOOK_H
#define TOPLEVEL_TABS_NOTEBOOK_H

#include "model.h"
#include "view.h"

#include <boost/shared_ptr.hpp>

namespace gui
{
  namespace toplevel
  {
    /** \brief Display a collection of tabs as a notebook widget.
     *
     *  The notebook owns the tabs that it contains, and destroys
     *  their widgets when they are removed from the given set.  It
     *  assumes that they have a floating reference and sinks that
     *  reference using manage().
     */
    boost::shared_ptr<view>
    create_tabs_notebook(const boost::shared_ptr<
                           aptitude::util::dynamic_set<
                             boost::shared_ptr<
                               tab_display_info> > > &tabs);
  }
}

#endif // TOPLEVEL_TABS_H
