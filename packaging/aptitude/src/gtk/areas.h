/** \file areas.h */    // -*-c++-*-


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

#ifndef APTITUDE_GTK_AREAS_H
#define APTITUDE_GTK_AREAS_H

#include <boost/shared_ptr.hpp>

namespace gui
{
  namespace toplevel
  {
    class area_info;
    class area_list;
  }

  /** \brief Interface to the standard set of areas (Upgrade, Browse,
   *  etc).
   *
   *  This class effectively defines the structure of a top-level
   *  aptitude window and the details of each area (name, icon, etc).
   *
   *  Note that this should only have one subclass!
   */
  class areas
  {
  public:
    virtual ~areas();

    /** \brief Get a list of all the areas. */
    virtual boost::shared_ptr<toplevel::area_list> get_areas() = 0;

    /** \brief The Browse area. */
    virtual boost::shared_ptr<toplevel::area_info> get_browse() = 0;

    /** \brief The Go area. */
    virtual boost::shared_ptr<toplevel::area_info> get_go() = 0;

    /** \brief The Preferences area. */
    virtual boost::shared_ptr<toplevel::area_info> get_preferences() = 0;

    /** \brief The Search area. */
    virtual boost::shared_ptr<toplevel::area_info> get_search() = 0;

    /** \brief The Upgrade area. */
    virtual boost::shared_ptr<toplevel::area_info> get_upgrade() = 0;
  };


  /** \brief Create a new standard area list with no tabs. */
  boost::shared_ptr<areas> create_areas();
}

#endif // APTITUDE_GTK_AREAS_H
