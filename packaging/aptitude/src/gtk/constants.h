// -*-c++-*-

// constants.h
//
//  Copyright 1999-2008 Daniel Burrows
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


#ifndef CONSTANTS_H
#define CONSTANTS_H

/** \file constants.h
 *
 *  Declarations of common constants that are used in multiple files.
 */

namespace gui
{
  /** \brief The actions that can be performed on a package-type
   *  entity (from the Package menu, for instance).
   */
  enum PackagesAction
  {
    /** \brief A synonym for Install.
     *
     *  This is used when building menus to decide whether to label
     *  the Install menu item "Install", "Upgrade", or
     *  "Install/Upgrade".
     */
    Upgrade, Downgrade, Install, Remove, Purge, Keep, Hold,
    /** \brief Mark the package as automatically installed. */
    MakeAutomatic,
    /** \brief Mark the package as manually installed. */
    MakeManual
  };
}

#endif // CONSTANTS_H
