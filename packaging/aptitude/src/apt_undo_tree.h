// apt_undo_tree.h                                    -*-c++-*-
//
//   Copyright (C) 2005 Daniel Burrows
//
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.
//

#ifndef APT_UNDO_TREE_H
#define APT_UNDO_TREE_H

#include "menu_tree.h"

/** \brief Extends the basic menu_tree with support for the APT undo queue
 *
 * 
 *  Extends the basic menu_tree with support for the APT undo queue.
 *  NB: this really ought to be a mixin for menu_redirect..
 * 
 *  \file apt_undo_tree.h
 */

class apt_undo_tree : public menu_tree
{
protected:
  apt_undo_tree();
public:
  cwidget::util::ref_ptr<apt_undo_tree> create()
  {
    cwidget::util::ref_ptr<apt_undo_tree> rval = new apt_undo_tree;
    rval->decref();
    return rval;
  }

  /** \return \b true if the global APT undo queue is non-empty. */
  bool undo_undo_enabled();

  /** Execute the top undo from the global APT undo queue.
   *
   *  \return \b true.
   */
  bool undo_undo();
};

#endif // APT_UNDO_TREE_H
