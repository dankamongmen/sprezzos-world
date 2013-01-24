// solution_dialog.h                                 -*-c++-*-
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

#ifndef SOLUTION_DIALOG_H
#define SOLUTION_DIALOG_H

/** \brief Display the current solution
 * 
 *  \file solution_dialog.h
 */

namespace cwidget
{
  namespace util
  {
    template<class T> class ref_ptr;
  }
  namespace widgets
  {
    class widget;
    typedef util::ref_ptr<widget> widget_ref;
  }
}

cwidget::widgets::widget_ref make_solution_dialog();

#endif // SOLUTION_DIALOG_H
