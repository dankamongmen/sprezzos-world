// broken_indicator.h                        -*-c++-*-
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


#ifndef BROKEN_INDICATOR_H
#define BROKEN_INDICATOR_H

/** \brief Generates a (hopefully) unobtrusive hint about how to use the
 *  problem resolver.
 * 
 *  \file broken_indicator.h
 */

namespace cwidget
{
  namespace widgets
  {
    class widget;
  }

  namespace util
  {
    template<class T> class ref_ptr;
  }
}

/** \return a newly generated "broken indicator". */
cwidget::util::ref_ptr<cwidget::widgets::widget> make_broken_indicator();

#endif // BROKEN_INDICATOR_H
