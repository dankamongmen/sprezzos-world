/** \file maybe.h */ // -*-c++-*-

//   Copyright (C) 2009 Daniel Burrows

//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.

//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.

//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.

#ifndef MAYBE_H
#define MAYBE_H

/** \brief Simple generic "maybe it exists, maybe it doesn't" class.
 *
 *  \tparam T   The type of value to store.  Must have a default
 *              constructor.
 */
template<typename T>
class maybe
{
  bool has_value;
  T value;
public:
  maybe()
    : has_value(false), value()
  {
  }

  maybe(const T &_value)
    : has_value(true), value(_value)
  {
  }

  // Default copy constructor and assignment.

  bool get_has_value() const { return has_value; }
  /** \brief Retrieve the value stored in this "maybe", if there is
   *  one.
   *
   *  \param out  The location in which to store the value.
   *  \return \b true if this object has a value.
   */
  bool extract(T &out) const { if(has_value) out = value; return has_value; }

  /** \brief Directly extract the value.
   *
   *  This is efficient but unsafe; it assumes the user has already
   *  invoked get_has_value().
   */
  const T &get_value() const { return value; }

  /** \brief Directly extract the value.
   *
   *  This is efficient but unsafe; it assumes the user has already
   *  invoked get_has_value().
   */
  T &get_value() { return value; }
};

#endif // MAYBE_H
