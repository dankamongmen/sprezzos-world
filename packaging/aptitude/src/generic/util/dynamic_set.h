/** \file dynamic_set.h */   // -*-c++-*-

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

#ifndef DYNAMIC_SET_H
#define DYNAMIC_SET_H

#include <boost/shared_ptr.hpp>

#include <sigc++/connection.h>
#include <sigc++/slot.h>

namespace aptitude
{
  namespace util
  {
    template<typename T>
    class enumerator;

    /** \brief An abstract interface for an unordered collection of
     *  objects that reports changes via signals.
     */
    template<typename T>
    class dynamic_set : public sigc::trackable
    {
    public:
      virtual ~dynamic_set();

      /** \brief Get the number of elements in the set. */
      virtual std::size_t size() = 0;

      /** \brief Get an enumerator over the elements of the set.
       *
       *  The elements are retrieved in an arbitrary order.
       *
       *  The returned enumerator may hold a strong reference to the
       *  set object; do not store it in a structure that could form a
       *  reference cycle with the set.
       */
      virtual boost::shared_ptr<enumerator<T> > enumerate() = 0;


      /** \brief Register a slot to be invoked after an object is
       *  inserted into this set.
       */
      virtual sigc::connection connect_inserted(const sigc::slot<void, T> &slot) = 0;

      /** \brief Register a slot to be invoked after an object is
       *  removed from this set.
       */
      virtual sigc::connection connect_removed(const sigc::slot<void, T> &slot) = 0;
    };

    template<typename T>
    dynamic_set<T>::~dynamic_set()
    {
    }

    /** \brief An abstract interface for an unordered collection of
     *  objects that reports changes via signals and that can be
     *  modified.
     */
    template<typename T>
    class writable_dynamic_set : public dynamic_set<T>
    {
    public:
      /** \brief Insert an element into this set if it is not present.
       *
       *  If the element was not already in the set, the inserted
       *  signal is invoked.
       */
      virtual void insert(const T &t) = 0;

      /** \brief Remove an element from this set if it is present.
       *
       *  If the element was in this set, the removed signal is
       *  invoked.
       */
      virtual void remove(const T &t) = 0;
    };
  }
}

#endif // DYNAMIC_SET_H
