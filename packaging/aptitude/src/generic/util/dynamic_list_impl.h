/** \brief dynamic_list_impl.h */   // -*-c++-*-

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

#ifndef DYNAMIC_LIST_IMPL_H
#define DYNAMIC_LIST_IMPL_H

#include "dynamic_list.h"

#include <boost/enable_shared_from_this.hpp>
#include <boost/make_shared.hpp>
#include <boost/shared_ptr.hpp>

#include <vector>

namespace aptitude
{
  namespace util
  {
    /** \brief A basic implementation of the dynamic_list interface. */
    template<typename T>
    class dynamic_list_impl
      : public writable_dynamic_list<T>,
        public boost::enable_shared_from_this<dynamic_list_impl<T> >
    {
      typedef std::vector<T> collection;
      collection entries;

    public:
      dynamic_list_impl();

      /** \brief Retrieve the size of this list. */
      std::size_t size();

      /** \brief Get the idxth element of this list.
       *
       *  \param idx A nonnegative integer less than size(), giving
       *             the index within this list of the desired
       *             element.
       */
      T get_at(std::size_t idx);

      /** \brief Create an empty list. */
      static boost::shared_ptr<dynamic_list_impl> create();

      void insert(const T &t, std::size_t position);
      void remove(std::size_t position);
      void move(std::size_t from, std::size_t to);
    };

    template<typename T>
    dynamic_list_impl<T>::dynamic_list_impl()
    {
    }

    template<typename T>
    boost::shared_ptr<dynamic_list_impl<T> >
    dynamic_list_impl<T>::create()
    {
      return boost::make_shared<dynamic_list_impl>();
    }

    template<typename T>
    std::size_t dynamic_list_impl<T>::size()
    {
      return entries.size();
    }

    template<typename T>
    T dynamic_list_impl<T>::get_at(std::size_t idx)
    {
      return entries[idx];
    }

    template<typename T>
    void dynamic_list_impl<T>::insert(const T &t, std::size_t position)
    {
      entries.insert(entries.begin() + position, t);
      this->signal_inserted(t, position);
    }

    template<typename T>
    void dynamic_list_impl<T>::remove(std::size_t position)
    {
      T val = entries[position];
      entries.erase(entries.begin() + position);
      this->signal_removed(val, position);
    }

    template<typename T>
    void dynamic_list_impl<T>::move(std::size_t from, std::size_t to)
    {
      // Don't do anything at all, don't even emit a signal, if a
      // value is being moved to the same location that it already
      // occupies.
      if(from == to)
        return;

      // Simple approach: insert the value at its new location, then
      // delete it from its old location.

      // Defensive copy in case inserting into a vector from itself
      // does anything weird.  Pure paranoia.
      T val = entries[from];

      // If from < to, then we need to insert one entry farther than
      // the target location, since the entry will shift left when we
      // remove its old location.
      const std::size_t idx_to_insert = from < to  ? to + 1  :  to;

      entries.insert(entries.begin() + idx_to_insert, val);
      // Note that if to < from, we just changed the index of the
      // source of the move!  Fix it up.
      const std::size_t idx_to_delete = to < from  ?  from + 1  :  from;
      entries.erase(entries.begin() + idx_to_delete);

      this->signal_moved(val, from, to);
    }
  }
}

#endif // DYNAMIC_LIST_IMPL_H
