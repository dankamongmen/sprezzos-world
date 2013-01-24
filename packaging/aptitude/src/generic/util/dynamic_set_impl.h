/** \file dynamic_set_impl.h */    // -*-c++-*-

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


#ifndef DYNAMIC_SET_IMPL_H
#define DYNAMIC_SET_IMPL_H

#include "dynamic_set.h"
#include "enumerator.h"

#include <boost/enable_shared_from_this.hpp>
#include <boost/make_shared.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/unordered_set.hpp>

#include <sigc++/signal.h>

namespace aptitude
{
  namespace util
  {
    /** \brief A dynamic set based on hash tables.
     *
     *  Entries in the set must be compatible with boost::hash
     *  and must support operator==.
     */
    template<typename T>
    class dynamic_set_impl
      : public boost::enable_shared_from_this<dynamic_set_impl<T> >,
        public writable_dynamic_set<T>
    {
      boost::unordered_set<T> values;
      sigc::signal<void, T> signal_inserted;
      sigc::signal<void, T> signal_removed;

    public:
      /** \warning Should only be invoked by make_shared().
       */
      dynamic_set_impl();

      /** \warning Should only be invoked by make_shared().
       */
      template<typename InputIter>
      dynamic_set_impl(const InputIter &begin, const InputIter &end);

      /** \brief Create an empty dynamic set. */
      static boost::shared_ptr<dynamic_set_impl> create();

      /** \brief Create a dynamic set from a range of values. */
      template<typename InputIter>
      static boost::shared_ptr<dynamic_set_impl>
      create(const InputIter &begin, const InputIter &end);


      void insert(const T &t);
      void remove(const T &t);

      std::size_t size();
      boost::shared_ptr<enumerator<T> > enumerate();
      sigc::connection connect_inserted(const sigc::slot<void, T> &slot);
      sigc::connection connect_removed(const sigc::slot<void, T> &slot);
    };

    template<typename T>
    dynamic_set_impl<T>::dynamic_set_impl()
    {
    }

    template<typename T>
    template<typename InputIter>
    dynamic_set_impl<T>::dynamic_set_impl(const InputIter &begin, const InputIter &end)
      : values(begin, end)
    {
    }

    template<typename T>
    boost::shared_ptr<dynamic_set_impl<T> > dynamic_set_impl<T>::create()
    {
      return boost::make_shared<dynamic_set_impl>();
    }

    template<typename T>
    template<typename InputIter>
    boost::shared_ptr<dynamic_set_impl<T> >
    dynamic_set_impl<T>::create(const InputIter &begin, const InputIter &end)
    {
      return boost::make_shared<dynamic_set_impl>(begin, end);
    }

    template<typename T>
    void dynamic_set_impl<T>::insert(const T &t)
    {
      std::pair<typename boost::unordered_set<T>::iterator, bool>
        insert_result = values.insert(t);

      if(insert_result.second)
        signal_inserted(t);
    }

    template<typename T>
    void dynamic_set_impl<T>::remove(const T &t)
    {
      const std::size_t num_erased = values.erase(t);
      if(num_erased > 0)
        signal_removed(t);
    }

    template<typename T>
    std::size_t dynamic_set_impl<T>::size()
    {
      return values.size();
    }

    template<typename T>
    boost::shared_ptr<enumerator<T> > dynamic_set_impl<T>::enumerate()
    {
      typedef typename boost::unordered_set<T>::const_iterator Iter;
      typedef iterator_enumerator_with_keepalive<Iter, dynamic_set_impl<T> > Enum;

      return boost::make_shared<Enum>(values.begin(),
                                      values.end(),
                                      this->shared_from_this());
    }

    template<typename T>
    sigc::connection dynamic_set_impl<T>::connect_inserted(const sigc::slot<void, T> &slot)
    {
      return signal_inserted.connect(slot);
    }

    template<typename T>
    sigc::connection dynamic_set_impl<T>::connect_removed(const sigc::slot<void, T> &slot)
    {
      return signal_removed.connect(slot);
    }
  }
}

#endif // DYNAMIC_SET_IMPL_H
