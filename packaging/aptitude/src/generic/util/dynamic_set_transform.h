/** \file dynamic_set_transform.h */       // -*-c++-*-

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

#ifndef DYNAMIC_SET_TRANSFORM_H
#define DYNAMIC_SET_TRANSFORM_H

#include "dynamic_set.h"
#include "enumerator_transform.h"

#include <boost/function.hpp>
#include <boost/make_shared.hpp>
#include <boost/shared_ptr.hpp>

#include <sigc++/signal.h>
#include <sigc++/slot.h>

namespace aptitude
{
  namespace util
  {
    /** \brief A wrapper that passes each element in a dynamic set
     *  through a function before passing it to client code.
     *
     *  \tparam From   The type of the values stored in the
     *                 underlying dynamic_set.
     *  \tparam To     The type of value produced by the transform.
     */
    template<typename From, typename To>
    class dynamic_set_transform : public dynamic_set<To>
    {
      boost::shared_ptr<dynamic_set<From> > wrapped_set;
      boost::function<To (From)> f;

      sigc::signal<void, To> signal_inserted;
      sigc::signal<void, To> signal_removed;

      void handle_inserted(const From &from);
      void handle_removed(const From &from);

    public:
      /** \warning Should only be used by create(). */
      dynamic_set_transform(const boost::shared_ptr<dynamic_set<From> > &_wrapped_set,
                            const boost::function<To (From)> &_f);

      static boost::shared_ptr<dynamic_set_transform>
      create(const boost::shared_ptr<dynamic_set<From> > &wrapped_set,
             const boost::function<To (From)> &f);

      std::size_t size();
      boost::shared_ptr<enumerator<To> > enumerate();
      sigc::connection connect_inserted(const sigc::slot<void, To> &slot);
      sigc::connection connect_removed(const sigc::slot<void, To> &slot);
    };

    template<typename From, typename To>
    dynamic_set_transform<From, To>::dynamic_set_transform(const boost::shared_ptr<dynamic_set<From> > &_wrapped_set,
                                                           const boost::function<To (From)> &_f)
      : wrapped_set(_wrapped_set),
        f(_f)
    {
      wrapped_set->connect_inserted(sigc::mem_fun(*this, &dynamic_set_transform::handle_inserted));
      wrapped_set->connect_removed(sigc::mem_fun(*this, &dynamic_set_transform::handle_removed));
    }

    template<typename From, typename To>
    boost::shared_ptr<dynamic_set_transform<From, To> >
    dynamic_set_transform<From, To>::create(const boost::shared_ptr<dynamic_set<From> > &wrapped_set,
                                            const boost::function<To (From)> &f)
    {
      return boost::make_shared<dynamic_set_transform<From, To> >(wrapped_set, f);
    }

    template<typename From, typename To>
    std::size_t dynamic_set_transform<From, To>::size()
    {
      return wrapped_set->size();
    }

    template<typename From, typename To>
    boost::shared_ptr<enumerator<To> > dynamic_set_transform<From, To>::enumerate()
    {
      return enumerator_transform<From, To>::create(wrapped_set->enumerate(), f);
    }

    template<typename From, typename To>
    void dynamic_set_transform<From, To>::handle_inserted(const From &from)
    {
      signal_inserted(f(from));
    }

    template<typename From, typename To>
    void dynamic_set_transform<From, To>::handle_removed(const From &from)
    {
      signal_removed(f(from));
    }

    template<typename From, typename To>
    sigc::connection dynamic_set_transform<From, To>::connect_inserted(const sigc::slot<void, To> &slot)
    {
      return signal_inserted.connect(slot);
    }

    template<typename From, typename To>
    sigc::connection dynamic_set_transform<From, To>::connect_removed(const sigc::slot<void, To> &slot)
    {
      return signal_removed.connect(slot);
    }
  }
}

#endif // DYNAMIC_SET_TRANSFORM_H

