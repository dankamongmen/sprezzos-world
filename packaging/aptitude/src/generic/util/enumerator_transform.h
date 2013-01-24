/** \file enumerator_transform.h */   // -*-c++-*-

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

#ifndef ENUMERATOR_TRANSFORM_H
#define ENUMERATOR_TRANSFORM_H

#include "enumerator.h"

#include <boost/function.hpp>
#include <boost/make_shared.hpp>

namespace aptitude
{
  namespace util
  {
    /** \brief An enumerator that applies a function argument to each
     *  element returned by another enumerator.
     *
     *  \tparam From   The original type stored in the enumeration.
     *  \tparam To     The type of value returned by this enumeration.
     */
    template<typename From, typename To>
    class enumerator_transform : public enumerator<To>
    {
      const boost::shared_ptr<enumerator<From> > sub_enumerator;
      const boost::function<To (From)> f;

    public:
      /** \warning Should only be used by create(). */
      enumerator_transform(const boost::shared_ptr<enumerator<From> > &
                           _sub_enumerator,
                           const boost::function<To (From)> &_f);

      /** \brief Create an enumerator_transform.
       *
       *  \param sub_enumerator   The enumerator that is to be transformed.
       *  \param f                The function to apply to each element of
       *                           _sub_enumerator.  Normally this should
       *                           not have side-effects.
       */
      static boost::shared_ptr<enumerator_transform>
      create(const boost::shared_ptr<enumerator<From> > &sub_enumerator,
             const boost::function<To (From)> &f);

      bool advance();
      To get_current();
    };

    template<typename From, typename To>
    enumerator_transform<From, To>::enumerator_transform(const boost::shared_ptr<enumerator<From> > &_sub_enumerator,
                                                         const boost::function<To (From)> &_f)
      : sub_enumerator(_sub_enumerator),
        f(_f)
    {
    }

    template<typename From, typename To>
    boost::shared_ptr<enumerator_transform<From, To> >
    enumerator_transform<From, To>::create(const boost::shared_ptr<enumerator<From> > &sub_enumerator,
                                           const boost::function<To (From)> &f)
    {
      return boost::make_shared<enumerator_transform<From, To> >(sub_enumerator, f);
    }

    template<typename From, typename To>
    bool enumerator_transform<From, To>::advance()
    {
      return sub_enumerator->advance();
    }

    template<typename From, typename To>
    To enumerator_transform<From, To>::get_current()
    {
      return f(sub_enumerator->get_current());
    }
  }
}

#endif // ENUMERATOR_TRANSFORM_H
