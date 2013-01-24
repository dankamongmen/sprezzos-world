/** \file post_thunk.h */  // -*-c++-*-

// Copyright (C) 2009 Daniel Burrows
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

#ifndef POST_THUNK_H
#define POST_THUNK_H

#include <sigc++/bind.h>
#include <sigc++/slot.h>

#include <boost/shared_ptr.hpp>

/** \brief The type of a function that invokes a slot.  Used to safely
 *  invoke continuations from background processes in the main thread.
 */
typedef void (*post_thunk_f)(const sigc::slot<void> &);

// These are usually useful with thunk posting but arguably belong elsewhere:

/** \brief Invoke f.
 *
 *  The parameter can be bound to ensure that it doesn't get deleted.
 */
template<typename T>
inline void keepalive(const sigc::slot<void> &f,
		      const boost::shared_ptr<T> &)
{
  f();
}

template<typename T>
sigc::slot<void> make_keepalive_slot(const sigc::slot<void> &f,
				     const boost::shared_ptr<T> &k)
{
  return sigc::bind(sigc::ptr_fun(keepalive<T>), f, k);
}

#endif
