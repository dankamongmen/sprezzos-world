// refcounted_base.h   -*-c++-*-
//
//   Copyright (C) 2008-2009 Daniel Burrows
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

#ifndef REFCOUNTED_BASE_H
#define REFCOUNTED_BASE_H

#include <sigc++/trackable.h>

#include <cwidget/generic/threads/threads.h>

namespace aptitude
{
  namespace util
  {
    /** \brief A class meant to be wrapped in cwidget::ref_ptr objects. */
    class refcounted_base_not_threadsafe : public sigc::trackable
    {
      mutable int refcount;

    public:
      /** \brief Create an object with a reference count of 0. */
      refcounted_base_not_threadsafe() : refcount(0) { }
      virtual ~refcounted_base_not_threadsafe();

      /** \brief Increment the reference count (normally ref_ptr will do this). */
      void incref() { ++refcount; }
      /** \brief Decrement the reference count (normally ref_ptr will do this). */
      void decref()
      {
	--refcount;
	if(refcount == 0)
	  delete this;
      }
    };

    /** \brief A class meant to be wrapped in cwidget::ref_ptr objects.
     *
     *  This variant is threadsafe, which means it's quite a bit more
     *  expensive to copy around.
     */
    class refcounted_base_threadsafe : public sigc::trackable
    {
      mutable int refcount;
      cwidget::threads::mutex m;

    public:
      /** \brief Create an object with a reference count of 0. */
      refcounted_base_threadsafe() : refcount(0) { }
      virtual ~refcounted_base_threadsafe();

      /** \brief Increment the reference count (normally ref_ptr will do this). */
      void incref()
      {
	cwidget::threads::mutex::lock l(m);
	++refcount;
      }
      /** \brief Decrement the reference count (normally ref_ptr will do this). */
      void decref()
      {
	int new_refcount;
	{
	  cwidget::threads::mutex::lock l(m);
	  --refcount;
	  new_refcount = refcount;
	}
	if(new_refcount == 0)
	  delete this;
      }
    };
  }
}

#endif
