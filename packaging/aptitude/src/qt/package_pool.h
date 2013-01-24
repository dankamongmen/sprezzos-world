/** \file package_pool.h */   // -*-c++-*-
//
// Copyright (C) 2010 Piotr Galiszewski
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

#ifndef APTITUDE_QT_PACKAGE_POOL_H
#define APTITUDE_QT_PACKAGE_POOL_H

// System includes
#include <boost/shared_ptr.hpp>

#include <sigc++/connection.h>
#include <sigc++/slot.h>

#include <vector>

namespace aptitude
{
  namespace gui
  {
    namespace qt
    {
      class package;
      typedef boost::shared_ptr<package> package_ptr;

      /** \brief A global pool of package objects.
       *
       *  The pool is filled when the object is created or when the
       *  cache is (re)loaded, and it is emptied when the cache is
       *  closed.
       *
       *  This pool also interprets signals for the benefit of its
       *  client code.
       */
      class package_pool
      {
	package_pool();
	virtual ~package_pool();

	class package_pool_impl;
	friend class package_pool_impl;

      public:
	/** \brief Return the globally unique instance of package_pool. */
	static package_pool *get_instance();

	/** \brief Retrieve the number of packages in this pool.
         *
         *  \return the number of package objects in this pool -- may
         *  not be equal to the PackageCount of the apt cache file,
         *  because some packages could be discarded in the pool.
         */
	virtual int get_packages_count() = 0;

	/** \brief Retrieve a pointer to the package at given index.
         *
         *  \param index The zero-based index of the package.  This is
         *                \e not equal to Pkg->ID.
         *
         *  \return the package at index if index is between 0 and
         *  get_packages_count() - 1, and an invalid pointer otherwise.
         */
	virtual package_ptr get_package_at_index(unsigned int index) = 0;

	/** \brief Register a slot to be invoked when the apt cache is reloaded.
         *
         *  The slot is guaranteed to be invoked after the pool has
         *  been initialized from the new apt cache.
         */
	virtual sigc::connection connect_cache_reloaded(const sigc::slot<void> &slot) = 0;

	/** \brief Register a slot to be invoked when the apt cache is closed.
         *
         *  The signal is guaranteed to be invoked before the pool of
         *  packages is emptied.
         */
	virtual sigc::connection connect_cache_closed(const sigc::slot<void> &slot) = 0;

	/** \brief Register a slot to be invoked when the state of one
         *  or more packages changes.
         */
	virtual sigc::connection connect_cache_state_changed(const sigc::slot<void, std::vector<package_ptr> > &slot) = 0;
      };
    }
  }
}

#endif // APTITUDE_QT_PACKAGE_POOL_H
