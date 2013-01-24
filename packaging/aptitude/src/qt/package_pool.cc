/** \file package_pool.cc */
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

// Local includes
#include "package_pool.h"

#include "package.h"

#include <generic/apt/apt.h>

// System includes
#include "vector"

namespace aptitude
{
  namespace gui
  {
    namespace qt
    {
      class package_pool::package_pool_impl : public package_pool,
                                              public sigc::trackable
      {
	std::vector<package_ptr> packages;

	/** \brief Metod invoked after cache reloading.
	 *
	 *  This method uses package_aware_object to inform others classes about
	 *  this event
	 */
	void handle_cache_reloaded();

	/** \brief Metod invoked after cache closing.
	 *
	 *  This method uses package_aware_object to inform others classes about
	 *  this event
	 */
	void handle_cache_closed();

	/** \brief Metod invoked after changing cache state.
	 *
	 *  This method uses package_aware_object to inform others classes about
	 *  packages which state has changed
	 */
	void cache_state_changed();

      public:
	/** \brief Create a new package_pool_impl. */
	explicit package_pool_impl();

	virtual ~package_pool_impl();

	/** \brief Return the globally unique instance of the package_pool_impl. */
	static package_pool_impl *get_instance();

	/** \brief Retrieve package count. */
	int get_packages_count();

	/** \brief Retrieve a pointer to package at given index. */
	package_ptr get_package_at_index(unsigned int index);

	sigc::signal0<void> cache_closed_signal;
	sigc::signal0<void> cache_reloaded_signal;
	sigc::signal1<void, std::vector<package_ptr> > cache_state_changed_signal;

	/** \brief Register a slot to be invoked when the apt cache is reloaded. */
	sigc::connection connect_cache_reloaded(const sigc::slot<void> &slot);

	/** \brief Register a slot to be invoked when the apt cache is closed. */
	sigc::connection connect_cache_closed(const sigc::slot<void> &slot);

	/** \brief Register a slot to be invoked when the state of packages changes. */
	sigc::connection connect_cache_state_changed(const sigc::slot<void, std::vector<package_ptr> > &slot);
      };

      package_pool::package_pool_impl::package_pool_impl()
      {
	cache_closed.connect(sigc::mem_fun(*this, &package_pool::package_pool_impl::handle_cache_closed));
        cache_reloaded.connect(sigc::mem_fun(*this, &package_pool::package_pool_impl::handle_cache_reloaded));

	handle_cache_reloaded();
      }

      package_pool::package_pool_impl::~package_pool_impl()
      {
      }

      void package_pool::package_pool_impl::handle_cache_reloaded()
      {
	if(apt_cache_file == NULL)
	  return;

	(*apt_cache_file)->package_state_changed.connect(sigc::mem_fun(*this, &package_pool::package_pool_impl::cache_state_changed));

	packages.reserve((*apt_cache_file)->Head().PackageCount);

	for(pkgCache::PkgIterator pkg = (*apt_cache_file)->PkgBegin(); !pkg.end(); ++pkg)
          {
            // Filter useless packages up-front.
            if(pkg.VersionList().end() && pkg.ProvidesList().end())
              continue;

            packages.push_back(package::create(pkg));
          }

	cache_reloaded_signal();
      }

      void package_pool::package_pool_impl::handle_cache_closed()
      {
	cache_closed_signal();

	packages.clear();
      }

      void package_pool::package_pool_impl::cache_state_changed()
      {
	// TODO
      }

      int package_pool::package_pool_impl::get_packages_count()
      {
	return packages.size();
      }

      package_ptr package_pool::package_pool_impl::get_package_at_index(unsigned int index)
      {
	if(index >= 0 && index < packages.size())
	  return packages.at(index);

	return package_ptr();
      }

      sigc::connection
      package_pool::package_pool_impl::connect_cache_reloaded(const sigc::slot<void> &slot)
      {
	return cache_reloaded_signal.connect(slot);
      }

      sigc::connection
      package_pool::package_pool_impl::connect_cache_closed(const sigc::slot<void> &slot)
      {
	return cache_closed_signal.connect(slot);
      }

      sigc::connection
      package_pool::package_pool_impl::connect_cache_state_changed(const sigc::slot<void, std::vector<package_ptr> > &slot)
      {
	return cache_state_changed_signal.connect(slot);
      }

      package_pool *package_pool::get_instance()
      {
	static package_pool_impl instance;

	return &instance;
      }

      package_pool::package_pool()
      {
      }

      package_pool::~package_pool()
      {
      }
    }
  }
}
