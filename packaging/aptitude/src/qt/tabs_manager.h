/** \file tabs_manager.h */   // -*-c++-*-
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

#ifndef APTITUDE_QT_TABS_MANAGER_H
#define APTITUDE_QT_TABS_MANAGER_H

class QWidget;

namespace aptitude
{
  namespace gui
  {
    namespace qt
    {
      class tab;

      /** \brief Defines the tabs that can be displayed in the main
       *  window, and manages their lifetimes.
       *
       *  tabs_manager provides methods to create and/or activate each
       *  of the tabs that the main window can display.  It manages the
       *  lifetime of all the tabs that it creates, ensuring that they
       *  are added to the correct tabs_widget and are destroyed when
       *  appropriate.
       *
       *  tabs_manager is responsible for managing policies regarding
       *  tab lifetimes, such as ensuring that some tabs are singletons,
       *  or ensuring that some tabs are hidden rather than being
       *  deleted.
       *
       *  tabs_manager is a singleton, reflecting the fact that the
       *  invariants it enforces are global over all tabs in the
       *  program.
       */
      class tabs_manager
      {
	tabs_manager();
	virtual ~tabs_manager();

	class tabs_manager_impl;
	friend class tabs_manager_impl;

      public:
	/** \brief Return the globally unique instance of tabs_manager. */
	static tabs_manager *get_instance();

	/** \brief Create a new packages_tab. */
	virtual void open_packages_tab(QWidget *tab_context) = 0;

	/** \brief Display the package_info_tab corresponding to the given
	 *  package.
	 *
	 *  If a package_info_tab is already displayed for this package, it
	 *  is reused; otherwise, a new tab is created and displayed.
	 */
	virtual void open_package_info_tab(QWidget *tab_context) = 0;

	/** \brief Display the resolver tab, creating it if it does not exist.
	 *
	 *  The resolver tab is globally unique.
	 */
	virtual void open_resolver_tab(QWidget *tab_context) = 0;

	/** \brief Display the changes_preview_tab, creating it if it does not exist.
	 *
	 *  The changes_preview_tab is globally unique.
	 */
	virtual void open_changes_preview_tab(QWidget *tab_context) = 0;

	/** \brief Display the perform_changes_tab, creating it if it does not exist.
	 *
	 *  The perform_changes_tab is globally unique.
	 */
	virtual void open_perform_changes_tab(QWidget *tab_context) = 0;
      };
    }
  }
}

#endif // APTITUDE_QT_TABS_MANAGER_H
