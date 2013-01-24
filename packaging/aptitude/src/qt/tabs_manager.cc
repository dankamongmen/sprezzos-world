/** \file tabs_manager_impl.cc */
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
#include "tabs_manager.h"

#include "widgets/tab_widget.h"
#include "widgets/changes_preview_tab.h"
#include "widgets/package_info_tab.h"
#include "widgets/packages_tab.h"
#include "widgets/perform_changes_tab.h"
#include "widgets/resolver_tab.h"
#include "windows/main_window.h"

#include "aptitude.h"

// System includes
#include <boost/unordered_map.hpp>
#include <boost/unordered_set.hpp>

#include <QtGui/QApplication>

namespace aptitude
{
  namespace gui
  {
    namespace qt
    {
      class tabs_manager::tabs_manager_impl : public QObject, public tabs_manager
      {
	Q_OBJECT
	Q_DISABLE_COPY(tabs_manager_impl)

	/** \brief Pointer to the globally unique changes_preview tab,
	 *  or NULL if none exists.
	 */
	changes_preview_tab *changes_preview;

	/** \brief Pointer to the globally unique perform_changes tab,
	 *  or NULL if none exists.
	 */
	perform_changes_tab *perform_changes;

	/** \brief Pointer to the globally unique resolver tab,
	 *  or NULL if none exists.
	 */
	resolver_tab *resolver;

	boost::unordered_map<tab_widget *, boost::unordered_set<packages_tab *> > active_packages_tabs;

	/** \brief List of all opened package_info tabs.
	 *   \todo shouldn't this be unordered_map?
	 */
	boost::unordered_set<package_info_tab *> package_info_tabs;

	/** \brief List of all registered tab_widgets. */
	boost::unordered_set<tab_widget *> tab_widgets;

	/** \brief Update tabs close button for given tab_widget. */
	void update_packages_tabs_closability(tab_widget *tabs);

	/** \brief Connect all required signals from tab_widget. */
	void connect_tab_widget_signals(tab_widget *tabs);

	/** \brief Activate given tab. */
	void activate_tab(tab *tab_to_activate);

	/** \brief If the given pointer variable is not NULL, displays the
	 *  corresponding tab.
	 *
	 *  Otherwise, instantiates a new instance of its type using the
	 *  default constructor and displays that instance with the given
	 *  title, storing the new instance in the given variable.
	 */
	template<typename TabType>
	void create_or_display(TabType *output, tab_widget *tabs, const char *name);

      private Q_SLOTS:
	/** \brief Slot invoked when the user asks to close a tab
	 *  managed by this object.
	 *
	 * This slot checks whether the given tab should be closed.  If it
	 * is, the tab is deleted; otherwise, it is hidden.
	 */
	void tab_deletion_requested(tab *tab_to_delete);

	/** \brief Slot invoked when a tab_widget is destroyed
	 *
	 * This slot removes all references to the destroyed tab_widget from manager data.
	 */
	void tab_widget_destroyed();

	/** \brief Slot invoked when a tab is destroyed
	 *
	 * This slot removes all references to the destroyed tab from manager data.
	 */
	void tab_destroyed();

      public:
	/** \brief Create a new tabs_manager_impl. */
	explicit tabs_manager_impl();

	virtual ~tabs_manager_impl();

	/** \brief Display the changes_preview_tab, creating it if it does not exist.
	 *
	 *  The changes_preview_tab is globally unique.
	 */
	void open_changes_preview_tab(QWidget *tab_context);

	/** \brief Create a new packages_tab. */
	void open_packages_tab(QWidget *tab_context);

	/** \brief Display the package_info_tab corresponding to the given
	 *  package.
	 *
	 *  If a package_info_tab is already displayed for this package, it
	 *  is reused; otherwise, a new tab is created and displayed.
	 */
	void open_package_info_tab(QWidget *tab_context);

	/** \brief Display the perform_changes_tab, creating it if it does not exist.
	 *
	 *  The perform_changes_tab is globally unique.
	 */
	void open_perform_changes_tab(QWidget *tab_context);

	/** \brief Display the resolver tab, creating it if it does not exist.
	 *
	 *  The resolver tab is globally unique.
	 */
	void open_resolver_tab(QWidget *tab_context);
      };

      tabs_manager::tabs_manager_impl::tabs_manager_impl()
	: changes_preview(0), perform_changes(0), resolver(0)
      {
      }

      tabs_manager::tabs_manager_impl::~tabs_manager_impl()
      {
      }

      void tabs_manager::tabs_manager_impl::update_packages_tabs_closability(tab_widget *tabs)
      {
	const boost::unordered_set<packages_tab *> &packages_tabs = active_packages_tabs[tabs];

	if(packages_tabs.size() == 1)
	  tabs->set_tab_closable((*packages_tabs.begin()), false);
	else if(packages_tabs.size() == 2)
	{
	  for(boost::unordered_set<packages_tab *>::const_iterator it =
	      packages_tabs.begin(); it != packages_tabs.end(); ++it)
	    tabs->set_tab_closable(*it, true);
	}
      }

      void tabs_manager::tabs_manager_impl::connect_tab_widget_signals(tab_widget *tabs)
      {
	if(tab_widgets.find(tabs) == tab_widgets.end())
	{
	  tab_widgets.insert(tabs);
	  connect(tabs, SIGNAL(tab_deletion_request(tab *)), this, SLOT(tab_deletion_requested(tab *)));
	  connect(tabs, SIGNAL(destroyed()), this, SLOT(tab_widget_destroyed()));
	}
      }

      void tabs_manager::tabs_manager_impl::activate_tab(tab *tab_to_activate)
      {
	main_window *window = qobject_cast<main_window *>(tab_to_activate->window());
	if(window == NULL)
	  return;

	if(!window->isActiveWindow())
	  qApp->alert(window);

	tab_to_activate->show();

	window->get_tab_widget()->setCurrentWidget(tab_to_activate);
      }

      template<typename TabType>
      void tabs_manager::tabs_manager_impl::create_or_display(TabType *output, tab_widget *tabs, const char *name)
      {
        if(output == NULL)
          {
            connect_tab_widget_signals(tabs);

            output = new TabType;
            connect(output, SIGNAL(destroyed()), this, SLOT(tab_destroyed()));

            tabs->addTab(output, name);
          }

        activate_tab(output);
      }

      void tabs_manager::tabs_manager_impl::open_changes_preview_tab(QWidget *tab_context)
      {
	main_window *window = qobject_cast<main_window *>(tab_context->window());
	if(window == NULL)
	  return;

	create_or_display<changes_preview_tab>(changes_preview, window->get_tab_widget(), _("Preview"));
      }

      void tabs_manager::tabs_manager_impl::open_package_info_tab(QWidget *tab_context)
      {
	main_window *window = qobject_cast<main_window *>(tab_context->window());
	if(window == NULL)
	  return;
      }

      void tabs_manager::tabs_manager_impl::open_packages_tab(QWidget *tab_context)
      {
	main_window *window = qobject_cast<main_window *>(tab_context->window());
	if(window == NULL)
	  return;

	tab_widget *widget = window->get_tab_widget();

	connect_tab_widget_signals(widget);

	packages_tab *tab = new packages_tab();
	widget->addTab(tab, _("Packages"));
	widget->setCurrentWidget(tab);

	active_packages_tabs[widget].insert(tab);
	update_packages_tabs_closability(widget);
      }

      void tabs_manager::tabs_manager_impl::open_perform_changes_tab(QWidget *tab_context)
      {
	main_window *window = qobject_cast<main_window *>(tab_context->window());
	if(window == NULL)
	  return;

	create_or_display<perform_changes_tab>(perform_changes, window->get_tab_widget(), _("Perform Changes"));
      }

      void tabs_manager::tabs_manager_impl::open_resolver_tab(QWidget *tab_context)
      {
	main_window *window = qobject_cast<main_window *>(tab_context->window());
	if(window == NULL)
	  return;

	create_or_display<resolver_tab>(resolver, window->get_tab_widget(), _("Resolver"));
      }

      void tabs_manager::tabs_manager_impl::tab_deletion_requested(tab *tab_to_delete)
      {
	// when for example performing changes tab is closed, we do not destroy it but only hide
	if(tab_to_delete->get_policy() == tab::closing_policy_hide)
	    tab_to_delete->hide();
	else if(tab_to_delete->get_type() == tab::tab_packages)
	{
	  main_window *window = qobject_cast<main_window *>(tab_to_delete->window());
	  if(window == NULL)
	    return;

	  active_packages_tabs[window->get_tab_widget()].erase(qobject_cast<packages_tab *>(tab_to_delete));

	  update_packages_tabs_closability(window->get_tab_widget());

	  tab_to_delete->deleteLater();
	}
	else if(tab_to_delete->get_type() == tab::tab_info)
	{
	  package_info_tabs.erase(qobject_cast<package_info_tab *>(tab_to_delete));

	  tab_to_delete->deleteLater();
	}
      }

      void tabs_manager::tabs_manager_impl::tab_widget_destroyed()
      {
	tab_widget *widget = qobject_cast<tab_widget *>(sender());
	if(widget != NULL)
	{
	  tab_widgets.erase(widget);
	  active_packages_tabs.erase(widget);
	}
      }

      void tabs_manager::tabs_manager_impl::tab_destroyed()
      {
	if(qobject_cast<perform_changes_tab *>(sender()) != NULL)
	  perform_changes = NULL;
	else if(qobject_cast<resolver_tab *>(sender()) != NULL)
	  resolver = NULL;
	else if(qobject_cast<changes_preview_tab *>(sender()) != NULL)
	  changes_preview = NULL;
      }

      tabs_manager *tabs_manager::get_instance()
      {
	static tabs_manager_impl instance;

	return &instance;
      }

      tabs_manager::tabs_manager()
      {
      }

      tabs_manager::~tabs_manager()
      {
      }
    }
  }
}

#include "tabs_manager.mocc"
