/** \file main_window.cc */
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
#include "main_window.h"
#include "tabs_manager.h"

#include "widgets/status_widget.h"
#include "widgets/tab_widget.h"

#include "aptitude.h"

#include <generic/apt/apt.h>
// System includes
#include <QtGui/QMenu>
#include <QtGui/QMenuBar>
#include <QtGui/QVBoxLayout>

namespace aptitude
{
  namespace gui
  {
    namespace qt
    {
      main_window::main_window()
      {
        setAttribute(Qt::WA_DeleteOnClose, true);
	setWindowTitle(_("Aptitude Package Manager"));

        create_actions();
        create_menus();
        create_gui();

	tabs_manager::get_instance()->open_packages_tab(this);

        initialize_apt();

        // TODO: Find a good default size. Add a way to save the
        // current window size on shutdown and restore it on startup.
        setGeometry(0, 0, 640, 480);
      }

      main_window::~main_window()
      {

      }

      void main_window::create_actions()
      {

      }

      void main_window::create_gui()
      {
        QWidget *main_widget = new QWidget();
        QVBoxLayout *main_layout = new QVBoxLayout(main_widget);
        main_layout->setMargin(0);
        main_layout->setSpacing(0);

        tabs = new tab_widget();
        status = new status_widget();

        main_layout->addWidget(tabs);
        main_layout->setStretchFactor(tabs, 100);
        main_layout->addWidget(status);

        setCentralWidget(main_widget);
      }

      void main_window::create_menus()
      {
        create_file_menu();
        create_packages_menu();
        create_help_menu();
      }

      void main_window::create_file_menu()
      {
        file_menu = new QMenu();
	file_menu->setTitle(_("&File"));

        menuBar()->addMenu(file_menu);
      }

      void main_window::create_packages_menu()
      {
        packages_menu = new QMenu();
	packages_menu->setTitle(_("&Packages"));

        menuBar()->addMenu(packages_menu);
      }

      void main_window::create_help_menu()
      {
        help_menu = new QMenu();
	help_menu->setTitle(_("&Help"));

        menuBar()->addMenu(help_menu);
      }

      tab_widget *main_window::get_tab_widget()
      {
	return tabs;
      }
      
      void main_window::initialize_apt()
      {
	// The progress should be displayed by status_widget
	OpProgress p;
	apt_init(&p, true, NULL);
      }
    }
  }
}

#include "main_window.moc"
