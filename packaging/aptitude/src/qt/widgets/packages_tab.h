/** \file packages_tab.h */   // -*-c++-*-
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

#ifndef APTITUDE_QT_PACKAGES_TAB_H
#define APTITUDE_QT_PACKAGES_TAB_H

// Local includes
#include "tab.h"

class QComboBox;
class QLineEdit;
class QListView;
class QPushButton;
class QTreeView;

namespace aptitude
{
  namespace gui
  {
    namespace qt
    {
      /** \brief Tab containing widgets which allow searching and
       *  browsing a list of packages.
       */
      class packages_tab : public tab
      {
	Q_OBJECT

	QComboBox *show_combobox;
	QListView *filters_view;
	QPushButton *manage_filters_button;

	QComboBox *search_by_combobox;
	QLineEdit *search_edit;
	QTreeView *packages_view;

	/** \brief Create layouts and widgets. */
	void create_gui();

	/** \brief Create left widget. */
	QWidget *create_left_widget();

	/** \brief Create right widget. */
	QWidget *create_right_widget();

      private Q_SLOTS:
	/** \brief Slot invoked when the user clicked on the Manage
         *  Filters button.
	 *
	 *  \todo This slot should open Manage Filters window
	 */
	void manage_filters_clicked();

      public:
	/** \brief Create a new packages_tab object. */
	explicit packages_tab(QWidget *parent = 0);

	virtual ~packages_tab();
      };
    }
  }
}

#endif // APTITUDE_QT_PACKAGES_TAB_H
