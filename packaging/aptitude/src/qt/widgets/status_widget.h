/** \file status_widget.h */   // -*-c++-*-
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

#ifndef APTITUDE_QT_STATUS_WIDGET_H
#define APTITUDE_QT_STATUS_WIDGET_H

// Local includes
#include "tab.h"

// System includes
#include <QtGui/QWidget>

class QLabel;
class QPushButton;
class QStackedWidget;

namespace aptitude
{
  namespace gui
  {
    namespace qt
    {
      /** \brief Widget showing informations about current state of the program.
       *
       *  This widget is always displayed at the bottom of the main window and
       *  is divided into two parts.
       *  The first part shows information about requested changes to package
       *  states, and the second part shows the progress of any currently running process'
       *  activities. At the top of the widget, a description of the last activity to complete
       *  is displayed.
       */
      class status_widget : public QWidget
      {
	Q_OBJECT

	QPushButton *show_changes_button;
	QPushButton *apply_changes_button;
	QPushButton *cancel_button;
	QPushButton *resolve_dependencies_button;

	QLabel *changes_summary;

	QWidget *changes_widget;
	QWidget *progress_widget;

	QStackedWidget *stacked_widget;

	/** \brief Create layouts and widgets. */
	void create_gui();

	/** \brief Create layout and widget of changes preview widget. */
	void create_changes_widget();

	/** \brief Create layout and widget of progress widget. */
	void create_progress_widget();

	/** \brief Update information about the current staged changes. */
	void update_changes_summary();

      private Q_SLOTS:
	/** \brief Slot triggered when the Show Changes button has been clicked
	 *  by the user.
	 *
	 *  This slot activates the changes preview tab, creating a
	 *  new one if it doesn't exist yet.
	 */
	void show_changes_button_clicked();

	/** \brief Slot triggered when the Apply Changes button has been clicked
	 *  by the user.
	 *
	 *  This slots starts applying the current changes in the
	 *  background.
	 */
	void apply_changes_button_clicked();

	/** \brief Slot triggered when the Resolve Dependencies button has been clicked
	 *  by the user.
	 *
         *  This slot activates the resolver tab, creating a new one
         *  if it doesn't exist yet.
	 */
	void resolve_dependencies_button_clicked();

	/** \brief Slot triggered when the Cancel button has been clicked
	 *  by the user.
	 *
	 *  This slot aborts the most recently started background
	 *  task.
	 */
	void cancel_button_clicked();

      public:
	/** \brief Create a new status_widget object. */
	explicit status_widget(QWidget *parent = 0);

	virtual ~status_widget();

      public Q_SLOTS:
	/** \brief Slot triggered when the main tab_widget is switched
         *  to a new tab.
	 *
	 *  This slot is responsible for ensuring that the correct
	 *  buttons are enabled for the newly selected tab.  For
	 *  example, when the Changes Preview tab is active, Show
	 *  Changes button should be disabled in order to not confuse
	 *  the user.
	 */
	void current_tab_changed(tab::tab_type type);
      };
    }
  }
}

#endif // APTITUDE_QT_STATUS_WIDGET_H
