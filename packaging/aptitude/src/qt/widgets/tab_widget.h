/** \file tab_widget.h */   // -*-c++-*-
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

#ifndef APTITUDE_QT_TAB_WIDGET_H
#define APTITUDE_QT_TAB_WIDGET_H

#include <QtGui/QTabWidget>

#include "tab.h"

class QKeyEvent;

namespace aptitude
{
  namespace gui
  {
    namespace qt
    {
      /** \brief A widget to manage the active views of the program.
       *
       *  This widget is the central part of the main program window.
       *  Client code can receive events when a new tab is selected or
       *  when the user asks to close a tab.
       */
      class tab_widget : public QTabWidget
      {
	Q_OBJECT

	/** \brief Activates the tab to the left of the active tab. */
	void switch_tab_left();

	/** \brief Activates the tab to the right of the active tab. */
	void switch_tab_right();

      private Q_SLOTS:
	/** \brief Slot invoked when user clicks on the close button
	 *  of a tab.
	 *
	 *  The signal is propagated via tab_deletion_request.
	 *
	 *  \param index The zero-based index of the affected tab in
	 *  the list of active tabs.
	 */
	void tab_deletion_requested(int index);

	/** \brief Slot invoked when the currently active tab has
	 *  changed.
	 *
	 *  This signal is propagated via current_tab_changed.
	 *
	 *  \param index The zero-based index of the newly selected
	 *  tab in the list of active tabs.
	 */
	void on_tab_change(int index);

      public:
	/** \brief Create a new tab widget with the given parent. */
	explicit tab_widget(QWidget *parent = 0);

	virtual ~tab_widget();

	/** \brief Enable or disable the close button on the given tab. */
	void set_tab_closable(tab *closable_tab, bool enabled);

      public Q_SLOTS:
	/** \brief Slot invoked when the user types a shortcut.
	 *
	 *  If the given key corresponds to a shortcut recognized by
	 *  this widget, then the appropriate action is taken and
	 *  handled is set to \b true.
	 */
	void key_pressed(QKeyEvent *e, bool &handled);

      Q_SIGNALS:
	/** \brief Signal emitted when the current tab has changed.
	 *
	 *  \param tab_type the type of the newly activated tab.
	 */
	void current_tab_changed(tab::tab_type);

	/** \brief Signal emitted when user asks to close a tab. */
	void tab_deletion_request(tab *);
      };
    }
  }
}

#endif // APTITUDE_QT_TAB_WIDGET_H
