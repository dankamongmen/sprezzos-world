/** \file tab_widget.cc */
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

#include <QtGui/QTabBar>

#include "tab_widget.h"

namespace aptitude
{
  namespace gui
  {
    namespace qt
    {
      tab_widget::tab_widget(QWidget *parent)
	: QTabWidget(parent)
      {
	setTabsClosable(true);
	setUsesScrollButtons(true);
	setMovable(true);

	connect(tabBar(), SIGNAL(tabCloseRequested(int)), this, SLOT(tab_deletion_requested(int)));
	connect(this, SIGNAL(currentChanged(int)), this, SLOT(on_tab_change(int)));
      }

      tab_widget::~tab_widget()
      {
      }

      void tab_widget::tab_deletion_requested(int index)
      {
	if(index < 0)
	  return;

	tab *tab_at_index = qobject_cast<tab *>(widget(index));

	if(!tab_at_index)
	  return;

	Q_EMIT(tab_deletion_request(tab_at_index));
      }

      void tab_widget::set_tab_closable(tab *closable_tab, bool enabled)
      {
	int index = indexOf(closable_tab);

	if(index < 0)
	  return;

	tabBar()->tabButton(0, QTabBar::RightSide)->setEnabled(false);
      }

      void tab_widget::on_tab_change(int index)
      {
	if(index < 0)
	  return;

	tab *tab_at_index = qobject_cast<tab *>(widget(index));

	if(!tab_at_index)
	  return;

	Q_EMIT(current_tab_changed(tab_at_index->get_type()));
      }

      void tab_widget::switch_tab_left()
      {
	if(currentIndex() == 0)
	  setCurrentIndex(count() - 1);
	else
	  setCurrentIndex(currentIndex() - 1);
      }

      void tab_widget::switch_tab_right()
      {
	if(currentIndex() == (count() - 1))
	  setCurrentIndex(0);
	else
	  setCurrentIndex(currentIndex() + 1);
      }

      void tab_widget::key_pressed(QKeyEvent *e, bool &handled)
      {
	//TODO: make it configurable
	if(handled)
	  return;

	//TODO: make shortcuts configurable and find good default values
	handled = true;
	if(true)
	  switch_tab_left();
	else if(false)
	  switch_tab_right();
	else
	  handled = false;
      }
    }
  }
}

#include "tab_widget.moc"
