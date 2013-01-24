/** \file tab.h */   // -*-c++-*-
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

#ifndef APTITUDE_QT_TAB_H
#define APTITUDE_QT_TAB_H

#include <QtCore/QObject>
#include <QtGui/QWidget>

namespace aptitude
{
  namespace gui
  {
    namespace qt
    {
      /** \brief tab is a base class for tabs shown in main window's tab widget.
       *
       *
       *  Every tab has a name, which will be displayed in the UI, and a type,
       *  indicating which of the UI pages it implements
       */
      class tab : public QWidget
      {
	Q_OBJECT

      public:
	/** \brief Indicates tab type */
	enum tab_type
          {
            tab_packages,
            tab_info,
            tab_preview,
            tab_resolver,
            tab_perform
          };

	/** \brief What to do when the tab is closed. */
	enum closing_policy
          {
            /** Destroy the tab when it's closed. */
            closing_policy_destroy,
            /** Hide the tab when it's closed. */
            closing_policy_hide,
          };

	const QString name;

	const tab_type type;
	const closing_policy policy;

	/** \todo Shouldn't this be const? */
	QString description;

      public:
	/** \brief Create a new tab object with the given type and parent. */
	explicit tab(tab_type type, closing_policy _policy, QWidget *parent = 0);

	virtual ~tab();

	/** \brief Retrieve the name of this tab. */
	const QString & get_name() const { return name; }

	/** \brief Retrieve the description of this tab.
	 *
	 *  The description is used as the tooltip of the tab.
	 */
	const QString & get_description() const { return description; }

	/** \brief Retrieve the type of this tab. */
	tab_type get_type() { return type; }

	/** \brief Retrieve the closing policy of this tab. */
	closing_policy get_policy() { return policy; }

      };
    }
  }
}

#endif // APTITUDE_QT_TAB_H
