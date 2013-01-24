/** \file perform_changes_tab.h */   // -*-c++-*-
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

#ifndef APTITUDE_QT_PERFORM_CHANGES_TAB_H
#define APTITUDE_QT_PERFORM_CHANGES_TAB_H

#include "tab.h"

namespace aptitude
{
  namespace gui
  {
    namespace qt
    {
      /** \brief Tab containing details informations about currently
       * performed changes (e.g. installing and removing packages).
       */
      class perform_changes_tab : public tab
      {
        Q_OBJECT

      public:
	/** \brief Create a new perform_changes_tab object. */
	explicit perform_changes_tab(QWidget *parent = 0);

	/** \brief Destroy a perform_changes_tab object. */
	virtual ~perform_changes_tab();
      };
    }
  }
}

#endif // APTITUDE_QT_PERFORM_CHANGES_TAB_H
