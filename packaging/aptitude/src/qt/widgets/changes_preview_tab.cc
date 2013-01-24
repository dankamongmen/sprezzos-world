/** \file changes_preview_tab.cc */
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

#include "changes_preview_tab.h"

namespace aptitude
{
  namespace gui
  {
    namespace qt
    {
      changes_preview_tab::changes_preview_tab(QWidget *parent)
	: tab(tab::tab_preview, tab::closing_policy_hide, parent)
      {
      }

      changes_preview_tab::~changes_preview_tab()
      {
      }
    }
  }
}

#include "changes_preview_tab.moc"
