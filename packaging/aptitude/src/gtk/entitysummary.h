/** \file entitysummary.h */       // -*-c++-*-


// Copyright (C) 2009 Daniel Burrows
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


#ifndef ENTITYSUMMARY_H
#define ENTITYSUMMARY_H

#include <cwidget/generic/util/ref_ptr.h>

#include <gtkmm/textview.h>

#include <gtk/entityview.h>

namespace gui
{
  /** \brief Display a summary of an entity in a text widget.
   *
   *  This is the same summary that's displayed next to the package
   *  list in the search view, for instance.
   */
  void show_entity_summary(const cwidget::util::ref_ptr<Entity> &ent,
			   Gtk::TextView *textView);
}

#endif // ENTITYSUMMARY_H
