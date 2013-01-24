// hyperlink.h           -*-c++-*-
//
//   Copyright (C) 2008 Daniel Burrows
//
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.

#ifndef HYPERLINK_H
#define HYPERLINK_H

#include <gtkmm.h>

#include <sigc++/slot.h>

/** \file hyperlink.h
 *
 *  Support for adding hyperlinks into text buffers.
 */

namespace gui
{
  /** \brief Add a hyperlink to a Gtk::TextBuffer.
   *
   *  \param buffer      The buffer to which the link should be added.
   *  \param where       The location in the buffer at which the link should be added.
   *  \param link_text   The text that the user will see (will be
   *                     displayed in a standard "link style").
   *  \param link_action A callback invoked when the user clicks the link.
   *
   *  \return an iterator pointing to the end of the newly inserted text.
   */
  Gtk::TextBuffer::iterator add_hyperlink(const Glib::RefPtr<Gtk::TextBuffer> &buffer,
					  Gtk::TextBuffer::iterator where,
					  const Glib::ustring &link_text,
					  const sigc::slot0<void> &link_action);
}

#endif // HYPERLINK_H
