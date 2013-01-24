/** \file search_input.h */   // -*-c++-*-

// Copyright (C) 2010 Daniel Burrows
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

#ifndef APTITUDE_GENERIC_VIEWS_SEARCH_INPUT_H
#define APTITUDE_GENERIC_VIEWS_SEARCH_INPUT_H

#include <sigc++/connection.h>
#include <sigc++/slot.h>
#include <sigc++/trackable.h>

#include <string>

namespace aptitude
{
  namespace views
  {
    class search_input : public sigc::trackable
    {
    public:
      virtual ~search_input();

      /** \brief Retrieve the input. */
      virtual std::wstring get_search_text() = 0;

      /** \brief Set the input.
       *
       *  \param text The value to be returned from future calls to
       *  get_search_text().
       *
       *  Should trigger the text_changed signal as a side-effect.
       */
      virtual void set_search_text(const std::wstring &text) = 0;

      /** \brief Set the currently displayed error message.
       *
       *  \param msg   The new message (blank to display no message).
       */
      virtual void set_error_message(const std::wstring &msg) = 0;

      /** \brief Indicate visually whether the current text is valid,
       *  without necessarily displaying an error message.
       *
       *  For instance, this might change the background of the text
       *  entry, or display a small icon next to it.
       *
       *  \param valid \b true to indicate that the input is valid, \b
       *               false otherwise.
       */
      virtual void set_input_validity(bool valid) = 0;

      /** \brief Set the sensitivity of the "find" button.
       *
       *  \param value  \b true to enable the find button, \b false
       *                to disable it.
       */
      virtual void set_find_sensitivity(bool value) = 0;

      /** \brief Register a slot to be invoked when the search text
       *  changes.
       */
      virtual sigc::connection
      connect_search_text_changed(const sigc::slot<void> &slot) = 0;

      /** \brief Register a slot to be invoked when the user
       *  explicitly triggers a search.
       */
      virtual sigc::connection
      connect_search(const sigc::slot<void> &slot) = 0;
    };
  }
}

#endif // APTITUDE_GENERIC_VIEWS_SEARCH_INPUT_H
