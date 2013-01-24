/** \file transient_message.h */     // -*-c++-*-


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

#ifndef APTITUDE_CMDLINE_TRANSIENT_MESSAGE_H
#define APTITUDE_CMDLINE_TRANSIENT_MESSAGE_H

#include <boost/shared_ptr.hpp>

namespace aptitude
{
  namespace cmdline
  {
    /** \brief A message that is displayed on the terminal, and that
     *  can be erased and redrawn with new text.
     */
    class transient_message
    {
    public:
      virtual ~transient_message();

      /** \brief Set the text displayed by this message, or hide the
       *  message entirely if the text is empty.
       *
       *  Updates the currently displayed text immediately.
       */
      virtual void set_text(const std::wstring &text) = 0;

      /** \brief Replace the currently displayed text with the given
       *  message, which will not be clipped if it exceeds the terminal
       *  width, and advance to the next line.
       */
      virtual void display_and_advance(const std::wstring &text) = 0;
    };

    class terminal_locale;
    class terminal_metrics;
    class terminal_output;

    /** \brief Create a new transient message object.
     *
     *  If the given terminal is not a tty when this function is
     *  invoked, display_and_advance will generate output as usual,
     *  but set_text will be ignored.
     *
     *  \param term_locale  Locale information for the given terminal.
     *  \param term_metrics The terminal metrics object to use.
     *  \param term_output  The terminal output object to use.
     */
    boost::shared_ptr<transient_message>
    create_transient_message(const boost::shared_ptr<terminal_locale> &term_locale,
                             const boost::shared_ptr<terminal_metrics> &term_metrics,
                             const boost::shared_ptr<terminal_output> &term_output);
  }
}

#endif // APTITUDE_CMDLINE_TRANSIENT_MESSAGE_H
