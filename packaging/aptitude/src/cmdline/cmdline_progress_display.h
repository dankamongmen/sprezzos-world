/** \file cmdline_progress_display.h */   // -*-c++-*-

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

#ifndef APTITUDE_CMDLINE_PROGRESS_DISPLAY_H
#define APTITUDE_CMDLINE_PROGRESS_DISPLAY_H

// System includes:
#include <boost/shared_ptr.hpp>

namespace aptitude
{
  namespace views
  {
    class progress;
  }

  namespace cmdline
  {
    class terminal_locale;
    class terminal_metrics;
    class terminal_output;
    class transient_message;

    /** \brief Create a blank progress display.
     *
     *  \param message The message object used to display the progress
     *                 message.
     *
     *  \param old_style_percentage The output mode.  Either "[NNN%] msg"
     *                              (new) or "msg... NNN%" (old).
     *
     *  \param retain_completed If \b true, completed messages will be
     *                          retained instead of being deleted.
     */
    boost::shared_ptr<views::progress>
    create_progress_display(const boost::shared_ptr<transient_message> &message,
                            bool old_style_percentage,
                            bool retain_completed);

    /** \brief Create a blank progress display.
     *
     *  This is a convenience routine, equivalent to creating a new
     *  transient message with the given terminal objects.
     */
    boost::shared_ptr<views::progress>
    create_progress_display(const boost::shared_ptr<terminal_locale> &term_locale,
                            const boost::shared_ptr<terminal_metrics> &term_metrics,
                            const boost::shared_ptr<terminal_output> &term_output);
  }
}

#endif // APTITUDE_CMDLINE_PROGRESS_DISPLAY_H
