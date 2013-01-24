/** \file cmdline_download_progress_display.h */    // -*-c++-*-

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

#ifndef CMDLINE_DOWNLOAD_PROGRESS_DISPLAY
#define CMDLINE_DOWNLOAD_PROGRESS_DISPLAY

// Local includes:
#include <generic/views/download_progress.h>

// System includes:
#include <boost/shared_ptr.hpp>

namespace aptitude
{
  namespace cmdline
  {
    class terminal_input;
    class terminal_locale;
    class terminal_metrics;
    class transient_message;

    /** \brief Used to display the current status of a command-line download.
     *
     *  This class exists for purely pragmatic reasons.
     *
     *  Splitting it out reduces the number of cases I need to check
     *  in the unit tests (display_messages and hide_status can be
     *  tested separately instead of having to test all four variants
     *  of them).
     */
    class download_status_display
    {
    public:
      virtual ~download_status_display();

      virtual void display_status(const views::download_progress::status &status) = 0;
    };

    /**
     * Create a new command-line download progress display.
     *
     *  \param message    The transient-message object used to display
     *                    messages about the download.
     *  \param terminal_input   The terminal from which to read input.
     *  \param display_messages If \b false, messages regarding download
     *                          events will be suppressed.
     */
    boost::shared_ptr<views::download_progress>
    create_download_progress_display(const boost::shared_ptr<transient_message> &message,
                                     const boost::shared_ptr<download_status_display> &status_display,
                                     const boost::shared_ptr<terminal_input> &term_input,
                                     bool display_messages);


    /** \brief Create a new command-line download status display object.
     *
     *  \param message    The transient-message object used to display the
     *                    download status.
     *  \param terminal_locale  The locale of the given terminal.
     *  \param terminal_metrics The dimensions of the terminal on which
     *                          the display will appear.
     *  \param hide_status      If \b true, all output from this object will
     *                          be suppressed.
     *
     *  \note The reason for including hide_status here is primarily that
     *        it makes testing more effective; the callers of this code aren't
     *        currently testable.
     */
    boost::shared_ptr<download_status_display>
    create_cmdline_download_status_display(const boost::shared_ptr<transient_message> &message,
                                           const boost::shared_ptr<terminal_locale> &term_locale,
                                           const boost::shared_ptr<terminal_metrics> &term_metrics,
                                           bool hide_status);
  }
}

#endif // CMDLINE_DOWNLOAD_PROGRESS_DISPLAY
