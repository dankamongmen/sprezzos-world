/** \file text_progress.h */      // -*-c++-*-


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

#ifndef APTITUDE_UTIL_TEXT_PROGRESS_H
#define APTITUDE_UTIL_TEXT_PROGRESS_H

#include <apt-pkg/progress.h>

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

    /** \brief Create a customized text spinner that's similar to
     *  apt's spinner, but "cleans up" after itself if stdout appears
     *  to be a terminal.
     *
     *  \note Reads the global apt configuration; settings there will
     *  override the creation parameters.
     *
     *  \param require_tty_decorations If tty decorations can't be
     *                                 shown, don't show a progress
     *                                 spinner at all.  Useful for
     *                                 ensuring that the progress
     *                                 display is self-disposing.
     *
     *  \param message A transient_message object used to display
     *                 output from this progress meter.
     */
    boost::shared_ptr<OpProgress>
    make_text_progress(bool require_tty_decorations,
                       const boost::shared_ptr<views::progress> &display);

    /** \brief Convenience routine to create a default text-progress
     *  object.
     *
     *  Equivalent to constructing a standard progress display on the
     *  given terminal and passing it to make_text_progress().
     */
    boost::shared_ptr<OpProgress>
    make_text_progress(bool require_tty_decorations,
                       const boost::shared_ptr<terminal_locale> &term_locale,
                       const boost::shared_ptr<terminal_metrics> &term_metrics,
                       const boost::shared_ptr<terminal_output> &term_output);
  }
}

#endif // APTITUDE_UTIL_TEXT_PROGRESS
