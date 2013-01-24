// cmdline_progress.h                  -*-c++-*-
//
//  Copyright 2004, 2010 Daniel Burrows
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

#ifndef CMDLINE_PROGRESS_H
#define CMDLINE_PROGRESS_H

/** \file cmdline_progress.h
 */

// System includes:

#include <boost/shared_ptr.hpp>

class download_signal_log;

namespace aptitude
{
  namespace controllers
  {
    class acquire_download_progress;
  }

  namespace cmdline
  {
    class terminal_input;
    class terminal_locale;
    class terminal_metrics;
    class terminal_output;

    /** \brief Create the objects required to display the progress of a
     *  download on the given terminal.
     *
     *  \note this is just a convenience routine that ties together
     *  some other modules of code.
     *
     *  \return a pair containing the download signal log to use and a
     *  strong reference to the download progress complex of objects.
     */
    std::pair<download_signal_log *, boost::shared_ptr<controllers::acquire_download_progress> >
    create_cmdline_download_progress(const boost::shared_ptr<terminal_input> &term_input,
                                     const boost::shared_ptr<terminal_locale> &term_locale,
                                     const boost::shared_ptr<terminal_metrics> &term_metrics,
                                     const boost::shared_ptr<terminal_output> &term_output);
  }
}

#endif // CMDLINE_PROGRESS_H
