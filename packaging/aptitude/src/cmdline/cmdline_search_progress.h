/** \file cmdline_search_progress.h */  // -*-c++-*-

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

#ifndef APTITUDE_CMDLINE_SEARCH_PROGRESS_H
#define APTITUDE_CMDLINE_SEARCH_PROGRESS_H

#include <boost/shared_ptr.hpp>

namespace aptitude
{
  namespace views
  {
    class progress;
  }

  namespace util
  {
    class throttle;
  }

  namespace cmdline
  {
    /** \brief Create a progress-display object specialized for
     *  showing the progress of a search.
     *
     *  \param pattern  The search pattern that is currently being
     *                  processed; used to generate messages.
     *
     *  \param display  Used to show progress messages from the new object.
     *
     *  \param throttle Used to determine when the new object should
     *                  display messages.
     */
    boost::shared_ptr<views::progress>
    create_search_progress(const std::string &pattern,
                           const boost::shared_ptr<views::progress> &display,
                           const boost::shared_ptr<util::throttle> &throttle);
  }
}

#endif // APTITUDE_CMDLINE_SEARCH_PROGRESS_H
