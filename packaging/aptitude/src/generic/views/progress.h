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

#ifndef APTITUDE_GENERIC_VIEWS_PROGRESS_H
#define APTITUDE_GENERIC_VIEWS_PROGRESS_H

// System includes:
#include <boost/shared_ptr.hpp>

namespace aptitude
{
  namespace util
  {
    class progress_info;
  }

  namespace views
  {
    /** \brief A general class for displaying a single line of
     *  progress information.
     *
     *  The progress information is delivered as a progress_info
     *  object.  A blank progress_info causes the display to be
     *  erased.  A "pulse" mode progress_info displays a message with
     *  no percent indicator.  And a "bar" mode progress_info displays
     *  a message with a percent indicator.
     */
    class progress
    {
    public:
      virtual ~progress();

      /** \brief Set the currently displayed progress.
       *
       *  \param progress      The new progress information to display.
       */
      virtual void set_progress(const aptitude::util::progress_info &progress) = 0;

      /** \brief Mark the currently displayed progress (if any) as done. */
      virtual void done() = 0;
    };
  }
}

#endif // APTITUDE_GENERIC_VIEWS_PROGRESS_H
