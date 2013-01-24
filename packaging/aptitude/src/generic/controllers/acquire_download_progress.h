/** \file acquire_download_progress.h */      // -*-c++-*-

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

#ifndef APTITUDE_CONTROLLERS_ACQUIRE_DOWNLOAD_PROGRESS_H
#define APTITUDE_CONTROLLERS_ACQUIRE_DOWNLOAD_PROGRESS_H

// System includes:
#include <apt-pkg/acquire.h>

#include <boost/shared_ptr.hpp>

class download_signal_log;

namespace aptitude
{
  namespace views
  {
    class download_progress;
  }

  namespace controllers
  {
    /** \brief Mediates between the Acquire system and a
     *  download_progress view.
     *
     *  More specifically, this receives the signals emitted by a
     *  download_signal_log and translates them into calls on the
     *  download_progress view.
     *
     *  An instance of this class exposes no behavior and is
     *  interesting only in that it will sever the connection between
     *  the download process and the view when it is destroyed.
     */
    class acquire_download_progress
    {
      class impl;
      acquire_download_progress();

      friend class impl;
      friend boost::shared_ptr<acquire_download_progress>
      create_acquire_download_progress(download_signal_log *,
                                       const boost::shared_ptr<views::download_progress> &);

    public:
      virtual ~acquire_download_progress() = 0;
    };

    /** \brief Create a new acquire_download_progress.
     *
     *  \param log       The download signal object whose events
     *                   will be received by the new controller.
     *  \param view      The view managed by the new controller.
     *
     *  The download progress instance holds a strong reference to the
     *  view, but no reference at all to the log; the log and the
     *  controller can be destroyed in any order, so long as they are
     *  not destroyed simultaneously in separate threads.
     */
    boost::shared_ptr<acquire_download_progress>
    create_acquire_download_progress(download_signal_log *log,
                                     const boost::shared_ptr<views::download_progress> &view);
  }
}

#endif // APTITUDE_CONTROLLERS_ACQUIRE_DOWNLOAD_PROGRESS_H

