/** \file download_status_display.h */ // -*-c++-*-

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

#ifndef APTITUDE_CMDLINE_MOCKS_DOWNLOAD_STATUS_DISPLAY_H
#define APTITUDE_CMDLINE_MOCKS_DOWNLOAD_STATUS_DISPLAY_H

// Local includes:
#include <cmdline/cmdline_download_progress_display.h>

#include <generic/util/mocks/mock_util.h>

namespace aptitude
{
  namespace cmdline
  {
    namespace mocks
    {
      class download_status_display : public cmdline::download_status_display,
                                      public util::mocks::Mock<download_status_display>
      {
        download_status_display();
        friend boost::shared_ptr<download_status_display>
        boost::make_shared<download_status_display>();

        MOCK_FRIENDS();

      public:
        MOCK_METHOD1(display_status, void(const views::download_progress::status &));
      };
    }
  }
}

#endif // APTITUDE_CMDLINE_MOCKS_DOWNLOAD_STATUS_DISPLAY_H
