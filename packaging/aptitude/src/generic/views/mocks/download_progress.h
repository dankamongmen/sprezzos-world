/** \file download_progress.h */    // -*-c++-*-

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

#ifndef APTITUDE_GENERIC_VIEWS_MOCKS_DOWNLOAD_PROGRESS_H
#define APTITUDE_GENERIC_VIEWS_MOCKS_DOWNLOAD_PROGRESS_H

// Local includes:
#include <generic/util/mocks/mock_util.h>
#include <generic/views/download_progress.h>

// System includes:
#include <boost/make_shared.hpp>

#include <gmock/gmock.h>

namespace aptitude
{
  namespace views
  {
    namespace mocks
    {
      class download_progress : public aptitude::views::download_progress,
                                public aptitude::util::mocks::Mock<download_progress>
      {
        download_progress();
        friend boost::shared_ptr<download_progress> boost::make_shared<download_progress>();
        MOCK_FRIENDS();

      public:
        MOCK_METHOD1(update_progress, bool(const status &));
        MOCK_METHOD3(file_started, void(const std::string &,
                                        const boost::optional<unsigned long> &,
                                        const boost::optional<unsigned long long> &));
        MOCK_METHOD4(error, void(bool,
                                 const std::string &,
                                 const std::string &,
                                 const boost::optional<unsigned long> &));
        MOCK_METHOD2(file_finished, void(const std::string &,
                                         const boost::optional<unsigned long> &));
        MOCK_METHOD3(done, void(unsigned long long, unsigned long long, unsigned long long));
        MOCK_METHOD3(media_change, void(const std::string &,
                                        const std::string &,
                                        const sigc::slot1<void, bool> &));
      };
    }
  }
}

#endif // APTITUDE_GENERIC_VIEWS_MOCKS_DOWNLOAD_PROGRESS_H
