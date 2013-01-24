/** \file transient_message.h */   // -*-c++-*-


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

#ifndef APTITUDE_CMDLINE_MOCKS_TRANSIENT_MESSAGE_H
#define APTITUDE_CMDLINE_MOCKS_TRANSIENT_MESSAGE_H

// Local includes:
#include <cmdline/transient_message.h>

#include <generic/util/mocks/mock_util.h>

// System includes:
#include <gmock/gmock.h>

namespace aptitude
{
  namespace cmdline
  {
    namespace mocks
    {
      class transient_message : public cmdline::transient_message,
                                public aptitude::util::mocks::Mock<transient_message>
      {
        friend boost::shared_ptr<transient_message>
        boost::make_shared<transient_message>();

        MOCK_FRIENDS();

      public:
        MOCK_METHOD1(set_text, void(const std::wstring &));
        MOCK_METHOD1(display_and_advance, void(const std::wstring &));
      };
    }
  }
}

#endif // APTITUDE_CMDLINE_MOCKS_TRANSIENT_MESSAGE_H
