// \file mock_util.h            -*-c++-*-
//
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

#ifndef APTITUDE_GENERIC_MOCK_UTIL_H
#define APTITUDE_GENERIC_MOCK_UTIL_H

// System includes:
#include <boost/make_shared.hpp>
#include <boost/shared_ptr.hpp>

#include <gmock/gmock.h>

namespace aptitude
{
  namespace util
  {
    namespace mocks
    {
      /** \brief Convenience mixin that instruments a mock class with
       *  a standard set of static constructors: one for nice mocks,
       *  one for default mocks, and one for strict mocks.
       *
       *  \tparam SubMockType The class to instrument; must be a subclass
       *  of this class.  Must be default-constructable.
       *
       *  If the constructor of MockType is hidden, then
       *  NiceMock<MockType> and StrictMock<MockType> must be friends
       *  of Mock (so that they can subclass it).  See MOCK_FRIENDS()
       *  for a convenient way to declare this.
       */
      template<typename SubMockType>
      class Mock
      {
      public:
        virtual ~Mock()
        {
        }

        typedef SubMockType MockType;
        typedef testing::NiceMock<MockType> NiceMockType;
        typedef testing::StrictMock<MockType> StrictMockType;

        /** \brief Create a default-style mock. */
        static boost::shared_ptr<MockType> create_default()
        {
          return boost::make_shared<MockType>();
        }

        /** \brief Create a nice mock. */
        static boost::shared_ptr<MockType> create_nice()
        {
          return boost::make_shared<NiceMockType>();
        }

        /** \brief Create a strict mock. */
        static boost::shared_ptr<MockType> create_strict()
        {
          return boost::make_shared<StrictMockType>();
        }
      };

      /** \brief Convenience macro to support mocks with hidden
       *  constructors.
       *
       *  This expands to the friend declarations that are required
       *  for Mock to function.  It must be invoked in the class body
       *  of a subclass of Mock that does not declare the name
       *  MockType.
       */
#define MOCK_FRIENDS()                                  \
      friend class testing::NiceMock<MockType>;         \
      friend class testing::StrictMock<MockType>;
    }
  }
}

#endif // APTITUDE_GENERIC_MOCK_UTIL_H
