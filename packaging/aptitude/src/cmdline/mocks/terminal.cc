/** \file terminal.cc */

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


// Local includes:
#include "terminal.h"

// System includes:
#include <boost/make_shared.hpp>

using boost::make_shared;
using boost::shared_ptr;
using testing::AnyNumber;
using testing::Invoke;
using testing::NiceMock;
using testing::Return;
using testing::StrictMock;
using testing::_;

namespace aptitude
{
  namespace cmdline
  {
    namespace mocks
    {
      terminal_input::terminal_input()
      {
        // Make sure the program doesn't abort if there's an
        // unexpected invocation of prompt_for_input().
        ON_CALL(*this, prompt_for_input(_))
          .WillByDefault(Return(std::wstring()));
      }

      terminal_locale::terminal_locale()
      {
        EXPECT_CALL(*this, wcwidth(_))
          .WillRepeatedly(Return(1));
      }

      terminal_metrics::terminal_metrics()
      {
      }

      terminal_output::terminal_output()
      {
      }

      class combining_terminal_output::impl : public combining_terminal_output
      {
        friend shared_ptr<impl> make_shared<impl>();
        impl();

        std::wstring pending_writes;

        void do_write_text(const std::wstring &s);

        friend class testing::NiceMock<impl>;
        friend class testing::StrictMock<impl>;

      public:
        void write_text(const std::wstring &s);
        void move_to_beginning_of_line();
        void flush();

        static shared_ptr<impl> create_default();
        static shared_ptr<impl> create_nice();
        static shared_ptr<impl> create_strict();
      };

      combining_terminal_output::impl::impl()
      {
      }

      void combining_terminal_output::impl::do_write_text(const std::wstring &s)
      {
        std::wstring::size_type start = 0;
        for(std::wstring::size_type nl = s.find('\n', start);
            nl != s.npos; nl = s.find('\n', start))
          {
            pending_writes.append(s, start, (nl - start) + 1);
            start = nl + 1;

            output(pending_writes);
            pending_writes.clear();
          }

        pending_writes.append(s, start, s.npos);
      }

      void combining_terminal_output::impl::write_text(const std::wstring &s)
      {
        do_write_text(s);
      }

      void combining_terminal_output::impl::move_to_beginning_of_line()
      {
        do_write_text(L"\r");
      }

      void combining_terminal_output::impl::flush()
      {
        if(!pending_writes.empty())
          {
            output(pending_writes);
            pending_writes.clear();
          }
      }

      terminal_with_combined_output::terminal_with_combined_output()
      {
      }

      terminal_with_combined_output::~terminal_with_combined_output()
      {
      }

      combining_terminal_output::combining_terminal_output()
      {
      }

      shared_ptr<combining_terminal_output::impl> combining_terminal_output::impl::create_default()
      {
        return make_shared<combining_terminal_output::impl>();
      }

      shared_ptr<combining_terminal_output::impl> combining_terminal_output::impl::create_nice()
      {
        return make_shared<NiceMock<combining_terminal_output::impl> >();
      }

      shared_ptr<combining_terminal_output::impl> combining_terminal_output::impl::create_strict()
      {
        return make_shared<StrictMock<combining_terminal_output::impl> >();
      }

      shared_ptr<combining_terminal_output> combining_terminal_output::create_default()
      {
        return impl::create_default();
      }

      shared_ptr<combining_terminal_output> combining_terminal_output::create_nice()
      {
        return impl::create_nice();
      }

      shared_ptr<combining_terminal_output> combining_terminal_output::create_strict()
      {
        return impl::create_strict();
      }
    }
  }
}
