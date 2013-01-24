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

#include <aptitude.h>

// System includes:

#include <boost/make_shared.hpp>
#include <boost/shared_ptr.hpp>

#include <cwidget/generic/util/transcode.h>

#include <iostream>

#include <sys/ioctl.h>

using boost::make_shared;
using boost::shared_ptr;
using cwidget::util::transcode;

std::string StdinEOFException::errmsg() const
{
  return _("Unexpected end-of-file on standard input");
}

namespace aptitude
{
  namespace cmdline
  {
    namespace
    {
      // \todo Consider using a signal handler to catch window resizes
      // -- watch out for what happens if the curses UI starts up,
      // we'll have to tear down the handler, and be sure to handle
      // memory barriers.
      class terminal_impl : public terminal_io
      {
      public:
        bool output_is_a_terminal();
        void write_text(const std::wstring &msg);
        void move_to_beginning_of_line();
        void flush();
        std::wstring prompt_for_input(const std::wstring &msg);
        unsigned int get_screen_width();
        int wcwidth(wchar_t ch);
      };

      bool terminal_impl::output_is_a_terminal()
      {
        return isatty(1);
      }

      void terminal_impl::write_text(const std::wstring &msg)
      {
        std::cout << transcode(msg);
      }

      void terminal_impl::move_to_beginning_of_line()
      {
        std::cout << '\r';
      }

      void terminal_impl::flush()
      {
        std::cout << std::flush;
      }

      std::wstring terminal_impl::prompt_for_input(const std::wstring &msg)
      {
        std::cout << transcode(msg) << std::flush;

        std::string rval;
        char buf[1024];
        std::cin.getline(buf, 1023);
        rval += buf;

        while(!std::cin && !std::cin.eof())
          {
            std::cin.getline(buf, 1023);
            rval += buf;
          }

        if(!std::cin)
          throw StdinEOFException();

        return transcode(rval);
      }

      unsigned int terminal_impl::get_screen_width()
      {
        // Ripped from apt-get, which ripped it from GNU ls
        winsize ws;

        if (ioctl(1, TIOCGWINSZ, &ws) != -1 && ws.ws_col >= 5)
          return ws.ws_col;
        else
          // \todo Should we distinguish between "can't read a
          // terminal size" and "read a tiny terminal size"?
          return 80;
      }

      int terminal_impl::wcwidth(wchar_t ch)
      {
        return ::wcwidth(ch);
      }
    }

    terminal_input::~terminal_input()
    {
    }

    terminal_locale::~terminal_locale()
    {
    }

    terminal_metrics::~terminal_metrics()
    {
    }

    terminal_output::~terminal_output()
    {
    }

    shared_ptr<terminal_io> create_terminal()
    {
      return make_shared<terminal_impl>();
    }
  }
}
