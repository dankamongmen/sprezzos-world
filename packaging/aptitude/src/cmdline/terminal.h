/** \brief terminal.h */    // -*-c++-*-


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

#ifndef APTITUDE_CMDLINE_SCREEN_WIDTH_H
#define APTITUDE_CMDLINE_SCREEN_WIDTH_H

#include <boost/shared_ptr.hpp>
#include <cwidget/generic/util/exception.h>

#include <iosfwd>

/** Thrown when we get EOF on stdin.  Should never be thrown
 *  to the cwidget::toplevel.
 *
 *  \todo It would be nice if this was a std::exception; watch out for
 *  places that just catch cw::util::Exception, though.
 */
class StdinEOFException : public cwidget::util::Exception
{
public:
  std::string errmsg() const;
};

namespace aptitude
{
  namespace cmdline
  {
    /** \brief Abstraction of the I/O device used for the command-line
     *  code.
     *
     *  Virtual interfaces are used so that we can dummy them out for
     *  testing (see mocks/terminal.h).  Using several virtual
     *  interfaces allows code to specify more precisely which
     *  functionality is required, so we don't have to
     */
    // @{

    /** \brief Interface representing the ability to write to the
     *  terminal.
     */
    class terminal_output
    {
    public:
      virtual ~terminal_output();

      /** \brief Check whether the output stream seems to be connected
       *  to a terminal.
       *
       *  Normally maps to isatty(1).
       */
      virtual bool output_is_a_terminal() = 0;

      /** \brief Write some text to the terminal. */
      virtual void write_text(const std::wstring &msg) = 0;

      /** \brief Return the cursor to the beginning of the current
       *  line.
       *
       *  Like writing '\r'.  Might not have an effect unless you call
       *  flush_output().
       */
      virtual void move_to_beginning_of_line() = 0;

      /** \brief Flush the output device.
       *
       *  Writing '\n' is assumed to implicitly flush it (as on a
       *  standard terminal), but this is necessary if you want to
       *  ensure that something like a prompt or a progress bar is
       *  printed.
       */
      virtual void flush() = 0;
    };

    /** \brief Interface representing the ability to read from the
     *  terminal.
     */
    class terminal_input
    {
    public:
      ~terminal_input();

      /** \brief Prompt for a line of input from the terminal device.
       *
       *  \param msg A message to display as the prompt string.  The
       *             cursor will be positioned immediately to the
       *             right of this message.
       *
       *  \return a single line of user input.
       *
       *  \throw StdinEOFException if EOF is encountered before the
       *  end of the current line.
       *
       *  \note The StdinEOFException is normally just passed along
       *  until it reaches main(), which prints an appropriate message
       *  and aborts the program.
       */
      virtual std::wstring prompt_for_input(const std::wstring &msg) = 0;
    };

    /** \brief Interface representing the ability to read the
     *  characteristics of the terminal.
     */
    class terminal_metrics
    {
    public:
      ~terminal_metrics();

      /** \brief Retrieve the current screen width.
       *
       *  This might make system calls and return a different result
       *  each time, so don't call it in a tight loop -- cache the
       *  value up-front.
       *
       *  A default value is returned if the output doesn't appear to
       *  be a tty.
       */
      virtual unsigned int get_screen_width() = 0;
    };

    /** \brief Locale functionality related to the terminal (thus,
     *  primarily LC_CTYPE).
     *
     *  This is wrapped up so that the unit tests can verify the
     *  code's output behavior without depending on the value of the
     *  system locale or the set of locale definitions that are
     *  installed on the system.
     */
    class terminal_locale
    {
    public:
      virtual ~terminal_locale();

      /** \brief Return the number of terminal columns occupied by a
       *  wide character.
       */
      virtual int wcwidth(wchar_t ch) = 0;
    };
    // @}

    /** \brief Master interface representing all the terminal
     *  capabilities at once.
     *
     *  This should normally not be passed as a parameter to
     *  functions; it exists so that create_terminal() has a return
     *  type.
     */
    class terminal_io : public terminal_input,
                        public terminal_locale,
                        public terminal_metrics,
                        public terminal_output
    {
    };

    /** \brief Create a terminal object attached to the standard I/O
     *  streams and using the system locale definitions.
     */
    boost::shared_ptr<terminal_io> create_terminal();
  }
}

#endif // APTITUDE_CMDLINE_SCREEN_WIDTH_H
