// parse_dpkg_status.h                 -*-c++-*-
//
//   Copyright (C) 2008 Daniel Burrows
//
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.

#ifndef PARSE_DPKG_STATUS_H
#define PARSE_DPKG_STATUS_H

#include <deque>
#include <string>

#include <ostream>

#include <cwidget/generic/util/exception.h>

namespace aptitude
{
  namespace apt
  {
    class DpkgStatusException : public cwidget::util::Exception
    {
      std::string msg;

    public:
      DpkgStatusException(const std::string &_msg)
	: msg(_msg)
      {
      }

      std::string errmsg() const { return msg; }
    };

    /** \brief Represents a status message from dpkg. */
    class dpkg_status_message
    {
    public:
      /** \brief The type of this message. */
      enum type
	{
	  /** \brief Something went wrong, tell the user.
	   *
	   *  The string data is the conffile.
	   */
	  error,
	  /** \brief A conffile needs to be updated; ask the
	   *  user what to do.
	   */
	  conffile,
	  /** \brief A status message from dpkg; update the
	   *  progress bar.
	   */
	  status
	};

    private:
      type tp;
      // Only meaningful for conffile messages.
      std::string existing_filename;
      std::string new_filename;
      double percent;
      std::string package;
      std::string text;

      dpkg_status_message(type _tp,
			  const std::string &_existing_filename,
			  const std::string &_new_filename,
			  double _percent,
			  const std::string &_package,
			  const std::string &_text)
	: tp(_tp),
	  existing_filename(_existing_filename),
	  new_filename(_new_filename),
	  percent(_percent),
	  text(_text)
      {
      }

    public:
      static dpkg_status_message make_error(double percent,
					    const std::string &package,
					    const std::string &text)
      {
	return dpkg_status_message(error, std::string(), std::string(), percent, package, text);
      }

      static dpkg_status_message make_conffile(const std::string &existing_filename,
					       const std::string &new_filename,
					       double percent,
					       const std::string &text)
      {
	return dpkg_status_message(conffile, existing_filename, new_filename, percent, std::string(), text);
      }

      static dpkg_status_message make_status(double percent,
					     const std::string &package,
					     const std::string &text)
      {
	return dpkg_status_message(status, std::string(), std::string(), percent, package, text);
      }

      bool operator==(const dpkg_status_message &other) const
      {
	if(tp != other.tp)
	  return false;

	switch(tp)
	  {
	  case error:
	    return
	      percent == other.percent &&
	      package == other.package &&
	      text == other.text;

	  case conffile:
	    return
	      existing_filename == other.existing_filename &&
	      new_filename == other.new_filename &&
	      percent == other.percent &&
	      text == other.text;

	  case status:
	    return
	      percent == other.percent &&
	      package == other.package &&
	      text == other.text;
	  }

	return false;
      }

      /** \brief Parse a status message from the given character buffer.
       *
       *  \param buf   The buffer containing the dpkg status message.
       *  \param len   The length of the status message.
       */
      static dpkg_status_message parse(const char *buf, size_t len);

      /** \brief Get the type of message that this is. */
      type get_type() const { return tp; }

      /** \brief Get the name of the package associated with this message.
       *
       *  Not meaningful for conffile messages.
       */
      const std::string &get_package() const { return package; }

      /** \brief Get the progress percentage associated with this message. */
      double get_percent() const { return percent; }

      /** \brief Get the message text associated with this message.
       *
       *  For errors, this is the error message; for conffile
       *  messages, this is the name of the file as reported in the
       *  dpkg message; for other status updates, this is a
       *  description of the current status.
       */
      const std::string &get_text() const { return text; }

      /** \brief For conffile messages, get the name of the currently
       *  installed version of the conffile.
       */
      const std::string &get_existing_filename() const { return existing_filename; }

      /** \brief For conffile messages, get the name of the new version
       *  of the conffile (the one that was unpacked from the .deb).
       */
      const std::string &get_new_filename() const { return new_filename; }
    };

    // Dump a status message for debugging purposes.
    std::ostream &operator<<(std::ostream &out, const dpkg_status_message &mgs);

    /** \brief A parser for the dpkg status pipe. */
    class dpkg_status_parser
    {
      /** \brief The parser's hidden state -- stores all the text
       *  since the last newline.
       */
      std::string msg;

      std::deque<dpkg_status_message> pending_messages;

    public:
      dpkg_status_parser();

      /** \return \b true if at least one message has been parsed
       *  from the status pipe that has not been retrieved using
       *  consume_message().
       */
      bool has_pending_message() const
      {
	return !pending_messages.empty();
      }

      /** \brief Remove the next message from the queue and return it.
       *
       *  The queue must be non-empty (invoke has_pending_message first).
       */
      dpkg_status_message pop_message()
      {
	dpkg_status_message rval = pending_messages.front();
	pending_messages.pop_front();

	return rval;
      }

      /** \brief Feed the given character buffer into the parser.
       *
       *  \param buf   The character buffer containing data from the
       *               dpkg status pipe.
       *  \param len   The length of the data.
       */
      void process_input(const char *buf, size_t len);

      /** \brief Feed the given string into the parser.
       *
       *  \param s   The data from dpkg's status pipe.
       */
      void process_input(const std::string &s)
      {
	process_input(s.c_str(), s.size());
      }

      /** \brief Feed a single character into the parser. */
      void process_input(char c)
      {
	process_input(&c, 1);
      }
    };
  }
}

#endif // PARSE_DPKG_STATUS_H
