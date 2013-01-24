/** \file cmdline_download_progress_display.cc */

// Copyright (C) 2010-2011 Daniel Burrows
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
#include "cmdline_download_progress_display.h"

#include "terminal.h"
#include "transient_message.h"

#include <aptitude.h>

#include <generic/views/download_progress.h>

// System includes:
#include <apt-pkg/strutl.h>

#include <boost/algorithm/string/join.hpp>
#include <boost/format.hpp>
#include <boost/make_shared.hpp>

#include <cwidget/generic/util/transcode.h>

#include <math.h>

using boost::algorithm::join;
using boost::format;
using boost::make_shared;
using boost::shared_ptr;
using boost::wformat;
using cwidget::util::transcode;

namespace aptitude
{
  namespace cmdline
  {
    namespace
    {
      class download_progress : public views::download_progress
      {
        bool display_messages;
        shared_ptr<transient_message> message;
        shared_ptr<download_status_display> status_display;
        shared_ptr<terminal_input> term_input;

        download_progress(bool _display_messages,
                          const shared_ptr<transient_message> &_message,
                          const shared_ptr<download_status_display> &_status_display,
                          const shared_ptr<terminal_input> &_term_input);

        friend shared_ptr<download_progress>
        make_shared<download_progress>(const bool &,
                                       const shared_ptr<transient_message> &,
                                       const shared_ptr<download_status_display> &,
                                       const shared_ptr<terminal_input> &);

      public:
        bool update_progress(const status &current_status);

        void file_started(const std::string &description,
                          const boost::optional<unsigned long> &id,
                          const boost::optional<unsigned long long> &file_size);

        void file_already_downloaded(const std::string &description,
                                     const boost::optional<unsigned long> &id,
                                     const boost::optional<unsigned long long> &file_size);

        void error(bool ignored,
                   const std::string &error,
                   const std::string &description,
                   const boost::optional<unsigned long> &id);

        void file_finished(const std::string &description,
                           const boost::optional<unsigned long> &id);

        void done(unsigned long long fetched_bytes,
                  unsigned long long elapsed_time,
                  unsigned long long latest_download_rate);

        void media_change(const std::string &media,
                          const std::string &drive,
                          const sigc::slot1<void, bool> &k);

        virtual void complete(unsigned long long fetched_bytes,
                              unsigned long long elapsed_time,
                              unsigned long long latest_download_rate);
      };

      download_progress::download_progress(bool _display_messages,
                                           const shared_ptr<transient_message> &_message,
                                           const shared_ptr<download_status_display> &_status_display,
                                           const shared_ptr<terminal_input> &_term_input)
        : display_messages(_display_messages),
          message(_message),
          status_display(_status_display),
          term_input(_term_input)
      {
      }


      bool download_progress::update_progress(const status &current_status)
      {
        status_display->display_status(current_status);

        return true;
      }

      void download_progress::file_started(const std::string &description,
                                           const boost::optional<unsigned long> &id,
                                           const boost::optional<unsigned long long> &file_size)
      {
        if(display_messages)
          {
            std::vector<std::string> entries;

            entries.push_back(_("Get:"));

            if(id)
              entries.push_back( (format("%d") % *id).str() );

            if(!description.empty())
              entries.push_back(description);

            if(file_size)
              entries.push_back( (format("[%sB]") % SizeToStr(*file_size)).str() );

            message->display_and_advance(transcode(join(entries, " ")));
          }
      }

      void download_progress::file_already_downloaded(const std::string &description,
                                                      const boost::optional<unsigned long> &id,
                                                      const boost::optional<unsigned long long> &file_size)
      {
        if(display_messages)
          {
            std::vector<std::string> entries;

            entries.push_back(_("Hit"));

            if(id)
              entries.push_back( (format("%d") % *id).str() );

            if(!description.empty())
              entries.push_back(description);

            if(file_size)
              entries.push_back( (format("[%sB]") % SizeToStr(*file_size)).str() );

            message->display_and_advance(transcode(join(entries, " ")));
          }
      }

      void download_progress::error(bool ignored,
                                    const std::string &error,
                                    const std::string &description,
                                    const boost::optional<unsigned long> &id)
      {
        if(display_messages)
          {
            std::vector<std::string> entries;

            if(ignored)
              // ForTranslators: this stands for "ignored" and should be
              // the same width as the translation of "Err".
              entries.push_back(_("Ign"));
            else
              // ForTranslators: this stands for "error" and should be the
              // same width as the translation of "Ign".
              entries.push_back(_("Err"));

            if(!description.empty())
              entries.push_back(description);

            message->display_and_advance(transcode(join(entries, " ")));

            if(!ignored && !error.empty())
              message->display_and_advance(transcode("  " + error));
          }
      }

      void download_progress::file_finished(const std::string &description,
                                            const boost::optional<unsigned long> &id)
      {
      }

      void download_progress::done(unsigned long long fetched_bytes,
                                   unsigned long long elapsed_time,
                                   unsigned long long latest_download_rate)
      {
        if(display_messages)
          {
            if(fetched_bytes != 0)
              {
                std::string text =
                  (format(_("Fetched %sB in %s (%sB/s)"))
                   % SizeToStr(fetched_bytes)
                   % TimeToStr(elapsed_time)
                   % SizeToStr(latest_download_rate)).str();

                message->display_and_advance(transcode(text));
              }
          }
      }

      void download_progress::media_change(const std::string &media,
                                           const std::string &drive,
                                           const sigc::slot1<void, bool> &k)
      {
        // Clear any existing text to ensure that the prompt starts at
        // the beginning of the line.
        message->set_text(L"");

        std::string prompt =
          (format(_("Media change: Please insert the disc labeled '%s' into "
                    "the drive '%s' and press [Enter]."))
           % media % drive).str();
        // Note that the value the user enters is discarded.
        try
          {
            term_input->prompt_for_input(transcode(prompt));
          }
        catch(StdinEOFException)
          {
            k(false);
            return;
          }
        // Now say it's OK to continue.
        k(true);
      }

      void download_progress::complete(unsigned long long fetched_bytes,
                                       unsigned long long elapsed_time,
                                       unsigned long long latest_download_rate)
      {
      }

      class dummy_status_display : public download_status_display
      {
        dummy_status_display();

        friend shared_ptr<dummy_status_display>
        make_shared<dummy_status_display>();

      public:
        void display_status(const download_progress::status &status);
      };

      dummy_status_display::dummy_status_display()
      {
      }

      void dummy_status_display::display_status(const download_progress::status &)
      {
      }

      class download_status_display_impl : public download_status_display
      {
        shared_ptr<transient_message> message;
        shared_ptr<terminal_locale> term_locale;
        shared_ptr<terminal_metrics> term_metrics;

        download_status_display_impl(const shared_ptr<transient_message> &_message,
                                     const shared_ptr<terminal_locale> &_term_locale,
                                     const shared_ptr<terminal_metrics> &_term_metrics);

        friend shared_ptr<download_status_display_impl>
        make_shared<download_status_display_impl>(const shared_ptr<transient_message> &,
                                                  const shared_ptr<terminal_locale> &,
                                                  const shared_ptr<terminal_metrics> &);

      public:
        void display_status(const download_progress::status &status);
      };

      download_status_display_impl::download_status_display_impl(const shared_ptr<transient_message> &_message,
                                                                 const shared_ptr<terminal_locale> &_term_locale,
                                                                 const shared_ptr<terminal_metrics> &_term_metrics)
        : message(_message),
          term_locale(_term_locale),
          term_metrics(_term_metrics)
      {
      }

      // \todo This should be generic code:
      int as_percent(double fraction)
      {
        const int result = static_cast<int>(round(fraction * 100));
        if(result < 0)
          return 0;
        else if(result > 100)
          return 100;
        else
          return result;
      }

      /** \brief Visitor for worker status objects that appends their
       *  rendering to a string.
       *
       *  Not responsible for bracketing the rendering in [].
       */
      class append_worker_status : public boost::static_visitor<>
      {
        std::wstring &output;
        typedef views::download_progress::file_progress file_progress;

      public:
        append_worker_status(std::wstring &_output)
          : output(_output)
        {
        }

        void operator()(const std::string &progress) const
        {
          output += transcode(progress);
        }

        void operator()(const file_progress &progress) const
        {
          const unsigned long long current_size = progress.get_current_size();
          const unsigned long long total_size = progress.get_total_size();

          const bool complete = progress.get_complete();
          const std::string &description = progress.get_description();
          const boost::optional<unsigned long> &id = progress.get_id();
          const std::string &mode = progress.get_mode();

          std::vector<std::wstring> components;

          if(id)
            components.push_back((wformat(L"%lu") % *id).str());

          if(!description.empty())
            components.push_back(transcode(description));

          if(!mode.empty())
            components.push_back(transcode(mode));

          if(total_size != 0 && !complete)
            components.push_back((wformat(L"%sB/%sB %lu%%")
                                  % transcode(SizeToStr(current_size))
                                  % transcode(SizeToStr(total_size))
                                  % as_percent( ((double) current_size) / total_size)).str());
          else if(current_size != 0)
            // The old download indicator displayed a size of 0 if
            // current_size was 0.  I figure if we have no total size
            // and we haven't downloaded anything, there's no poing.
            components.push_back((wformat(L"%sB")
                                  % transcode(SizeToStr(current_size))).str());

          output += join(components, L" ");
        }
      };

      /** \brief Find the index in the given string of a display
       *  column.
       *
       *  \param s      The string to process.
       *
       *  \param target_column The column to locate, where the first
       *                       character of s starts at column 0.
       *
       *  \param term_locale Locale information that should be used when
       *                     determining the column.
       *
       *  \param result_index Set to the index of the first character
       *                      which is partly or wholly at or beyond
       *                      the given display column.  If target_column
       *                      is negative, this will always be 0.
       *
       *  \param result_column Set to the first column in which the
       *                       character indicated by result_index
       *                       appears.
       *
       *  \todo Could move to a common module and get a unit test.
       */
      void find_column_index(const std::wstring &s,
                             int target_column,
                             const shared_ptr<terminal_locale> &term_locale,
                             int &result_index,
                             int &result_column)
      {
        result_index = 0;
        result_column = 0;

        std::wstring::const_iterator it = s.begin();

        while(it != s.end() && result_column < target_column)
          {
            const int curr_width = term_locale->wcwidth(*it);

            // Don't include this character if it starts before the
            // target column and extends past it.  Be careful not to
            // return a character that ends just before the target
            // column (>= vs >).
            if(result_column + curr_width > target_column)
              break;

            ++it;
            ++result_index;
            result_column += curr_width;
          }
      }

      void download_status_display_impl::display_status(const download_progress::status &status)
      {
        typedef views::download_progress::status::worker_status worker_status;
        const unsigned long long download_rate = status.get_download_rate();
        const std::vector<worker_status> &active_downloads =
          status.get_active_downloads();
        const double fraction_complete = status.get_fraction_complete();
        const unsigned long long time_remaining = status.get_time_remaining();

        const int percent = as_percent(fraction_complete);

        std::wstring message_text;

        message_text += (wformat(L"%d%% ") % percent).str();

        if(active_downloads.empty())
          message_text += transcode(_("[Working]"));
        else
          {
            for(std::vector<worker_status>::const_iterator it =
                  active_downloads.begin(); it != active_downloads.end(); ++it)
              {
                if(it != active_downloads.begin())
                  message_text.push_back(L' ');

                message_text.push_back(L'[');
                boost::apply_visitor(append_worker_status(message_text), *it);
                message_text.push_back(L']');
              }
          }

        if(download_rate > 0 || time_remaining > 0)
          {
            std::wstring progress_str;

            if(download_rate > 0)
              progress_str = (wformat(L" %sB/s %s")
                              % transcode(SizeToStr(download_rate))
                              % transcode(TimeToStr(time_remaining))).str();
            else
              progress_str = (wformat(L" %s")
                              % transcode(TimeToStr(time_remaining))).str();

            int progress_str_width = 0;
            for(std::wstring::const_iterator it = progress_str.begin();
                it != progress_str.end(); ++it)
              progress_str_width += term_locale->wcwidth(*it);

            int message_text_width = 0;
            for(std::wstring::const_iterator it = message_text.begin();
                it != message_text.end(); ++it)
              message_text_width += term_locale->wcwidth(*it);

            const int screen_width = term_metrics->get_screen_width();

            // Format the progress string so that it overlaps the
            // previous message text and is right-justified.

            int progress_str_start_column = screen_width - progress_str_width;

            // First, pad the message text with spaces so that there's
            // a place to insert the progress string.
            while(message_text_width < screen_width - progress_str_start_column)
              {
                message_text.push_back(L' ');
                message_text_width += term_locale->wcwidth(L' ');
              }

            // Now, find the location in the message text where the
            // progress string will begin and drop the rest of the
            // string.
            int first_overwritten_character_index;
            int first_overwritten_character_column;

            find_column_index(message_text,
                              progress_str_start_column,
                              term_locale,
                              first_overwritten_character_index,
                              first_overwritten_character_column);

            // Drop the part of the string that's overwritten;
            // everything from first_overwritten_character_index
            // onwards.
            message_text.erase(first_overwritten_character_index);

            // We might have split a wide character; replace its first
            // portion with spaces.
            while(first_overwritten_character_column < progress_str_start_column)
              {
                message_text.push_back(L' ');
                first_overwritten_character_column += term_locale->wcwidth(L' ');
              }

            // OK, now message_text is exactly long enough to
            // concatenate it with the progress string.  The progress
            // string might running off the right side of the terminal
            // if it replaced the whole message text and was still too
            // long; I leave it to the transient message to deal with
            // that case.
            message_text += progress_str;
          }

        message->set_text(message_text);
      }
    }

    download_status_display::~download_status_display()
    {
    }

    shared_ptr<views::download_progress>
    create_download_progress_display(const boost::shared_ptr<transient_message> &message,
                                     const boost::shared_ptr<download_status_display> &status_display,
                                     const boost::shared_ptr<terminal_input> &term_input,
                                     bool display_messages)
    {
      return make_shared<download_progress>(display_messages,
                                            message,
                                            status_display,
                                            term_input);
    }

    shared_ptr<download_status_display>
    create_cmdline_download_status_display(const shared_ptr<transient_message> &message,
                                           const shared_ptr<terminal_locale> &term_locale,
                                           const shared_ptr<terminal_metrics> &term_metrics,
                                           bool hide_status)
    {
      if(hide_status)
        return make_shared<dummy_status_display>();
      else
        return make_shared<download_status_display_impl>(message,
                                                         term_locale,
                                                         term_metrics);
    }
  }
}
