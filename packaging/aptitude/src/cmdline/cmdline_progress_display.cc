/** \file cmdline_progress_display.cc */

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
#include "cmdline_progress_display.h"

#include "transient_message.h"

#include <aptitude.h>
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/util/progress_info.h>
#include <generic/views/progress.h>

// System includes:
#include <boost/format.hpp>
#include <boost/make_shared.hpp>

#include <cwidget/generic/util/transcode.h>

#include <sys/time.h>

using aptitude::util::progress_info;
using aptitude::util::progress_type_bar;
using aptitude::util::progress_type_none;
using aptitude::util::progress_type_pulse;
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
      // \todo This should be configurable.
      const int progress_update_interval = 0.25;

      class progress_display_impl : public views::progress
      {
        // Set to "true" when done() is called, and to "false" when
        // any other method is called.  Used to suppress calls to
        // done() when a "Done" message is already being displayed.
        bool is_done;

        // The last progress that was displayed; we always update the
        // display if the mode or the message changed.
        progress_info last_progress;

        shared_ptr<transient_message> message;

        bool old_style_percentage;
        bool retain_completed;

        // Using the current time and the progress information to
        // display, determine if an update is required.
        bool should_update(const progress_info &progress) const;

      public:
        progress_display_impl(const shared_ptr<transient_message> &_message,
                              bool _old_style_percentage,
                              bool _retain_completed);

        void set_progress(const progress_info &progress);
        void done();
      };

      progress_display_impl::progress_display_impl(const shared_ptr<transient_message> &_message,
                                                   bool _old_style_percentage,
                                                   bool _retain_completed)
        : is_done(false),
          last_progress(progress_info::none()),
          message(_message),
          old_style_percentage(_old_style_percentage),
          retain_completed(_retain_completed)
      {
      }

      bool progress_display_impl::should_update(const progress_info &progress) const
      {
        if(progress.get_type() != last_progress.get_type())
          return true;

        if(progress.get_type() == progress_type_bar &&
           progress.get_progress_percent_int() != last_progress.get_progress_percent_int())
          return true;

        if(progress.get_progress_status() != last_progress.get_progress_status())
          return true;

        return false;
      }

      void progress_display_impl::set_progress(const progress_info &progress)
      {
        if(should_update(progress))
          {
            switch(progress.get_type())
              {
              case progress_type_none:
                message->set_text(L"");
                break;

              case progress_type_pulse:
                if(old_style_percentage)
                  message->set_text( (wformat(L"%s...")
                                      % transcode(progress.get_progress_status())).str() );
                else
                  message->set_text( (wformat(L"[----] %s")
                                      % transcode(progress.get_progress_status())).str() );
                break;

              case progress_type_bar:
                if(old_style_percentage)
                  message->set_text( (wformat(L"%s... %d%%")
                                      % transcode(progress.get_progress_status())
                                      % progress.get_progress_percent_int()).str() );
                else
                  message->set_text( (wformat(L"[%3d%%] %s")
                                      % progress.get_progress_percent_int()
                                      % transcode(progress.get_progress_status())).str() );
                break;

              default:
                message->set_text(L"INTERNAL ERROR");
                break;
              }

            is_done = false;
            last_progress = progress;
          }
      }

      void progress_display_impl::done()
      {
        if(last_progress.get_type() != progress_type_none &&
           !is_done)
          {
            if(retain_completed)
              {
                const std::wstring msg =
                  transcode(last_progress.get_progress_status());
                if(old_style_percentage)
                  message->display_and_advance( (wformat(L"%s... %s")
                                                 % msg
                                                 % transcode(_("Done"))).str() );
                else
                  message->display_and_advance( (wformat(L"[%4s] %s")
                  // ForTranslators: the string replacing "DONE"
                  // be truncated or padded to 4 characters.
                                                 % transcode(_("DONE"))
                                                 % msg).str() );
              }
            else
              message->set_text(L"");

            last_progress = progress_info::none();
            is_done = true;
          }
      }
    }

    shared_ptr<views::progress>
    create_progress_display(const shared_ptr<transient_message> &message,
                            bool old_style_percentage,
                            bool retain_completed)
    {
      return make_shared<progress_display_impl>(message,
                                                old_style_percentage,
                                                retain_completed);
    }

    shared_ptr<views::progress>
    create_progress_display(const shared_ptr<terminal_locale> &term_locale,
                            const shared_ptr<terminal_metrics> &term_metrics,
                            const shared_ptr<terminal_output> &term_output)
    {
      const shared_ptr<transient_message> message =
        create_transient_message(term_locale, term_metrics, term_output);

      const bool old_style_percentage =
        aptcfg->FindB(PACKAGE "::CmdLine::Progress::Percent-On-Right", false);

      const bool retain_completed =
        aptcfg->FindB(PACKAGE "::CmdLine::Progress::Retain-Completed", false);

      return create_progress_display(message,
                                     old_style_percentage,
                                     retain_completed);
    }
  }
}
