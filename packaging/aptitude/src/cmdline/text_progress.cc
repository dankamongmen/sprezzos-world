/** \file text_progress.cc */


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
#include "text_progress.h"

#include "cmdline_progress_display.h"

#include <aptitude.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/util/progress_info.h>
#include <generic/views/progress.h>


// System includes:
#include <apt-pkg/error.h>

#include <boost/format.hpp>
#include <boost/make_shared.hpp>

#include <cwidget/generic/util/transcode.h>

#include <iostream>

namespace cw = cwidget;

using aptitude::util::progress_info;
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
      class text_progress : public OpProgress
      {
        // If true, this code will assume it has a TTY as its output
        // and use terminal-based trickery.
        bool use_tty_decorations;

        // The last operation we were displaying; used to output an
        // indicator when the operation finishes.
        std::string last_op;

        shared_ptr<views::progress> display;

      public:
        text_progress(bool _use_tty_decorations,
                      const shared_ptr<views::progress> &_display)
          : use_tty_decorations(_use_tty_decorations),
            display(_display)
        {
        }

        void Done();
        void Update();
      };

      void text_progress::Done()
      {
        // If we displayed a progress indicator, finish it off.
        if(use_tty_decorations)
          {
            if(!last_op.empty())
              {
                display->done();

                if(_error->PendingError() == true)
                  // ForTranslators: the text between [] should be
                  // exactly 4 character cells wide; "ERR" is short
                  // for "ERROR".
                  std::cout << (format(_("[ ERR] %s")) % last_op) << std::endl;
              }
          }
        else if(!last_op.empty())
          {
            std::cout << std::endl;
            last_op.clear();
          }
      }

      void text_progress::Update()
      {
        if(CheckChange(0.7))
          {
            if(!use_tty_decorations)
              {
                if(MajorChange)
                  {
                    if(!last_op.empty())
                      std::cout << std::endl;

                    std::cout << Op << "...";
                    last_op = Op;
                  }
              }
            else
              {
                progress_info info = progress_info::bar(Percent, Op);
                display->set_progress(info);

                last_op = Op;
              }
          }
      }
    }

    shared_ptr<OpProgress>
    make_text_progress(bool require_tty_decorations,
                       const shared_ptr<views::progress> &display)
    {
      bool hide_tty_decorations = false;
      bool hidden = false;

      if(!isatty(1) ||
         aptcfg->FindI("quiet", 0) >= 1 ||
         aptcfg->FindB("quiet::NoUpdate", false) == true)
        hide_tty_decorations = true;

      if(aptcfg->FindI("quiet", 0) >= 2)
        hidden = true;

      if(require_tty_decorations && hide_tty_decorations)
        hidden = true;

      if(hidden)
        return make_shared<OpProgress>();
      else
        return make_shared<text_progress>(!hide_tty_decorations, display);
    }

    shared_ptr<OpProgress>
    make_text_progress(bool require_tty_decorations,
                       const shared_ptr<terminal_locale> &term_locale,
                       const shared_ptr<terminal_metrics> &term_metrics,
                       const shared_ptr<terminal_output> &term_output)
    {
      return make_text_progress(require_tty_decorations,
                                create_progress_display(term_locale, term_metrics, term_output));
    }
  }
}
