/** \file cmdline_search_progress.cc */


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


#include "cmdline_search_progress.h"

#include "cmdline_progress_display.h"

#include <generic/util/progress_info.h>
#include <generic/util/throttle.h>
#include <generic/views/progress.h>


#include <boost/format.hpp>
#include <boost/make_shared.hpp>

#include <cwidget/generic/util/ref_ptr.h>

using aptitude::util::progress_info;
using aptitude::util::progress_type_bar;
using aptitude::util::progress_type_none;
using aptitude::util::progress_type_pulse;
using boost::format;
using boost::make_shared;
using boost::shared_ptr;
using cwidget::util::ref_ptr;

namespace util = aptitude::util;

namespace aptitude
{
  namespace cmdline
  {
    namespace
    {
      class search_progress : public views::progress
      {
        shared_ptr<views::progress> display;
        shared_ptr<util::throttle> throttle;
        std::string pattern;

      public:
        search_progress(const shared_ptr<views::progress> &_display,
                        const shared_ptr<util::throttle> &_throttle,
                        const std::string &_pattern)
          : display(_display),
            throttle(_throttle),
            pattern(_pattern)
        {
        }

        void set_progress(const progress_info &info);
        void done();
      };

      void search_progress::set_progress(const progress_info &info)
      {
        // This is why the throttling happens at this layer rather than
        // below: we can avoid some expensive string formatting with an
        // up-front check.
        if(!throttle->update_required())
          return;

        // We interpret the progress_info to add a prefix to its message
        // if it has one.
        switch(info.get_type())
          {
          case progress_type_none:
            display->set_progress(info);
            break;

          case progress_type_pulse:
            display->set_progress(progress_info::pulse( (format("%s: %s")
                                                         % pattern
                                                         % info.get_progress_status())
                                                        .str()));
            break;

          case progress_type_bar:
            display->set_progress(progress_info::bar(info.get_progress_fraction(),
                                                     (format("%s: %s")
                                                      % pattern
                                                      % info.get_progress_status())
                                                     .str()));
            break;
          }

        throttle->reset_timer();
      }

      void search_progress::done()
      {
        display->done();
      }
    }

    shared_ptr<views::progress>
    create_search_progress(const std::string &pattern,
                           const shared_ptr<views::progress> &display,
                           const shared_ptr<util::throttle> &throttle)
    {
      return make_shared<search_progress>(display,
                                          throttle,
                                          pattern);
    }
  }
}
