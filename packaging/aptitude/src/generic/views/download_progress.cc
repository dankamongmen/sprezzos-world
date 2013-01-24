// download_progress.cc
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

#include "download_progress.h"

#include <cwidget/generic/util/transcode.h>

namespace cw = cwidget;

using cw::util::transcode;

namespace aptitude
{
  namespace views
  {
    download_progress::~download_progress()
    {
    }

    bool download_progress::file_progress::operator==(const file_progress &other) const
    {
      return current_size == other.current_size
          && total_size   == other.total_size
          && complete     == other.complete
          && description  == other.description
          && id           == other.id
          && mode         == other.mode;
    }

    std::ostream &operator<<(std::ostream &out, const download_progress::file_progress &progress)
    {
      out << "(current_size = "
          << progress.get_current_size()
          << ", total_size = "
          << progress.get_total_size()
          << ", complete = "
          << (progress.get_complete() ? "yes" : "no")
          << ", description = "
          << progress.get_description()
          << ", id = ";
      if(progress.get_id())
        out << *progress.get_id();
      else
        out << "(none)";
      out << ", mode = "
          << progress.get_mode();
      return out << ")";
    }

    bool download_progress::status::operator==(const status &other) const
    {
      return download_rate     == other.download_rate
          && fraction_complete == other.fraction_complete
          && time_remaining    == other.time_remaining
          && active_downloads  == other.active_downloads;
    }

    class show_worker_status : public boost::static_visitor<>
    {
      std::ostream &out;

    public:
      show_worker_status(std::ostream &_out)
        : out(_out)
      {
      }

      void operator()(const download_progress::file_progress &progress) const
      {
        out << "progress:" << progress;
      }

      void operator()(const std::string &msg) const
      {
        out << "msg:\"" << msg << "\"";
      }
    };

    std::ostream &operator<<(std::ostream &out,
                             const download_progress::status &status)
    {
      out << "(download_rate = " << status.get_download_rate()
          << ", fraction_complete = " << status.get_fraction_complete()
          << ", time_remaining = " << status.get_time_remaining()
          << ", active_downloads = [";

      typedef download_progress::status::worker_status worker_status;

      show_worker_status f(out);
      const std::vector<worker_status> active_downloads =
        status.get_active_downloads();
      for(std::vector<worker_status>::const_iterator it
            = active_downloads.begin(); it != active_downloads.end(); ++it)
        {
          if(it != active_downloads.begin())
            out << ", ";
          boost::apply_visitor(f, *it);
        }

      return out << "]";
    }
  }
}

