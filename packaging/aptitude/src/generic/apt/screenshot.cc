/** \file screenshot.cc */


// Copyright (C) 2009 Daniel Burrows
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

#include "screenshot.h"

#include <generic/apt/download_queue.h>

#include <boost/format.hpp>

namespace aptitude
{
  boost::shared_ptr<download_request>
  get_screenshot(const screenshot_key &key,
		 const boost::shared_ptr<download_callbacks> &callbacks,
		 post_thunk_f post_thunk)
  {
    std::string uri, short_description;
    switch(key.get_type())
      {
      case screenshot_thumbnail:
      default: // If we get a bad value, just fall back to thumbnail
	       // mode.
	uri = (boost::format("http://screenshots.debian.net/thumbnail/%s")
	       % key.get_package_name()).str();
	short_description = (boost::format("Thumbnail screenshot of %s")
			     % key.get_package_name()).str();
	break;

      case screenshot_full:
	uri = (boost::format("http://screenshots.debian.net/screenshot/%s")
	       % key.get_package_name()).str();
	short_description = (boost::format("Screenshot of %s")
			     % key.get_package_name()).str();
	break;
      }

    return queue_download(uri, short_description,
			  callbacks, post_thunk);
  }

  std::ostream &operator<<(std::ostream &out, const screenshot_key &key)
  {
    std::string typestr;
    switch(key.get_type())
      {
      case screenshot_thumbnail:
	typestr = "thumbnail";
	break;

      case screenshot_full:
	typestr = "full-size";
	break;

      default:
	typestr = "BADTYPE";
	break;
      }

    return out << "(package_name = " << key.get_package_name()
	       << ", type = " << typestr << ")";
  }
}
