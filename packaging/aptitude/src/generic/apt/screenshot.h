/** \file screenshot.h */   // -*-c++-*-

// Copyright (C) 2009 Daniel Burrows
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

#ifndef SCREENSHOT_H
#define SCREENSHOT_H

#include <boost/functional/hash.hpp>
#include <boost/shared_ptr.hpp>

#include <generic/util/post_thunk.h>
#include <generic/util/safe_slot.h>

namespace aptitude
{
  class download_callbacks;
  class download_request;

  /** \brief The type of screenshot to download. */
  enum screenshot_type
    {
      /** \brief Download a small thumbnail image. */
      screenshot_thumbnail,
      /** \brief Download a full-size screenshot. */
      screenshot_full
    };

  /** \brief Describes the information that identifies a particular
   *  screenshot.
   */
  class screenshot_key
  {
    screenshot_type type;
    std::string package_name;

  public:
    /** \brief Create a screenshot key.
     *
     *  \param _type           The type of the screenshot to select.
     *  \param _package_name   The name of the package the screenshot
     *                         is for.
     */
    screenshot_key(screenshot_type _type,
		   const std::string &_package_name)
      : type(_type),
	package_name(_package_name)
    {
    }

    /** \brief Get the type of the screenshot identified by this key. */
    screenshot_type get_type() const { return type; }
    /** \brief Get the package name of the screenshot identified by this key. */
    const std::string &get_package_name() const { return package_name; }

    bool operator==(const screenshot_key &other) const
    {
      return type == other.type && package_name == other.package_name;
    }

    bool operator!=(const screenshot_key &other) const
    {
      return type != other.type && package_name != other.package_name;
    }

    bool operator<(const screenshot_key &other) const
    {
      if(type < other.type)
	return true;
      else if(type > other.type)
	return false;
      else
	return package_name < other.package_name;
    }
  };

  std::ostream &operator<<(std::ostream &out, const screenshot_key &key);

  inline std::size_t hash_value(const screenshot_key &key)
  {
    std::size_t seed = 0;

    boost::hash_combine(seed, key.get_type());
    boost::hash_combine(seed, key.get_package_name());

    return seed;
  }

  /** \brief  Add a download to the global queue that will fetch a
   *          screenshot for the given package.
   *
   *  \param key           The screenshot to download.
   *
   *  \param callbacks     A callback object invoked for events relating
   *                       to the download.
   *
   *  \param post_thunk    A function used to invoke callbacks in the
   *                       main thread.
   *
   *  This function will search the global download cache, if one is
   *  initialized, for an existing screenshot.  It can be invoked from
   *  a background thread, as long as the download cache isn't
   *  destroyed while it's running (i.e., whatever calls it should
   *  hook into cache_closing and wait for get_screenshot() to
   *  return before it allows cache_closing to return).
   *
   *  \return a handle that can be used to cancel the download.
   */
  boost::shared_ptr<download_request>
  get_screenshot(const screenshot_key &key,
		 const boost::shared_ptr<download_callbacks> &callbacks,
		 post_thunk_f post_thunk);
}

#endif // SCREENSHOT_H
