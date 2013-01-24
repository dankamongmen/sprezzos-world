/** \file file_cache.h */      // -*-c++-*-

//   Copyright (C) 2009 Daniel Burrows
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

#include <cwidget/generic/util/exception.h>

#include <boost/shared_ptr.hpp>

#include <time.h>

#include "temp.h"

namespace aptitude
{
  namespace util
  {
    /** \brief An exception related to the file cache. */
    class FileCacheException : public cwidget::util::Exception
    {
      std::string msg;

    public:
      FileCacheException(const std::string &_msg)
	: msg(_msg)
      {
      }

      std::string errmsg() const { return msg; }
    };

    /** \brief An cache of file contents.
     *
     *  aptitude uses this to store downloaded blobs of data:
     *  changelogs, screenshots, etc.
     *
     *  \note The implementation is based on SQLite, and only hidden
     *  behind an abstract interface so that its details aren't
     *  unnecessarily exposed.
     */
    class file_cache
    {
    public:
      /** \brief Store a file in the cache.
       *
       *  \param key   The key under which the file is to be stored.
       *  \param fd    The name of a file to store in the cache; the
       *               caller is responsible for ensuring that the file
       *               won't be deleted or altered while putItem() is
       *               running.
       *  \param mtime The last modified time of the item to insert.
       */
      virtual void putItem(const std::string &key,
			   const std::string &path,
			   time_t mtime) = 0;

      /** \brief Store a file in the cache.
       *
       *  \param key   The key under which the file is to be stored.
       *  \param fd    The name of a file to store in the cache; the
       *               caller is responsible for ensuring that the file
       *               won't be deleted or altered while putItem() is
       *               running.
       *
       *  The item is stored with a last modified time of 0.
       */
      virtual void putItem(const std::string &key,
			   const std::string &path)
      {
	putItem(key, path, 0);
      }

      /** \brief Retrieve a file from the cache.
       *
       *  As a side effect, marks the file as recently visited, so it
       *  will be less likely to be removed from the cache.
       *
       *  \param key   The key under which the file was stored.
       *  \param mtime Set to the most recent date and time at which
       *               the given key was modified.
       */
      virtual temp::name getItem(const std::string &key,
				 time_t &mtime) = 0;

      /** \brief Retrieve a file from the cache.
       *
       *  As a side effect, marks the file as recently visited, so it
       *  will be less likely to be removed from the cache.
       *
       *  \param key   The key under which the file was stored.
       */
      temp::name getItem(const std::string &key)
      {
	time_t mtime;
	return getItem(key, mtime);
      }

      /** \brief Open or create a new file cache with the given
       *  parameters.
       *
       *  \param filename       The file in which the on-disk cache is
       *                        stored.
       *  \param memory_size    The maximum allowed size in bytes of the in-memory
       *                        cache. (if zero, only an on-disk cache
       *                        will be used)
       *  \param disk_size      The maximum allowed size in bytes of the on-disk
       *                        cache.  (if zero, only a memory cache
       *                        will be used)
       */
      static boost::shared_ptr<file_cache> create(const std::string &filename,
						  int memory_size,
						  int disk_size);

      virtual ~file_cache();
    };
  }
}
