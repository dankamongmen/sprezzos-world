// pkg_changelog.h    -*-c++-*-
//
//  Copyright 2000, 2005, 2008-2009 Daniel Burrows
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; see the file COPYING.  If not, write to
//  the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//  Boston, MA 02111-1307, USA.
//


#ifndef PKG_CHANGELOG_H
#define PKG_CHANGELOG_H

#include <map>
#include <string>

#include <generic/util/post_thunk.h>
#include <generic/util/temp.h>

#include <apt-pkg/pkgcache.h>

#include <sigc++/signal.h>
#include <sigc++/slot.h>

#include <boost/shared_ptr.hpp>

/** \brief Routines to download a Debian changelog for a given package.
 *
 *  \file pkg_changelog.h
 */

class download_manager;

namespace aptitude
{
  class download_callbacks;
  class download_request;

  namespace apt
  {
    /** \brief Carries information about which changelog is to be
     *  downloaded.
     */
    class changelog_info
    {
      const std::string source_package;
      const std::string source_version;
      const std::vector<std::string> uri_list;
      const std::string display_name;

    public:
      /** \brief This constructor is public for make_shared; it should
       *  never be invoked directly.  Use the static create() methods
       *  instead.
       */
      changelog_info(const std::string &_source_package,
		     const std::string &_source_version,
                     const std::vector<std::string> &_uri_list,
		     const std::string &_display_name)
	: source_package(_source_package),
	  source_version(_source_version),
          uri_list(_uri_list),
	  display_name(_display_name)
      {
      }

      static boost::shared_ptr<changelog_info>
      create(const std::string &source_package,
	     const std::string &source_version,
             const std::vector<std::string> &uri_list,
	     const std::string &display_name);

      static boost::shared_ptr<changelog_info>
      guess(const std::string &source_package,
	    const std::string &source_version,
            const std::string &section,
	    const std::string &display_name);

      /** \brief Create a changelog_info structure that describes the
       *  changelog of the given package version.
       *
       *  \return the new object, or an invalid pointer if the
       *  changelog can't be downloaded.
       */
      static boost::shared_ptr<changelog_info>
      create(const pkgCache::VerIterator &ver);

      /** \brief Retrieve the name of the changelog's source package. */
      const std::string &get_source_package() const { return source_package; }
      /** \brief Retrieve the name of the changelog's source version. */
      const std::string &get_source_version() const { return source_version; }
      /** \brief Retrieve the list of URIs to try, in order. */
      const std::vector<std::string> &get_uri_list() const { return uri_list; }
      /** \brief Retrieve the display name of the changelog's package.
       *
       *  This is the name that should be displayed to the user when,
       *  for instance, the changelog is being downloaded.  The name
       *  of the binary package that was selected is a good choice
       *  here.
       */
      const std::string &get_display_name() const { return display_name; }
    };

    /** \brief Start downloading a changelog.
     *
     *  \param info       The changelog that is to be fetched.
     *  \param callbacks  The callbacks to invoke for download events.
     *  \param post_thunk How to post thunks to the foreground thread.
     */
    boost::shared_ptr<download_request>
    get_changelog(const boost::shared_ptr<changelog_info> &info,
		  const boost::shared_ptr<download_callbacks> &callbacks,
		  post_thunk_f post_thunk);

    /** \brief Convenience code to download a version's changelog.
     *
     *  The given slots are invoked in the main thread via post_thunk
     *  when the download completes.
     *
     *  \note The slots are handled safely as long as it is safe to
     *  copy them in the thread that invokes get_changelog().  No
     *  special treatment is necessary.
     */
    boost::shared_ptr<download_request>
    get_changelog(const pkgCache::VerIterator &ver,
		  post_thunk_f post_thunk,
		  const sigc::slot<void, temp::name> &success,
		  const sigc::slot<void, std::string> &failure);
  }
}

#endif
