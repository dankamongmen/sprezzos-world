/** \file package.h */   // -*-c++-*-
//
// Copyright 1999-2008 Daniel Burrows
// Copyright 2008 Obey Arthur Liu
// Copyright (C) 2010 Piotr Galiszewski
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

#ifndef APTITUDE_QT_PACKAGE_H
#define APTITUDE_QT_PACKAGE_H

// Local includes
#include "version.h"

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <generic/apt/desc_parse.h>
#include <generic/apt/tags.h>

// System includes
#include <apt-pkg/pkgcache.h>

#include <boost/make_shared.hpp>
#include <boost/optional.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/utility.hpp>

#include <set>
#include <vector>

#ifdef HAVE_QT
#include <QtCore/QMetaType>
#endif

using aptitude::apt::tag;
using boost::make_shared;
using boost::optional;

namespace aptitude
{
  namespace gui
  {
    namespace qt
    {
      class package;
      typedef boost::shared_ptr<package> package_ptr;

      /** \brief An object representing a displayable package.
       *
       *  This class contains methods extracting package informations. Except name
       *  (which is loaded during object creation), every component is lazy loaded
       *  on the first request for its value.
       *
       *  It is also possible to retrieve the raw PkgIterator object for this package.
       */
      class package : boost::noncopyable
      {
        friend boost::shared_ptr<package> make_shared<package>(const pkgCache::PkgIterator &);

        const pkgCache::PkgIterator pkg;
        mutable boost::optional<version_ptr> candidate_ver;
        mutable boost::optional<version_ptr> visible_ver;

        const std::string empty_string;
        const std::wstring empty_wstring;
        const std::vector<description_element_ref> long_description_fallback;

        mutable boost::optional<std::string> current_version;
        mutable std::string name;
        mutable boost::optional<std::string> section;
        mutable boost::optional<std::string> short_desc_fallback;
        mutable boost::optional<std::string> source_package;
        mutable boost::optional<std::set<tag> > tags;
        mutable boost::optional<std::vector<version_ptr> > versions;

        void load_versions() const;

        /** \brief Create a new package object for the given PkgIterator. */
        explicit package(const pkgCache::PkgIterator &_pkg);

      public:
        /** \brief Create a new package object for the given PkgIterator. */
        static package_ptr create(const pkgCache::PkgIterator &pkg);

        ~package();

        typedef std::vector<version_ptr>::const_iterator version_iterator;
        typedef std::set<tag>::const_iterator tag_iterator;

        /** \brief Retrieve all available versions of this package. */
        version_iterator versions_begin() const;
        version_iterator versions_end() const;

        /** \brief Retrieve a list of tags for this package. */
        tag_iterator tags_begin() const;
        tag_iterator tags_end() const;

        /** \brief Retrieve the archive which contains the candidate version of this package. */
        const std::string &get_archive() const;

        /** \brief Retrieve the candidate version number of this package. */
        const std::string &get_candidate_version() const;

        /** \brief Retrieve the installed version number of this package. */
        const std::string &get_current_version() const;

        /** \brief Retrieve the package's homepage. */
        const std::string &get_homepage() const;

        /** \brief Retrieve the maintainer of this package. */
        const std::string &get_maintainer() const;

        /** \brief Retrieve the name of this package. */
        const std::string &get_name() const;

        /** \brief Retrieve the list of a parsed long description's elements of this package. */
        const std::vector<description_element_ref> &get_parsed_long_description() const;

        /** \brief Retrieve the priority of this version. */
        const std::string &get_priority() const;

        /** \brief Retrieve the raw form of the long description of this package. */
        const std::wstring &get_raw_long_description() const;

        /** \brief Retrieve the section to which this package belongs. */
        const std::string &get_section() const;

        /** \brief Retrieve the short description of this package. */
        const std::string &get_short_description() const;

        /** \brief Retrieve the name of this package's source package. */
        const std::string &get_source_package() const;

        /** \brief Retrieve the PkgIterator corresponding to this package. */
        const pkgCache::PkgIterator &get_pkg() const;

        /** \brief Retrieve the candidate version of this package, or an invalid reference
         *  if the package has no candidate version.
         */
        const version_ptr &get_candidate_ver() const;

        /** \brief Retrieve the default visible version of this package, or an invalid reference
         *  if the package has no version.
         */
        const version_ptr &get_visible_ver() const;
      };
    }
  }
}

#ifdef HAVE_QT
Q_DECLARE_METATYPE(aptitude::gui::qt::package_ptr)
#endif

#endif // APTITUDE_QT_PACKAGE_H
