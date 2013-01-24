/** \file version.h */   // -*-c++-*-
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

#ifndef APTITUDE_QT_VERSION_H
#define APTITUDE_QT_VERSION_H

// Local includes
#include <generic/apt/desc_parse.h>

// System includes
#include <apt-pkg/pkgcache.h>

#include <boost/make_shared.hpp>
#include <boost/optional.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/utility.hpp>

#include <list>
#include <vector>

using aptitude::description_element_ref;
using boost::make_shared;
using boost::optional;

namespace aptitude
{
  namespace gui
  {
    namespace qt
    {
      class version;
      typedef boost::shared_ptr<version> version_ptr;

      /** \brief An object representing a displayable version of a package.
       *
       *  This class contains methods extracting version informations. Every component
       *  is lazy loaded on the first request for its value.
       *
       *  It is also possible to retrieve the raw VerIterator object for this version.
       */
      class version : boost::noncopyable
      {
        friend boost::shared_ptr<version> make_shared<version>(const pkgCache::VerIterator &);

        const pkgCache::VerIterator ver;

        mutable boost::optional<std::string> archive;
        mutable boost::optional<std::string> homepage;
        mutable boost::optional<std::string> maintainer;
        mutable boost::optional<std::vector<description_element_ref> > parsed_long_description;
        mutable boost::optional<std::string> priority;
        mutable boost::optional<std::wstring> raw_long_description;
        mutable boost::optional<std::string> short_description;
        mutable boost::optional<std::string> source_package;
        mutable boost::optional<std::string> version_number;

        /** \brief Create a new version object for the given VerIterator. */
        explicit version(const pkgCache::VerIterator &_ver);

      public:
        /** \brief Create a new version object for the given VerIterator. */
        static version_ptr create(const pkgCache::VerIterator &ver);

        ~version();

        /** \brief Retrieve a description of the archives that contain this version. */
        const std::string &get_archive() const;

        /** \brief Retrieve the package's homepage. */
        const std::string &get_homepage() const;

        /** \brief Retrieve the maintainer of this version. */
        const std::string &get_maintainer() const;

        /** \brief Retrieve the list of a parsed long description's elements of this version. */
        const std::vector<description_element_ref> &get_parsed_long_description() const;

        /** \brief Retrieve the priority of this version. */
        const std::string &get_priority() const;

        /** \brief Retrieve the raw form of the long description of this version. */
        const std::wstring &get_raw_long_description() const;

        /** \brief Retrieve the short description of this version. */
        const std::string &get_short_description() const;

        /** \brief Retrieve the name of this version's source package. */
        const std::string &get_source_package() const;

        /** \brief Retrieve the version number of this version. */
        const std::string &get_version_number() const;

        /** \brief Retrieve the VerIterator corresponding to this version. */
        const pkgCache::VerIterator &get_ver() const;
      };
    }
  }
}

#endif // APTITUDE_QT_VERSION_H
