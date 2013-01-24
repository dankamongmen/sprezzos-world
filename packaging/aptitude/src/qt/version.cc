/** \file version.cc */
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

// Local includes
#include "version.h"

#include "aptitude.h"
#include "solution_fragment.h" // For archives_text.

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>

// System includes
#include <apt-pkg/pkgcache.h>
#include <apt-pkg/strutl.h>
#include <apt-pkg/pkgrecords.h>

namespace cw = cwidget;

using aptitude::description_element_ref;
using aptitude::parse_desc;
using cw::util::transcode;

namespace aptitude
{
  namespace gui
  {
    namespace qt
    {
      version_ptr version::create(const pkgCache::VerIterator &ver)
      {
        return make_shared<version>(ver);
      }

      version::version(const pkgCache::VerIterator &_ver)
        : ver(_ver)
      {
      }

      version::~version()
      {
      }

      const pkgCache::VerIterator &version::get_ver() const
      {
        return ver;
      }

      const std::string &version::get_archive() const
      {
        if(!archive)
        {
          archive = archives_text(ver);
        }
        return *archive;
      }

      const std::string &version::get_homepage() const
      {
        if(!homepage)
        {
          pkgRecords::Parser & parser = apt_package_records->Lookup(ver.FileList());
          homepage = parser.Homepage();
        }
        return *homepage;
      }

      const std::string &version::get_maintainer() const
      {
        if(!maintainer)
        {
          pkgRecords::Parser &rec=apt_package_records->Lookup(ver.FileList());
          maintainer = rec.Maintainer().c_str();
        }
        return *maintainer;
      }

      const std::vector<description_element_ref> &version::get_parsed_long_description() const
      {
        if(!parsed_long_description)
        {
          const std::wstring fulldesc = get_long_description(ver, apt_package_records);

          parsed_long_description = std::vector<description_element_ref>();

          parse_desc(fulldesc, *parsed_long_description);
        }
        return *parsed_long_description;
      }

      const std::string &version::get_priority() const
      {
        if(!priority)
        {
          pkgCache::VerIterator &temp = const_cast<pkgCache::VerIterator &>(ver);
          priority = temp.PriorityType()?temp.PriorityType():_("Unknown");
        }
        return *priority;
      }

      const std::wstring &version::get_raw_long_description() const
      {
        if(!raw_long_description)
          raw_long_description = get_long_description(ver, apt_package_records);

        return *raw_long_description;
      }

      const std::string &version::get_short_description() const
      {
        if(!short_description)
          short_description = transcode(::get_short_description(ver, apt_package_records), "UTF-8");

        return *short_description;
      }

      const std::string &version::get_source_package() const
      {
        if(!source_package)
        {
          pkgRecords::Parser &rec=apt_package_records->Lookup(ver.FileList());
          source_package = rec.SourcePkg().empty()?"":rec.SourcePkg().c_str();
        }
        return *source_package;
      }

      const std::string &version::get_version_number() const
      {
        if(!version_number)
          version_number = ver.VerStr();

        return *version_number;
      }
    }
  }
}

