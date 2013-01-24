/** \file package.cc */
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
#include "package.h"

#include "aptitude.h"
#include "solution_fragment.h" // For archives_text.

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>

// System includes
#include <apt-pkg/pkgcache.h>
#include <apt-pkg/strutl.h>
#include <apt-pkg/pkgrecords.h>

namespace cw = cwidget;

using aptitude::apt::get_tags;
using aptitude::apt::tag;
using aptitude::description_element_ref;
using aptitude::parse_desc;
using boost::make_shared;
using cw::util::transcode;

namespace aptitude
{
  namespace gui
  {
    namespace qt
    {
      package_ptr package::create(const pkgCache::PkgIterator &pkg)
      {
        return make_shared<package>(pkg);
      }

      package::package(const pkgCache::PkgIterator &_pkg)
        : pkg(_pkg)
      {
        // name is always used during during initial sorting.
        // So lazy loading doesn't have sense here.
        name = pkg.Name();
      }

      package::~package()
      {
      }

      const pkgCache::PkgIterator &package::get_pkg() const
      {
        return pkg;
      }

      const version_ptr &package::get_candidate_ver() const
      {
        if(!candidate_ver)
        {
          pkgDepCache::StateCache &state = (*apt_cache_file)[pkg];

          pkgCache::VerIterator candver = state.CandidateVerIter(*apt_cache_file);

          if(!candver.end())
            candidate_ver = version::create(candver);
          else
            candidate_ver = version_ptr();
        }
        return *candidate_ver;
      }

      const version_ptr &package::get_visible_ver() const
      {
        if(!visible_ver)
        {
          pkgDepCache::StateCache &state = (*apt_cache_file)[pkg];

          if(get_candidate_ver())
            visible_ver = get_candidate_ver();
          else if(pkg.CurrentVer().end() && state.Install())
            visible_ver = version::create(state.InstVerIter(*apt_cache_file));
          else if(!pkg.CurrentVer().end())
            visible_ver = version::create(pkg.CurrentVer());
          else if(!pkg.VersionList().end())
            visible_ver = version::create(pkg.VersionList());
          else
            visible_ver = version_ptr();
        }
        return *visible_ver;
      }

      package::version_iterator package::versions_begin() const
      {
        if(!versions)
          load_versions();

        return (*versions).begin();
      }

      package::version_iterator package::versions_end() const
      {
        if(!versions)
          load_versions();

        return (*versions).end();
      }

      package::tag_iterator package::tags_begin() const
      {
        if(!tags)
          tags = aptitude::apt::get_tags(pkg);

        return (*tags).begin();
      }

      package::tag_iterator package::tags_end() const
      {
        if(!tags)
          tags = aptitude::apt::get_tags(pkg);

        return (*tags).end();
      }

      void package::load_versions() const
      {
        versions = std::vector<version_ptr>();

        for(pkgCache::VerIterator i=pkg.VersionList(); !i.end(); i++)
          if(i!=get_visible_ver()->get_ver())
            (*versions).push_back(version::create(i));
        else
          (*versions).push_back(get_visible_ver());
      }

      const std::string &package::get_archive() const
      {
        return get_visible_ver()
             ? get_visible_ver()->get_archive()
             : empty_string;
      }

      const std::string &package::get_candidate_version() const
      {
        return get_candidate_ver()
             ? get_candidate_ver()->get_version_number()
             : empty_string;
      }

      const std::string &package::get_current_version() const
      {
        if(!current_version)
          current_version = pkg.CurrentVer()
                          ? pkg.CurrentVer().VerStr()
                          : std::string();

        return *current_version;
      }

      const std::string &package::get_homepage() const
      {
        return get_visible_ver()
             ? get_visible_ver()->get_homepage()
             : empty_string;
      }

      const std::string &package::get_maintainer() const
      {
        return get_visible_ver()
             ? get_visible_ver()->get_maintainer()
             : empty_string;
      }

      const std::string &package::get_name() const
      {
        return name;
      }

      const std::vector<description_element_ref> &package::get_parsed_long_description() const
      {
        return get_visible_ver()
             ? get_visible_ver()->get_parsed_long_description()
             : long_description_fallback;
      }

      const std::string &package::get_priority() const
      {
        return get_visible_ver()
             ? get_visible_ver()->get_priority()
             : empty_string;
      }

      const std::wstring &package::get_raw_long_description() const
      {
        return get_visible_ver()
             ? get_visible_ver()->get_raw_long_description()
             : empty_wstring;
      }

      const std::string &package::get_section() const
      {
        if(!get_visible_ver())
          return empty_string;

        if(!section)
          section = pkg.Section()
                  ? pkg.Section()
                  : _("Unknown");

        return *section;
      }

      const std::string &package::get_short_description() const
      {
        if(!get_visible_ver())
        {
          if(!short_desc_fallback)
            short_desc_fallback = _("virtual");

          return *short_desc_fallback;
        }

        return get_visible_ver()->get_short_description();
      }

      const std::string &package::get_source_package() const
      {
        if(!get_visible_ver())
          return empty_string;

        if(!source_package)
        {
          source_package = get_visible_ver()->get_source_package();
          if((*source_package).empty())
            source_package = get_name();
        }
        return *source_package;
      }
    }
  }
}
