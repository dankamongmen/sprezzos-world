// cmdline_check_resolver.cc
//
//   Copyright (C) 2005, 2007-2008 Daniel Burrows

//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.

//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.

//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.
//
// Just print out the current resolver state (debugging tool)

#include "cmdline_check_resolver.h"

#include <generic/apt/aptitude_resolver_universe.h>
#include <generic/problemresolver/sanity_check_universe.h>

#include <apt-pkg/error.h>

#include <algorithm>
#include <iterator>

using namespace std;

namespace
{
  // Check that each real (non-virtual) package and real version are
  // represented in the universe.
  void check_packages_and_versions(const aptitude_universe &u)
  {
    std::set<pkgCache::PkgIterator> real_packages;

    // Insert each non-virtual package into real_packages.
    for(pkgCache::PkgIterator pkg = (*apt_cache_file)->PkgBegin();
	!pkg.end(); ++pkg)
      {
	if(!pkg.VersionList().end())
	  real_packages.insert(pkg);
      }

    std::set<pkgCache::PkgIterator> seen_packages;

    for(aptitude_universe::package_iterator pi = u.packages_begin();
	!pi.end(); ++pi)
      {
	const aptitude_universe::package p = *pi;
	const pkgCache::PkgIterator apt_pkg = p.get_pkg();
	bool seen_uninst = false;

	seen_packages.insert(apt_pkg);

	std::set<pkgCache::VerIterator> real_versions;
	for(pkgCache::VerIterator ver = apt_pkg.VersionList();
	    !ver.end(); ++ver)
	  {
	    // Skip versions that exist only on the current system and
	    // that have been removed; the resolver does.
	    if( ! (!ver.Downloadable() &&
		   (ver != apt_pkg.CurrentVer() ||
		    apt_pkg->CurrentState == pkgCache::State::ConfigFiles) ))
	      real_versions.insert(ver);
	  }

	std::set<pkgCache::VerIterator> seen_versions;
	for(aptitude_universe::package::version_iterator vi = p.versions_begin();
	    !vi.end(); ++vi)
	  {
	    if(vi.get_ver().end())
	      seen_uninst = true;
	    else
	      seen_versions.insert(vi.get_ver());
	  }

	if(!seen_uninst)
	  std::cout << "Didn't see the UNINST version of the package " << apt_pkg.FullName(false) << "." << std::endl;

	std::vector<pkgCache::VerIterator> remaining_versions;
	std::set_difference(real_versions.begin(), real_versions.end(),
			    seen_versions.begin(), seen_versions.end(),
			    std::back_inserter(remaining_versions));

	for(std::vector<pkgCache::VerIterator>::const_iterator it =
	      remaining_versions.begin(); it != remaining_versions.end();
	    ++it)
	  {
	    std::cout << "The package version " << it->ParentPkg().FullName(false)
		      << " " << it->VerStr() << " is missing from the resolver."
		      << std::endl;
	  }

	if((*apt_cache_file)[apt_pkg].Keep() &&
	   apt_pkg->CurrentState == pkgCache::State::ConfigFiles)
	  {
	    if(!p.current_version().get_ver().end())
	      {
		std::cout << "The package " << apt_pkg.FullName(false)
			  << " only has config files installed, but version "
			  << p.current_version().get_ver().VerStr()
			  << " is installed according to the resolver."
			  << std::endl;
	      }
	  }
	else
	  {
	    pkgCache::VerIterator cache_instver = (*apt_cache_file)[apt_pkg].InstVerIter(*apt_cache_file);
	    pkgCache::VerIterator resolver_instver = p.current_version().get_ver();

	    if(cache_instver != resolver_instver)
	      {
		if(cache_instver.end())
		  std::cout << "The package " << apt_pkg.FullName(false)
			    << " should not be installed, but version "
			    << resolver_instver.VerStr()
			    << " is installed according to the resolver."
			    << std::endl;
		else if(resolver_instver.end())
		  std::cout << "The package " << apt_pkg.FullName(false)
			    << " should be installed at version "
			    << cache_instver.VerStr()
			    << ", but it isn't installed according to the resolver."
			    << std::endl;
		else
		  std::cout << "The package " << apt_pkg.FullName(false)
			    << " should be installed at version "
			    << cache_instver.VerStr()
			    << ", but version "
			    << resolver_instver.VerStr()
			    << " is installed according to the resolver."
			    << std::endl;
	      }
	  }
      }

    std::vector<pkgCache::PkgIterator> remaining_packages;
    std::set_difference(real_packages.begin(), real_packages.end(),
			seen_packages.begin(), seen_packages.end(),
			std::back_inserter(remaining_packages));

    for(std::vector<pkgCache::PkgIterator>::const_iterator it =
	  remaining_packages.begin(); it != remaining_packages.end(); ++it)
      {
	std::cout << "The package " << it->FullName(true)
		  << " is missing from the resolver model." << std::endl;
      }
  }
}

int cmdline_check_resolver(int argc, char *argv[],
			  const char *status_fname)
{
  consume_errors();

  OpProgress progress;

  apt_init(&progress, true, status_fname);

  if(_error->PendingError())
    return 100;

  aptitude_universe u(*apt_cache_file);

  std::cout << "Checking that packages and versions are properly projected."
	    << std::endl;

  check_packages_and_versions(u);

  // TODO: test that all dependencies are represented, somehow.  This
  // is complicated since dependency representation isn't one-to-one.

  std::cout << "Checking internal consistency of the dependency model."
	    << std::endl;

  sanity_check_universe(u);

  std::cout << "Sanity check complete." << std::endl;

  return 0;
}
