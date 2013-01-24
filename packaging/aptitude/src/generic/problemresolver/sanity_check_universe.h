// sanity_check_universe.h                  -*-c++-*-
//
//   Copyright (C) 2007-2009 Daniel Burrows
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

#include <iostream>
#include <set>

#include "problemresolver.h"
#include "solution.h"

/** \file sanity_check_universe.h */

/** Check that forward/reverse linkages in a package universe
 *  are correct, and spit errors to stdout if not.
 *
 *  Tests that:
 *    - For each package, each version of that package links
 *      back to the package.
 *    - For each package, the package's "current version" occurs
 *      in its version list.
 *    - For each version, the version is the source of all its
 *      forward dependencies.
 *    - For each version of each package, and each reverse
 *      dependency of that version, some version of that package
 *      occurs in the solvers list.
 *    - For each version of each package, each dependency and
 *      reverse dependency of the version occur in the global
 *      dependency list. (TODO: unimplemented)
 *    - Each dependency in the universe occurs in its
 *      source version's dependency list.
 *    - For each dependency in the universe, solved_by() returns
 *      \b true for all its solvers and all other versions of its
 *      source, and \b false for all other package versions.
 *    - Each dependency in the universe, for each of its solvers,
 *      appears either in that solver's reverse dependency list
 *      or in the reverse dependency list of EACH version of
 *      that same package that is not a solver of the dependency.
 *    - Each broken dependency is broken in an empty solution.
 */
template<typename PackageUniverse>
void sanity_check_universe(const PackageUniverse &universe)
{
  typedef typename PackageUniverse::package package;
  typedef typename PackageUniverse::version version;
  typedef typename PackageUniverse::dep dep;

  typedef typename PackageUniverse::package_iterator package_iterator;
  typedef typename PackageUniverse::dep_iterator dep_iterator;
  typedef typename PackageUniverse::broken_dep_iterator broken_dep_iterator;

  std::set<dep> seenRevDeps, seenDeps;
  std::set<package> seenPkgs;
  std::set<version> seenVers;

  for(package_iterator pIt = universe.packages_begin();
      !pIt.end(); ++pIt)
    {
      seenPkgs.insert(*pIt);

      for(typename package::version_iterator vIt = (*pIt).versions_begin();
	  !vIt.end(); ++vIt)
	{
	  seenVers.insert(*vIt);
	}
    }

  for(package_iterator pIt = universe.packages_begin();
      !pIt.end(); ++pIt)
    {
      for(typename package::version_iterator vIt = (*pIt).versions_begin();
	  !vIt.end(); ++vIt)
	{
	  for(typename version::dep_iterator dIt = (*vIt).deps_begin();
	      !dIt.end(); ++dIt)
	    {
	      if((*dIt).get_source() != *vIt)
		{
		  std::cout << "Error: "
			    << *dIt
			    << " is a forward dep of a different version "
			    << *vIt
			    << std::endl;
		}

	      for(typename dep::solver_iterator sIt = (*dIt).solvers_begin();
		  !sIt.end(); ++sIt)
		{
		  if(seenVers.find(*sIt) == seenVers.end())
		    std::cout << "Error: "
			      << *sIt
			      << " exists as a solver of "
			      << *dIt
			      << " but is not a member of the global version list."
			      << std::endl;
		}

	      seenDeps.insert(*dIt);

	      if((*dIt).get_source() != (*vIt))
		{
		  std::cout << "Error: "
			    << (*dIt)
			    << " belongs to the forward dependency list of "
			    << (*vIt)
			    <<std::endl;
		}
	    }

	  for(typename version::revdep_iterator rdIt = (*vIt).revdeps_begin();
	      !rdIt.end(); ++rdIt)
	    {
	      if(seenVers.find((*rdIt).get_source()) == seenVers.end())
		std::cout << "Error: "
			  << (*rdIt).get_source()
			  << " exists as the source of the revdep "
			  << *rdIt
			  << " linked from version "
			  << *vIt
			  << " but is not a member of the global version list."
			  << std::endl;

	      for(typename dep::solver_iterator sIt = (*rdIt).solvers_begin();
		  !sIt.end(); ++sIt)
		{
		  if(seenVers.find(*sIt) == seenVers.end())
		    std::cout << "Error: "
			      << *sIt
			      << " exists as a solver of the revdep "
			      << *rdIt
			      << " linked from "
			      << *vIt
			      << " but is not a member of the global version list."
			      << std::endl;
		}

	      seenRevDeps.insert(*rdIt);

	      bool found = false;
	      for(typename dep::solver_iterator sIt = (*rdIt).solvers_begin();
		  !found && !sIt.end(); ++sIt)
		{
		  std::string solverpkg = (*sIt).get_package().get_name();
		  std::string solverver = (*sIt).get_name();
		  /* heh heh */
		  solverpkg = solverpkg; solverver = solverver;
		  if((*sIt).get_package() == *pIt)
		    found = true;
		}

	      if(!found)
		std::cout << "Error: spurious revdep link from "
			  << (*vIt)
			  << " to "
			  << (*rdIt)
			  << std::endl;
	    }

	  if((*vIt).get_package() != (*pIt))
	    {
	      std::cout << "Error: "
			<< (*vIt)
			<< " is a member of the version list of a different package "
			<< (*pIt).get_name()
			<< std::endl;
	    }
	}

      if((*pIt).current_version().get_package() != (*pIt))
	{
	  std::cout << "Error: the current version of "
		    << (*pIt).get_name()
		    << " is a version of another package, "
		    << (*pIt).current_version()
		    << std::endl;
	}
    }

  for(dep_iterator dIt = universe.deps_begin();
      !dIt.end(); ++dIt)
    {
      // Test solved_by.
      for(package_iterator pIt = universe.packages_begin();
	  !pIt.end(); ++pIt)
	{
	  for(typename package::version_iterator vIt = (*pIt).versions_begin();
	      !vIt.end(); ++vIt)
	    {
	      bool should_be_solver
		= (*pIt) == (*dIt).get_source().get_package() &&
		  (*vIt) != (*dIt).get_source();

	      for(typename dep::solver_iterator sIt = (*dIt).solvers_begin();
		  !should_be_solver && !sIt.end(); ++sIt)
		{
		  if(*sIt == *vIt)
		    should_be_solver = true;
		}

	      if(should_be_solver)
		{
		  if(!(*dIt).solved_by(*vIt))
		    {
		      std::cout << "Error: "
				<< *vIt
				<< " should solve "
				<< *dIt
				<< " but solved_by returned false."
				<< std::endl;
		    }
		}
	      else
		{
		  if((*dIt).solved_by(*vIt))
		    {
		      std::cout << "Error: "
				<< *vIt
				<< " should NOT solve "
				<< *dIt
				<< " but solved_by returned true."
				<< std::endl;
		    }
		}
	    }
	}
    }

  // Check dependency linkage.
  for(dep_iterator dIt = universe.deps_begin();
      !dIt.end(); ++dIt)
    {
      if(seenDeps.find(*dIt) == seenDeps.end())
	std::cout << "Error: "
		  << *dIt
		  << " is not forward-linked from any package."
		  << std::endl;

      if(!(*dIt).solvers_begin().end() &&
	 seenRevDeps.find(*dIt) == seenRevDeps.end())
	std::cout << "Error: "
		  << *dIt
		  << " is not back-linked from any package."
		  << std::endl;

    }

  for(dep_iterator dIt = universe.deps_begin();
      !dIt.end(); ++dIt)
    {
      seenDeps.erase(*dIt);
      seenRevDeps.erase(*dIt);

      {
	// Look for the dependency in it's source's forward list.
	bool found = false;
	for(typename version::dep_iterator dIt2 = (*dIt).get_source().deps_begin();
	    !found && !dIt2.end(); ++dIt2)
	  {
	    if(*dIt2 == *dIt)
	      found = true;
	  }

	if(!found)
	  {
	    std::cout << "Error: Dependency "
		      << (*dIt)
		      << " does not occur in its source's forward list."
		      << std::endl;
	  }
      }

      // Check that this dependency occurs in each of its solver's
      // revdep lists, or in the revdep list of all non-solving
      // versions of that package.
      for(typename dep::solver_iterator sIt = (*dIt).solvers_begin();
	  !sIt.end(); ++sIt)
	{
	  bool found = false;
	  for(typename version::revdep_iterator rdIt = (*sIt).revdeps_begin();
	      !found && !rdIt.end(); ++rdIt)
	    {
	      if(*rdIt == *dIt)
		found = true;
	    }

	  if(!found)
	    {
	      // Check that for each non-solving version of the package,
	      // a revdep link exists.
	      for(typename package::version_iterator vIt = (*sIt).get_package().versions_begin();
		  !found && !vIt.end(); ++vIt)
		{
		  bool is_solver = false;
		  for(typename dep::solver_iterator sIt2 = (*dIt).solvers_begin();
		      !is_solver && !sIt2.end(); ++sIt2)
		    {
		      if(*sIt2 == *vIt)
			is_solver = true;
		    }

		  // This is a non-solving version, so a link must exist.
		  if(!is_solver)
		    {
		      for(typename version::revdep_iterator rdIt = (*vIt).revdeps_begin();
			  !found && !rdIt.end(); ++rdIt)
			{
			  if(*rdIt == *dIt)
			    found = true;
			}

		      if(!found)
			{
			  // TODO: check how many alternates *are* linked
			  // (do all of them fail or just some?)
			  std::cout << "Error: "
				    << *dIt
				    << " is not linked into the revdep list of "
				    << *sIt
				    << " and neither is the alternate version "
				    << *vIt
				    << std::endl;
			}
		    }
		}
	    }
	}
    }

  // TODO: check the reverse containment property too.
  for(typename std::set<dep>::const_iterator it = seenDeps.begin();
      it != seenDeps.end(); ++it)
    {
      std::cout << "Error: "
		<< (*it)
		<< " is contained in a package dep list but not the global dep list."
		<< std::endl;
    }

  for(typename std::set<dep>::const_iterator it = seenRevDeps.begin();
      it != seenRevDeps.end(); ++it)
    {
      std::cout << "Error: "
		<< (*it)
		<< " is contained in a package reverse dep list but not the global dep list."
		<< std::endl;
    }

  resolver_initial_state<PackageUniverse>
    initial_state(imm::map<package, version>(), universe.get_package_count());

  for(broken_dep_iterator bdIt = universe.broken_begin();
      !bdIt.end(); ++bdIt)
    {
      if(!(*bdIt).broken_under(initial_state))
	{
	  std::cout << "Error: "
		    << (*bdIt)
		    <<" is in the broken dep list but isn't broken in the empty solution."
		    << std::endl;
	}
    }
}
