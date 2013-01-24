// test_apt_universe.cc                       -*-c++-*-
//
//   Copyright (C) 2005 Daniel Burrows
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
//
// A test of the aptitude universe wrapper.

#include <generic/aptitude_resolver_universe.h>
#include <generic/config_signal.h>

#include <generic/immset.h>
#include <generic/problemresolver/solution.h>
#include <generic/undo.h>

#include <cppunit/extensions/HelperMacros.h>

#include <apt-pkg/error.h>

#include <sstream>

typedef generic_solution<aptitude_universe> aptitude_solution;

class AptUniverseTest : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(AptUniverseTest);

  CPPUNIT_TEST(testSolves);
  CPPUNIT_TEST(testBrokenList);
  CPPUNIT_TEST(testInteresting);
  CPPUNIT_TEST(testReverseConnectivity);

  CPPUNIT_TEST_SUITE_END();

public:
  void setUp()
  {
    apt_preinit();

    _error->Discard();

    // These state locations are set up for the case of running the
    // program from the 'tests' directory.
    aptcfg->Set("Dir::Etc", "./data");
    aptcfg->Set("Dir::State::Lists", "./data/lists");
    aptcfg->Set("Dir::State::status", "./data/dpkg/status");
    aptcfg->Set("Dir::Aptitude::state", "./data");

    OpProgress p;
    apt_init(&p, false);

    _error->DumpErrors();
  }

  void tearDown()
  {
    // TODO: close the global cache file.
  }

  /** Test that solved_by() is a precise representation of the
   *  dependency solver graph, and incidentally (just to be anal) test
   *  that solvers appear exactly once in the global versions list and
   *  that each solver appears there.
   *
   *  NB: so that this test terminates before the end of the
   *  millenium, I placed a limit on the number of dependencies to
   *  check thoroughly.  That number of dependencies are checked in
   *  the fullest way possible; the remainder are subjected to a more
   *  cursory check that should nevertheless detect the most likely
   *  source of problems.
   */
  void testSolves()
  {
    CPPUNIT_ASSERT(apt_cache_file != NULL);

    aptitude_universe u(*apt_cache_file);

    int dep_count = 500;

    for(aptitude_universe::dep_iterator di = u.deps_begin();
	!di.end(); ++di, dep_count = dep_count > 0 ? dep_count-1 : dep_count)
      {
	aptitude_universe::dep d = *di;
	std::set<aptitude_universe::version> solvers;

	for(aptitude_universe::dep::solver_iterator si
	      = d.solvers_begin(); !si.end(); ++si)
	  {
	    if(!d.solved_by(*si))
	      {
		std::ostringstream out;

		out << (*si).get_package().get_name() << " "
		    << (*si).get_name()
		    << " should solve "
		    << d << " but doesn't.";

		CPPUNIT_FAIL(out.str());
	      }
	    solvers.insert(*si);
	  }

	if(dep_count > 0)
	  {
	    for(aptitude_universe::package_iterator pi
		  = u.packages_begin(); !pi.end(); ++pi)
	      for(aptitude_universe::package::version_iterator vi
		    = (*pi).versions_begin(); !vi.end(); ++vi)
		{
		  if((*pi) == d.get_source().get_package())
		    {
		      if(*vi == d.get_source())
			{
			  if(d.solved_by(*vi))
			    {
			      std::ostringstream out;

			      out << "Dependency source "
				  << (*vi).get_package().get_name() << " "
				  << (*vi).get_name()
				  << " unexpectedly solves " << d;

			      CPPUNIT_FAIL(out.str());
			    }
			}
		      else if(!d.solved_by(*vi))
			{
			  std::ostringstream out;

			  out << "Dependency source removal "
			      << (*vi).get_package().get_name() << " "
			      << (*vi).get_name()
			      << " unexpectedly doesn't solve " << d;

			  CPPUNIT_FAIL(out.str());
			}
		    }
		  else if(solvers.find(*vi) == solvers.end())
		    {
		      if(d.solved_by(*vi))
			{
			  std::ostringstream out;

			  out << (*vi).get_package().get_name() << " "
			      << (*vi).get_name()
			      << " unexpectedly solves "
			      << d;

			  CPPUNIT_FAIL(out.str());
			}
		    }
		  else
		    solvers.erase(*vi);
		}

	    if(!solvers.empty())
	      {
		std::ostringstream out;

		for(std::set<aptitude_universe::version>::const_iterator
		      si = solvers.begin(); si != solvers.end(); ++si)
		  out << "Solver " << (*si).get_package().get_name()
		      << " " << (*si).get_name() << " of dependency "
		      << d << " does not appear in the global list of versions!";

		CPPUNIT_FAIL(out.str());
	      }
	  }
	else
	  for(std::set<aptitude_universe::version>::const_iterator si
		= solvers.begin(); si != solvers.end(); ++si)
	    for(aptitude_universe::package::version_iterator vi
		  = (*si).get_package().versions_begin();
		!vi.end(); ++vi)
	      {
		if(solvers.find(*vi) == solvers.end() &&
		   (*vi == (*di).get_source() || (*vi).get_package() != (*di).get_source().get_package()) &&
		   (*di).solved_by(*vi))
		  {
		    std::ostringstream out;

		    out << (*vi).get_package().get_name() << " "
			<< (*vi).get_name() << " should not solve "
			<< d << " but does.";

		    CPPUNIT_FAIL(out.str());
		  }
	      }
      }
  }

  // Ensure that brokenness corresponds to in-the-broken-listness.
  void testBrokenList()
  {
    undo_group *undo = new undo_group;
    (*apt_cache_file)->mark_all_upgradable(true, undo);

    aptitude_universe u(*apt_cache_file);

    imm::set<aptitude_resolver_dep> broken;
    for(aptitude_universe::broken_dep_iterator bi = u.broken_begin();
	!bi.end(); ++bi)
      broken.insert(*bi);

    aptitude_solution s = aptitude_solution::root_node(broken,
						       u,
						       solution_weights(0, 0, 0, 0, 0));

    for(aptitude_universe::dep_iterator di = u.deps_begin();
	!di.end(); ++di)
      {
	aptitude_resolver_dep d = *di;

	if(broken.contains(d))
	  CPPUNIT_ASSERT(d.broken_under(s));
	else
	  CPPUNIT_ASSERT(!d.broken_under(s));
      }

    undo->undo();
    delete undo;
  }

  // Ensure that all dependencies in the world really are interesting.
  void testInteresting()
  {
    aptitude_universe u(*apt_cache_file);

    for(aptitude_universe::dep_iterator di = u.deps_begin();
	!di.end(); ++di)
      {
	if(!is_interesting_dep((*di).get_dep(), *apt_cache_file))
	  {
	    std::ostringstream out;

	    out << *di << " is uninteresting but is in the global dep list.";

	    CPPUNIT_FAIL(out.str());
	  }
      }

    for(aptitude_universe::package_iterator pi
	  = u.packages_begin(); !pi.end(); ++pi)
      for(aptitude_universe::package::version_iterator vi
	    = (*pi).versions_begin(); !vi.end(); ++vi)
	{
	  for(aptitude_universe::version::dep_iterator di
		= (*vi).deps_begin(); !di.end(); ++di)
	    if(!is_interesting_dep((*di).get_dep(), *apt_cache_file))
	      {
		std::ostringstream out;

		out << *di << " is uninteresting but is in the dep list for " << (*pi).get_name() << " " << (*vi).get_name();

		CPPUNIT_FAIL(out.str());
	      }

	  for(aptitude_universe::version::revdep_iterator rdi
		= (*vi).revdeps_begin(); !rdi.end(); ++rdi)
	    if(!is_interesting_dep((*rdi).get_dep(), *apt_cache_file))
	      {
		std::ostringstream out;

		out << *rdi << " is uninteresting but is in the reverse dep list for " << (*pi).get_name() << " " << (*vi).get_name();

		CPPUNIT_FAIL(out.str());
	      }
	}
  }

  void dump_revdeps(std::ostream &out, const aptitude_universe::version v)
  {
    out << "Reverse dependencies of "
	<< v.get_package().get_name() << " "
	<< v.get_name() << " are:" << std::endl;

    for(aptitude_universe::version::revdep_iterator rdi
	  = v.revdeps_begin(); !rdi.end(); ++rdi)
      {
	out << *rdi
	    << "(" << (pkgCache::Dependency *) (*rdi).get_dep()
	    << ":" << (pkgCache::Provides *) (*rdi).get_prv() << ")"
	    << std::endl;
      }

    if(v.get_ver().end())
      return;

    out << "Low-level reverse dependencies of "
	<< v.get_package().get_name() << " "
	<< v.get_name() << " are:" << std::endl;

    for(pkgCache::DepIterator d = v.get_pkg().RevDependsList();
	!d.end(); ++d)
      {
	out << d.ParentPkg().Name() << " " << d.ParentVer().VerStr()
	    << " " << d.DepType() << " " << d.TargetPkg().Name();

	if(d.TargetVer() != NULL)
	  out << "(" << d.CompType() << " " << d.TargetVer() << ")";

	if(is_interesting_dep(d, *apt_cache_file))
	  out << "[INTERESTING]";

	out << std::endl;
      }
  }

  void testReverseConnectivity()
  {
    CPPUNIT_ASSERT(apt_cache_file != NULL);

    aptitude_universe u(*apt_cache_file);

    // For each dep, look at each of its solvers.  For every /other/
    // version of the solver that is not itself a solver, the
    // dependency should appear in one of their revdep lists.

    for(aptitude_universe::dep_iterator di = u.deps_begin();
	!di.end(); ++di)
      for(aptitude_universe::dep::solver_iterator si
	    = (*di).solvers_begin(); !si.end(); ++si)
	{
	  if(!is_interesting_dep((*di).get_dep(), *apt_cache_file))
	    {
	      std::ostringstream out;
	      out << "Encountered uninteresting dep:"
		  << *di;
	      CPPUNIT_FAIL(out.str());
	    }

	  if(!(*di).solved_by(*si))
	    {
	      std::ostringstream out;

	      out << (*si).get_package().get_name() << " "
		  << (*si).get_name() << " should solve "
		  << *di << " but doesn't.";

	      CPPUNIT_FAIL(out.str());
	    }

	  bool found = false;

	  for(aptitude_universe::version::revdep_iterator rdi
		= (*si).revdeps_begin(); !found && !rdi.end(); ++rdi)
	    if(*rdi == *di)
	      found = true;

	  if(found)
	    continue;

	  // If the above fails, then each of the other versions of
	  // the package should either solve this dep or have this dep
	  // as a revdep.

	  for(aptitude_universe::package::version_iterator vi
		= (*si).get_package().versions_begin(); !vi.end(); ++vi)
	    {
	      // NB: relies on the properties of solved_by tested above.
	      if(*vi != *si && !(*di).solved_by(*vi))
		{
		  bool vi_found = false;

		  for(aptitude_universe::version::revdep_iterator rdi
			= (*vi).revdeps_begin();
		      !vi_found && !rdi.end(); ++rdi)
		    if(*rdi == *di)
		      vi_found = true;

		  if(!vi_found)
		    {
		      std::ostringstream out;
		      out << "The dependency " << *di
			  << "(" << (pkgCache::Dependency *) (*di).get_dep()
			  << ":" << (pkgCache::Provides *) (*di).get_prv()
			  << ") does not occur in the revdep list of either "
			  << (*si).get_package().get_name() << " "
			  << (*si).get_name() << " or "
			  << (*vi).get_package().get_name() << " "
			  << (*vi).get_name() << std::endl;

		      dump_revdeps(out, *si);
		      dump_revdeps(out, *vi);

		      CPPUNIT_FAIL(out.str());
		    }
		}
	    }
	}
  }
};

CPPUNIT_TEST_SUITE_REGISTRATION(AptUniverseTest);
