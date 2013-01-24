// test.cc
//
//   Copyright (C) 2005, 2007-2010 Daniel Burrows
//
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.  You should have
//   received a copy of the GNU General Public License along with this
//   program; see the file COPYING.  If not, write to the Free
//   Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
//   MA 02111-1307, USA.
//
// Demonstrates the use of the problem-resolver and (eventually) runs
// tests on it.  The convoluted adaptor classes are the way they are
// in order to keep the APT end of things reasonably thin and
// efficient.

#include "dummy_universe.h"
#include "problemresolver.h"
#include "sanity_check_universe.h"

#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <vector>

#include <cwidget/generic/util/eassert.h>
#include <cwidget/generic/util/ssprintf.h>

#include <string.h>

using namespace std;

namespace cw = cwidget;

// This is a gross hack; without defining this here, we'd have to
// somehow link in a higher-level library.
logging::LoggerPtr aptitude::Loggers::getAptitudeResolver()
{
  return logging::Logger::getLogger("aptitude.resolver");
}

logging::LoggerPtr aptitude::Loggers::getAptitudeResolverSearch()
{
  return logging::Logger::getLogger("aptitude.resolver.search");
}

logging::LoggerPtr aptitude::Loggers::getAptitudeResolverSearchGraph()
{
  return logging::Logger::getLogger("aptitude.resolver.search.graph");
}

logging::LoggerPtr aptitude::Loggers::getAptitudeResolverSearchCosts()
{
  return logging::Logger::getLogger("aptitude.resolver.search.costs");
}

// To make things easier, the tests are specified as plaintext files.
// The syntax is quite simple: it consists of whitespace-separated
// words, of the form:
//
// SCRIPT ::= "UNIVERSE" "[" UNIVERSE "]" TEST ...
// UNIVERSE ::= (PACKAGE | DEP) ...
// PACKAGE ::= "PACKAGE" pkgname "<" vername1 ... ">" currentver
// DEP ::= "DEP" pkgname1 vername1 "->" "<" pkgname2 vername2 ... ">"
//       | "SOFTDEP" pkgname1 vername1 "->" "<" pkgname2 vername2 ... ">"
//       | "DEP" pkgname1 vername1 "!!" "<" pkgname2 vername2 ... ">"
//
// TEST ::= "TEST" step_score broken_score "{" SCORE ... "}" "EXPECT" "(" SOLN ... ")"
// SCORE ::= "SCORE" pkgname "<" vername1 score1 ... ">"
//        |  "SCORE" { (pkgname vername)* } score
// SOLN ::= step_count "ANY"
//       |  step_count "<" pkgname1 vername1 ... ">"
//
// The second DEP form is a Conflicts, and is implicitly converted to
// dependency form internally.

namespace
{
  dummy_universe_ref::package parse_package(istream &f,
					    dummy_universe_ref universe)
  {
    f >> ws;
    if(f.eof())
      throw ParseError("Expected package name, got EOF");

    string package;

    f >> package >> ws;

    return universe.find_package(package);
  }

  dummy_universe_ref::version parse_version(istream &f,
					    dummy_universe_ref universe)
  {
    dummy_universe_ref::package package = parse_package(f, universe);

    f >> ws;

    if(f.eof())
      throw ParseError("Expected version name, got EOF.");

    string version;
    f >> version >> ws;

    return package.version_from_name(version);
  }

  void parse_expected(istream &f, const std::string &expected)
  {
    f >> ws;

    if(f.eof())
      throw ParseError(cw::util::ssprintf("Expected %s, got EOF.", expected.c_str()));

    string s;
    f >> s >> ws;

    if(s != expected)
      throw ParseError(cw::util::ssprintf("Expected %s, got %s.",
					  expected.c_str(),
					  s.c_str()));
  }

  imm::set<dummy_universe_ref::version> parse_version_set_tail(istream &f,
							       dummy_universe_ref universe)
  {
    imm::set<dummy_universe_ref::version> rval;
    f >> ws;
    while(f.peek() != '}')
      {
	rval.insert(parse_version(f, universe));

	f >> ws;
      }

    parse_expected(f, "}");

    return rval;
  }
}

/** Reads the list of scores into the resolver. */
void read_scores(istream &f,
		 dummy_universe_ref universe, dummy_resolver &resolver)
{
  if(!universe)
    throw ParseError("Internal error: NULL universe in read_scores");

  f >> ws;

  while(f)
    {
      f >> ws;

      if(f.eof())
	throw ParseError("Expected '}' or SCORE, got EOF");

      string s;

      f >> s >> ws;

      if(s == "}")
	return;
      else if(s == "SCORE")
	{
	  if(f.eof())
	    throw ParseError("Expected package name following SCORE, got "+s);

	  string pkgname;

	  f >> pkgname >> ws;

	  if(pkgname == "{")
	    {
	      imm::set<dummy_universe_ref::version> joint_set = parse_version_set_tail(f, universe);

	      int score;
	      f >> ws;

	      if(f.eof())
		throw ParseError("Expected score, got EOF");

	      f >> score >> ws;

	      if(!f)
		throw ParseError("Error parsing the score of a joint score set.");

	      resolver.add_joint_score(joint_set, score);
	    }
	  else
	    {
	      dummy_universe::package pkg=universe.find_package(pkgname);

	      if(f.eof())
		throw ParseError("Expected '<' following package name, got EOF");

	      f >> s >> ws;

	      if(s != "<")
		throw ParseError("Expected '<' following package name, got "+s);

	      if(f.eof())
		throw ParseError("Expected '>' or version name, got EOF");

	      while(f)
		{
		  string vername;
		  int score;

		  f >> vername >> ws;

		  if(vername == ">")
		    break;

		  dummy_universe::version ver=pkg.version_from_name(vername);

		  if(f.eof())
		    throw ParseError("Expected score, got EOF");

		  f >> score >> ws;

		  if(f.eof())
		    throw ParseError("Expected '>' or version name, got EOF");

		  if(!f)
		    throw ParseError("Error reading score of " + pkgname + " " + vername);

		  resolver.set_version_score(ver, score);
		}
	    }
	}
      else
	throw ParseError("Expected 'SCORE' or '}', got "+s);
    }

  throw ParseError("Unexpected error reading score list.");
}

/** Reads the tail of a non-ANY SOLN form. */
map<dummy_universe::package, dummy_resolver::version> read_solution(istream &f, dummy_universe_ref universe)
{
  if(!universe)
    throw ParseError("Internal error: NULL universe passed to read_solution");

  map<dummy_universe::package, dummy_resolver::version> rval;

  f >> ws;

  while(f)
    {
      string s;

      if(f.eof())
	throw ParseError("Expected '>' or package, got EOF");

      f >> s >> ws;

      if(s == ">")
	return rval;
      else
	{
	  if(f.eof())
	    throw ParseError("Expected version, got EOF");

	  dummy_universe::package pkg=universe.find_package(s);

	  f >> s >> ws;

	  if(f.eof())
	    throw ParseError("Expected '>' or package name, got EOF");

	  if(s == ">")
	    throw ParseError("Expected version name, got '>'");

	  dummy_universe::version ver=pkg.version_from_name(s);

	  if(rval.find(pkg) != rval.end())
	    throw ParseError("Package "+pkg.get_name()+" bound twice in solution");

	  rval[pkg]=ver;
	}
    }

  throw ParseError("Unexpected error reading solution list");
}

int parse_int(const std::string &s)
{
  if(s.empty())
    throw ParseError("Can't parse an integer from an empty string.");

  char *endptr;
  long int rval = strtol(s.c_str(), &endptr, 0);

  if(*endptr != '\0')
    throw ParseError("Error in integer constant \"" + s + "\"");

  return rval;
}

void run_test_file(istream &f, bool show_world)
{
  dummy_universe_ref universe=NULL;

  f >> ws;

  while(f)
    {
      string s;

      if(f.eof())
	// This is the only place where EOF is valid.
	return;

      f >> s >> ws;

      if(s == "UNIVERSE")
	{
	  if(f.eof())
	    throw ParseError("Expected '[' following UNIVERSE, got EOF.");

	  f >> s >> ws;

	  if(s != "[")
	    throw ParseError("Expected '[' following UNIVERSE, got " + s);

	  universe=parse_universe_tail(f);

	  sanity_check_universe(universe);

	  if(show_world)
	    {
	      cout << "Input universe:" << endl;
	      dump_universe(universe, cout);
	    }
	}
      else if(s == "TEST")
	{
	  if(!universe)
	    throw ParseError("Expected UNIVERSE before TEST");

	  if(f.eof())
	    throw ParseError("Expected step_score and broken_score following 'TEST', got EOF");

	  int step_score;
	  int broken_score;
	  int unfixed_soft_score;
	  int infinity;
	  int goal_score = 50;
	  int future_horizon = 0;
	  std::string goal_score_or_brace;

	  f >> step_score >> broken_score >> unfixed_soft_score >> infinity >> ws >> goal_score_or_brace >> ws;

	  if(f.eof())
	    throw ParseError("Expected '{' following broken_score, got EOF");

	  if(!f)
	    throw ParseError("Error reading step_score, broken_score, unfixed_soft_score, infinity, max_succ, and goal_score after 'TEST'");

	  if(goal_score_or_brace != "{")
	    {
	      goal_score = parse_int(goal_score_or_brace);

	      std::string future_horizon_or_brace;
	      f >> future_horizon_or_brace >> ws;

	      if(future_horizon_or_brace != "{")
		{
		  future_horizon = parse_int(future_horizon_or_brace);

		  f >> s >> ws;
		}
	      else
		s = future_horizon_or_brace;
	    }
	  else
	    s = goal_score;

	  if(s != "{")
	    throw ParseError("Expected '{' following TEST, got "+s);

	  dummy_resolver resolver(step_score, broken_score,
				  unfixed_soft_score,
				  infinity,
				  goal_score,
                                  // TODO: add the broken soft dep
                                  // cost to the test input language.
                                  cost_limits::minimum_cost,
				  future_horizon,
				  imm::map<dummy_universe::package, dummy_universe::version>(),
				  universe);

	  resolver.set_debug(true);

	  read_scores(f, universe, resolver);

	  if(f.eof())
	    throw ParseError("Expected 'EXPECT', got EOF");

	  f >> s >> ws;

	  if(s != "EXPECT")
	    throw ParseError("Expected 'EXPECT', got "+s);

	  if(f.eof())
	    throw ParseError("Expected '(' following EXPECT, got EOF");

	  f >> s >> ws;

	  if(s != "(")
	    throw ParseError("Expected '(' following EXPECT, got "+s);

	  while(f)
	    {
	      if(f.eof())
		throw ParseError("Expected ')' or package name, got EOF");

	      f >> s >> ws;

	      if(s == ")")
		break;

	      int step_count=atoi(s.c_str());

	      if(step_count<=0)
		throw ParseError("step_count must be a positive integer, not "+s);

	      f >> s >> ws;

	      if(s == "ANY")
		{
		  try
		    {
		      dummy_resolver::solution next_soln = resolver.find_next_solution(step_count, NULL);

		      cout << "Next solution is ";
		      next_soln.dump(cout);

		      cout << " (ignored)" << endl;
		    }
		  catch(NoMoreTime)
		    {
		      cout << "Ran out of steps (ignored)" << endl;
		    }
		  catch(NoMoreSolutions)
		    {
		      cout << "Ran out of solutions (ignored)" << endl;
		    }
		}
	      else if(s == "<")
		{
		  try
		    {
		      map<dummy_universe::package, dummy_resolver::version> expected=read_solution(f, universe);

		      dummy_resolver::solution next_soln=resolver.find_next_solution(step_count, NULL);


		      cout << "Next solution is ";
		      next_soln.dump(cout);

		      bool equal=true;

		      // Compare the solution and the expected set by
		      // checking their value at each package.
		      // Slightly lame but correct and easier than
		      // writing a proper cross-compare.
		      for(dummy_universe::package_iterator pi = universe.packages_begin();
			  equal && !pi.end(); ++pi)
			{
			  std::map<dummy_universe::package, dummy_universe::version>::const_iterator expect_found
			    = expected.find(*pi);
			  dummy_resolver::version soln_version;
			  bool soln_touches_package = next_soln.get_choices().get_version_of(*pi, soln_version);

			  if(expect_found != expected.end())
			    {
			      if(!soln_touches_package || soln_version != expect_found->second)
				equal = false;
			    }
			  else if(soln_touches_package)
			    equal = false;
			}

		      if(equal)
			cout << " (OK)" << endl;
		      else
			{
			  cout << " (FAILED)" << endl;
			  cout << "Expected <";
			  for(map<dummy_universe::package, dummy_universe::version>::const_iterator i=expected.begin();
			      i!=expected.end(); ++i)
			    cout << i->first.get_name()
				 << ":="
				 << i->second.get_name();
			  cout << ">" << endl;
			}
		    }
		  catch(NoMoreSolutions)
		    {
		      cout << "Ran out of solutions (FAILED)" << endl;
		    }
		  catch(NoMoreTime)
		    {
		      cout << "Ran out of time (FAILED)" << endl;
		    }
		}
	      else
		throw ParseError("Expected ANY or '<', got "+s);
	    }
	}
      else
	throw ParseError("Expected UNIVERSE or TEST, got "+s);
    }
}

int main(int argc, char **argv)
{
  int rval=0;
  bool show_world=false;

  for(int i=1; i<argc; ++i)
    {
      // lame man's command line
      if(!strcmp(argv[i], "--dump"))
        {
	  show_world=true;
          continue;
        }

      ifstream f(argv[i]);

      if(!f)
	{
	  cerr << "Couldn't read from file " << argv[i] << "." << endl;
	  rval=-1;
	}

      try
	{
	  f >> ws;
	  run_test_file(f, show_world);
	}
      catch(const cwidget::util::Exception &e)
	{
	  cerr << "Error reading " << argv[i] << ": " << e.errmsg() << endl;
	  rval=-1;
	}
    }

  return rval;
}

