// test_resolver.cc                       -*-c++-*-
//
//   Copyright (C) 2005, 2007-2011 Daniel Burrows
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
// A test of the generic resolver layer.

#include <generic/problemresolver/dummy_universe.h>
#include <generic/problemresolver/problemresolver.h>
#include <generic/problemresolver/cost_limits.h>
#include <generic/problemresolver/cost.h>

#include <cppunit/extensions/HelperMacros.h>

#include <sstream>

#include <boost/lexical_cast.hpp>

using namespace std;

typedef generic_solution<dummy_universe_ref> dummy_solution;

const char *dummy_universe_1 = "\
UNIVERSE [ \
  PACKAGE a < v1 v2 v3 > v1 \
  PACKAGE b < v1 v2 v3 > v1 \
  PACKAGE c < v1 v2 v3 > v1 \
\
  DEP a v1 -> < b v2 > \
  DEP b v2 -> < c v2 > \
\
  DEP a v2 -> < > \
  DEP a v3 -> < > \
]";

const char *dummy_universe_2 = "\
UNIVERSE [ \
  PACKAGE a < v1 v2 > v1 \
  PACKAGE b < v1 v2 > v1 \
  PACKAGE c < v1 v2 > v1 \
\
  DEP a v1 -> < b v2 > \
  DEP a v1 -> < c v2 > \
]";

// Has only one solution, and that requires breaking a soft
// dependency.  Used to check that breaking soft dependencies works
// and that the resulting score is correct.
const char *dummy_universe_3 = "\
UNIVERSE [ \
  PACKAGE a < v1 v2 v3 > v1 \
  PACKAGE b < v1 v2 v3 > v1 \
  PACKAGE c < v1 v2 v3 > v1 \
\
  DEP a v1 -> < b v2 > \
  DEP b v2 -> < c v2 > \
  SOFTDEP c v2 -> < a v2  a v3 > \
\
  DEP a v2 -> < > \
  DEP a v3 -> < > \
]";

// Used to test mandating a single version.
const char *dummy_universe_4 = "\
UNIVERSE [ \
  PACKAGE a < v1 v2 v3 > v1 \
  PACKAGE b < v1 v2 v3 > v1 \
  PACKAGE c < v1 > v1 \
\
  SOFTDEP a v2 -> < b v2  b v3 > \
  DEP c v1 -> < a v2 > \
]";

// Similar -- the non-soft dependency is needed because altering the
// source is not an option for a soft dependency.
const char *dummy_universe_4_not_soft = "\
UNIVERSE [ \
  PACKAGE a < v1 v2 v3 > v1 \
  PACKAGE b < v1 v2 v3 > v1 \
\
  DEP a v1 -> < b v2  b v3 > \
]";

// Used to test breaking and hardening a soft dependency, as well as
// assigning costs to broken soft-deps.
const char *dummy_universe_5 = "\
UNIVERSE [ \
  PACKAGE a < v1 v2 > v1 \
  PACKAGE b < v1 v2 v3 > v1 \
  PACKAGE c < v1 > v1 \
\
  SOFTDEP a v2 -> < b v2 > \
  DEP c v1 -> < a v2 > \
]";

// Check that the resolver doesn't wander off and try to fix soft
// dependencies that aren't broken to start with.  The "-?>" tells the
// resolver to not place the dependency in the initial set.
const char *dummy_universe_6 = "\
UNIVERSE [ \
  PACKAGE a < v1 > v1 \
  PACKAGE b < v1 v2 v3 > v1 \
  PACKAGE c < v1 v2 > v1 \
\
  DEP a v1 -> < c v2 > \
  SOFTDEP a v1 -?> < b v2  b v3 > \
]";

// Done this way so meaningful line numbers are generated.
#define assertEqEquivalent(x1, x2) \
  do {									\
    std::stringstream ____x1_ss, ____x2_ss;				\
    ____x1_ss << (x1); ____x2_ss << (x2);				\
    std::string ____x1_s(____x1_ss.str()), ____x2_s(____x2_ss.str());	\
    CPPUNIT_ASSERT_MESSAGE(____x1_s + " does not equal " + ____x2_s,	\
			   (x1) == (x2));				\
    CPPUNIT_ASSERT_MESSAGE(____x2_s + " does not equal " + ____x1_s,	\
			   (x2) == (x1));				\
    CPPUNIT_ASSERT_MESSAGE(____x1_s + " is inequal to " + ____x2_s,	\
			   !((x1) != (x2)));				\
    CPPUNIT_ASSERT_MESSAGE(____x2_s + " is inequal to " + ____x1_s,	\
			   !((x2) != (x1)));				\
    CPPUNIT_ASSERT_MESSAGE(____x1_s + " is less than " + ____x2_s,	\
			   !((x1) < (x2)));				\
    CPPUNIT_ASSERT_MESSAGE(____x2_s + " is less than " + ____x1_s,	\
			   !((x2) < (x1)));				\
  } while(0)

#define assertEqInequivalent(x1, x2) \
  do {									\
    std::stringstream ____x1_ss, ____x2_ss;				\
    ____x1_ss << (x1); ____x2_ss << (x2);				\
    std::string ____x1_s(____x1_ss.str()), ____x2_s(____x2_ss.str());	\
    CPPUNIT_ASSERT_MESSAGE(____x1_s + " is not inequal to " + ____x2_s,		\
			   (x1) != (x2));				\
    CPPUNIT_ASSERT_MESSAGE(____x2_s + " is not inequal to " + ____x1_s,		\
			   (x2) != (x1));				\
    CPPUNIT_ASSERT_MESSAGE(____x1_s + " equals " + ____x2_s,	\
			   !((x1) == (x2)));				\
    CPPUNIT_ASSERT_MESSAGE(____x2_s + " equals " + ____x1_s,	\
			   !((x2) == (x1)));				\
    CPPUNIT_ASSERT_MESSAGE(____x1_s + " is unrelated to " + ____x2_s + " under <", \
			   (x1) < (x2) || (x2) < (x1));			\
    CPPUNIT_ASSERT_MESSAGE(____x1_s + " is both less than and greater than " + ____x2_s, \
			   !((x1) < (x2) && (x2) < (x1)));		\
  } while(0)

#define assertLtEquivalent(x1, x2, lt)					\
  do {									\
    std::stringstream ____x1_ss, ____x2_ss;				\
    ____x1_ss << (x1); ____x2_ss << (x2);				\
    std::string ____x1_s(____x1_ss.str()), ____x2_s(____x2_ss.str());	\
    CPPUNIT_ASSERT_MESSAGE(____x1_s + " is less than " + ____x2_s,	\
			   !(lt((x1), (x2))));				\
    CPPUNIT_ASSERT_MESSAGE(____x2_s + " is less than " + ____x1_s,	\
			   !(lt((x2), (x1))));				\
  } while(0)

#define assertLtInequivalent(x1, x2, lt) \
  do {									\
    std::stringstream ____x1_ss, ____x2_ss;				\
    ____x1_ss << (x1); ____x2_ss << (x2);				\
    std::string ____x1_s(____x1_ss.str()), ____x2_s(____x2_ss.str());	\
    CPPUNIT_ASSERT_MESSAGE(____x1_s + " is not related under < to " + ____x2_s, \
			   lt((x1), (x2)) || lt((x2), (x1)));		\
    CPPUNIT_ASSERT_MESSAGE(____x1_s + " is both less than and greater than " + ____x2_s, \
			   !(lt((x1), (x2)) && lt((x2), (x1))));	\
  } while(0)

#define ASSERT_ABOVE_OR_EQUAL( e1, e2 )		\
  CPPUNIT_ASSERT_MESSAGE( boost::lexical_cast<std::string>(e1) + " should be above or equal to " + boost::lexical_cast<std::string>(e2), \
			  (e1).is_above_or_equal(e2) );

#define ASSERT_NOT_ABOVE_OR_EQUAL( e1, e2 )		\
  CPPUNIT_ASSERT_MESSAGE( boost::lexical_cast<std::string>(e1) + " should not be above or equal to " + boost::lexical_cast<std::string>(e2), \
			  !((e1).is_above_or_equal(e2)) );

class ResolverTest : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(ResolverTest);

  CPPUNIT_TEST(testRejections);
  CPPUNIT_TEST(testMandateDepTarget);
  CPPUNIT_TEST(testMandateDepSource);
  CPPUNIT_TEST(testHardenDependency);
  CPPUNIT_TEST(testApproveBreak);
  CPPUNIT_TEST(testInitialSetExclusion);
  CPPUNIT_TEST(testSimpleResolution);
  CPPUNIT_TEST(testSimpleBreakSoftDep);
  CPPUNIT_TEST(testCostEffects);
  CPPUNIT_TEST(testCostOperations);
  CPPUNIT_TEST(testInitialState);
  CPPUNIT_TEST(testJointScores);
  CPPUNIT_TEST(testDropSolutionSupersets);
  CPPUNIT_TEST(testBreakSoftDepCost);

  CPPUNIT_TEST_SUITE_END();

private:
  typedef dummy_universe_ref::package package;
  typedef dummy_universe_ref::version version;
  typedef dummy_universe_ref::dep dep;

  typedef generic_solution<dummy_universe_ref> solution;
  typedef generic_choice_set<dummy_universe_ref> choice_set;
  typedef generic_choice<dummy_universe_ref> choice;

  static dummy_universe_ref parseUniverse(const std::string &s)
  {
    std::istringstream in(s);

    return parse_universe(in);
  }

  /** \brief Handle finding all the solutions a resolver can produce.
   *
   *  \throws NoMoreTime if the resolver runs out of time trying to
   *                     find a solution.  Already-discovered
   *                     solutions are available in "output".
   */
  static void find_all_solutions(dummy_resolver &r,
                                 int num_steps,
                                 std::set<package> *visited_packages,
                                 std::vector<solution> &output)
  {
    while(1)
      {
        try
          {
            output.push_back(r.find_next_solution(num_steps, visited_packages));
          }
        catch(NoMoreSolutions)
          {
            break;
          }
      }
  }

  // Check that the first set is a (perhaps) more general version of
  // the second, but that there are no missing items.
  static void assertSameEffect(const choice_set &s1, const choice_set &s2)
  {
    std::ostringstream tmp1, tmp2;
    tmp1 << s1;
    tmp2 << s2;
    std::string str1(tmp1.str()), str2(tmp2.str());

    CPPUNIT_ASSERT_EQUAL_MESSAGE("The sets " + str1 + " and " + str2 + " are not the same size.",
				 s1.size(), s2.size());

    CPPUNIT_ASSERT_MESSAGE("The set " + str1 + " does not contain " + str2 + ".",
			   s1.contains(s2));
  }

#if 0
  // Old routines that need to be adapted to the new resolver code.

  /** Generate a successor solution that just contains the given
   *  information by using internal constructors of the solution
   *  class.  Used when testing the solution class itself.
   */
  template<class c_iter>
  dummy_solution unsafe_successor(const dummy_solution &parent,
				  c_iter cbegin, c_iter cend)
  {
    choice_set choices(parent.get_choices());

    for(c_iter ci = cbegin; ci != cend; ++ci)
      choices.insert_or_narrow(*ci);

    return dummy_solution(new dummy_solution::solution_rep(choices,
							   parent.get_broken(),
							   parent.get_forbidden_versions(),
							   parent.get_initial_state(),
							   parent.get_score(),
							   parent.get_action_score(),
							   parent.get_cost()));
  }

  /** Tests that the comparison operations on solutions work. */
  void testSolutionCompare()
  {
    dummy_universe_ref u = parseUniverse(dummy_universe_1);

    // Grab two arbitrary deps.
    dummy_universe::dep_iterator di = u.deps_begin();
    CPPUNIT_ASSERT(!di.end());
    dummy_universe::dep d1 = *di;
    ++di;
    CPPUNIT_ASSERT(!di.end());
    dummy_universe::dep d2 = *di;

    resolver_initial_state<dummy_universe_ref> initial_state(imm::map<dummy_universe::package, dummy_universe::version>(), u.get_package_count());

    solution_weights<dummy_universe_ref> weights(0, 0, 0, 0, u.get_version_count(),
						 initial_state);

    imm::set<dummy_universe::dep> u_broken;
    for(dummy_universe::broken_dep_iterator bi = u.broken_begin();
	!bi.end(); ++bi)
      u_broken.insert(*bi);

    choice c1(choice::make_install_version(u.find_package("a").version_from_name("v1"),
					   false, d1, 49));
    choice c2(choice::make_install_version(u.find_package("a").version_from_name("v1"),
					   false, d2, 21));
    choice c3(choice::make_install_version(u.find_package("a").version_from_name("v2"),
					   false, d1, 49));

    choice c4(choice::make_install_version(u.find_package("c").version_from_name("v3"),
					   false, d2, 21));

    choice c5(choice::make_break_soft_dep(d1, 443));


    // Generate some meaningless solutions to check that equivalency
    // is correctly calculated according to the version mappings and
    // the set of unsolved soft deps.
    dummy_solution s0 = dummy_solution::root_node(u_broken,
						  u, weights, initial_state,
						  cost());
    dummy_solution s1
      = unsafe_successor(s0, &c1, &c1 + 1);
    dummy_solution s2
      = unsafe_successor(s0, &c2, &c2 + 1);
    dummy_solution s3
      = unsafe_successor(s0, &c3, &c3 + 1);
    dummy_solution s4
      = unsafe_successor(s0, &c4, &c4 + 1);

    // the following two should be equal.
    dummy_solution s5
      = unsafe_successor(s1, &c4, &c4 + 1);
    dummy_solution s6
      = unsafe_successor(s4, &c1, &c1 + 1);

    // and this should not equal any other solution.
    dummy_solution s7
      = unsafe_successor(s0, &c5, &c5+1);

    dummy_resolver::solution_contents_compare solcmp;

    assertLtEquivalent(s0, s0, solcmp);
    assertLtInequivalent(s0, s1, solcmp);
    assertLtInequivalent(s0, s2, solcmp);
    assertLtInequivalent(s0, s3, solcmp);
    assertLtInequivalent(s0, s4, solcmp);
    assertLtInequivalent(s0, s5, solcmp);
    assertLtInequivalent(s0, s6, solcmp);
    assertLtInequivalent(s0, s7, solcmp);

    assertLtEquivalent(s1, s1, solcmp);
    assertLtInequivalent(s1, s2, solcmp);
    assertLtInequivalent(s1, s3, solcmp);
    assertLtInequivalent(s1, s4, solcmp);
    assertLtInequivalent(s1, s5, solcmp);
    assertLtInequivalent(s1, s6, solcmp);
    assertLtInequivalent(s1, s7, solcmp);

    assertLtEquivalent(s2, s2, solcmp);
    assertLtInequivalent(s2, s3, solcmp);
    assertLtInequivalent(s2, s4, solcmp);
    assertLtInequivalent(s2, s5, solcmp);
    assertLtInequivalent(s2, s6, solcmp);
    assertLtInequivalent(s2, s7, solcmp);

    assertLtEquivalent(s3, s3, solcmp);
    assertLtInequivalent(s3, s4, solcmp);
    assertLtInequivalent(s3, s5, solcmp);
    assertLtInequivalent(s3, s6, solcmp);
    assertLtInequivalent(s3, s7, solcmp);

    assertLtEquivalent(s4, s4, solcmp);
    assertLtInequivalent(s4, s5, solcmp);
    assertLtInequivalent(s4, s6, solcmp);
    assertLtInequivalent(s4, s7, solcmp);

    assertLtEquivalent(s5, s5, solcmp);
    assertLtEquivalent(s5, s6, solcmp);
    assertLtInequivalent(s5, s7, solcmp);

    assertLtEquivalent(s6, s6, solcmp);
    assertLtInequivalent(s6, s7, solcmp);

    assertLtEquivalent(s7, s7, solcmp);
  }
#endif

  // Check that rejections of versions don't block out all versions of
  // the package (this actually happened once).  As installing version
  // 2 of everything is a solution, rejecting the third versions
  // should be OK.
  void testRejections()
  {
    dummy_universe_ref u = parseUniverse(dummy_universe_1);
    dummy_resolver r(10, -300, -100, 100000, 50000,
                     cost_limits::minimum_cost,
                     50,
		     imm::map<dummy_universe::package, dummy_universe::version>(),
		     u);

    r.reject_version(u.find_package("a").version_from_name("v3"));
    r.reject_version(u.find_package("b").version_from_name("v3"));
    r.reject_version(u.find_package("c").version_from_name("v3"));

    try
      {
	r.find_next_solution(1000000, NULL);
      }
    catch(NoMoreSolutions)
      {
	CPPUNIT_FAIL("Expected at least one solution, got none.");
      }
  }

  // Check that mandating a version in a dep target forces that
  // version to be chosen.
  void testMandateDepTarget()
  {
    logging::LoggerPtr logger(logging::Logger::getLogger("test.resolver.testMandateDepTarget"));

    LOG_TRACE(logger, "Entering testMandateDepTarget.");

    dummy_universe_ref u = parseUniverse(dummy_universe_4);
    dummy_resolver r(10, -300, -100, 100000, 50000,
                     cost_limits::minimum_cost,
                     50,
		     imm::map<dummy_universe::package, dummy_universe::version>(),
		     u);

    package a = u.find_package("a");
    package b = u.find_package("b");

    r.mandate_version(b.version_from_name("v3"));
    solution sol;
    try
      {
	sol = r.find_next_solution(1000, NULL);
	LOG_TRACE(logger, "Got first solution: " << sol);
      }
    catch(NoMoreSolutions)
      {
	LOG_ERROR(logger, "Expected exactly one solution, got none.");
	CPPUNIT_FAIL("Expected exactly one solution, got none.");
      }

    choice_set expected_solution;
    expected_solution.insert_or_narrow(choice::make_install_version(b.version_from_name("v3"), 0));
    expected_solution.insert_or_narrow(choice::make_install_version(a.version_from_name("v2"), 1));
    assertSameEffect(expected_solution, sol.get_choices());

    try
      {
	sol = r.find_next_solution(1000, NULL);
      }
    catch(NoMoreSolutions)
      {
	LOG_TRACE(logger, "Success: only one solution was found.");

	r.mandate_version(b.version_from_name("v2"));

	try
	  {
	    sol = r.find_next_solution(1000, NULL);
	    LOG_TRACE(logger, "Got another solution after a second mandate: " << sol);

	    choice_set expected_solution2;
	    expected_solution2.insert_or_narrow(choice::make_install_version(b.version_from_name("v2"), 0));
	    expected_solution2.insert_or_narrow(choice::make_install_version(a.version_from_name("v2"), 1));
	    assertSameEffect(expected_solution2, sol.get_choices());
	  }
	catch(NoMoreSolutions)
	  {
	    LOG_ERROR(logger, "Mandating a second solution didn't cancel the first mandate.");
	    CPPUNIT_FAIL("Mandating a second solution didn't cancel the first mandate.");
	  }

	try
	  {
	    sol = r.find_next_solution(1000, NULL);
	    LOG_ERROR(logger, "Got an extra solution: " << sol);
	    CPPUNIT_FAIL("Too many solutions after a solution was re-enabled because a solver was mandated.");
	  }
	catch(NoMoreSolutions)
	  {
	    try
	      {
		dep av2d1 = *a.version_from_name("v2").deps_begin();
		r.harden(av2d1);
		r.unharden(av2d1);

		sol = r.find_next_solution(1000, NULL);
		LOG_ERROR(logger, "Got an extra solution: " << sol);
		CPPUNIT_FAIL("Too many solutions after a solution was re-enabled because a solver was mandated, and after breaking the dep was rejected and unrejected.");
	      }
	    catch(NoMoreSolutions)
	      {
		return;
	      }
	  }
      }

    LOG_ERROR(logger, "Found an unexpected solution: " << sol);
    CPPUNIT_FAIL("Expected exactly one solution, got two.");
  }

  // Check that mandating an alternate version of a dep source forces
  // that version to be chosen.
  void testMandateDepSource()
  {
    logging::LoggerPtr logger(logging::Logger::getLogger("test.resolver.testMandateDepSource"));

    LOG_TRACE(logger, "Entering testMandateDepSource.");

    dummy_universe_ref u = parseUniverse(dummy_universe_4_not_soft);
    dummy_resolver r(10, -300, -100, 100000, 50000,
                     cost_limits::minimum_cost,
                     50,
		     imm::map<dummy_universe::package, dummy_universe::version>(),
		     u);

    package a = u.find_package("a");
    package b = u.find_package("b");

    r.mandate_version(a.version_from_name("v3"));
    solution sol;
    try
      {
	sol = r.find_next_solution(1000, NULL);
	LOG_TRACE(logger, "Got first solution: " << sol);
      }
    catch(NoMoreSolutions)
      {
	LOG_ERROR(logger, "Expected exactly one solution, got none.");
	CPPUNIT_FAIL("Expected exactly one solution, got none.");
      }

    choice_set expected_solution;
    expected_solution.insert_or_narrow(choice::make_install_version(a.version_from_name("v3"), 0));
    assertSameEffect(expected_solution, sol.get_choices());

    try
      {
	sol = r.find_next_solution(1000, NULL);
      }
    catch(NoMoreSolutions)
      {
	LOG_TRACE(logger, "Success: only one solution was found.");


	r.mandate_version(b.version_from_name("v2"));

	try
	  {
	    sol = r.find_next_solution(1000, NULL);
	    LOG_TRACE(logger, "Got another solution after a second mandate: " << sol);

	    choice_set expected_solution2;
	    expected_solution2.insert_or_narrow(choice::make_install_version(b.version_from_name("v2"), 0));
	    assertSameEffect(expected_solution2, sol.get_choices());
	  }
	catch(NoMoreSolutions)
	  {
	    LOG_ERROR(logger, "Mandating a second solution didn't cancel the first mandate.");
	    CPPUNIT_FAIL("Mandating a second solution didn't cancel the first mandate.");
	  }

	try
	  {
	    sol = r.find_next_solution(1000, NULL);
	    LOG_ERROR(logger, "Got an extra solution: " << sol);
	    CPPUNIT_FAIL("Too many solutions after a solution was re-enabled because a solver was mandated.");
	  }
	catch(NoMoreSolutions)
	  {
	    try
	      {
		version bv3(b.version_from_name("v3"));
		r.reject_version(bv3);
		r.unreject_version(bv3);

		sol = r.find_next_solution(1000, NULL);
		LOG_ERROR(logger, "Got an extra solution: " << sol);
		CPPUNIT_FAIL("Too many solutions after a solution was re-enabled because a solver was mandated, and after a version was rejected and unrejected.");
	      }
	    catch(NoMoreSolutions)
	      {
		return;
	      }
	  }
      }

    LOG_ERROR(logger, "Found an unexpected solution: " << sol);
    CPPUNIT_FAIL("Expected exactly one solution, got two.");
  }

  // Check that hardening a soft dependency forces it to be solved.
  void testHardenDependency()
  {
    logging::LoggerPtr logger(logging::Logger::getLogger("test.resolver.testHardenDependency"));

    LOG_TRACE(logger, "Entering testHardenDependency.");

    dummy_universe_ref u = parseUniverse(dummy_universe_5);
    dummy_resolver r(10, -300, -100, 100000, 50000,
                     cost_limits::minimum_cost,
                     50,
		     imm::map<dummy_universe::package, dummy_universe::version>(),
		     u);

    package a = u.find_package("a");
    package b = u.find_package("b");
    dep av2d1(*a.version_from_name("v2").deps_begin());

    r.harden(av2d1);
    solution sol;
    try
      {
	sol = r.find_next_solution(1000, NULL);
	LOG_TRACE(logger, "Got first solution: " << sol);
      }
    catch(NoMoreSolutions)
      {
	LOG_ERROR(logger, "Expected exactly one solution, got none.");
	CPPUNIT_FAIL("Expected exactly one solution, got none.");
      }

    choice_set expected_solution;
    expected_solution.insert_or_narrow(choice::make_install_version(a.version_from_name("v2"), 0));
    expected_solution.insert_or_narrow(choice::make_install_version(b.version_from_name("v2"), 1));
    assertSameEffect(expected_solution, sol.get_choices());

    try
      {
	sol = r.find_next_solution(1000, NULL);
      }
    catch(NoMoreSolutions)
      {
	LOG_TRACE(logger, "Success: only one solution was found.");
	return;
      }

    LOG_ERROR(logger, "Found an unexpected solution: " << sol);
    CPPUNIT_FAIL("Expected exactly one solution, got two.");
  }

  // Check that breaking a soft dependency forces it to be left
  // unsolved.
  void testApproveBreak()
  {
    logging::LoggerPtr logger(logging::Logger::getLogger("test.resolver.testApproveBreak"));

    LOG_TRACE(logger, "Entering testApproveBreak.");

    dummy_universe_ref u = parseUniverse(dummy_universe_5);
    dummy_resolver r(10, -300, -100, 100000, 50000,
                     cost_limits::minimum_cost,
                     50,
		     imm::map<dummy_universe::package, dummy_universe::version>(),
		     u);

    package a = u.find_package("a");
    dep av2d1(*a.version_from_name("v2").deps_begin());

    r.approve_break(av2d1);
    solution sol;
    try
      {
	sol = r.find_next_solution(1000, NULL);
	LOG_TRACE(logger, "Got first solution: " << sol);
      }
    catch(NoMoreSolutions)
      {
	LOG_ERROR(logger, "Expected exactly one solution, got none.");
	CPPUNIT_FAIL("Expected exactly one solution, got none.");
      }

    choice_set expected_solution;
    expected_solution.insert_or_narrow(choice::make_break_soft_dep(av2d1, 0));
    expected_solution.insert_or_narrow(choice::make_install_version(a.version_from_name("v2"), 1));
    assertSameEffect(expected_solution, sol.get_choices());

    try
      {
	sol = r.find_next_solution(1000, NULL);
      }
    catch(NoMoreSolutions)
      {
	LOG_TRACE(logger, "Success: only one solution was found.");
	return;
      }

    LOG_ERROR(logger, "Found an unexpected solution: " << sol);
    CPPUNIT_FAIL("Expected exactly one solution, got two.");
  }

  // Test that excluding dependencies from the set to solve works.
  void testInitialSetExclusion()
  {
    logging::LoggerPtr logger(logging::Logger::getLogger("test.resolver.testInitialSetExclusion"));

    LOG_TRACE(logger, "Entering testInitialSetExclusion.");

    dummy_universe_ref u = parseUniverse(dummy_universe_6);
    dummy_resolver r(10, -300, -100, 100000, 50000,
                     cost_limits::minimum_cost,
                     50,
		     imm::map<dummy_universe::package, dummy_universe::version>(),
		     u);

    package c = u.find_package("c");

    solution sol;
    try
      {
	sol = r.find_next_solution(1000, NULL);
	LOG_TRACE(logger, "Got first solution: " << sol);
      }
    catch(NoMoreSolutions)
      {
	LOG_ERROR(logger, "Expected exactly one solution, got none.");
	CPPUNIT_FAIL("Expected exactly one solution, got none.");
      }

    choice_set expected_solution;
    expected_solution.insert_or_narrow(choice::make_install_version(c.version_from_name("v2"), 1));
    assertSameEffect(expected_solution, sol.get_choices());

    try
      {
	sol = r.find_next_solution(1000, NULL);
      }
    catch(NoMoreSolutions)
      {
	LOG_TRACE(logger, "Success: only one solution was found.");
	return;
      }

    LOG_ERROR(logger, "Found an unexpected solution: " << sol);
    CPPUNIT_FAIL("Expected exactly one solution, got two.");
  }

  void testSimpleResolution()
  {
    logging::LoggerPtr logger(logging::Logger::getLogger("test.resolver.testSimpleResolution"));

    LOG_TRACE(logger, "Entering testSimpleResolution");

    // dummy_universe_1 has only one solution: installing bv2 and cv2.
    dummy_universe_ref u = parseUniverse(dummy_universe_1);

    package b = u.find_package("b");
    package c = u.find_package("c");
    version bv2 = b.version_from_name("v2");
    version cv2 = c.version_from_name("v2");

    choice_set expected_solution;
    expected_solution.insert_or_narrow(choice::make_install_version(bv2, 0));
    expected_solution.insert_or_narrow(choice::make_install_version(cv2, 0));
    LOG_TRACE(logger, "Expected solution: " << expected_solution);

    const int step_score = 10;
    const int unfixed_soft_score = -100;
    const int full_solution_score = 50000;
    dummy_resolver r(step_score, -300, unfixed_soft_score,
		     100000, full_solution_score,
                     cost_limits::minimum_cost,
                     50,
		     imm::map<dummy_universe::package, dummy_universe::version>(),
		     u);

    r.set_version_score(cv2, -1000);

    solution sol;
    try
      {
	sol = r.find_next_solution(50, NULL);
	LOG_TRACE(logger, "Got solution: " << sol << ".");
      }
    catch(NoMoreSolutions)
      {
	LOG_TRACE(logger, "Unable to find a solution.");
	CPPUNIT_FAIL("Unable to find a solution.");
      }
    catch(NoMoreTime)
      {
	LOG_TRACE(logger, "No more time to find a solution.");
	CPPUNIT_FAIL("No more time to find a solution.");
      }

    assertSameEffect(expected_solution, sol.get_choices());
    CPPUNIT_ASSERT_EQUAL(2 * step_score + full_solution_score - 1000,
			 sol.get_score());
  }

  void testSimpleBreakSoftDep()
  {
    logging::LoggerPtr logger(logging::Logger::getLogger("test.resolver.testSimpleBreakSoftDep"));

    LOG_TRACE(logger, "Entering testSimpleBreakSoftDep");

    // dummy_universe_1 has only one solution: installing bv2 and cv2,
    // and leaving the only dependency of cv2 broken.
    dummy_universe_ref u = parseUniverse(dummy_universe_3);

    package b = u.find_package("b");
    package c = u.find_package("c");
    version bv2 = b.version_from_name("v2");
    version cv2 = c.version_from_name("v2");
    dep cv2d1 = *cv2.deps_begin();

    choice_set expected_solution;
    expected_solution.insert_or_narrow(choice::make_install_version(bv2, 0));
    expected_solution.insert_or_narrow(choice::make_install_version(cv2, 0));
    expected_solution.insert_or_narrow(choice::make_break_soft_dep(cv2d1, 0));
    LOG_TRACE(logger, "Expected solution: " << expected_solution);

    const int step_score = 10;
    const int unfixed_soft_score = -100;
    const int full_solution_score = 50000;
    dummy_resolver r(step_score, -300, unfixed_soft_score,
		     100000, full_solution_score,
                     cost_limits::minimum_cost,
                     50,
		     imm::map<dummy_universe::package, dummy_universe::version>(),
		     u);

    solution sol;
    try
      {
	sol = r.find_next_solution(50, NULL);
	LOG_TRACE(logger, "Got solution: " << sol << ".");
      }
    catch(NoMoreSolutions)
      {
	LOG_TRACE(logger, "Unable to find a solution.");
	CPPUNIT_FAIL("Unable to find a solution.");
      }
    catch(NoMoreTime)
      {
	LOG_TRACE(logger, "No more time to find a solution.");
	CPPUNIT_FAIL("No more time to find a solution.");
      }

    assertSameEffect(expected_solution, sol.get_choices());
    CPPUNIT_ASSERT_EQUAL(sol.get_score(),
			 3 * step_score + unfixed_soft_score + full_solution_score);
  }

  void testCostEffects()
  {
    logging::LoggerPtr logger(logging::Logger::getLogger("test.resolver.testCostEffects"));
    LOG_TRACE(logger, "Entering testCostEffects");

    dummy_universe_ref u = parseUniverse(dummy_universe_2);

    package a = u.find_package("a");
    package b = u.find_package("b");
    package c = u.find_package("c");
    version av2 = a.version_from_name("v2");
    version bv2 = b.version_from_name("v2");
    version cv2 = c.version_from_name("v2");

    choice_set av1_choices;
    av1_choices.insert_or_narrow(choice::make_install_version(bv2, 0));
    av1_choices.insert_or_narrow(choice::make_install_version(cv2, 0));

    choice_set av2_choices;
    av2_choices.insert_or_narrow(choice::make_install_version(av2, 0));

    LOG_TRACE(logger, "Verifying that without a cost the shortest solution is produced first and there are two solutions.");

    // Verify that without a cost we get the shorter solution first.
    // Without this we aren't testing anything!
    {
      dummy_resolver r(10, -300, -100, 100000, 50000,
                       cost_limits::minimum_cost,
                       50,
		       imm::map<dummy_universe::package, dummy_universe::version>(),
		       u);
      r.set_version_score(av2, 10000);
      r.set_version_score(bv2, -100);
      r.set_version_score(cv2, -100);

      solution sol;
      try
	{
	  sol = r.find_next_solution(1000000, NULL);
	}
      catch(NoMoreSolutions)
	{
	  CPPUNIT_FAIL("Expected two solutions, got none.");
	}

      LOG_TRACE(logger, "Got first solution: " << sol);

      assertSameEffect(av2_choices, sol.get_choices());

      try
	{
	  sol = r.find_next_solution(1000000, NULL);
	}
      catch(NoMoreSolutions)
	{
	  CPPUNIT_FAIL("Expected two solutions, got only one.");
	}

      LOG_TRACE(logger, "Got second solution: " << sol);

      assertSameEffect(av1_choices, sol.get_choices());

      bool done = false;
      try
	{
	  sol = r.find_next_solution(1000000, NULL);
	}
      catch(NoMoreSolutions)
	{
	  done = true;
	}

      if(!done)
	LOG_ERROR(logger, "Got unexpected third solution: " << sol);

      CPPUNIT_ASSERT_MESSAGE("Expected two solutions, got more.", done);
    }

    LOG_TRACE(logger, "Checking that adjusting costs changes the output.");

    // Now check that adjusting costs changes the output.
    {
      dummy_resolver r(10, -300, -100, 100000, 50000,
                       cost_limits::minimum_cost,
                       50,
		       imm::map<dummy_universe::package, dummy_universe::version>(),
		       u);
      r.set_version_score(av2, 1000);
      r.set_version_score(bv2, -100);
      r.set_version_score(cv2, -100);
      r.set_version_cost(av2,
                         cost::make_advance_user_level(0, 100));

      solution sol;
      try
	{
	  sol = r.find_next_solution(1000000, NULL);
	}
      catch(NoMoreSolutions)
	{
	  CPPUNIT_FAIL("Expected two solutions, got none.");
	}

      LOG_TRACE(logger, "Got first solution: " << sol);

      assertSameEffect(av1_choices, sol.get_choices());

      try
	{
	  sol = r.find_next_solution(1000000, NULL);
	}
      catch(NoMoreSolutions)
	{
	  CPPUNIT_FAIL("Expected two solutions, got only one.");
	}

      LOG_TRACE(logger, "Got second solution: " << sol);

      assertSameEffect(av2_choices, sol.get_choices());

      bool done = false;
      try
	{
	  sol = r.find_next_solution(1000000, NULL);
	}
      catch(NoMoreSolutions)
	{
	  done = true;
	}

      if(!done)
	LOG_ERROR(logger, "Got an unexpected third solution: " << sol);

      CPPUNIT_ASSERT_MESSAGE("Expected two solutions, got more.", done);
    }
  }

  void doTestCostOperations()
  {
    logging::LoggerPtr logger(logging::Logger::getLogger("test.resolver.testCostOperations"));
    LOG_TRACE(logger, "Entering testCostOperations");

    LOG_TRACE(logger, "Testing basic above-or-equal relationships.");
    ASSERT_ABOVE_OR_EQUAL( cost::make_advance_structural_level(50),
			   cost_limits::minimum_cost );
    ASSERT_ABOVE_OR_EQUAL( cost::make_advance_structural_level(50),
			   cost::make_advance_structural_level(35) );
    ASSERT_ABOVE_OR_EQUAL( cost::make_advance_structural_level(50),
			   cost::make_advance_structural_level(50) );
    ASSERT_NOT_ABOVE_OR_EQUAL( cost::make_advance_structural_level(50),
			       cost::make_advance_structural_level(75) );

    ASSERT_ABOVE_OR_EQUAL( cost::make_advance_user_level(1, 50),
			   cost_limits::minimum_cost );
    ASSERT_ABOVE_OR_EQUAL( cost::make_advance_user_level(1, 50),
			   cost::make_advance_user_level(1, 35) );
    ASSERT_ABOVE_OR_EQUAL( cost::make_advance_user_level(1, 50),
			   cost::make_advance_user_level(1, 50) );
    ASSERT_NOT_ABOVE_OR_EQUAL( cost::make_advance_user_level(1, 50),
			       cost::make_advance_user_level(1, 75) );

    ASSERT_ABOVE_OR_EQUAL( cost::make_add_to_user_level(1, 50),
			   cost_limits::minimum_cost );
    ASSERT_ABOVE_OR_EQUAL( cost::make_add_to_user_level(1, 50),
			   cost::make_add_to_user_level(1, 35) );
    ASSERT_ABOVE_OR_EQUAL( cost::make_add_to_user_level(1, 50),
			   cost::make_add_to_user_level(1, 50) );
    ASSERT_NOT_ABOVE_OR_EQUAL( cost::make_add_to_user_level(1, 50),
			       cost::make_add_to_user_level(1, 75) );


    ASSERT_NOT_ABOVE_OR_EQUAL( cost::make_advance_structural_level(50),
			       cost::make_advance_user_level(1, 50) );
    ASSERT_NOT_ABOVE_OR_EQUAL( cost::make_advance_structural_level(50),
			       cost::make_add_to_user_level(1, 50) );
    ASSERT_NOT_ABOVE_OR_EQUAL( cost::make_advance_user_level(1, 50),
			       cost::make_add_to_user_level(1, 50) );

    ASSERT_NOT_ABOVE_OR_EQUAL( cost::make_advance_user_level(1, 50),
			       cost::make_advance_user_level(2, 50) );
    ASSERT_NOT_ABOVE_OR_EQUAL( cost::make_advance_user_level(1, 50),
			       cost::make_add_to_user_level(2, 50) );
    ASSERT_NOT_ABOVE_OR_EQUAL( cost::make_add_to_user_level(1, 50),
			       cost::make_advance_user_level(2, 50) );

    ASSERT_ABOVE_OR_EQUAL( cost::make_add_to_user_level(1, 50) +
			   cost::make_advance_user_level(2, 50),
			   cost::make_advance_user_level(2, 50) );
    ASSERT_ABOVE_OR_EQUAL( cost::make_advance_user_level(1, 50) +
			   cost::make_advance_user_level(2, 50),
			   cost::make_advance_user_level(2, 50) );
    ASSERT_ABOVE_OR_EQUAL( cost::make_advance_user_level(1, 50) +
			   cost::make_add_to_user_level(2, 50),
			   cost::make_add_to_user_level(2, 50) );

    ASSERT_NOT_ABOVE_OR_EQUAL( cost::make_add_to_user_level(1, 50) +
			       cost::make_advance_user_level(2, 45),
			       cost::make_advance_user_level(2, 50) );
    ASSERT_NOT_ABOVE_OR_EQUAL( cost::make_advance_user_level(1, 50) +
			       cost::make_advance_user_level(2, 45),
			       cost::make_advance_user_level(2, 50) );
    ASSERT_NOT_ABOVE_OR_EQUAL( cost::make_advance_user_level(1, 50) +
			       cost::make_add_to_user_level(2, 45),
			       cost::make_add_to_user_level(2, 50) );

    ASSERT_NOT_ABOVE_OR_EQUAL( cost::make_advance_user_level(1, 50) +
			       cost::make_advance_user_level(2, 45),
			       cost::make_advance_user_level(1, 45) +
			       cost::make_advance_user_level(2, 50) );

    ASSERT_NOT_ABOVE_OR_EQUAL( cost::make_advance_structural_level(50) +
			       cost::make_advance_user_level(2, 45),
			       cost::make_advance_structural_level(45) +
			       cost::make_advance_user_level(2, 50) );


    // We will use three cost operations here:
    //
    // (advance: 100)
    // (nop, add: 2, add: 4)
    // (nop, add: 1, nop, advance: 5)

    std::vector<cost> ops;
    ops.push_back(cost::make_advance_structural_level(100));
    ops.push_back(cost::make_add_to_user_level(0, 2) +
		  cost::make_add_to_user_level(1, 4));
    ops.push_back(cost::make_add_to_user_level(0, 1) +
		  cost::make_advance_user_level(2, 5));

    LOG_TRACE(logger, "Checking that converting operations to strings works.");

    CPPUNIT_ASSERT_EQUAL(std::string("(advance: 100)"),
			 boost::lexical_cast<std::string>(ops[0]));
    CPPUNIT_ASSERT_EQUAL(std::string("(nop, add: 2, add: 4)"),
			 boost::lexical_cast<std::string>(ops[1]));
    CPPUNIT_ASSERT_EQUAL(std::string("(nop, add: 1, nop, advance: 5)"),
			 boost::lexical_cast<std::string>(ops[2]));

    LOG_TRACE(logger, "Checking that instantiating illegal operations fails.");

    // Test that instantiating illegal operations fails.
    CPPUNIT_ASSERT_THROW(cost::make_add_to_user_level(0, 0),
			 NonPositiveCostAdditionException);
    CPPUNIT_ASSERT_THROW(cost::make_add_to_user_level(0, -1),
			 NonPositiveCostAdditionException);
    CPPUNIT_ASSERT_THROW(cost::make_add_to_user_level(-1, 5),
			 std::out_of_range);
    CPPUNIT_ASSERT_THROW(cost::make_advance_user_level(-1, 5),
			 std::out_of_range);

    LOG_TRACE(logger, "Testing that combining incompatible operations fails.");

    // Test that combining incompatible operations fails.  Also, take
    // the opportunity to test a few additional operation
    // combinations.
    const cost incompatible1(cost::make_add_to_user_level(0, 2) +
                             cost::make_advance_user_level(1, 6) +
                             cost::make_advance_user_level(2, 3));

    CPPUNIT_ASSERT_EQUAL(std::string("(nop, add: 2, advance: 6, advance: 3)"),
			 boost::lexical_cast<std::string>(incompatible1));

    CPPUNIT_ASSERT_EQUAL(std::string("(advance: 100, add: 2, advance: 6, advance: 3)"),
			 boost::lexical_cast<std::string>(incompatible1 + ops[0]));
    CPPUNIT_ASSERT_EQUAL(std::string("(advance: 100, add: 2, advance: 6, advance: 3)"),
			 boost::lexical_cast<std::string>(cost::least_upper_bound(incompatible1, ops[0])));
    CPPUNIT_ASSERT_EQUAL(std::string("(nop)"),
			 boost::lexical_cast<std::string>(cost::greatest_lower_bound(incompatible1, ops[0])));

    try
      {
        cost impossible = incompatible1 + ops[1];
        CPPUNIT_FAIL("Expected a CostOperationMismatchException from " + boost::lexical_cast<std::string>(incompatible1) + " + " + boost::lexical_cast<std::string>(ops[1]) + ", got a result: " + boost::lexical_cast<std::string>(impossible));
      }
    catch(CostOperationMismatchException &)
      {
      }
    catch(...)
      {
        CPPUNIT_FAIL("Unexpected exception!");
      }

    try
      {
        cost impossible = cost::least_upper_bound(incompatible1, ops[1]);
        CPPUNIT_FAIL("Expected a CostOperationMismatchException from lub(" + boost::lexical_cast<std::string>(incompatible1) + ", " + boost::lexical_cast<std::string>(ops[1]) + "), got a result: " + boost::lexical_cast<std::string>(impossible));
      }
    catch(CostOperationMismatchException &)
      {
      }
    catch(...)
      {
        CPPUNIT_FAIL("Unexpected exception!");
      }

    try
      {
        cost impossible = cost::greatest_lower_bound(incompatible1, ops[1]);
        CPPUNIT_FAIL("Expected a CostOperationMismatchException from glb(" + boost::lexical_cast<std::string>(incompatible1) + ", " + boost::lexical_cast<std::string>(ops[1]) + "), got a result: " + boost::lexical_cast<std::string>(impossible));
      }
    catch(CostOperationMismatchException &)
      {
      }
    catch(...)
      {
        CPPUNIT_FAIL("Unexpected exception!");
      }

    // Just to be sure, test those again here:
    CPPUNIT_ASSERT_THROW(incompatible1 + ops[1], CostOperationMismatchException);
    CPPUNIT_ASSERT_THROW(cost::least_upper_bound(incompatible1, ops[1]),
                         CostOperationMismatchException);
    CPPUNIT_ASSERT_THROW(cost::greatest_lower_bound(incompatible1, ops[1]),
                         CostOperationMismatchException);

    LOG_TRACE(logger, "Checking the outcome of some particular operation compositions.");

    CPPUNIT_ASSERT_EQUAL(std::string("(nop, add: 3, advance: 6, advance: 5)"),
			 boost::lexical_cast<std::string>(incompatible1 + ops[2]));
    CPPUNIT_ASSERT_EQUAL(std::string("(nop, add: 2, advance: 6, advance: 5)"),
			 boost::lexical_cast<std::string>(cost::least_upper_bound(incompatible1, ops[2])));
    CPPUNIT_ASSERT_EQUAL(std::string("(nop, add: 1, nop, advance: 3)"),
			 boost::lexical_cast<std::string>(cost::greatest_lower_bound(incompatible1, ops[2])));

    LOG_TRACE(logger, "Testing out-of-bounds operations.");

    // Test out-of-bound operations.
    CPPUNIT_ASSERT_THROW(cost::make_add_to_user_level(1, INT_MAX) +
			 cost::make_add_to_user_level(1, 5),
			 CostTooBigException);

    // Test rendering the initial operations:
    std::vector<std::string> cost_renderings;
    cost_renderings.push_back("(advance: 100)");
    cost_renderings.push_back("(nop, add: 2, add: 4)");
    cost_renderings.push_back("(nop, add: 1, nop, advance: 5)");

    // Input costs and the costs we expect to see after applying each operation.

    LOG_TRACE(logger, "Testing sums, lubs, and glbs of three cost operations.");

    // Combined values.  Each vector is a matrix where [i][j] contains
    // the combination of entries i and j for i<=j.  Combined values
    // are stored as strings to avoid any bias from passing through
    // the constructor (although of course this means we rely on a
    // working operator<<).  As a bonus, using strings should make
    // this part of the code much more readable.

    // Least upper bounds:
    std::vector<std::vector<std::string> > lubs(3, std::vector<std::string>(3, std::string()));
    lubs[0][0] = cost_renderings[0];
    lubs[0][1] = "(advance: 100, add: 2, add: 4)";
    lubs[0][2] = "(advance: 100, add: 1, nop, advance: 5)";

    lubs[1][0] = lubs[0][1];
    lubs[1][1] = cost_renderings[1];
    lubs[1][2] = "(nop, add: 2, add: 4, advance: 5)";

    lubs[2][0] = lubs[0][2];
    lubs[2][1] = lubs[1][2];
    lubs[2][2] = cost_renderings[2];

    // Greatest lower bounds:
    std::vector<std::vector<std::string> > glbs(3, std::vector<std::string>(3, std::string()));

    glbs[0][0] = cost_renderings[0];
    glbs[0][1] = "(nop)";
    glbs[0][2] = "(nop)";

    glbs[1][0] = glbs[0][1];
    glbs[1][1] = cost_renderings[1];
    glbs[1][2] = "(nop, add: 1)";

    glbs[2][0] = glbs[0][2];
    glbs[2][1] = glbs[1][2];
    glbs[2][2] = cost_renderings[2];

    // Sums:
    std::vector<std::vector<std::string> > sums(3, std::vector<std::string>(3, std::string()));

    sums[0][0] = "(advance: 100)";
    sums[0][1] = "(advance: 100, add: 2, add: 4)";
    sums[0][2] = "(advance: 100, add: 1, nop, advance: 5)";

    sums[1][0] = sums[0][1];
    sums[1][1] = "(nop, add: 4, add: 8)";
    sums[1][2] = "(nop, add: 3, add: 4, advance: 5)";

    sums[2][0] = sums[0][2];
    sums[2][1] = sums[1][2];
    sums[2][2] = "(nop, add: 2, nop, advance: 5)";


    for(std::size_t i = 0; i < ops.size(); ++i)
      {
        const cost &cost1(ops[i]);

        CPPUNIT_ASSERT_EQUAL(cost_renderings[i], boost::lexical_cast<std::string>(cost1));

        for(std::size_t j = 0; j < ops.size(); ++j)
          {
            const cost &cost2(ops[j]);

            std::string wheremsg = "(" + boost::lexical_cast<std::string>(cost1) + ", " + boost::lexical_cast<std::string>(cost2) + "); i=" + boost::lexical_cast<std::string>(i) + ", j=" + boost::lexical_cast<std::string>(j);

            CPPUNIT_ASSERT_EQUAL_MESSAGE("lub" + wheremsg, lubs[i][j], boost::lexical_cast<std::string>(cost::least_upper_bound(cost1, cost2)));
            CPPUNIT_ASSERT_EQUAL_MESSAGE("glb" + wheremsg, glbs[i][j], boost::lexical_cast<std::string>(cost::greatest_lower_bound(cost1, cost2)));
	    CPPUNIT_ASSERT_EQUAL_MESSAGE("sum" + wheremsg, sums[i][j], boost::lexical_cast<std::string>(cost1 + cost2));
          }
      }
  }

  void testCostOperations()
  {
    try
      {
        doTestCostOperations();
      }
    catch(const cwidget::util::Exception &ex)
      {
        CPPUNIT_FAIL("Uncaught exception: " + ex.errmsg());
      }
  }

  // Check that initial states work.
  void testInitialState()
  {
    logging::LoggerPtr logger(logging::Logger::getLogger("test.resolver.testInitialState"));
    LOG_TRACE(logger, "Entering testInitialState");

    dummy_universe_ref u = parseUniverse(dummy_universe_2);

    package a = u.find_package("a");
    package c = u.find_package("c");
    version av2 = a.version_from_name("v2");
    version cv2 = c.version_from_name("v2");


    imm::map<package, version> initial_state;
    initial_state.put(a, av2);
    initial_state.put(c, cv2);

    dummy_resolver r(10, -300, -100, 10000000, 500,
                     cost_limits::minimum_cost,
                     1,
		     initial_state,
		     u);

    CPPUNIT_ASSERT_EQUAL(r.get_initial_state().version_of(a), av2);
    CPPUNIT_ASSERT_EQUAL(r.get_initial_state().version_of(c), cv2);

    // I use an immset here because it has a decent operator<< for
    // printing error messages, and to smooth out differences in order
    // (which shouldn't happen in this implementation, but shouldn't
    // be wrong either).
    imm::set<version> expected_initial_state;
    expected_initial_state.insert(av2);
    expected_initial_state.insert(cv2);

    {
      std::set<version> initial_state;
      r.get_initial_state().get_initial_versions(initial_state);
      imm::set<version> initial_state_set;
      for(std::set<version>::const_iterator it = initial_state.begin();
	  it != initial_state.end(); ++it)
	initial_state_set.insert(*it);
      CPPUNIT_ASSERT_EQUAL(initial_state_set,
			   expected_initial_state);
    }

    try
      {
	LOG_TRACE(logger, "Checking that the empty solution is the first solution.");
	dummy_solution sol = r.find_next_solution(1000000, NULL);

	CPPUNIT_ASSERT_MESSAGE("There are no broken deps, so only the empty solution should be returned.",
			       sol.get_choices().size() == 0);

	{
	  std::set<version> initial_state;
	  sol.get_initial_state().get_initial_versions(initial_state);
	  imm::set<version> initial_state_set;
	  for(std::set<version>::const_iterator it = initial_state.begin();
	      it != initial_state.end(); ++it)
	    initial_state_set.insert(*it);
	  CPPUNIT_ASSERT_EQUAL(initial_state_set,
			       expected_initial_state);
	}

	LOG_TRACE(logger, "Checking that the empty solution is the only solution.");
	bool out_of_solutions = false;
	try
	  {
	    sol = r.find_next_solution(1000000, NULL);
	  }
	catch(NoMoreSolutions&)
	  {
	    out_of_solutions = true;
	  }
	CPPUNIT_ASSERT_MESSAGE("There should be only one solution.",
			       out_of_solutions);
      }
    catch(NoMoreTime&)
      {
	CPPUNIT_FAIL("Unable to solve the solution in the ridiculous amount of time I allocated.");
      }
    catch(NoMoreSolutions&)
      {
	CPPUNIT_FAIL("The empty solution should be returned.");
      }
  }

  // Check that joint scores work.
  void testJointScores()
  {
    dummy_universe_ref u = parseUniverse(dummy_universe_2);
    dummy_resolver r(10, -300, -100, 10000000, 500,
                     cost_limits::minimum_cost,
                     500,
		     imm::map<package, version>(),
		     u);
    // Disable this to debug the resolver test.
    //r.set_debug(true);
    // Score the combination of b, v1 and a, v2 highly.
    package a = u.find_package("a");
    package b = u.find_package("b");
    package c = u.find_package("c");
    version av2 = a.version_from_name("v2");
    version bv1 = b.version_from_name("v1");
    version bv2 = b.version_from_name("v2");
    version cv2 = c.version_from_name("v2");

    imm::set<version> choice_pair, choice_pair2;
    choice_pair.insert(av2);
    choice_pair.insert(bv1);
    choice_pair2.insert(bv2);
    choice_pair2.insert(cv2);

    const int test_score = 100000;
    const int test_score2 = -200000;
    r.add_joint_score(choice_pair, test_score);
    r.add_joint_score(choice_pair2, test_score2);

    bool saw_one_positive = false;
    bool saw_one_negative = false;
    bool saw_one_positive2 = false;
    while(1)
      {
	try
	  {
	    dummy_solution sol = r.find_next_solution(1000000, NULL);

	    if(sol.version_of(a) == av2 &&
	       sol.version_of(b) == bv1)
	      {
		saw_one_positive = true;
		// Check that we *don't* apply the joint score in
		// this case, since b starts out at v1.
		CPPUNIT_ASSERT_EQUAL((int)(sol.get_choices().size()) * r.get_step_score() + r.get_full_solution_score(),
				     sol.get_score());
	      }
	    else if(sol.version_of(b) == bv2 &&
		    sol.version_of(c) == cv2)
	      {
		saw_one_positive2 = true;
		saw_one_negative = true;
		CPPUNIT_ASSERT_EQUAL(test_score2 + ((int)sol.get_choices().size()) * r.get_step_score() + r.get_full_solution_score(),
				     sol.get_score());
	      }
	    else
	      {
		saw_one_negative = true;
		CPPUNIT_ASSERT_EQUAL((int)(sol.get_choices().size() * r.get_step_score()) + r.get_full_solution_score(),
				     sol.get_score());
	      }
	  }
	catch(NoMoreTime&)
	  {
	    CPPUNIT_FAIL("Unable to solve the solution in the ridiculous amount of time I allocated.");
	  }
	catch(NoMoreSolutions&)
	  {
	    if(!saw_one_positive)
	      CPPUNIT_FAIL("Expected at least one solution containing a, version 2 and b, version 1");
	    else if(!saw_one_negative)
	      CPPUNIT_FAIL("Expected at least one solution containing a, version 1 or b, version 2");
	    else if(!saw_one_positive2)
	      CPPUNIT_FAIL("Expected at least one solution containing b, version 2 and c, version 2");

	    break;
	  }
      }
  }

  // Test that the resolver ignores already-generated solutions when
  // generating successors.  Also tests that the resolver generates
  // solutions in the expected order in a simple situation.
  void testDropSolutionSupersets()
  {
    dummy_universe_ref u = parseUniverse(dummy_universe_2);
    dummy_resolver r(10, -300, -100, 100000, 50000,
                     cost_limits::minimum_cost,
                     500000,
		     imm::map<dummy_universe::package, dummy_universe::version>(),
		     u);

    dummy_universe::package a = u.find_package("a");
    dummy_universe::version av2 = a.version_from_name("v2");

    r.add_version_score(av2, 100);

    dummy_solution s;

    try
      {
	s = r.find_next_solution(100, NULL);
      }
    catch(NoMoreSolutions)
      {
	CPPUNIT_FAIL("Expected at least one solution, got none.");
      }

    CPPUNIT_ASSERT(s.version_of(a) == av2);
    CPPUNIT_ASSERT_EQUAL(s.get_choices().size(), 1U);

    try
      {
	s = r.find_next_solution(100, NULL);
      }
    catch(NoMoreSolutions)
      {
	CPPUNIT_FAIL("Expected at least two solutions, got only one.");
      }

    // Note that the only solutions to this problem are (a) install
    // a:v2 and do anything else, or (b) leave a:v1 installed and
    // install b:v2 and c:v2.  The search algorithm will "see" the
    // option of installing a:v2 to resolve the second dep of a:v1
    // after seeing the solution <a:=v2>.  If the bug we're testing
    // for is present, it'll try to install a:v2 again; otherwise
    // it'll reject that solution as containing a conflict.
    CPPUNIT_ASSERT(s.version_of(a) != av2);

    try
      {
	s = r.find_next_solution(100, NULL);
	CPPUNIT_FAIL("Expected exactly two solutions, got more than two.");
      }
    catch(NoMoreSolutions)
      {
      }
  }

  void testBreakSoftDepCost()
  {
    logging::LoggerPtr logger(logging::Logger::getLogger("test.resolver.testBreakSoftDepCost"));

    LOG_TRACE(logger, "Entering testBreakSoftDepCost.");

    // Run two tests here, both with a large score-level penalty for
    // installing b:v2.  First, verify that without the cost, we get
    // the break-soft-dep solution (to avoid b:v2).  Second, verify
    // that with the cost, we get the other one.
    dummy_universe_ref u = parseUniverse(dummy_universe_5);
    package a = u.find_package("a");
    package b = u.find_package("b");

    version av2 = a.version_from_name("v2");
    version bv2 = b.version_from_name("v2");

    dep av2d1 = *(av2.deps_begin());

    // The solution where installing av2 leads to installing bv2.
    choice_set install_bv2_solution;
    install_bv2_solution.insert_or_narrow(choice::make_install_version(av2, 0));
    install_bv2_solution.insert_or_narrow(choice::make_install_version(bv2, 1));

    // The solution where installing av2 leads to breaking a -?> bv2.
    choice_set break_av2d1_solution;
    break_av2d1_solution.insert_or_narrow(choice::make_install_version(av2, 0));
    break_av2d1_solution.insert_or_narrow(choice::make_break_soft_dep(av2d1, 1));

    {
      dummy_resolver r(10, -300, -100, 100000, 50000,
                       cost_limits::minimum_cost,
                       50,
                       imm::map<dummy_universe::package, dummy_universe::version>(),
                       u);

      // Bias the resolver strongly against installing bv2.
      r.set_version_score(bv2, -100000);

      std::vector<solution> sols;
      try
        {
          find_all_solutions(r, 50, NULL, sols);
        }
      catch(NoMoreTime)
        {
          CPPUNIT_FAIL("Ran out of time trying to solve a simple problem.");
        }

      CPPUNIT_ASSERT_EQUAL((std::size_t)2, sols.size());
      assertSameEffect(break_av2d1_solution, sols[0].get_choices());
      assertSameEffect(install_bv2_solution, sols[1].get_choices());

      CPPUNIT_ASSERT_EQUAL(cost(), sols[0].get_cost());
      CPPUNIT_ASSERT_EQUAL(cost(), sols[1].get_cost());
    }

    {
      dummy_resolver r(10, -300, -100, 100000, 50000,
                       cost::make_add_to_user_level(0, 1),
                       50,
                       imm::map<dummy_universe::package, dummy_universe::version>(),
                       u);

      // Bias the resolver strongly against installing bv2.
      r.set_version_score(bv2, -100000);

      std::vector<solution> sols;
      try
        {
          find_all_solutions(r, 50, NULL, sols);
        }
      catch(NoMoreTime)
        {
          CPPUNIT_FAIL("Ran out of time trying to solve a simple problem.");
        }

      CPPUNIT_ASSERT_EQUAL((std::size_t)2, sols.size());
      assertSameEffect(install_bv2_solution, sols[0].get_choices());
      assertSameEffect(break_av2d1_solution, sols[1].get_choices());

      CPPUNIT_ASSERT_EQUAL(cost(), sols[0].get_cost());
      CPPUNIT_ASSERT_EQUAL(cost::make_add_to_user_level(0, 1), sols[1].get_cost());
    }
  }
};

CPPUNIT_TEST_SUITE_REGISTRATION(ResolverTest);
