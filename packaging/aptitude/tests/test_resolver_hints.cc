// test_resolver_hints.cc                       -*-c++-*-
//
//   Copyright (C) 2009-2010 Daniel Burrows
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
// A test of the resolver-hint structure.

#include <generic/apt/aptitude_resolver.h>

#include <generic/problemresolver/cost.h>
#include <generic/problemresolver/cost_limits.h>

#include <cppunit/extensions/HelperMacros.h>

#include <boost/lexical_cast.hpp>

using namespace aptitude::matching;

namespace
{
  typedef aptitude_resolver::hint hint;

  struct test
  {
    // A string version of the hint.
    std::string text;

    // The resolver hint.
    aptitude_resolver::hint h;

    test(const std::string &_text, const hint &_h)
      : text(_text), h(_h)
    {
    }
  };

  const test resolver_tests[] =
    {
      test("40 g++", hint::make_tweak_score(pattern::make_exact_name("g++"),
					    hint::version_selection::make_inst(),
					    40)),
      test("-143 aptitude <4.3.0", hint::make_tweak_score(pattern::make_exact_name("aptitude"),
							  hint::version_selection::make_version(hint::version_selection::less_than, "4.3.0"),
							  -143)),
      test("+621 ?section(games) /unstable", hint::make_tweak_score(pattern::make_section("games"),
								    hint::version_selection::make_archive("unstable"),
								    621)),
      test("reject libc6 <=0.5.0", hint::make_reject(pattern::make_exact_name("libc6"),
						     hint::version_selection::make_version(hint::version_selection::less_than_or_equal_to,
											   "0.5.0"))),
      test("approve entropy =0", hint::make_mandate(pattern::make_exact_name("entropy"),
						   hint::version_selection::make_version(hint::version_selection::equal_to,
											 "0"))),
      test("approve entropy 1", hint::make_mandate(pattern::make_exact_name("entropy"),
						  hint::version_selection::make_version(hint::version_selection::equal_to,
											"1"))),

      test("reject aptitude :UNINST", hint::make_reject(pattern::make_exact_name("aptitude"),
							hint::version_selection::make_uninst())),
      test("+29387 xmonad >=5.4.3.2.1", hint::make_tweak_score(pattern::make_exact_name("xmonad"),
							       hint::version_selection::make_version(hint::version_selection::greater_than_or_equal_to, "5.4.3.2.1"),
							       29387)),
      test("approve dc >1", hint::make_mandate(pattern::make_exact_name("dc"),
					      hint::version_selection::make_version(hint::version_selection::greater_than, "1"))),
      test("reject ?task(desktop) <>4.0", hint::make_reject(pattern::make_task("desktop"),
							    hint::version_selection::make_version(hint::version_selection::not_equal_to, "4.0"))),
      test("increase-tier-to 100 wesnoth <5.0.0",
	   hint::make_raise_cost_component(pattern::make_exact_name("wesnoth"),
					   hint::version_selection::make_version(hint::version_selection::less_than, "5.0.0"),
					   "safety",
					   100)),
      test("increase-tier-to maximum xroach",
	   hint::make_raise_cost_component(pattern::make_exact_name("xroach"),
					   hint::version_selection::make_inst(),
					   "safety",
					   INT_MAX)),
      test("increase-tier-to conflict xroach",
	   hint::make_discard(pattern::make_exact_name("xroach"),
                              hint::version_selection::make_inst())),
      test("increase-tier-to discard bob",
	   hint::make_discard(pattern::make_exact_name("bob"),
                              hint::version_selection::make_inst())),
      test("increase-tier-to 800 xroach",
	   hint::make_raise_cost_component(pattern::make_exact_name("xroach"),
					   hint::version_selection::make_inst(),
					   "safety",
					   800)),

      test("add-to-cost-component nadaComponent 45 nada",
	   hint::make_add_to_cost_component(pattern::make_exact_name("nada"),
					    hint::version_selection::make_inst(),
					    "nadaComponent",
					    45)),

      test("raise-cost-component albertComponent 299 albert",
	   hint::make_raise_cost_component(pattern::make_exact_name("albert"),
					   hint::version_selection::make_inst(),
					   "albertComponent",
					   299)),
      test("discard albert",
           hint::make_discard(pattern::make_exact_name("albert"),
                              hint::version_selection::make_inst())),
      test("discard xyzzy /unstable",
           hint::make_discard(pattern::make_exact_name("xyzzy"),
                              hint::version_selection::make_archive("unstable"))),
    };
  const int num_resolver_tests =
    sizeof(resolver_tests) / sizeof(resolver_tests[0]);
}

class ResolverHintsTest : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(ResolverHintsTest);

  CPPUNIT_TEST(testHintParse);
  CPPUNIT_TEST(testHintCopy);
  CPPUNIT_TEST(testHintOrdering);

  CPPUNIT_TEST_SUITE_END();

public:
  void testHintParse()
  {
    for(int i = 0; i < num_resolver_tests; ++i)
      {
	// \todo When we have an operator<< to write out hints, we
	// should use ASSERT_EQUAL to get better messages.
	const test &t(resolver_tests[i]);

	hint h;
	CPPUNIT_ASSERT_MESSAGE("Parsing " + t.text, hint::parse(t.text, h));

	CPPUNIT_ASSERT_MESSAGE("Checking " + t.text + ": " +
			       boost::lexical_cast<std::string>(h) +
			       " == " + boost::lexical_cast<std::string>(t.h),
			       h == t.h);
      }

    // Try some failure cases too.
    hint dummy;
    CPPUNIT_ASSERT(!hint::parse("", dummy));
    CPPUNIT_ASSERT(!hint::parse("823", dummy));
    CPPUNIT_ASSERT(!hint::parse("badact target", dummy));
    CPPUNIT_ASSERT(!hint::parse("approve ?version(423 1234)", dummy));
    CPPUNIT_ASSERT(!hint::parse("approve ?version(3425", dummy));
  }

  void testHintCopy()
  {
    for(int i = 0; i < num_resolver_tests; ++i)
      {
	hint h(resolver_tests[i].h);

	CPPUNIT_ASSERT_MESSAGE("Testing copy constructor: ", h == resolver_tests[i].h);

	hint h2;
	h2 = resolver_tests[i].h;

	CPPUNIT_ASSERT_MESSAGE("Testing operator=: ", h2 == resolver_tests[i].h);
	CPPUNIT_ASSERT_MESSAGE("Testing operator= and copy constructor: ", h == h2);
      }
  }

  void testHintOrdering()
  {
    // Brute-force verification of the axioms of a total ordering.
    // Uses the fact that all the hints in the test array are
    // different.
    for(int i = 0; i < num_resolver_tests; ++i)
      for(int j = 0; j < num_resolver_tests; ++j)
	{
	  const hint &hi(resolver_tests[i].h);
	  const hint &hj(resolver_tests[j].h);

	  // Test consistency with .compare().
	  int cmp = hi.compare(hj);
	  if(cmp < 0)
	    {
	      CPPUNIT_ASSERT(hi < hj);
	      CPPUNIT_ASSERT(hi <= hj);
	      CPPUNIT_ASSERT(!(hi == hj));
	      CPPUNIT_ASSERT(hi != hj);
	      CPPUNIT_ASSERT(!(hi >= hj));
	      CPPUNIT_ASSERT(!(hi > hj));
	    }
	  else if(cmp == 0)
	    {
	      CPPUNIT_ASSERT(!(hi < hj));
	      CPPUNIT_ASSERT(hi <= hj);
	      CPPUNIT_ASSERT(hi == hj);
	      CPPUNIT_ASSERT(!(hi != hj));
	      CPPUNIT_ASSERT(hi >= hj);
	      CPPUNIT_ASSERT(!(hi > hj));
	    }
	  else // if(cmp > 0)
	    {
	      CPPUNIT_ASSERT(!(hi < hj));
	      CPPUNIT_ASSERT(!(hi <= hj));
	      CPPUNIT_ASSERT(!(hi == hj));
	      CPPUNIT_ASSERT(hi != hj);
	      CPPUNIT_ASSERT(hi >= hj);
	      CPPUNIT_ASSERT(hi > hj);
	    }

	  // Check that identity comparisons work (x == x' <=> x is x').
	  //
	  // Check that for all x, x <= x and x >= x, and also !(x<x)
	  // and !(x>x).
	  if(i == j)
	    {
	      CPPUNIT_ASSERT(hi == hj);
	      CPPUNIT_ASSERT(hi >= hj);
	      CPPUNIT_ASSERT(hi <= hj);
	      CPPUNIT_ASSERT(!(hi != hj));
	      CPPUNIT_ASSERT(!(hi < hj));
	      CPPUNIT_ASSERT(!(hi > hj));
	    }
	  else
	    {
	      CPPUNIT_ASSERT(hi != hj);
	      CPPUNIT_ASSERT(!(hi == hj));
	      CPPUNIT_ASSERT(hi > hj ||
			     hi < hj);

	      if(hi > hj)
		{
		  CPPUNIT_ASSERT(hi >= hj);
		  CPPUNIT_ASSERT(!(hi < hj));
		  CPPUNIT_ASSERT(!(hi <= hj));
		}

	      if(hi < hj)
		{
		  CPPUNIT_ASSERT(hi <= hj);
		  CPPUNIT_ASSERT(!(hi > hj));
		  CPPUNIT_ASSERT(!(hi >= hj));
		}
	    }

	  for(int k = 0; k < num_resolver_tests; ++k)
	    {
	      const hint &hk(resolver_tests[k].h);

	      // Test transitivity -- testing both or-equal and not-equal
	      // variants might be excessive?
	      if(hi <= hj && hj <= hk)
		CPPUNIT_ASSERT(hi <= hk);
	      if(hi < hj && hj < hk)
		CPPUNIT_ASSERT(hi < hk);

	      if(hi >= hj && hj >= hk)
		CPPUNIT_ASSERT(hi >= hk);
	      if(hi > hj && hj > hk)
		CPPUNIT_ASSERT(hi > hk);
	    }
	}
  }
};


CPPUNIT_TEST_SUITE_REGISTRATION(ResolverHintsTest);
