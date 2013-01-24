// test_dense_setset.cc
//
//   Copyright (C) 2005, 2009-2010 Daniel Burrows
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

#include <generic/problemresolver/dummy_universe.h>
#include <generic/problemresolver/promotion_set.h>
#include <generic/problemresolver/cost_limits.h>

#include <cppunit/extensions/HelperMacros.h>

#include <iostream>

namespace
{
  const char *dummy_universe_1 = "\
UNIVERSE [			  \
  PACKAGE a < v1 v2 v3 > v1	  \
  PACKAGE b < v1 v2 v3 > v1	  \
  PACKAGE c < v1 v2 v3 > v1	  \
				  \
  DEP a v1 -> < b v2 >		  \
  DEP b v2 -> < c v2 >		  \
				  \
  DEP a v2 -> < >		  \
  DEP a v3 -> < >		  \
]";
}

template<typename Key, typename Val>
std::ostream &operator<<(std::ostream &out, const boost::unordered_map<Key, Val> &m)
{
  out << "{";
  bool first = true;
  for(typename boost::unordered_map<Key, Val>::const_iterator it =
	m.begin(); it != m.end(); ++it)
    {
      if(first)
	first = false;
      else
	out << ", ";

      out << it->first << " -> " << it->second;
    }
  out << "}";

  return out;
}

class Promotion_SetTest : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(Promotion_SetTest);

  CPPUNIT_TEST(testFindHighestPromotion);
  CPPUNIT_TEST(testErase);

  CPPUNIT_TEST_SUITE_END();

  static dummy_universe_ref parseUniverse(const std::string &s)
  {
    std::istringstream in(s);

    return parse_universe(in);
  }

  typedef dummy_universe_ref::package package;
  typedef dummy_universe_ref::version version;
  typedef dummy_universe_ref::dep dep;
  typedef generic_choice<dummy_universe_ref> choice;
  typedef generic_choice_set<dummy_universe_ref> choice_set;
  typedef generic_promotion_set<dummy_universe_ref> dummy_promotion_set;
  typedef dummy_promotion_set::promotion promotion;

  static imm::set<promotion> get_promotions(const dummy_promotion_set &promotions)
  {
    imm::set<promotion> rval;
    for(dummy_promotion_set::const_iterator it = promotions.begin();
	it != promotions.end(); ++it)
      rval.insert(*it);
    return rval;
  }

  static unsigned int empirical_promotions_size(const dummy_promotion_set &promotions)
  {
    unsigned int rval = 0;
    for(dummy_promotion_set::const_iterator it = promotions.begin();
	it != promotions.end(); ++it)
      ++rval;
    return rval;
  }

  static cost make_cost(int first_user_level)
  {
    return
      cost::make_advance_user_level(0, first_user_level);
  }

  static cost make_cost(int first_user_level, int second_user_level)
  {
    return
      cost::make_advance_user_level(0, first_user_level) +
      cost::make_advance_user_level(1, second_user_level);
  }

  static void make_test_promotions(const dummy_universe_ref &u,
				   dummy_promotion_set &promotions)
  {
    // Otherwise the tests below will fail.
    CPPUNIT_ASSERT_EQUAL((unsigned int)0, promotions.size());

    package a(u.find_package("a"));
    package b(u.find_package("b"));
    package c(u.find_package("c"));

    version av1(a.version_from_name("v1"));
    version av2(a.version_from_name("v2"));
    version av3(a.version_from_name("v3"));

    version bv1(b.version_from_name("v1"));
    version bv2(b.version_from_name("v2"));
    version bv3(b.version_from_name("v3"));

    version cv1(c.version_from_name("v1"));
    version cv2(c.version_from_name("v2"));
    version cv3(c.version_from_name("v3"));

    dep av1d1(*av1.deps_begin());
    dep bv2d1(*bv2.deps_begin());
    dep av2d1(*av2.deps_begin());
    dep av3d1(*av3.deps_begin());

    // Insert promotions:
    // (Install(a v1 [a v2 -> <>])): 100
    // (Install(a v1, b v2, c v3)): 50
    // (Install(a v1, b v2)): 75
    // (Install(a v1, b v2, c v1)): 10
    // (Install(b v2)): 30
    // (Break(b v2 -> <c v2>)): 125
    // (Install(c v3), Break (b v2 -> <c v2>)): (125, 10)
    // (Install(bv3 [b v2 -> <c v2>], cv2)): 50
    //
    // Note that the third entry should override the second one.  The
    // fourth entry shouldn't be stored at all.

    // Verify that the promotion set has the expected
    // entries at each point in its construction.
    imm::set<promotion> expected_promotions;
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), promotions.size());
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), empirical_promotions_size(promotions));
    CPPUNIT_ASSERT_EQUAL(expected_promotions, get_promotions(promotions));

    choice_set p1_choices;
    p1_choices.insert_or_narrow(make_install_version_from_dep_source(av1, av2d1));
    promotion p1(p1_choices, make_cost(100));
    expected_promotions.insert(p1);
    promotions.insert(p1);
    CPPUNIT_ASSERT_EQUAL(expected_promotions, get_promotions(promotions));
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), promotions.size());
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), empirical_promotions_size(promotions));

    choice_set p2_choices;
    p2_choices.insert_or_narrow(make_install_version(av1));
    p2_choices.insert_or_narrow(make_install_version(bv2));
    p2_choices.insert_or_narrow(make_install_version(cv3));
    promotion p2(p2_choices, make_cost(50));
    expected_promotions.insert(p2);
    promotions.insert(p2);
    CPPUNIT_ASSERT_EQUAL(expected_promotions, get_promotions(promotions));
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), promotions.size());
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), empirical_promotions_size(promotions));

    choice_set p3_choices;
    p3_choices.insert_or_narrow(make_install_version(av1));
    p3_choices.insert_or_narrow(make_install_version(bv2));
    promotion p3(p3_choices, make_cost(75));
    expected_promotions.insert(p3);
    expected_promotions.erase(p2);
    promotions.insert(p3);
    CPPUNIT_ASSERT_EQUAL(expected_promotions, get_promotions(promotions));
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), promotions.size());
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), empirical_promotions_size(promotions));

    choice_set p4_choices;
    p4_choices.insert_or_narrow(make_install_version(av1));
    p4_choices.insert_or_narrow(make_install_version(bv2));
    p4_choices.insert_or_narrow(make_install_version(cv1));
    promotion p4(p4_choices, make_cost(10));
    promotions.insert(p4);
    CPPUNIT_ASSERT_EQUAL(expected_promotions, get_promotions(promotions));
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), promotions.size());
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), empirical_promotions_size(promotions));

    choice_set p5_choices;
    p5_choices.insert_or_narrow(make_install_version(bv2));
    promotion p5(p5_choices, make_cost(30));
    expected_promotions.insert(p5);
    promotions.insert(p5);
    CPPUNIT_ASSERT_EQUAL(expected_promotions, get_promotions(promotions));
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), promotions.size());
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), empirical_promotions_size(promotions));

    choice_set p6_choices;
    p6_choices.insert_or_narrow(make_break_soft_dep(bv2d1));
    promotion p6(p6_choices, make_cost(125));
    expected_promotions.insert(p6);
    promotions.insert(p6);
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), promotions.size());
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), empirical_promotions_size(promotions));
    CPPUNIT_ASSERT_EQUAL(expected_promotions, get_promotions(promotions));

    choice_set p7_choices;
    p7_choices.insert_or_narrow(make_install_version(cv3));
    p7_choices.insert_or_narrow(make_break_soft_dep(bv2d1));
    promotion p7(p7_choices, make_cost(125, 10));
    expected_promotions.insert(p7);
    promotions.insert(p7);
    std::cout << expected_promotions << std::endl;
    std::cout << promotions << std::endl;
    CPPUNIT_ASSERT_EQUAL(expected_promotions, get_promotions(promotions));
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), promotions.size());
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), empirical_promotions_size(promotions));

    choice_set p8_choices;
    p8_choices.insert_or_narrow(make_install_version_from_dep_source(bv3, bv2d1));
    p8_choices.insert_or_narrow(make_install_version(cv2));
    promotion p8(p8_choices, make_cost(50));
    expected_promotions.insert(p8);
    promotions.insert(p8);
    CPPUNIT_ASSERT_EQUAL(expected_promotions, get_promotions(promotions));
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), promotions.size());
    CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), empirical_promotions_size(promotions));
  }


  // We don't care about id in these tests, so these are convenience
  // routines that use a dummy value.
  static choice make_install_version(const version &v)
  {
    return choice::make_install_version(v, -1);
  }

  static choice make_install_version_from_dep_source(const version &v, const dep &d)
  {
    return choice::make_install_version_from_dep_source(v, d, -1);
  }

  static choice make_break_soft_dep(const dep &d)
  {
    return choice::make_break_soft_dep(d, -1);
  }

  class dummy_promotion_set_callbacks : public promotion_set_callbacks<dummy_universe_ref>
  {
    void promotion_retracted(const promotion &)
    {
    }
  };

  struct pick_all_promotions
  {
    bool operator()(const promotion &p) const
    {
      return true;
    }
  };

public:
  // Test searching for the highest promotion contained in a 
  void testFindHighestPromotion()
  {
    logging::LoggerPtr logger(logging::Logger::getLogger("test.promotion_set.testFindHighestPromotion"));
    LOG_TRACE(logger, "Entering testFindHighestPromotion.");

    dummy_universe_ref u(parseUniverse(dummy_universe_1));
    dummy_promotion_set_callbacks callbacks;
    dummy_promotion_set p(u, callbacks);

    package a(u.find_package("a"));
    package b(u.find_package("b"));
    package c(u.find_package("c"));

    version av1(a.version_from_name("v1"));
    version av2(a.version_from_name("v2"));
    version av3(a.version_from_name("v3"));

    version bv1(b.version_from_name("v1"));
    version bv2(b.version_from_name("v2"));
    version bv3(b.version_from_name("v3"));

    version cv1(c.version_from_name("v1"));
    version cv2(c.version_from_name("v2"));
    version cv3(c.version_from_name("v3"));

    dep av1d1(*av1.deps_begin());
    dep bv2d1(*bv2.deps_begin());
    dep av2d1(*av2.deps_begin());
    dep av3d1(*av3.deps_begin());

    make_test_promotions(u, p);

    // First search: (Install(a v1), Install(b v3),
    //                Install(c v3), Break(b v2 -> <c v2>))
    //
    // Should turn up only (T(125, 10): Install(c v3), Break(b v2 -> <c v2>))
    //
    // Checks that a search for a set with several hits returns the
    // highest-valued one.
    choice_set search1;
    search1.insert_or_narrow(make_install_version(av1));
    search1.insert_or_narrow(make_install_version(bv3));
    search1.insert_or_narrow(make_install_version(cv3));
    search1.insert_or_narrow(make_break_soft_dep(bv2d1));

    choice_set expected1_choices;
    expected1_choices.insert_or_narrow(make_install_version(cv3));
    expected1_choices.insert_or_narrow(make_break_soft_dep(bv2d1));
    promotion expected1(expected1_choices, make_cost(125, 10));

    cost found_cost = p.find_highest_promotion_cost(search1);
    CPPUNIT_ASSERT_EQUAL(expected1.get_cost(), found_cost);

    promotion found2 =
      p.find_highest_promotion_containing(search1, make_install_version(av1));
    CPPUNIT_ASSERT(found2 == promotion());

    found2 = p.find_highest_promotion_containing(search1, make_install_version(bv3));
    CPPUNIT_ASSERT(found2 == promotion());

    found2 = p.find_highest_promotion_containing(search1, make_install_version(cv3));
    CPPUNIT_ASSERT_EQUAL(expected1, found2);

    found2 = p.find_highest_promotion_containing(search1, make_break_soft_dep(bv2d1));
    CPPUNIT_ASSERT_EQUAL(expected1, found2);

    // Incipient search: (Install(a v1), Install(b v3),
    //                    Install(c v3)) + {Break(b v2 -> <c v2>)})
    choice_set search1_incipient1;
    search1_incipient1.insert_or_narrow(make_install_version(av1));
    search1_incipient1.insert_or_narrow(make_install_version(bv3));
    search1_incipient1.insert_or_narrow(make_install_version(cv3));
    choice search1_incipient_key1(make_break_soft_dep(bv2d1));

    generic_choice_indexed_map<dummy_universe_ref, bool> output_domain1_1;
    output_domain1_1.put(search1_incipient_key1, true);

    {
      boost::unordered_map<choice, promotion> output;
      maybe<promotion> output_non_incipient;

      LOG_TRACE(logger, "Testing that " << search1_incipient1
		<< " contains " << expected1
		<< " as an incipient promotion for the choice "
		<< search1_incipient_key1);
      p.find_highest_incipient_promotions(search1_incipient1,
					  output_domain1_1,
					  output,
					  output_non_incipient);

      CPPUNIT_ASSERT(output.size() >= 1);
      CPPUNIT_ASSERT(output.find(search1_incipient_key1) != output.end());
      CPPUNIT_ASSERT_EQUAL(expected1, output[search1_incipient_key1]);
    }

    {
      boost::unordered_map<choice, promotion> output;

      LOG_TRACE(logger, "Testing that " << search1_incipient1
		<< " contains " << expected1
		<< " as an incipient promotion restricted to the choice "
		<< search1_incipient_key1);
      p.find_highest_incipient_promotions_containing(search1_incipient1,
						     search1_incipient_key1,
						     output_domain1_1,
						     pick_all_promotions(),
						     output);

      LOG_TRACE(logger, "Output: " << output);

      CPPUNIT_ASSERT(output.size() >= 1);
      CPPUNIT_ASSERT(output.find(search1_incipient_key1) != output.end());
      CPPUNIT_ASSERT_EQUAL(expected1, output[search1_incipient_key1]);
    }

    // Incipient search: (Install(a v1), Install(b v3),
    //                    Break(b v2 -> c v2) + {Install(c v3)}
    choice_set search1_incipient2;
    search1_incipient2.insert_or_narrow(make_install_version(av1));
    search1_incipient2.insert_or_narrow(make_install_version(bv3));
    search1_incipient2.insert_or_narrow(make_break_soft_dep(bv2d1));
    choice search1_incipient_key2(make_install_version(cv3));

    generic_choice_indexed_map<dummy_universe_ref, bool> output_domain1_2;
    output_domain1_2.put(search1_incipient_key2, true);

    {
      boost::unordered_map<choice, promotion> output;
      maybe<promotion> output_non_incipient;

      LOG_TRACE(logger, "Testing that " << search1_incipient2
		<< " contains " << expected1
		<< " as an incipient promotion for the choice "
		<< search1_incipient_key2);
      p.find_highest_incipient_promotions(search1_incipient2,
					  output_domain1_2,
					  output,
					  output_non_incipient);

      LOG_TRACE(logger, "Output: " << output);

      CPPUNIT_ASSERT(output.size() >= 1);
      CPPUNIT_ASSERT(output.find(search1_incipient_key2) != output.end());
      CPPUNIT_ASSERT_EQUAL(expected1, output[search1_incipient_key2]);
    }

    {
      boost::unordered_map<choice, promotion> output;

      LOG_TRACE(logger, "Testing that " << search1_incipient2
		<< " contains " << expected1
		<< " as an incipient promotion restricted to the choice "
		<< search1_incipient_key2);
      p.find_highest_incipient_promotions_containing(search1_incipient2,
						     search1_incipient_key2,
						     output_domain1_2,
						     pick_all_promotions(),
						     output);

      LOG_TRACE(logger, "Output: " << output);

      CPPUNIT_ASSERT(output.size() >= 1);
      CPPUNIT_ASSERT(output.find(search1_incipient_key2) != output.end());
      CPPUNIT_ASSERT_EQUAL(expected1, output[search1_incipient_key2]);
    }


    // Second search: (a v1, b v1)
    //
    // Should turn up nothing.
    //
    // Checks that a search for a set with no hits returns nothing.
    choice_set search2;
    search2.insert_or_narrow(make_install_version(av1));
    search2.insert_or_narrow(make_install_version(bv1));

    CPPUNIT_ASSERT(p.find_highest_promotion_cost(search2) == cost_limits::minimum_cost);
    CPPUNIT_ASSERT(p.find_highest_promotion_containing(search2, make_install_version(av1)) == promotion());
    CPPUNIT_ASSERT(p.find_highest_promotion_containing(search2, make_install_version(bv1)) == promotion());

    // Third search: (Break(b v2 -> <c v2>))
    //
    // Should turn up only (T125: Break(b v2 -> <c v2>))
    //
    // Checks that a higher-valued superset is correctly ignored.
    choice_set search3;
    search3.insert_or_narrow(make_break_soft_dep(bv2d1));

    choice_set expected_choices3 = search3;
    promotion expected3(expected_choices3, make_cost(125));

    found_cost = p.find_highest_promotion_cost(search3);
    CPPUNIT_ASSERT_EQUAL(expected3.get_cost(), found_cost);

    found2 = p.find_highest_promotion_containing(search3, make_break_soft_dep(bv2d1));
    CPPUNIT_ASSERT_EQUAL(expected3, found2);

    // Fourth search: (Install(a v1 [a v3 -> <>]), Install(b v2))
    //
    // Should turn up only (T75: Install(a v1), Install(b v2))
    //
    // Tests that choices made due to a dependency are matched by
    // choices made not due to a dependency, but not by choices made
    // due to a different dependency.
    choice_set search4;
    search4.insert_or_narrow(make_install_version_from_dep_source(av1, av3d1));
    search4.insert_or_narrow(make_install_version(bv2));

    choice_set expected_choices4;
    expected_choices4.insert_or_narrow(make_install_version(av1));
    expected_choices4.insert_or_narrow(make_install_version(bv2));
    promotion expected4(expected_choices4, make_cost(75));

    found_cost = p.find_highest_promotion_cost(search4);
    CPPUNIT_ASSERT_EQUAL(expected4.get_cost(), found_cost);

    found2 = p.find_highest_promotion_containing(search4, make_install_version_from_dep_source(av1, av3d1));
    CPPUNIT_ASSERT_EQUAL(expected4, found2);

    found2 = p.find_highest_promotion_containing(search4, make_install_version(bv2));
    CPPUNIT_ASSERT_EQUAL(expected4, found2);


    // Fifth search: (Install(a v1 [a v2 -> <>]), Install(b v2))
    //
    // Should turn up only (T100: Install(a v1 [a v2 -> <>]))
    //
    // Tests that choices made due to a dependency are matched by
    // choices made due to the same dependency.
    choice_set search5;
    search5.insert_or_narrow(make_install_version_from_dep_source(av1, av2d1));
    search5.insert_or_narrow(make_install_version(bv2));

    choice_set expected_choices5;
    expected_choices5.insert_or_narrow(make_install_version_from_dep_source(av1, av2d1));
    promotion expected5(expected_choices5, make_cost(100));

    found_cost = p.find_highest_promotion_cost(search5);
    CPPUNIT_ASSERT_EQUAL(expected5.get_cost(), found_cost);

    found2 = p.find_highest_promotion_containing(search5, make_install_version_from_dep_source(av1, av2d1));
    CPPUNIT_ASSERT_EQUAL(expected5, found2);

    // Incipient search: (Install(b v2)) + {Install(a v1 [a v2 -> <>])}
    choice_set search5_incipient1;
    search5_incipient1.insert_or_narrow(make_install_version(bv2));
    choice search5_incipient_key1(make_install_version_from_dep_source(av1, av2d1));

    generic_choice_indexed_map<dummy_universe_ref, bool> output_domain5_1;
    output_domain5_1.put(search5_incipient_key1, true);

    {
      boost::unordered_map<choice, promotion> output;
      maybe<promotion> output_non_incipient;

      LOG_TRACE(logger, "Testing that " << search5_incipient1
		<< " contains " << expected5
		<< " as an incipient promotion for the choice "
		<< search5_incipient_key1);
      p.find_highest_incipient_promotions(search5_incipient1,
					  output_domain5_1,
					  output,
					  output_non_incipient);

      LOG_TRACE(logger, "Output: " << output);

      CPPUNIT_ASSERT(output.size() >= 1);
      CPPUNIT_ASSERT(output.find(search5_incipient_key1) != output.end());
      CPPUNIT_ASSERT_EQUAL(expected5, output[search5_incipient_key1]);
    }

    {
      boost::unordered_map<choice, promotion> output;

      LOG_TRACE(logger, "Testing that " << search5_incipient1
		<< " contains " << expected1
		<< " as an incipient promotion restricted to the choice "
		<< search5_incipient_key1);
      p.find_highest_incipient_promotions_containing(search5_incipient1,
						     search5_incipient_key1,
						     output_domain5_1,
						     pick_all_promotions(),
						     output);

      LOG_TRACE(logger, "Output: " << output);

      CPPUNIT_ASSERT(output.size() >= 1);
      CPPUNIT_ASSERT(output.find(search5_incipient_key1) != output.end());
      CPPUNIT_ASSERT_EQUAL(expected5, output[search5_incipient_key1]);
    }

    // In this case there is a different expectation: we should find
    // (T75: Install(a v1, b v2)), since the otherwise expected
    // solution doesn't contain the "key" element.
    choice_set expected_choices5_2;
    expected_choices5_2.insert_or_narrow(make_install_version(av1));
    expected_choices5_2.insert_or_narrow(make_install_version(bv2));
    promotion expected5_2(expected_choices5_2, make_cost(75));
    found2 = p.find_highest_promotion_containing(search5, make_install_version(bv2));
    CPPUNIT_ASSERT_EQUAL(expected5_2, found2);

    // Incipient search: (Install(a v1)) + {Install(b v2)}
    choice_set search5_incipient2;
    search5_incipient2.insert_or_narrow(make_install_version(av1));
    choice search5_incipient_key2(make_install_version(bv2));

    generic_choice_indexed_map<dummy_universe_ref, bool> output_domain5_2;
    output_domain5_2.put(search5_incipient_key2, true);

    {
      boost::unordered_map<choice, promotion> output;

      LOG_TRACE(logger, "Testing that " << search5_incipient2
		<< " contains " << expected5_2
		<< " as an incipient promotion restricted to the choice "
		<< search5_incipient_key2);
      p.find_highest_incipient_promotions_containing(search5_incipient2,
						     search5_incipient_key2,
						     output_domain5_2,
						     pick_all_promotions(),
						     output);

      LOG_TRACE(logger, "Output: " << output);

      CPPUNIT_ASSERT(output.size() >= 1);
      CPPUNIT_ASSERT(output.begin()->first.contains(search5_incipient_key2));
      CPPUNIT_ASSERT_EQUAL(expected5_2, output[search5_incipient_key2]);
    }

    // Check that nothing matches (Install(bv3, cv2)), because it
    // doesn't have the right from-dep-source information.  Tests that
    // find_highest_promotion_containing correctly checks the
    // from-dep-source information.
    choice_set search6;
    search6.insert_or_narrow(make_install_version(bv3));
    search6.insert_or_narrow(make_install_version(cv2));

    CPPUNIT_ASSERT(cost_limits::minimum_cost == p.find_highest_promotion_cost(search6));
    CPPUNIT_ASSERT(promotion() == p.find_highest_promotion_containing(search6, make_install_version(bv3)));
    CPPUNIT_ASSERT(promotion() == p.find_highest_promotion_containing(search6, make_install_version(cv2)));

    // Check that we can match (Install(bv3 [bv2 -> <c v2>], cv2))
    // instead.
    choice_set search7;
    search7.insert_or_narrow(make_install_version_from_dep_source(bv3, bv2d1));
    search7.insert_or_narrow(make_install_version(cv2));

    promotion expected7(search7, make_cost(50));

    found_cost = p.find_highest_promotion_cost(search7);
    CPPUNIT_ASSERT_EQUAL(expected7.get_cost(), found_cost);

    found2 = p.find_highest_promotion_containing(search7, make_install_version_from_dep_source(bv3, bv2d1));
    CPPUNIT_ASSERT_EQUAL(expected7, found2);

    found2 = p.find_highest_promotion_containing(search7, make_install_version(cv2));
    CPPUNIT_ASSERT_EQUAL(expected7, found2);
  }

  void testErase()
  {
    dummy_universe_ref u(parseUniverse(dummy_universe_1));
    dummy_promotion_set_callbacks callbacks;
    dummy_promotion_set p(u, callbacks);

    package a(u.find_package("a"));
    package b(u.find_package("b"));
    package c(u.find_package("c"));

    version av1(a.version_from_name("v1"));
    version av2(a.version_from_name("v2"));
    version av3(a.version_from_name("v3"));

    version bv1(b.version_from_name("v1"));
    version bv2(b.version_from_name("v2"));
    version bv3(b.version_from_name("v3"));

    version cv1(c.version_from_name("v1"));
    version cv2(c.version_from_name("v2"));
    version cv3(c.version_from_name("v3"));

    dep av1d1(*av1.deps_begin());
    dep bv2d1(*bv2.deps_begin());
    dep av2d1(*av2.deps_begin());
    dep av3d1(*av3.deps_begin());

    make_test_promotions(u, p);

    // Remove tiers 50 and (125, 10), and make sure they don't show up
    // in the results.
    {
      dummy_promotion_set::iterator it = p.begin(), next = it;

      while(it != p.end())
	{
	  ++next;

	  if(it->get_cost() == make_cost(50) ||
	     it->get_cost() == make_cost(125, 10))
	    p.erase(it);

	  it = next;
	}
    }

    // Check that the size and contents (when iterating) of the
    // promotion set are maintained correctly.
    {
      imm::set<promotion> expected_promotions;
      choice_set p1_choices;
      p1_choices.insert_or_narrow(make_install_version_from_dep_source(av1, av2d1));
      promotion p1(p1_choices, make_cost(100));
      expected_promotions.insert(p1);
      p.insert(p1);


      choice_set p2_choices;
      p2_choices.insert_or_narrow(make_install_version(av1));
      p2_choices.insert_or_narrow(make_install_version(bv2));
      promotion p2(p2_choices, make_cost(75));
      expected_promotions.insert(p2);
      p.insert(p2);


      choice_set p3_choices;
      p3_choices.insert_or_narrow(make_install_version(bv2));
      promotion p3(p3_choices, make_cost(30));
      expected_promotions.insert(p3);
      p.insert(p3);

      choice_set p4_choices;
      p4_choices.insert_or_narrow(make_break_soft_dep(bv2d1));
      promotion p4(p4_choices, make_cost(125));
      expected_promotions.insert(p4);
      p.insert(p4);



      CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), p.size());
      CPPUNIT_ASSERT_EQUAL(expected_promotions.size(), empirical_promotions_size(p));
      CPPUNIT_ASSERT_EQUAL(expected_promotions, get_promotions(p));
    }

    // First search: (Install(a v1), Install(b v3),
    //                Install(c v3), Break(b v2 -> <c v2>))
    //
    // Should turn up (T125: Break(b v2 -> <c v2>))
    choice_set search1;
    search1.insert_or_narrow(make_install_version(av1));
    search1.insert_or_narrow(make_install_version(bv3));
    search1.insert_or_narrow(make_install_version(cv3));
    search1.insert_or_narrow(make_break_soft_dep(bv2d1));

    choice_set expected_choices1;
    expected_choices1.insert_or_narrow(make_break_soft_dep(bv2d1));
    promotion expected1(expected_choices1, make_cost(125));

    cost found_cost = p.find_highest_promotion_cost(search1);
    CPPUNIT_ASSERT_EQUAL(expected1.get_cost(), found_cost);

    promotion found_p = p.find_highest_promotion_containing(search1, make_install_version(av1));
    CPPUNIT_ASSERT(found_p == promotion());

    found_p = p.find_highest_promotion_containing(search1, make_install_version(bv3));
    CPPUNIT_ASSERT(found_p == promotion());

    found_p = p.find_highest_promotion_containing(search1, make_install_version(cv3));
    CPPUNIT_ASSERT(found_p == promotion());

    found_p = p.find_highest_promotion_containing(search1, make_break_soft_dep(bv2d1));
    CPPUNIT_ASSERT_EQUAL(expected1, found_p);


    // Nothing should match the tier-50 promotion.

    choice_set search2;
    search2.insert_or_narrow(make_install_version_from_dep_source(bv3, bv2d1));
    search2.insert_or_narrow(make_install_version(cv2));

    CPPUNIT_ASSERT(cost_limits::minimum_cost == p.find_highest_promotion_cost(search2));
    CPPUNIT_ASSERT(promotion() == p.find_highest_promotion_containing(search2, make_install_version_from_dep_source(bv3, bv2d1)));
    CPPUNIT_ASSERT(promotion() == p.find_highest_promotion_containing(search2, make_install_version(cv2)));
  }
};

CPPUNIT_TEST_SUITE_REGISTRATION(Promotion_SetTest);
