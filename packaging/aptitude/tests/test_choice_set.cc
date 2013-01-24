// test_choice_set.cc
//
//   Copyright (C) 2009 Daniel Burrows
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

#include <generic/problemresolver/choice_set.h>
#include <generic/problemresolver/dummy_universe.h>

#include <cppunit/extensions/HelperMacros.h>

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

class Choice_Set_Test : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(Choice_Set_Test);

  CPPUNIT_TEST(testInsertNarrow);
  CPPUNIT_TEST(testRemoveOverlaps);
  CPPUNIT_TEST(testGetVersionOf);
  CPPUNIT_TEST(testClone);
  CPPUNIT_TEST(testContainsChoice);
  CPPUNIT_TEST(testContainsChoiceSet);
  // No test for for_each(), because it's tested in testInsertNarrow
  // (at each step, we learn what's really in the set using
  // for_each())

  CPPUNIT_TEST_SUITE_END();


  typedef dummy_universe_ref::package package;
  typedef dummy_universe_ref::version version;
  typedef dummy_universe_ref::dep dep;
  typedef generic_choice<dummy_universe_ref> choice;
  typedef generic_choice_set<dummy_universe_ref> choice_set;

  static dummy_universe_ref parseUniverse(const std::string &s)
  {
    std::istringstream in(s);

    return parse_universe(in);
  }

  struct contents_extractor
  {
    imm::set<choice> &rval;

    contents_extractor(imm::set<choice> &_rval)
      : rval(_rval)
    {
    }

    bool operator()(const choice &c) const
    {
      CPPUNIT_ASSERT(!rval.contains(c));

      rval.insert(c);

      return true;
    }
  };

  struct contents_enumerator
  {
    std::vector<choice> &rval;

    contents_enumerator(std::vector<choice> &_rval)
      : rval(_rval)
    {
    }

    bool operator()(const choice &c) const
    {
      rval.push_back(c);
      return true;
    }
  };

  void test_contents_iterator(const choice_set &s)
  {
    std::vector<choice> expected, observed;

    s.for_each(contents_enumerator(expected));

    for(choice_set::const_iterator it = s.begin(); it != s.end(); ++it)
      {
	// Force operator-> to be compiled and invoked.
	CPPUNIT_ASSERT_EQUAL((*it).get_type(), it->get_type());
	observed.push_back(*it);
      }

    CPPUNIT_ASSERT_EQUAL(expected.size(), observed.size());
    for(std::vector<choice>::size_type i = 0; i < expected.size(); ++i)
      CPPUNIT_ASSERT_EQUAL(expected[i], observed[i]);
  };

  static imm::set<choice> get_contents(const choice_set &s)
  {
    imm::set<choice> rval;
    s.for_each(contents_extractor(rval));
    return rval;
  }

  static choice_set make_choice_set_narrow(const imm::set<choice> &s)
  {
    choice_set rval;
    rval.insert_or_narrow(s);
    return rval;
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

  static package a;
  static package b;
  static package c;

  static version av1;
  static version av2;
  static version av3;

  static version bv1;
  static version bv2;
  static version bv3;

  static version cv1;
  static version cv2;
  static version cv3;

  static dep av1d1;
  static dep bv2d1;
  static dep av2d1;
  static dep av3d1;

  static dummy_universe_ref u;

public:
  void testInsertNarrow()
  {
    dummy_universe_ref u(parseUniverse(dummy_universe_1));

    choice_set s;
    imm::set<choice> expected;

    CPPUNIT_ASSERT_EQUAL(expected.size(), s.size());
    CPPUNIT_ASSERT_EQUAL(expected, get_contents(s));
    CPPUNIT_ASSERT_EQUAL(expected, get_contents(make_choice_set_narrow(expected)));
    CPPUNIT_ASSERT_EQUAL(s, make_choice_set_narrow(expected));
    test_contents_iterator(s);

    const choice c1(make_install_version(av2));
    s.insert_or_narrow(c1);
    expected.insert(c1);

    CPPUNIT_ASSERT_EQUAL(expected.size(), s.size());
    CPPUNIT_ASSERT_EQUAL(expected, get_contents(s));
    CPPUNIT_ASSERT_EQUAL(expected, get_contents(make_choice_set_narrow(expected)));
    CPPUNIT_ASSERT_EQUAL(s, make_choice_set_narrow(expected));
    test_contents_iterator(s);

    const choice c2(make_install_version_from_dep_source(av2, av3d1));
    expected.erase(c1);
    expected.insert(c2);
    s.insert_or_narrow(c2);

    CPPUNIT_ASSERT_EQUAL(expected.size(), s.size());
    CPPUNIT_ASSERT_EQUAL(expected, get_contents(s));
    CPPUNIT_ASSERT_EQUAL(expected, get_contents(make_choice_set_narrow(expected)));
    CPPUNIT_ASSERT_EQUAL(s, make_choice_set_narrow(expected));
    test_contents_iterator(s);

    const choice c3(make_break_soft_dep(av2d1));
    s.insert_or_narrow(c3);
    expected.insert(c3);
    CPPUNIT_ASSERT_EQUAL(expected.size(), s.size());
    CPPUNIT_ASSERT_EQUAL(expected, get_contents(s));
    CPPUNIT_ASSERT_EQUAL(expected, get_contents(make_choice_set_narrow(expected)));
    CPPUNIT_ASSERT_EQUAL(s, make_choice_set_narrow(expected));
    test_contents_iterator(s);

    const choice c4(make_install_version_from_dep_source(bv1, bv2d1));
    s.insert_or_narrow(c4);
    expected.insert(c4);

    CPPUNIT_ASSERT_EQUAL(expected.size(), s.size());
    CPPUNIT_ASSERT_EQUAL(expected, get_contents(s));
    CPPUNIT_ASSERT_EQUAL(expected, get_contents(make_choice_set_narrow(expected)));
    CPPUNIT_ASSERT_EQUAL(s, make_choice_set_narrow(expected));
    test_contents_iterator(s);

    const choice c5(make_install_version(bv1));
    s.insert_or_narrow(c5);

    CPPUNIT_ASSERT_EQUAL(expected.size(), s.size());
    CPPUNIT_ASSERT_EQUAL(expected, get_contents(s));
    CPPUNIT_ASSERT_EQUAL(expected, get_contents(make_choice_set_narrow(expected)));
    CPPUNIT_ASSERT_EQUAL(s, make_choice_set_narrow(expected));
    test_contents_iterator(s);


    const choice c6(make_break_soft_dep(av3d1));
    s.insert_or_narrow(c6);
    expected.insert(c6);
    CPPUNIT_ASSERT_EQUAL(expected.size(), s.size());
    CPPUNIT_ASSERT_EQUAL(expected, get_contents(s));
    CPPUNIT_ASSERT_EQUAL(expected, get_contents(make_choice_set_narrow(expected)));
    CPPUNIT_ASSERT_EQUAL(s, make_choice_set_narrow(expected));
    test_contents_iterator(s);
  }

  void testRemoveOverlaps()
  {
    const choice c1(make_install_version(av1));
    const choice c2(make_install_version_from_dep_source(bv1, bv2d1));
    const choice c3(make_install_version(cv2));
    const choice c4(make_break_soft_dep(av2d1));
    const choice c5(make_break_soft_dep(bv2d1));

    choice_set s;
    s.insert_or_narrow(c1);
    CPPUNIT_ASSERT(s.contains(c1));
    s.insert_or_narrow(c2);
    CPPUNIT_ASSERT(s.contains(c1));
    CPPUNIT_ASSERT(s.contains(c2));
    s.insert_or_narrow(c3);
    CPPUNIT_ASSERT(s.contains(c1));
    CPPUNIT_ASSERT(s.contains(c2));
    CPPUNIT_ASSERT(s.contains(c3));
    s.insert_or_narrow(c4);
    CPPUNIT_ASSERT(s.contains(c1));
    CPPUNIT_ASSERT(s.contains(c2));
    CPPUNIT_ASSERT(s.contains(c3));
    CPPUNIT_ASSERT(s.contains(c4));
    s.insert_or_narrow(c5);
    CPPUNIT_ASSERT(s.contains(c1));
    CPPUNIT_ASSERT(s.contains(c2));
    CPPUNIT_ASSERT(s.contains(c3));
    CPPUNIT_ASSERT(s.contains(c4));
    CPPUNIT_ASSERT(s.contains(c5));


    const choice r1(make_install_version_from_dep_source(av1, av2d1));
    s.remove_overlaps(r1);
    CPPUNIT_ASSERT(!s.contains(r1));
    CPPUNIT_ASSERT(!s.contains(c1));
    CPPUNIT_ASSERT(s.contains(c2));
    CPPUNIT_ASSERT(s.contains(c3));
    CPPUNIT_ASSERT(s.contains(c4));
    CPPUNIT_ASSERT(s.contains(c5));

    CPPUNIT_ASSERT(s.contains(c2));
    const choice r2(make_install_version(bv1));
    s.remove_overlaps(r2);
    CPPUNIT_ASSERT(!s.contains(r2));
    CPPUNIT_ASSERT(!s.contains(c1));
    CPPUNIT_ASSERT(!s.contains(c2));
    CPPUNIT_ASSERT(s.contains(c3));
    CPPUNIT_ASSERT(s.contains(c4));
    CPPUNIT_ASSERT(s.contains(c5));

    CPPUNIT_ASSERT(s.contains(c3));
    const choice r3(make_install_version(cv2));
    s.remove_overlaps(r3);
    CPPUNIT_ASSERT(!s.contains(r3));
    CPPUNIT_ASSERT(!s.contains(c1));
    CPPUNIT_ASSERT(!s.contains(c2));
    CPPUNIT_ASSERT(!s.contains(c3));
    CPPUNIT_ASSERT(s.contains(c4));
    CPPUNIT_ASSERT(s.contains(c5));

    CPPUNIT_ASSERT(s.contains(c4));
    const choice r4(make_break_soft_dep(av2d1));
    s.remove_overlaps(r4);
    CPPUNIT_ASSERT(!s.contains(c1));
    CPPUNIT_ASSERT(!s.contains(c2));
    CPPUNIT_ASSERT(!s.contains(c3));
    CPPUNIT_ASSERT(!s.contains(r4));
    CPPUNIT_ASSERT(!s.contains(c4));
    CPPUNIT_ASSERT(s.contains(c5));

    const choice r5(make_break_soft_dep(bv2d1));
    s.remove_overlaps(r5);
    CPPUNIT_ASSERT(!s.contains(c1));
    CPPUNIT_ASSERT(!s.contains(c2));
    CPPUNIT_ASSERT(!s.contains(c3));
    CPPUNIT_ASSERT(!s.contains(c4));
    CPPUNIT_ASSERT(!s.contains(r5));
    CPPUNIT_ASSERT(!s.contains(c5));
  }

  void testContainsChoice()
  {
    choice_set s;
    s.insert_or_narrow(make_install_version_from_dep_source(av1, av3d1));
    s.insert_or_narrow(make_install_version(cv3));
    s.insert_or_narrow(make_install_version(bv1));
    s.insert_or_narrow(make_break_soft_dep(av2d1));

    CPPUNIT_ASSERT(s.contains(make_install_version_from_dep_source(av1, av3d1)));
    CPPUNIT_ASSERT(!s.contains(make_install_version_from_dep_source(av1, av2d1)));
    CPPUNIT_ASSERT(!s.contains(make_install_version(av1)));

    CPPUNIT_ASSERT(s.contains(make_install_version(cv3)));
    CPPUNIT_ASSERT(!s.contains(make_install_version(cv2)));
    CPPUNIT_ASSERT(s.contains(make_install_version_from_dep_source(cv3, av2d1)));

    CPPUNIT_ASSERT(s.contains(make_install_version(bv1)));
    CPPUNIT_ASSERT(!s.contains(make_install_version(bv2)));
    CPPUNIT_ASSERT(s.contains(make_install_version_from_dep_source(bv1, bv2d1)));

    CPPUNIT_ASSERT(s.contains(make_break_soft_dep(av2d1)));
    CPPUNIT_ASSERT(!s.contains(make_break_soft_dep(av3d1)));
  }

  void testClone()
  {
    choice_set s;
    s.insert_or_narrow(make_install_version_from_dep_source(av1, av3d1));
    s.insert_or_narrow(make_install_version(cv3));
    s.insert_or_narrow(make_install_version(bv1));
    s.insert_or_narrow(make_break_soft_dep(av2d1));

    CPPUNIT_ASSERT_EQUAL(s, s.clone());
    CPPUNIT_ASSERT_EQUAL(s, s.clone());
    CPPUNIT_ASSERT_EQUAL(s, s.clone());
  }

  void testGetVersionOf()
  {
    choice_set s;
    s.insert_or_narrow(make_install_version(av1));
    s.insert_or_narrow(make_install_version(bv3));

    version v;
    CPPUNIT_ASSERT(s.get_version_of(a, v));
    CPPUNIT_ASSERT_EQUAL(av1, v);

    CPPUNIT_ASSERT(s.get_version_of(b, v));
    CPPUNIT_ASSERT_EQUAL(bv3, v);

    s.insert_or_narrow(choice::make_install_version_from_dep_source(bv3, bv2d1, -1));
    CPPUNIT_ASSERT(s.get_version_of(b, v));
    CPPUNIT_ASSERT_EQUAL(bv3, v);

    CPPUNIT_ASSERT(!s.get_version_of(c, v));
  }

  void testContainsChoiceSet()
  {
    // Test sets:
    // S0: (Install(av1, bv3, cv2))
    // S1: (Install(av1 [av2d1], bv3, cv2))
    // S2: (Install(av1 [av3d1], bv3, cv2))
    // S3: (Install(cv2))
    // S4: (Install(cv2), Break(av2d1))
    // S5: (Break(av2d1), Break(bv2d1))
    // S6: (Break(bv2d1))
    //
    // S0 contains S1
    // S0 contains S2
    // S0 contains S3
    // S1 contains S3
    // S2 contains S3
    // S4 contains S3
    // S5 contains S6
    // Sn contains Sn for all n.

    choice_set s0;
    s0.insert_or_narrow(make_install_version(av1));
    s0.insert_or_narrow(make_install_version(bv3));
    s0.insert_or_narrow(make_install_version(cv2));

    choice_set s1;
    s1.insert_or_narrow(make_install_version_from_dep_source(av1, av2d1));
    s1.insert_or_narrow(make_install_version(bv3));
    s1.insert_or_narrow(make_install_version(cv2));

    choice_set s2;
    s2.insert_or_narrow(make_install_version_from_dep_source(av1, av3d1));
    s2.insert_or_narrow(make_install_version(bv3));
    s2.insert_or_narrow(make_install_version(cv2));

    choice_set s3;
    s3.insert_or_narrow(make_install_version(cv2));

    choice_set s4;
    s4.insert_or_narrow(make_install_version(cv2));
    s4.insert_or_narrow(make_break_soft_dep(av2d1));

    choice_set s5;
    s5.insert_or_narrow(make_break_soft_dep(av2d1));
    s5.insert_or_narrow(make_break_soft_dep(bv2d1));

    choice_set s6;
    s6.insert_or_narrow(make_break_soft_dep(bv2d1));

    std::vector<choice_set> sets;
    sets.push_back(s0);
    sets.push_back(s1);
    sets.push_back(s2);
    sets.push_back(s3);
    sets.push_back(s4);
    sets.push_back(s5);
    sets.push_back(s6);

    for(std::vector<choice_set>::size_type i = 0; i < sets.size(); ++i)
      for(std::vector<choice_set>::size_type j = 0; j < sets.size(); ++j)
	{
	  std::ostringstream msg;
	  msg << "In iteration (" << i << ", " << j << ")";

	  if((i == j) ||
	     (i == 0 && (j == 1 || j == 2 || j == 3)) ||
	     (i == 1 && j == 3) ||
	     (i == 2 && j == 3) ||
	     (i == 4 && j == 3) ||
	     (i == 5 && j == 6))
	    CPPUNIT_ASSERT_MESSAGE(msg.str(), sets[i].contains(sets[j]));
	  else
	    CPPUNIT_ASSERT_MESSAGE(msg.str(), !sets[i].contains(sets[j]));
	}
  }
};

dummy_universe_ref Choice_Set_Test::u(parseUniverse(dummy_universe_1));

Choice_Set_Test::package Choice_Set_Test::a(u.find_package("a"));
Choice_Set_Test::package Choice_Set_Test::b(u.find_package("b"));
Choice_Set_Test::package Choice_Set_Test::c(u.find_package("c"));

Choice_Set_Test::version Choice_Set_Test::av1(a.version_from_name("v1"));
Choice_Set_Test::version Choice_Set_Test::av2(a.version_from_name("v2"));
Choice_Set_Test::version Choice_Set_Test::av3(a.version_from_name("v3"));

Choice_Set_Test::version Choice_Set_Test::bv1(b.version_from_name("v1"));
Choice_Set_Test::version Choice_Set_Test::bv2(b.version_from_name("v2"));
Choice_Set_Test::version Choice_Set_Test::bv3(b.version_from_name("v3"));

Choice_Set_Test::version Choice_Set_Test::cv1(c.version_from_name("v1"));
Choice_Set_Test::version Choice_Set_Test::cv2(c.version_from_name("v2"));
Choice_Set_Test::version Choice_Set_Test::cv3(c.version_from_name("v3"));

Choice_Set_Test::dep Choice_Set_Test::av1d1(*av1.deps_begin());
Choice_Set_Test::dep Choice_Set_Test::bv2d1(*bv2.deps_begin());
Choice_Set_Test::dep Choice_Set_Test::av2d1(*av2.deps_begin());
Choice_Set_Test::dep Choice_Set_Test::av3d1(*av3.deps_begin());

CPPUNIT_TEST_SUITE_REGISTRATION(Choice_Set_Test);
