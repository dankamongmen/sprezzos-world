// test_choice.cc
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

#include <generic/problemresolver/choice.h>
#include <generic/problemresolver/dummy_universe.h>

#include <generic/util/compare3.h>

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

class Choice_Test : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(Choice_Test);

  CPPUNIT_TEST(testContains);
  CPPUNIT_TEST(testCompare);

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

  // We don't care about id in these tests, so these are convenience
  // routines that use a dummy value.
  static choice make_install_version(const version &v)
  {
    return choice::make_install_version(v, -1);
  }

  static choice make_install_version(const version &v, const dep &d)
  {
    return choice::make_install_version(v, d, -1);
  }

  static choice make_install_version_from_dep_source(const version &v, const dep &d)
  {
    return choice::make_install_version_from_dep_source(v, d, -1);
  }

  static choice make_break_soft_dep(const dep &d)
  {
    return choice::make_break_soft_dep(d, -1);
  }

public:
  void testCompare()
  {
    dummy_universe_ref u(parseUniverse(dummy_universe_1));

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

    std::vector<choice> choices;
    choices.push_back(make_install_version(av1));
    choices.push_back(make_install_version_from_dep_source(av1, av2d1));
    choices.push_back(make_install_version(av1, av2d1));
    choices.push_back(make_install_version_from_dep_source(av1, av3d1));
    choices.push_back(make_install_version(av2));
    choices.push_back(make_install_version(bv1));
    choices.push_back(make_break_soft_dep(av1d1));
    choices.push_back(make_break_soft_dep(av2d1));
    choices.push_back(make_install_version_from_dep_source(bv1, bv2d1));
    choices.push_back(make_install_version(cv2));

    using aptitude::util::compare3;
    for(std::vector<choice>::const_iterator itA = choices.begin(); itA != choices.end(); ++itA)
      {
	// Check reflexivity.
	CPPUNIT_ASSERT_EQUAL(0, compare3(*itA, *itA));
	CPPUNIT_ASSERT_EQUAL(*itA, *itA);

	for(std::vector<choice>::const_iterator itB = choices.begin(); itB != choices.end(); ++itB)
	  {
	    int cmpAB = compare3(*itA, *itB);
	    int cmpBA = compare3(*itB, *itA);

	    if(itA != itB)
	      {
		CPPUNIT_ASSERT(!(*itA == *itB));
		CPPUNIT_ASSERT(cmpAB != 0);
		CPPUNIT_ASSERT(cmpBA != 0);
	      }

	    // Check antisymmetry.
	    if(cmpAB < 0)
	      CPPUNIT_ASSERT(cmpBA > 0);
	    else if(cmpAB > 0)
	      CPPUNIT_ASSERT(cmpBA < 0);
	    else
	      CPPUNIT_ASSERT_EQUAL(0, cmpBA);

	    for(std::vector<choice>::const_iterator itC = choices.begin(); itC != choices.end(); ++itC)
	      {
		// Check transitivity.
		int cmpBC = compare3(*itB, *itC);
		int cmpAC = compare3(*itA, *itC);

		if(cmpAB == 0 && cmpBC == 0)
		  CPPUNIT_ASSERT_EQUAL(0, cmpAC);
		else if(cmpAB <= 0 && cmpBC <= 0)
		  CPPUNIT_ASSERT(cmpAC < 0);
		else if(cmpAB >= 0 && cmpBC >= 0)
		  CPPUNIT_ASSERT(cmpAC > 0);
	      }
	  }
      }
  }

  void testContains()
  {
    dummy_universe_ref u(parseUniverse(dummy_universe_1));

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

    const choice cav1 = make_install_version(av1);
    const choice cav1av2d1 = make_install_version_from_dep_source(av1, av2d1);
    const choice cav1av2d1_unscoped = make_install_version(av1, av2d1);
    const choice cav1av3d1 = make_install_version_from_dep_source(av1, av3d1);
    const choice cav2 = make_install_version(av2);
    const choice cbv1 = make_install_version(bv1);
    const choice cav1d1 = make_break_soft_dep(av1d1);
    const choice cav2d1 = make_break_soft_dep(av2d1);

    // Check that all the containment relations here are correct.
    CPPUNIT_ASSERT(cav1.contains(cav1));
    CPPUNIT_ASSERT(cav1.contains(cav1av2d1));
    CPPUNIT_ASSERT(cav1.contains(cav1av2d1_unscoped));
    CPPUNIT_ASSERT(cav1.contains(cav1av3d1));
    CPPUNIT_ASSERT(!cav1.contains(cav2));
    CPPUNIT_ASSERT(!cav1.contains(cbv1));
    CPPUNIT_ASSERT(!cav1.contains(cav1d1));
    CPPUNIT_ASSERT(!cav1.contains(cav2d1));

    CPPUNIT_ASSERT(!cav1av2d1.contains(cav1));
    CPPUNIT_ASSERT(cav1av2d1.contains(cav1av2d1));
    CPPUNIT_ASSERT(!cav1av2d1.contains(cav1av2d1_unscoped));
    CPPUNIT_ASSERT(!cav1av2d1.contains(cav1av3d1));
    CPPUNIT_ASSERT(!cav1av2d1.contains(cav2));
    CPPUNIT_ASSERT(!cav1av2d1.contains(cbv1));
    CPPUNIT_ASSERT(!cav1av2d1.contains(cav1d1));
    CPPUNIT_ASSERT(!cav1av2d1.contains(cav2d1));

    CPPUNIT_ASSERT(cav1av2d1_unscoped.contains(cav1));
    CPPUNIT_ASSERT(cav1av2d1_unscoped.contains(cav1av2d1));
    CPPUNIT_ASSERT(cav1av2d1_unscoped.contains(cav1av2d1_unscoped));
    CPPUNIT_ASSERT(cav1av2d1_unscoped.contains(cav1av3d1));
    CPPUNIT_ASSERT(!cav1av2d1_unscoped.contains(cav2));
    CPPUNIT_ASSERT(!cav1av2d1_unscoped.contains(cbv1));
    CPPUNIT_ASSERT(!cav1av2d1_unscoped.contains(cav1d1));
    CPPUNIT_ASSERT(!cav1av2d1_unscoped.contains(cav2d1));

    CPPUNIT_ASSERT(!cav1av3d1.contains(cav1));
    CPPUNIT_ASSERT(!cav1av3d1.contains(cav1av2d1));
    CPPUNIT_ASSERT(!cav1av3d1.contains(cav1av2d1_unscoped));
    CPPUNIT_ASSERT(cav1av3d1.contains(cav1av3d1));
    CPPUNIT_ASSERT(!cav1av3d1.contains(cav2));
    CPPUNIT_ASSERT(!cav1av3d1.contains(cbv1));
    CPPUNIT_ASSERT(!cav1av3d1.contains(cav1d1));
    CPPUNIT_ASSERT(!cav1av3d1.contains(cav2d1));

    // All the rest should only contain themselves.

    CPPUNIT_ASSERT(!cav2.contains(cav1));
    CPPUNIT_ASSERT(!cav2.contains(cav1av2d1));
    CPPUNIT_ASSERT(!cav2.contains(cav1av2d1_unscoped));
    CPPUNIT_ASSERT(!cav2.contains(cav1av3d1));
    CPPUNIT_ASSERT(cav2.contains(cav2));
    CPPUNIT_ASSERT(!cav2.contains(cbv1));
    CPPUNIT_ASSERT(!cav2.contains(cav1d1));
    CPPUNIT_ASSERT(!cav2.contains(cav2d1));

    CPPUNIT_ASSERT(!cbv1.contains(cav1));
    CPPUNIT_ASSERT(!cbv1.contains(cav1av2d1));
    CPPUNIT_ASSERT(!cbv1.contains(cav1av2d1_unscoped));
    CPPUNIT_ASSERT(!cbv1.contains(cav1av3d1));
    CPPUNIT_ASSERT(!cbv1.contains(cav2));
    CPPUNIT_ASSERT(cbv1.contains(cbv1));
    CPPUNIT_ASSERT(!cbv1.contains(cav1d1));
    CPPUNIT_ASSERT(!cbv1.contains(cav2d1));

    CPPUNIT_ASSERT(!cav1d1.contains(cav1));
    CPPUNIT_ASSERT(!cav1d1.contains(cav1av2d1));
    CPPUNIT_ASSERT(!cav1d1.contains(cav1av2d1_unscoped));
    CPPUNIT_ASSERT(!cav1d1.contains(cav1av3d1));
    CPPUNIT_ASSERT(!cav1d1.contains(cav2));
    CPPUNIT_ASSERT(!cav1d1.contains(cbv1));
    CPPUNIT_ASSERT(cav1d1.contains(cav1d1));
    CPPUNIT_ASSERT(!cav1d1.contains(cav2d1));

    CPPUNIT_ASSERT(!cav2d1.contains(cav1));
    CPPUNIT_ASSERT(!cav2d1.contains(cav1av2d1));
    CPPUNIT_ASSERT(!cav2d1.contains(cav1av2d1_unscoped));
    CPPUNIT_ASSERT(!cav2d1.contains(cav1av3d1));
    CPPUNIT_ASSERT(!cav2d1.contains(cav2));
    CPPUNIT_ASSERT(!cav2d1.contains(cbv1));
    CPPUNIT_ASSERT(!cav2d1.contains(cav1d1));
    CPPUNIT_ASSERT(cav2d1.contains(cav2d1));
  }
};

CPPUNIT_TEST_SUITE_REGISTRATION(Choice_Test);
