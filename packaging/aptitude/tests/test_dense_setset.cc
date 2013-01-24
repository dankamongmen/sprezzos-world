// test_dense_setset.cc
//
//   Copyright (C) 2005, 2009 Daniel Burrows
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

#include <cppunit/extensions/HelperMacros.h>

#include <generic/util/dense_setset.h>

#include <iostream>

class Dense_SetsetTest : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(Dense_SetsetTest);

  CPPUNIT_TEST(testSubsetSearch);
  CPPUNIT_TEST(testSubsetPredicateSearch);

  CPPUNIT_TEST(testSubmapSearch);

  CPPUNIT_TEST_SUITE_END();

  struct identity
  {
    int operator()(int a) const
    {
      return a;
    }
  };

public:
  // Test searching for subsets of a set.
  void testSubsetSearch()
  {
    imm::set<int> s1, s2, s3;

    s1.insert(1);
    s1.insert(2);
    s1.insert(5);

    s2.insert(5);

    s3.insert(4);
    s3.insert(5);

    dense_setset<int, identity> S(10);

    S.insert(s1);
    S.insert(s2);
    S.insert(s3);

    imm::set<int> t;

    t.insert(1);

    dense_setset<int, identity>::const_iterator found = S.find_subset(t);
    CPPUNIT_ASSERT(found == S.end());

    t.insert(5);

    found = S.find_subset(t);
    CPPUNIT_ASSERT(found != S.end());
    CPPUNIT_ASSERT_EQUAL(s2, *found);

    found = S.find_subset_containing(t, 1);
    CPPUNIT_ASSERT(found == S.end());

    t.insert(6);
    t.insert(4);
    t.insert(3);

    CPPUNIT_ASSERT(S.find_subset(t) != S.end());

    found = S.find_subset_containing(t, 1);
    CPPUNIT_ASSERT(found == S.end());

    found = S.find_subset_containing(t, 4);
    CPPUNIT_ASSERT(found != S.end());
    CPPUNIT_ASSERT_EQUAL(s3, *found);
  }

  struct HalfCmp
  {
  public:
    int operator()(int a, int b) const
    {
      return aptitude::util::compare3(a/2, b/2);
    }
  };

  struct HalfExtractId
  {
  public:
    size_t operator()(int a) const
    {
      return a/2;
    }
  };

  // Test searching for subsets of a set using subsumption.
  void testSubsetPredicateSearch()
  {
    imm::set<int, HalfCmp> s1, s2, s3;

    dense_setset<int, HalfExtractId, HalfCmp> S(20);

    s1.insert(5);
    CPPUNIT_ASSERT(s1.contains(5));
    CPPUNIT_ASSERT_EQUAL(1U, s1.size());
    S.insert(s1);

    s2.insert(6);
    s2.insert(8);
    CPPUNIT_ASSERT_EQUAL(2U, s2.size());
    S.insert(s2);

    s3.insert(9);
    s3.insert(11);
    CPPUNIT_ASSERT_EQUAL(2U, s3.size());
    S.insert(s3);

    imm::set<int, HalfCmp> t;

    t.insert(5);

    dense_setset<int, HalfExtractId, HalfCmp>::const_iterator found = S.find_subset(t);
    CPPUNIT_ASSERT(found != S.end());
    CPPUNIT_ASSERT_EQUAL(s1, *found);

    found = S.find_subset_containing(t, 4);
    CPPUNIT_ASSERT(found != S.end());
    CPPUNIT_ASSERT_EQUAL(s1, *found);

    found = S.find_subset_containing(t, 8);
    CPPUNIT_ASSERT(found == S.end());

    t.insert(10);
    t.insert(11);
    t.insert(15);
    t.insert(17);

    found = S.find_subset(t);
    CPPUNIT_ASSERT(found != S.end());
    CPPUNIT_ASSERT_EQUAL(s1, *found);
    
    t.erase(5);

    t.insert(4);
    found = S.find_subset(t);
    CPPUNIT_ASSERT(found != S.end());
    CPPUNIT_ASSERT_EQUAL(s1, *found);

    t.erase(4);
    t.insert(7);
    t.insert(8);
    t.insertUpdate(11);

    found = S.find_subset(t);
    CPPUNIT_ASSERT(found != S.end());
    CPPUNIT_ASSERT_EQUAL(s2, *found);

    found = S.find_subset(t, std::greater<int>());

    // Shouldn't show up since t contains 6 and 7, and the set is 6
    // and 8.
    CPPUNIT_ASSERT(found == S.end());

    t.insertUpdate(10);
    t.insertUpdate(8);

    found = S.find_subset(t, std::greater<int>());

    CPPUNIT_ASSERT(found != S.end());
    CPPUNIT_ASSERT_EQUAL(s3, *found);

    found = S.find_subset_containing(t, 6);

    CPPUNIT_ASSERT(found != S.end());
    CPPUNIT_ASSERT_EQUAL(s2, *found);

    found = S.find_subset_containing(t, 11);

    CPPUNIT_ASSERT(found != S.end());
    CPPUNIT_ASSERT_EQUAL(s3, *found);
  }

  void testSubmapSearch()
  {
    imm::map<int, int> m1, m2, m3;

    m1.put(1, 2);
    m1.put(5, 2);

    m2.put(1, 5);

    m3.put(5, 2);
    m3.put(6, 2);

    dense_mapset<int, int, identity> S(10);

    S.insert(m1);
    S.insert(m2);
    S.insert(m3);

    imm::map<int, int> t;

    t.put(1, 2);
    t.put(3, 2);

    dense_mapset<int, int, identity>::const_iterator found = S.find_submap(t);

    CPPUNIT_ASSERT(found != S.end());
    CPPUNIT_ASSERT_EQUAL(m2, *found);

    found = S.find_submap(t, std::equal_to<std::pair<int, int> >());
    CPPUNIT_ASSERT(found == S.end());

    t.put(5, 2);
    found = S.find_submap(t, std::equal_to<std::pair<int, int> >());
    CPPUNIT_ASSERT(found != S.end());
    CPPUNIT_ASSERT_EQUAL(m1, *found);

    t.put(1, 5);
    found = S.find_submap(t, std::equal_to<std::pair<int, int> >());
    CPPUNIT_ASSERT(found != S.end());
    CPPUNIT_ASSERT_EQUAL(m2, *found);

    t.erase(1);
    t.put(6, 2);
    found = S.find_submap(t);
    CPPUNIT_ASSERT(found != S.end());
    CPPUNIT_ASSERT_EQUAL(m3, *found);
  }
};

CPPUNIT_TEST_SUITE_REGISTRATION(Dense_SetsetTest);
