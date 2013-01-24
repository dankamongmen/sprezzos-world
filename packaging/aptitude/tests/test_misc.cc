// Miscellaneous tests.
//
//   Copyright (C) 2005, 2007, 2010 Daniel Burrows
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

// Local includes:
#include <generic/util/util.h>

// System includes:
#include <cppunit/extensions/HelperMacros.h>

#include <sys/time.h>

using aptitude::util::subtract_timevals;

class MiscTest : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(MiscTest);

  CPPUNIT_TEST(testStripWS);
  CPPUNIT_TEST(testOrderlessLt);
  CPPUNIT_TEST(testSubtractTimevalGreaterInBothComponents);
  CPPUNIT_TEST(testSubtractTimevalGreaterInSecondsLessInMilliseconds);
  CPPUNIT_TEST(testSubtractTimevalEqual);
  CPPUNIT_TEST(testSubtractTimevalLessInSecondsGreaterInMilliseconds);
  CPPUNIT_TEST(testSubtractTimevalLessInBothComponents);

  CPPUNIT_TEST_SUITE_END();
private:
  void assertStripWS(const std::string &s,
		     const std::string &expected)
  {
    std::string s2 = s;
    stripws(s2);

    CPPUNIT_ASSERT_EQUAL(expected, s2);
  }

  void testStripWS()
  {
    assertStripWS("    abc", "abc");
    assertStripWS("abc    ", "abc");
    assertStripWS("   abc   ", "abc");

    assertStripWS("    ", "");

    // double-check that there are no weird corner cases involving
    // singly letters.
    assertStripWS(" a", "a");
    assertStripWS("a ", "a");
    assertStripWS(" a ", "a");
  }

  void testOrderlessLt()
  {
    orderless_lt<int> cmp;

    std::pair<int, int> a(1, 2);
    std::pair<int, int> b(2, 1);
    std::pair<int, int> c(4, 1);
    std::pair<int, int> d(1, 4);
    std::pair<int, int> e(4, 6);

    CPPUNIT_ASSERT(!cmp(a, a));
    CPPUNIT_ASSERT(!cmp(b, b));
    CPPUNIT_ASSERT(!cmp(c, c));
    CPPUNIT_ASSERT(!cmp(d, d));
    CPPUNIT_ASSERT(!cmp(e, e));

    CPPUNIT_ASSERT(!cmp(a, b));
    CPPUNIT_ASSERT( cmp(b, c));
    CPPUNIT_ASSERT(!cmp(c, d));
    CPPUNIT_ASSERT( cmp(d, e));
    CPPUNIT_ASSERT(!cmp(e, a));

    CPPUNIT_ASSERT( cmp(a, c));
    CPPUNIT_ASSERT( cmp(b, d));
    CPPUNIT_ASSERT( cmp(c, e));
    CPPUNIT_ASSERT(!cmp(d, a));
    CPPUNIT_ASSERT(!cmp(e, b));

    CPPUNIT_ASSERT( cmp(a, d));
    CPPUNIT_ASSERT( cmp(b, e));
    CPPUNIT_ASSERT(!cmp(c, a));
    CPPUNIT_ASSERT(!cmp(d, b));
    CPPUNIT_ASSERT(!cmp(e, c));

    CPPUNIT_ASSERT( cmp(a, e));
    CPPUNIT_ASSERT(!cmp(b, a));
    CPPUNIT_ASSERT(!cmp(c, b));
    CPPUNIT_ASSERT(!cmp(d, c));
    CPPUNIT_ASSERT(!cmp(e, d));
  }

  void testSubtractTimevalGreaterInBothComponents()
  {
    struct timeval a, b;

    a.tv_sec = 10;
    a.tv_usec = 1000;

    b.tv_sec = 5;
    b.tv_usec = 50;

    struct timeval c = subtract_timevals(a, b);

    CPPUNIT_ASSERT_EQUAL(static_cast<time_t>(5), c.tv_sec);
    CPPUNIT_ASSERT_EQUAL(static_cast<suseconds_t>(950), c.tv_usec);
  }

  void testSubtractTimevalGreaterInSecondsLessInMilliseconds()
  {
    struct timeval a, b;

    a.tv_sec = 10;
    a.tv_usec = 75;

    b.tv_sec = 5;
    b.tv_usec = 100;

    struct timeval c = subtract_timevals(a, b);

    CPPUNIT_ASSERT_EQUAL(static_cast<time_t>(4), c.tv_sec);
    CPPUNIT_ASSERT_EQUAL(static_cast<suseconds_t>(999975), c.tv_usec);
  }


  void testSubtractTimevalEqual()
  {
    struct timeval a, b;

    a.tv_sec = 50;
    a.tv_usec = 230;

    b.tv_sec = 50;
    b.tv_usec = 230;

    struct timeval c = subtract_timevals(a, b);

    CPPUNIT_ASSERT_EQUAL(static_cast<time_t>(0), c.tv_sec);
    CPPUNIT_ASSERT_EQUAL(static_cast<suseconds_t>(0), c.tv_usec);
  }

  void testSubtractTimevalLessInSecondsGreaterInMilliseconds()
  {
    struct timeval a, b;

    a.tv_sec = 70;
    a.tv_usec = 500;

    b.tv_sec = 80;
    b.tv_usec = 10;

    struct timeval c = subtract_timevals(a, b);

    CPPUNIT_ASSERT_EQUAL(static_cast<time_t>(-10), c.tv_sec);
    CPPUNIT_ASSERT_EQUAL(static_cast<suseconds_t>(490), c.tv_usec);
  }

  void testSubtractTimevalLessInBothComponents()
  {
    struct timeval a, b;

    a.tv_sec = 100;
    a.tv_usec = 130;

    b.tv_sec = 123;
    b.tv_usec = 131;

    struct timeval c = subtract_timevals(a, b);

    CPPUNIT_ASSERT_EQUAL(static_cast<time_t>(-24), c.tv_sec);
    CPPUNIT_ASSERT_EQUAL(static_cast<suseconds_t>(999999), c.tv_usec);
  }
};

CPPUNIT_TEST_SUITE_REGISTRATION(MiscTest);
