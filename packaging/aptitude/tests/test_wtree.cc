// test_wtree.cc
//
//   Copyright (C) 2005, 2008-2010 Daniel Burrows
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

#include <generic/util/immset.h>

#include <limits.h>

using imm::map;
using imm::nil_t;
using imm::set;
using imm::wtree_node;

nil_t nil;

template<typename T>
std::ostream &operator<<(std::ostream &out, const imm::set<T> &t)
{
  out << "{";
  for(typename imm::set<T>::const_iterator i = t.begin();
      i != t.end(); ++i)
    {
      if(i != t.begin())
	out << ", ";
      out << *i;
    }
  out << "}";

  return out;
}

template<typename T1, typename T2>
std::ostream &operator<<(std::ostream &out, const std::pair<T1, T2> &p)
{
  out << "(" << p.first << ", " << p.second << ")";
  return out;
}

template<typename Key, typename Val>
std::ostream &operator<<(std::ostream &out, const imm::map<Key, Val> &m)
{
  out << "{";
  for(typename imm::map<Key, Val>::const_iterator i = m.begin();
      i != m.end(); ++i)
    {
      if(i != m.begin())
	out << ", ";
      out << i->first << " |-> " << i->second;
    }
  out << "}";
  return out;
}

#define CPPUNIT_ASSERT_LEAF(n) \
  do { \
    CPPUNIT_ASSERT(!n.getLeft().isValid()); \
    CPPUNIT_ASSERT(!n.getRight().isValid()); \
  } while(0)

class WTreeTest : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(WTreeTest);

  CPPUNIT_TEST(testRotateLeft);
  CPPUNIT_TEST(testRotateRight);
  CPPUNIT_TEST(testDoubleRotateLeft);
  CPPUNIT_TEST(testDoubleRotateRight);
  CPPUNIT_TEST(testEquality);
  CPPUNIT_TEST(testLessThan);
  CPPUNIT_TEST(testIntersects);
  CPPUNIT_TEST(testContains);
  CPPUNIT_TEST(testAccumulation);
  CPPUNIT_TEST(generalWTreeTest);
  CPPUNIT_TEST(mapTest);
  CPPUNIT_TEST(mapIntersectTest);
  CPPUNIT_TEST(setForEachBreakTest);

  CPPUNIT_TEST_SUITE_END();
public:
  typedef wtree_node<int> int_node;

  // Simple test of the rotate facilities.
  void testRotateLeft()
  {
    int_node a(5,
	       int_node(3, int_node(2, nil), int_node(4, nil), nil),
	       int_node(7, int_node(6, nil), int_node(8, nil), nil),
	       nil);

    int_node a_left = a.getLeft();
    int_node a_right = a.getRight();

    CPPUNIT_ASSERT(a_left.isValid());
    CPPUNIT_ASSERT(a_right.isValid());

    int_node a_left_left = a_left.getLeft();
    int_node a_left_right = a_left.getRight();
    int_node a_right_left = a_right.getLeft();
    int_node a_right_right = a_right.getRight();

    CPPUNIT_ASSERT(a_left_left.isValid());
    CPPUNIT_ASSERT(a_left_right.isValid());
    CPPUNIT_ASSERT(a_right_left.isValid());
    CPPUNIT_ASSERT(a_right_right.isValid());

    CPPUNIT_ASSERT_LEAF(a_left_left);
    CPPUNIT_ASSERT_LEAF(a_left_right);
    CPPUNIT_ASSERT_LEAF(a_right_left);
    CPPUNIT_ASSERT_LEAF(a_right_right);


    CPPUNIT_ASSERT_EQUAL(2, a_left_left.getVal());
    CPPUNIT_ASSERT_EQUAL(3, a_left.getVal());
    CPPUNIT_ASSERT_EQUAL(4, a_left_right.getVal());
    CPPUNIT_ASSERT_EQUAL(5, a.getVal());
    CPPUNIT_ASSERT_EQUAL(6, a_right_left.getVal());
    CPPUNIT_ASSERT_EQUAL(7, a_right.getVal());
    CPPUNIT_ASSERT_EQUAL(8, a_right_right.getVal());



    int_node b = a.left_rotate_single(nil);

    CPPUNIT_ASSERT_EQUAL(a.size(), b.size());

    CPPUNIT_ASSERT(b.isValid());

    int_node b_left = b.getLeft();
    int_node b_right = b.getRight();

    CPPUNIT_ASSERT(b_left.isValid());
    CPPUNIT_ASSERT(b_right.isValid());
    CPPUNIT_ASSERT_LEAF(b_right);

    int_node b_left_left = b_left.getLeft();
    int_node b_left_right = b_left.getRight();

    CPPUNIT_ASSERT(b_left_left.isValid());
    CPPUNIT_ASSERT(b_left_right.isValid());
    CPPUNIT_ASSERT_LEAF(b_left_right);

    int_node b_left_left_left = b_left_left.getLeft();
    int_node b_left_left_right = b_left_left.getRight();

    CPPUNIT_ASSERT(b_left_left_left.isValid());
    CPPUNIT_ASSERT(b_left_left_right.isValid());

    CPPUNIT_ASSERT_LEAF(b_left_left_left);
    CPPUNIT_ASSERT_LEAF(b_left_left_right);



    CPPUNIT_ASSERT_EQUAL(2, b_left_left_left.getVal());
    CPPUNIT_ASSERT_EQUAL(3, b_left_left.getVal());
    CPPUNIT_ASSERT_EQUAL(4, b_left_left_right.getVal());
    CPPUNIT_ASSERT_EQUAL(5, b_left.getVal());
    CPPUNIT_ASSERT_EQUAL(6, b_left_right.getVal());
    CPPUNIT_ASSERT_EQUAL(7, b.getVal());
    CPPUNIT_ASSERT_EQUAL(8, b_right.getVal());
  }

  void testRotateRight()
  {
    int_node a(5,
	       int_node(3, int_node(2, nil), int_node(4, nil), nil),
	       int_node(7, int_node(6, nil), int_node(8, nil), nil),
	       nil);

    int_node a_left = a.getLeft();
    int_node a_right = a.getRight();

    CPPUNIT_ASSERT(a_left.isValid());
    CPPUNIT_ASSERT(a_right.isValid());

    int_node a_left_left = a_left.getLeft();
    int_node a_left_right = a_left.getRight();
    int_node a_right_left = a_right.getLeft();
    int_node a_right_right = a_right.getRight();

    CPPUNIT_ASSERT(a_left_left.isValid());
    CPPUNIT_ASSERT(a_left_right.isValid());
    CPPUNIT_ASSERT(a_right_left.isValid());
    CPPUNIT_ASSERT(a_right_right.isValid());

    CPPUNIT_ASSERT_LEAF(a_left_left);
    CPPUNIT_ASSERT_LEAF(a_left_right);
    CPPUNIT_ASSERT_LEAF(a_right_left);
    CPPUNIT_ASSERT_LEAF(a_right_right);


    CPPUNIT_ASSERT_EQUAL(2, a_left_left.getVal());
    CPPUNIT_ASSERT_EQUAL(3, a_left.getVal());
    CPPUNIT_ASSERT_EQUAL(4, a_left_right.getVal());
    CPPUNIT_ASSERT_EQUAL(5, a.getVal());
    CPPUNIT_ASSERT_EQUAL(6, a_right_left.getVal());
    CPPUNIT_ASSERT_EQUAL(7, a_right.getVal());
    CPPUNIT_ASSERT_EQUAL(8, a_right_right.getVal());



    int_node c = a.right_rotate_single(nil);

    CPPUNIT_ASSERT_EQUAL(a.size(), c.size());
    CPPUNIT_ASSERT(c.isValid());

    int_node c_left = c.getLeft(), c_right = c.getRight();

    CPPUNIT_ASSERT(c_left.isValid());
    CPPUNIT_ASSERT(c_right.isValid());
    CPPUNIT_ASSERT_LEAF(c_left);

    int_node c_right_left = c_right.getLeft(), c_right_right = c_right.getRight();

    CPPUNIT_ASSERT(c_right_left.isValid());
    CPPUNIT_ASSERT(c_right_right.isValid());
    CPPUNIT_ASSERT_LEAF(c_right_left);

    int_node c_right_right_left  = c_right_right.getLeft();
    int_node c_right_right_right = c_right_right.getRight();

    CPPUNIT_ASSERT(c_right_right_left.isValid());
    CPPUNIT_ASSERT(c_right_right_right.isValid());
    CPPUNIT_ASSERT_LEAF(c_right_right_left);
    CPPUNIT_ASSERT_LEAF(c_right_right_right);

    CPPUNIT_ASSERT_EQUAL(2, c_left.getVal());
    CPPUNIT_ASSERT_EQUAL(3, c.getVal());
    CPPUNIT_ASSERT_EQUAL(4, c_right_left.getVal());
    CPPUNIT_ASSERT_EQUAL(5, c_right.getVal());
    CPPUNIT_ASSERT_EQUAL(6, c_right_right_left.getVal());
    CPPUNIT_ASSERT_EQUAL(7, c_right_right.getVal());
    CPPUNIT_ASSERT_EQUAL(8, c_right_right_right.getVal());
  }

  void testDoubleRotateLeft()
  {
    int_node a = int_node(3,
			  int_node(2, nil),
			  int_node(7,
				   int_node(5, int_node(4, nil),
					    int_node(6, nil), nil),
				   int_node(8, nil), nil),
			  nil);

    int_node a_left = a.getLeft(), a_right = a.getRight();

    CPPUNIT_ASSERT(a_left.isValid());
    CPPUNIT_ASSERT(a_right.isValid());
    CPPUNIT_ASSERT_LEAF(a_left);

    int_node a_right_left = a_right.getLeft(), a_right_right = a_right.getRight();

    CPPUNIT_ASSERT(a_right_left.isValid());
    CPPUNIT_ASSERT(a_right_right.isValid());
    CPPUNIT_ASSERT_LEAF(a_right_right);

    int_node a_right_left_left = a_right_left.getLeft();
    int_node a_right_left_right = a_right_left.getRight();

    CPPUNIT_ASSERT(a_right_left_left.isValid());
    CPPUNIT_ASSERT(a_right_left_right.isValid());
    CPPUNIT_ASSERT_LEAF(a_right_left_left);
    CPPUNIT_ASSERT_LEAF(a_right_left_right);


    CPPUNIT_ASSERT_EQUAL(2, a_left.getVal());
    CPPUNIT_ASSERT_EQUAL(3, a.getVal());
    CPPUNIT_ASSERT_EQUAL(4, a_right_left_left.getVal());
    CPPUNIT_ASSERT_EQUAL(5, a_right_left.getVal());
    CPPUNIT_ASSERT_EQUAL(6, a_right_left_right.getVal());
    CPPUNIT_ASSERT_EQUAL(7, a_right.getVal());
    CPPUNIT_ASSERT_EQUAL(8, a_right_right.getVal());


    int_node b = a.left_rotate_double(nil);

    CPPUNIT_ASSERT(b.isValid());

    int_node b_left = b.getLeft();
    int_node b_right = b.getRight();

    CPPUNIT_ASSERT(b_left.isValid());
    CPPUNIT_ASSERT(b_right.isValid());

    int_node b_left_left = b_left.getLeft(), b_left_right = b_left.getRight();

    CPPUNIT_ASSERT(b_left_left.isValid());
    CPPUNIT_ASSERT(b_left_right.isValid());

    CPPUNIT_ASSERT_LEAF(b_left_left);
    CPPUNIT_ASSERT_LEAF(b_left_right);

    int_node b_right_left = b_right.getLeft(), b_right_right = b_right.getRight();

    CPPUNIT_ASSERT(b_right_left.isValid());
    CPPUNIT_ASSERT(b_right_right.isValid());

    CPPUNIT_ASSERT_LEAF(b_right_left);
    CPPUNIT_ASSERT_LEAF(b_right_right);


    CPPUNIT_ASSERT_EQUAL(2, b_left_left.getVal());
    CPPUNIT_ASSERT_EQUAL(3, b_left.getVal());
    CPPUNIT_ASSERT_EQUAL(4, b_left_right.getVal());
    CPPUNIT_ASSERT_EQUAL(5, b.getVal());
    CPPUNIT_ASSERT_EQUAL(6, b_right_left.getVal());
    CPPUNIT_ASSERT_EQUAL(7, b_right.getVal());
    CPPUNIT_ASSERT_EQUAL(8, b_right_right.getVal());
  }

  void testDoubleRotateRight()
  {
    int_node a = int_node(7,
			  int_node(3,
				   int_node(2, nil),
				   int_node(5,
					    int_node(4, nil),
					    int_node(6, nil), nil),
				   nil),
			  int_node(8, nil), nil);

    CPPUNIT_ASSERT(a.isValid());

    int_node a_left = a.getLeft(), a_right = a.getRight();

    CPPUNIT_ASSERT(a_left.isValid());
    CPPUNIT_ASSERT(a_right.isValid());
    CPPUNIT_ASSERT_LEAF(a_right);

    int_node a_left_left = a_left.getLeft(), a_left_right = a_left.getRight();

    CPPUNIT_ASSERT(a_left_left.isValid());
    CPPUNIT_ASSERT(a_left_right.isValid());
    CPPUNIT_ASSERT_LEAF(a_left_left);

    int_node a_left_right_left = a_left_right.getLeft();
    int_node a_left_right_right = a_left_right.getRight();

    CPPUNIT_ASSERT(a_left_right_left.isValid());
    CPPUNIT_ASSERT(a_left_right_right.isValid());
    CPPUNIT_ASSERT_LEAF(a_left_right_left);
    CPPUNIT_ASSERT_LEAF(a_left_right_right);

    CPPUNIT_ASSERT_EQUAL(2, a_left_left.getVal());
    CPPUNIT_ASSERT_EQUAL(3, a_left.getVal());
    CPPUNIT_ASSERT_EQUAL(4, a_left_right_left.getVal());
    CPPUNIT_ASSERT_EQUAL(5, a_left_right.getVal());
    CPPUNIT_ASSERT_EQUAL(6, a_left_right_right.getVal());
    CPPUNIT_ASSERT_EQUAL(7, a.getVal());
    CPPUNIT_ASSERT_EQUAL(8, a_right.getVal());



    int_node b = a.right_rotate_double(nil);

    CPPUNIT_ASSERT(b.isValid());

    int_node b_left = b.getLeft(), b_right = b.getRight();
    CPPUNIT_ASSERT(b_left.isValid());
    CPPUNIT_ASSERT(b_right.isValid());

    int_node b_left_left = b_left.getLeft(), b_left_right = b_left.getRight();
    CPPUNIT_ASSERT(b_left_left.isValid());
    CPPUNIT_ASSERT(b_left_right.isValid());
    CPPUNIT_ASSERT_LEAF(b_left_left);
    CPPUNIT_ASSERT_LEAF(b_left_right);

    int_node b_right_left = b_right.getLeft(), b_right_right = b_right.getRight();
    CPPUNIT_ASSERT(b_right_left.isValid());
    CPPUNIT_ASSERT(b_right_right.isValid());
    CPPUNIT_ASSERT_LEAF(b_right_left);
    CPPUNIT_ASSERT_LEAF(b_right_right);


    CPPUNIT_ASSERT_EQUAL(2, b_left_left.getVal());
    CPPUNIT_ASSERT_EQUAL(3, b_left.getVal());
    CPPUNIT_ASSERT_EQUAL(4, b_left_right.getVal());
    CPPUNIT_ASSERT_EQUAL(5, b.getVal());
    CPPUNIT_ASSERT_EQUAL(6, b_right_left.getVal());
    CPPUNIT_ASSERT_EQUAL(7, b_right.getVal());
    CPPUNIT_ASSERT_EQUAL(8, b_right_right.getVal());
  }

  static bool WTreeValuesMatch(const set<int> &t,
			       const int *values, int count)
  {
    if(count == 0)
      return t.empty();
    else
      {
	set<int>::const_iterator i = t.begin();

	while(i != t.end() && count > 0)
	  {
	    if(*values != *i)
	      return false;

	    ++i;
	    ++values;
	    --count;
	  }

	return i == t.end() && count == 0;
      }
  }

  static void dumpMatchFailure(std::ostream &out,
			       const set<int> &t,
			       const int *values,
			       int count)
  {
    out << "Expected {";
    while(count > 0)
      {
	out << *values;
	if(count > 1)
	  out << ", ";

	++values;
	--count;
      }

    out << "}; got {";

    for(set<int>::const_iterator i = t.begin(); i != t.end(); ++i)
      {
	if(i != t.begin())
	  out << ", ";

	out << *i;
      }

    out << "}";
  }

  /** A macro to assert that a given wtree has exactly the listed
   *  contents.  A macro is used instead of a procedure so that line
   *  numbers in test failures are useful.
   */
#define assertWTreeValues(t, values, count) \
do  { \
  if(!WTreeValuesMatch(t, values, count)) \
    { \
      std::ostringstream out; \
      dumpMatchFailure(out, t, values, count); \
      CPPUNIT_FAIL(out.str()); \
    } } while(0)

  void testEquality()
  {
    set<int> a;
    set<int> b;

    CPPUNIT_ASSERT(a == b);

    a.insert(1);

    CPPUNIT_ASSERT(!(a == b));

    a.insert(6);
    a.insert(4);
    a.insert(5);
    a.insert(2);
    a.insert(3);

    CPPUNIT_ASSERT(!(a == b));

    b.insert(1);
    b.insert(2);
    b.insert(3);
    b.insert(4);
    b.insert(5);
    b.insert(6);

    int tmp[] = {1, 2, 3, 4, 5, 6};
    assertWTreeValues(a, tmp, 6);
    assertWTreeValues(b, tmp, 6);

    CPPUNIT_ASSERT(a == b);

    a.erase(4);

    CPPUNIT_ASSERT(!(a == b));

    a.insert(4);

    CPPUNIT_ASSERT(a == b);

    b.insert(10);

    CPPUNIT_ASSERT(!(a == b));

    b.erase(5);

    CPPUNIT_ASSERT(!(a == b));

    a.erase(5);

    CPPUNIT_ASSERT(!(a == b));

    a.insert(10);

    CPPUNIT_ASSERT(a == b);
  }

  void testLessThan()
  {
    set<int> a;
    set<int> b;

    a.insert(1);
    a.insert(2);
    a.insert(3);
    a.insert(4);
    a.insert(5);
    a.insert(6);

    b.insert(2);
    b.insert(3);
    b.insert(4);
    b.insert(5);
    b.insert(6);

    CPPUNIT_ASSERT(a < b);
    CPPUNIT_ASSERT(!(b < a));

    b.insert(1);

    CPPUNIT_ASSERT(!(a < b));
    CPPUNIT_ASSERT(!(b < a));

    a.erase(1);

    CPPUNIT_ASSERT(!(a < b));
    CPPUNIT_ASSERT(b < a);
  }

#define assertNoIntersect(a, b) \
  do { \
    if(a.intersects(b)) \
      { \
        std::ostringstream out; \
        out << "Sets " << a << " and " << b << " unexpectedly intersect:"\
            << std::endl; \
        a.dump(out); \
        b.dump(out); \
        CPPUNIT_FAIL(out.str()); \
      } \
     } while(0)

#define assertIntersect(a, b) \
  do { \
    if(!a.intersects(b)) \
      { \
        std::ostringstream out; \
        out << "Sets " << a << " and " << b << " unexpectedly fail to intersect:"\
            << std::endl; \
        a.dump(out); \
        b.dump(out); \
        CPPUNIT_FAIL(out.str()); \
      } \
     } while(0)

  void testIntersects()
  {
    set<int> a;
    set<int> b;

    a.insert(1);
    assertNoIntersect(a, b);
    assertNoIntersect(b, a);
    a.insert(3);
    assertNoIntersect(a, b);
    assertNoIntersect(b, a);
    a.insert(5);
    assertNoIntersect(a, b);
    assertNoIntersect(b, a);
    a.insert(7);
    assertNoIntersect(a, b);
    assertNoIntersect(b, a);

    b.insert(0);
    assertNoIntersect(a, b);
    assertNoIntersect(b, a);
    b.insert(2);
    assertNoIntersect(a, b);
    assertNoIntersect(b, a);
    b.insert(4);
    assertNoIntersect(a, b);
    assertNoIntersect(b, a);
    b.insert(6);
    assertNoIntersect(a, b);
    assertNoIntersect(b, a);
    b.insert(8);
    assertNoIntersect(a, b);
    assertNoIntersect(b, a);
    b.insert(5);
    assertIntersect(a, b);
    assertIntersect(b, a);
    b.erase(6);
    assertIntersect(a, b);
    assertIntersect(b, a);
    a.erase(1);
    assertIntersect(a, b);
    assertIntersect(b, a);
    a.erase(3);
    assertIntersect(a, b);
    assertIntersect(b, a);
    b.erase(8);
    assertIntersect(a, b);
    assertIntersect(b, a);
    b.erase(0);
    assertIntersect(a, b);
    assertIntersect(b, a);
    a.erase(5);
    assertNoIntersect(a, b);
    assertNoIntersect(b, a);
    b.erase(4);
    assertNoIntersect(a, b);
    assertNoIntersect(b, a);
    b.erase(5);
    assertNoIntersect(a, b);
    assertNoIntersect(b, a);
    b.erase(2);
    assertNoIntersect(a, b);
    assertNoIntersect(b, a);
  }

#define assertNotContains(a, b) \
  do { \
    if(a.contains(b)) \
      { \
        std::ostringstream out; \
        out << a << " unexpectedly contains " << b << ":"\
            << std::endl; \
        a.dump(out); \
        b.dump(out); \
        CPPUNIT_FAIL(out.str()); \
      } \
     } while(0)

#define assertContains(a, b) \
  do { \
    if(!a.contains(b)) \
      { \
        std::ostringstream out; \
        out << a << " unexpectedly does not contain " << b << ":"\
            << std::endl; \
        a.dump(out); \
        b.dump(out); \
        CPPUNIT_FAIL(out.str()); \
      } \
     } while(0)

  void testContains()
  {
    set<int> s1;
    set<int> s2;
    set<int> s3;

    s1.insert(5);

    s2.insert(6);
    s2.insert(8);

    s3.insert(10);
    s3.insert(5);

    set<int> t;

    assertContains(s1, t);
    assertContains(s2, t);
    assertContains(s3, t);
    assertNotContains(t, s1);
    assertNotContains(t, s2);
    assertNotContains(t, s3);



    t.insert(5);

    assertContains(s1, t);
    assertNotContains(s2, t);
    assertContains(s3, t);
    assertContains(t, s1);
    assertNotContains(t, s2);
    assertNotContains(t, s3);


    t.insert(10);

    assertNotContains(s1, t);
    assertNotContains(s2, t);
    assertContains(s3, t);
    assertContains(t, s1);
    assertNotContains(t, s2);
    assertContains(t, s3);

    t.insert(6);


    assertNotContains(s1, t);
    assertNotContains(s2, t);
    assertNotContains(s3, t);
    assertContains(t, s1);
    assertNotContains(t, s2);
    assertContains(t, s3);

    t.erase(5);

    assertNotContains(s1, t);
    assertNotContains(s2, t);
    assertNotContains(s3, t);
    assertNotContains(t, s1);
    assertNotContains(t, s2);
    assertNotContains(t, s3);


    t.insert(9);


    assertNotContains(s1, t);
    assertNotContains(s2, t);
    assertNotContains(s3, t);
    assertNotContains(t, s1);
    assertNotContains(t, s2);
    assertNotContains(t, s3);


    t.insert(8);

    assertNotContains(s1, t);
    assertNotContains(s2, t);
    assertNotContains(s3, t);
    assertNotContains(t, s1);
    assertContains(t, s2);
    assertNotContains(t, s3);


    t.erase(9);


    assertNotContains(s1, t);
    assertNotContains(s2, t);
    assertNotContains(s3, t);
    assertNotContains(t, s1);
    assertContains(t, s2);
    assertNotContains(t, s3);
  }

  class maxAccumOps
  {
  public:
    int empty() const { return INT_MIN; }
    int project(const std::pair<int, int> &p) const { return p.first; }
    int merge(int n, int m) const { return std::max<int>(n, m); }
  };

  void testAccumulation()
  {
    // Build a set of pairs where we accumulate the maximum of the
    // first element in the pair.
    set<std::pair<int, int>,
        aptitude::util::compare3_f<std::pair<int, int> >,
        int,
        maxAccumOps> s;

    CPPUNIT_ASSERT_EQUAL(s.getAccumVal(), INT_MIN);

    s.insert(std::make_pair(5, 20));
    CPPUNIT_ASSERT_EQUAL(s.getAccumVal(), 5);

    s.insert(std::make_pair(10, 5));
    CPPUNIT_ASSERT_EQUAL(s.getAccumVal(), 10);

    s.insert(std::make_pair(5, 10));
    CPPUNIT_ASSERT_EQUAL(s.getAccumVal(), 10);

    s.insert(std::make_pair(-1, 4));
    CPPUNIT_ASSERT_EQUAL(s.getAccumVal(), 10);

    s.insert(std::make_pair(1, 6));
    CPPUNIT_ASSERT_EQUAL(s.getAccumVal(), 10);

    s.erase(std::make_pair(10, 5));
    CPPUNIT_ASSERT_EQUAL(s.getAccumVal(), 5);

    s.erase(std::make_pair(5, 20));
    CPPUNIT_ASSERT_EQUAL(s.getAccumVal(), 5);

    s.insert(std::make_pair(2, 4));
    CPPUNIT_ASSERT_EQUAL(s.getAccumVal(), 5);

    s.erase(std::make_pair(5, 10));
    CPPUNIT_ASSERT_EQUAL(s.getAccumVal(), 2);

    s.erase(std::make_pair(2, 4));
    CPPUNIT_ASSERT_EQUAL(s.getAccumVal(), 1);

    s.erase(std::make_pair(1, 6));
    CPPUNIT_ASSERT_EQUAL(s.getAccumVal(), -1);

    s.erase(std::make_pair(-1, 4));
    CPPUNIT_ASSERT_EQUAL((unsigned)0, s.size());
    CPPUNIT_ASSERT(s.empty());
    CPPUNIT_ASSERT_EQUAL(s.getAccumVal(), INT_MIN);
  }

#define assertNotSupermap(a, b) \
  do { \
    if(a.is_supermap_of(b)) \
      { \
        std::ostringstream out; \
        out << a << " unexpectedly is a supermap of " << b << ":"\
            << std::endl; \
        a.dump(out); \
        b.dump(out); \
        CPPUNIT_FAIL(out.str()); \
      } \
     } while(0)

#define assertSupermap(a, b) \
  do { \
    if(!a.is_supermap_of(b)) \
      { \
        std::ostringstream out; \
        out << a << " unexpectedly is not a supermap of " << b << ":"\
            << std::endl; \
        a.dump(out); \
        b.dump(out); \
        CPPUNIT_FAIL(out.str()); \
      } \
     } while(0)

  void testSupermap()
  {
    map<int, int> s1;
    map<int, int> s2;
    map<int, int> s3;

    s1.put(5, 6);

    s2.put(6, 2);
    s2.put(8, 10);

    s3.put(10, 3);
    s3.put(5, 7);

    map<int, int> t;

    assertSupermap(s1, t);
    assertSupermap(s2, t);
    assertSupermap(s3, t);
    assertNotSupermap(t, s1);
    assertNotSupermap(t, s2);
    assertNotSupermap(t, s3);



    t.put(5, 2);

    assertNotSupermap(s1, t);
    assertNotSupermap(s2, t);
    assertNotSupermap(s3, t);
    assertNotSupermap(t, s1);
    assertNotSupermap(t, s2);
    assertNotSupermap(t, s3);


    t.put(5, 6);

    assertSupermap(s1, t);
    assertNotSupermap(s2, t);
    assertNotSupermap(s3, t);
    assertSupermap(t, s1);
    assertNotSupermap(t, s2);
    assertNotSupermap(t, s3);


    t.put(5, 7);

    assertNotSupermap(s1, t);
    assertNotSupermap(s2, t);
    assertSupermap(s3, t);
    assertNotSupermap(t, s1);
    assertNotSupermap(t, s2);
    assertNotSupermap(t, s3);


    t.put(10, 3);
    t.put(5, 6);

    assertNotSupermap(s1, t);
    assertNotSupermap(s2, t);
    assertNotSupermap(s3, t);
    assertNotSupermap(t, s1);
    assertNotSupermap(t, s2);
    assertNotSupermap(t, s3);

    t.put(5, 7);
    t.put(10, 3);

    assertNotSupermap(s1, t);
    assertNotSupermap(s2, t);
    assertSupermap(s3, t);
    assertSupermap(t, s1);
    assertNotSupermap(t, s2);
    assertSupermap(t, s3);

    t.put(6, 2);


    assertNotSupermap(s1, t);
    assertNotSupermap(s2, t);
    assertNotSupermap(s3, t);
    assertSupermap(t, s1);
    assertNotSupermap(t, s2);
    assertSupermap(t, s3);

    t.erase(5);

    assertNotSupermap(s1, t);
    assertNotSupermap(s2, t);
    assertNotSupermap(s3, t);
    assertNotSupermap(t, s1);
    assertNotSupermap(t, s2);
    assertNotSupermap(t, s3);


    t.put(9, 100);


    assertNotSupermap(s1, t);
    assertNotSupermap(s2, t);
    assertNotSupermap(s3, t);
    assertNotSupermap(t, s1);
    assertNotSupermap(t, s2);
    assertNotSupermap(t, s3);


    t.put(8, 10);

    assertNotSupermap(s1, t);
    assertNotSupermap(s2, t);
    assertNotSupermap(s3, t);
    assertNotSupermap(t, s1);
    assertSupermap(t, s2);
    assertNotSupermap(t, s3);


    t.erase(9);


    assertNotSupermap(s1, t);
    assertNotSupermap(s2, t);
    assertNotSupermap(s3, t);
    assertNotSupermap(t, s1);
    assertSupermap(t, s2);
    assertNotSupermap(t, s3);
  }

  void generalWTreeTest()
  {
    set<int> t;

    assertWTreeValues(t, NULL, 0);

    {
      t.insert(5);
      int vals[] = {5};
      assertWTreeValues(t, vals, 1);
    }

    CPPUNIT_ASSERT(!t.find_node(3).isValid());

    {
      wtree_node<int> found = t.find_node(5);
      CPPUNIT_ASSERT(found.isValid());
      CPPUNIT_ASSERT_EQUAL(5, found.getVal());
    }


    {
      t.insert(3);
      int vals[] = {3, 5};
      assertWTreeValues(t, vals, 2);
    }

    {
      t.insert(8);
      int vals[] = {3, 5, 8};
      assertWTreeValues(t, vals, 3);
    }


    {
      t.insert(7);
      int vals[] = {3, 5, 7, 8};
      assertWTreeValues(t, vals, 4);
    }


    CPPUNIT_ASSERT(!t.find_node(9).isValid());

    {
      wtree_node<int> found = t.find_node(5);

      CPPUNIT_ASSERT(found.isValid());
      CPPUNIT_ASSERT_EQUAL(5, found.getVal());
    }

    {
      t.insert(4);
      int vals[] = {3, 4, 5, 7, 8};
      assertWTreeValues(t, vals, 5);
    }


    {
      t.insert(3);
      int vals[] = {3, 4, 5, 7, 8};
      assertWTreeValues(t, vals, 5);
    }


    {
      t.insertUpdate(3);
      int vals[] = {3, 4, 5, 7, 8};
      assertWTreeValues(t, vals, 5);
    }


    {
      t.insertUpdate(6);
      int vals[] = {3, 4, 5, 6, 7, 8};
      assertWTreeValues(t, vals, 6);
    }


    {
      t.erase(5);
      int vals[] = {3, 4, 6, 7, 8};
      assertWTreeValues(t, vals, 5);
    }


    {
      t.erase(7);
      int vals[] = {3, 4, 6, 8};
      assertWTreeValues(t, vals, 4);
    }

    {
      t.erase(7);
      int vals[] = {3, 4, 6, 8};
      assertWTreeValues(t, vals, 4);
    }

    {
      t.erase(10);
      int vals[] = {3, 4, 6, 8};
      assertWTreeValues(t, vals, 4);
    }


    {
      t.erase(-1);
      int vals[] = {3, 4, 6, 8};
      assertWTreeValues(t, vals, 4);
    }


    {
      t.erase(4);
      int vals[] = {3, 6, 8};
      assertWTreeValues(t, vals, 3);
    }


    {
      t.erase(8);
      int vals[] = {3, 6};
      assertWTreeValues(t, vals, 2);
    }



    {
      t.erase(3);
      int vals[] = {6};
      assertWTreeValues(t, vals, 1);
    }



    {
      t.erase(6);
      int *vals = NULL;
      assertWTreeValues(t, vals, 0);
    }

    // Test for a specific error that occurs when deleting a node for
    // which the smallest element of the right child has its own right
    // child.
    //
    // \todo this is tuned for a specific balancing implementation and
    // might not detect an equivalent error in another implementation.
    {
      t = imm::set<int>();
      t.insert(50);
      t.insert(75);
      t.insert(25);
      t.insert(62);
      t.insert(87);
      t.insert(68);
      t.erase(50);

      int vals[] = {25, 62, 68, 75, 87};
      assertWTreeValues(t, vals, 5);
    }
  }

  void mapTest()
  {
    map<int, int> m;

    CPPUNIT_ASSERT(m.empty());
    CPPUNIT_ASSERT_EQUAL(0U, m.size());

    m.put(5, 2);
    CPPUNIT_ASSERT(!m.empty());
    CPPUNIT_ASSERT_EQUAL(1U, m.size());

    m.put(3, 10);
    CPPUNIT_ASSERT(!m.empty());
    CPPUNIT_ASSERT_EQUAL(2U, m.size());

    m.put(4, 10);
    CPPUNIT_ASSERT(!m.empty());
    CPPUNIT_ASSERT_EQUAL(3U, m.size());

    m.put(1, 3);
    CPPUNIT_ASSERT(!m.empty());
    CPPUNIT_ASSERT_EQUAL(4U, m.size());

    m.put(2, 8);
    CPPUNIT_ASSERT(!m.empty());
    CPPUNIT_ASSERT_EQUAL(5U, m.size());

    m.put(6, 11);
    CPPUNIT_ASSERT(!m.empty());
    CPPUNIT_ASSERT_EQUAL(6U, m.size());

    m.put(1, 0);
    CPPUNIT_ASSERT(!m.empty());
    CPPUNIT_ASSERT_EQUAL(6U, m.size());


    CPPUNIT_ASSERT_EQUAL(-1, m.get(0, -1));
    CPPUNIT_ASSERT_EQUAL(0, m.get(1, -1));
    CPPUNIT_ASSERT_EQUAL(8, m.get(2, -1));
    CPPUNIT_ASSERT_EQUAL(10, m.get(3, -1));
    CPPUNIT_ASSERT_EQUAL(10, m.get(4, -1));
    CPPUNIT_ASSERT_EQUAL(2, m.get(5, -1));
    CPPUNIT_ASSERT_EQUAL(11, m.get(6, -1));

    m.put(6, 12);
    CPPUNIT_ASSERT_EQUAL(12, m.get(6, -1));
  }

  template<typename T1, typename T2>
  struct second_less
  {
    typedef std::pair<T1, T2> X;

    bool operator()(const X &x1, const X &x2) const
    {
      return x1.second < x2.second;
    }
  };

  template<typename T1, typename T2>
  struct second_greater
  {
    typedef std::pair<T1, T2> X;

    bool operator()(const X &x1, const X &x2) const
    {
      return x1.second > x2.second;
    }
  };

  typedef second_less<int, int> int_mapping_less;
  typedef second_greater<int, int> int_mapping_greater;

  void mapIntersectTest()
  {
    imm::map<int, int> m1, m2;

    m1.put(3, 5);
    m1.put(6, 1);
    m1.put(2, 8);
    m1.put(10, 7);
    m1.put(0, 3);
    m1.put(30, 5);
    m1.put(20, 7);

    m2.put(1, 4);
    m2.put(7, 9);
    m2.put(-10, 5);
    m2.put(-5, 1);
    m2.put(-7, 50);
    m2.put(-100, -8);

    CPPUNIT_ASSERT(!m1.shares_value(m2));
    CPPUNIT_ASSERT(!m1.domain_intersects(m2));

    m2.put(6, 1);
    CPPUNIT_ASSERT(m1.shares_value(m2));
    CPPUNIT_ASSERT(m1.domain_intersects(m2));

    m2.put(6, 5);


    CPPUNIT_ASSERT(!m1.shares_value(m2));
    CPPUNIT_ASSERT(m1.domain_intersects(m2));
    CPPUNIT_ASSERT(m1.has_related_mapping(m2, std::less<std::pair<int, int> >()));
    CPPUNIT_ASSERT(m2.has_related_mapping(m1, std::greater<std::pair<int, int> >()));

    m2.put(6, 0);

    CPPUNIT_ASSERT(!m1.shares_value(m2));
    CPPUNIT_ASSERT(m1.domain_intersects(m2));
    CPPUNIT_ASSERT(!m1.has_related_mapping(m2, std::less<std::pair<int, int> >()));
    CPPUNIT_ASSERT(m1.has_related_mapping(m2, std::greater<std::pair<int, int> >()));
    CPPUNIT_ASSERT(!m2.has_related_mapping(m1, std::greater<std::pair<int, int> >()));
    CPPUNIT_ASSERT(m2.has_related_mapping(m1, std::less<std::pair<int, int> >()));
  }

  // Set the Nth entry in the given array to true, until the entry
  // number is above 5.
  struct set_or_break
  {
    bool *array;

    set_or_break(bool *_array) : array(_array)
    {
    }

    bool operator()(int n) const
    {
      array[n] = true;

      return n <= 5;
    }
  };

  void setForEachBreakTest()
  {
    const int array_len = 20;
    bool array[array_len];

    for(int i = 0; i < array_len; ++i)
      array[i] = false;
    {
      imm::set<int> vals;

      vals.insert(2);
      CPPUNIT_ASSERT(vals.for_each(set_or_break(array)));

      for(int i = 0; i < array_len; ++i)
	{
	  CPPUNIT_ASSERT_EQUAL(vals.contains(i), array[i]);
	}
    }


    for(int i = 0; i < array_len; ++i)
      array[i] = false;
    {
      imm::set<int> vals;

      vals.insert(2);
      vals.insert(4);
      vals.insert(5);
      vals.insert(6);
      vals.insert(8);
      CPPUNIT_ASSERT(!vals.for_each(set_or_break(array)));

      // Now everything *up to and including* 6 should be set.
      for(int i = 0; i < array_len; ++i)
	{
	  CPPUNIT_ASSERT_EQUAL(vals.contains(i) && i <= 6, array[i]);
	}
    }
  }
};

CPPUNIT_TEST_SUITE_REGISTRATION(WTreeTest);
