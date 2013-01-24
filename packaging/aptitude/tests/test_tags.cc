// Tester for the tag parser.
//
//   Copyright (C) 2005, 2008 Daniel Burrows
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif


// TODO: These tests need updating for recent (2012-06-30) changes
// to out interface to debtags.
#if 0

#include <cppunit/extensions/HelperMacros.h>

#include <generic/apt/tags.h>

#include <string.h>

#include <sstream>

std::ostream &operator<<(std::ostream &out, const tag &t)
{
  return out << t.str();
}

#define CPPUNIT_ASSERT_GT(a, b) \
  do { if(!((a) > (b))) \
         {std::ostringstream out; \
          out << "comparison assertion failed" << std::endl; \
          out << "\"" << (a) << "\" is not > \"" << (b) << "\"" << std::endl; \
          CppUnit :: Asserter :: fail(out.str()); } } while(0)

#define CPPUNIT_ASSERT_LT(a, b) \
  do { if(!((a) < (b))) \
         {std::ostringstream out; \
          out << "comparison assertion failed" << std::endl; \
          out << "\"" << (a) << "\" is not < \"" << (b) << "\"" << std::endl; \
          CppUnit :: Asserter :: fail(out.str()); } } while(0)

#define CPPUNIT_ASSERT_LTEQUAL(a, b) \
  do { if(!((a) <= (b))) \
         {std::ostringstream out; \
          out << "comparison assertion failed" << std::endl; \
          out << "\"" << (a) << "\" is not <= \"" << (b) << "\"" << std::endl; \
          CppUnit :: Asserter :: fail(out.str()); } } while(0)

#define CPPUNIT_ASSERT_GTEQUAL(a, b) \
  do { if(!((a) >= (b))) \
         {std::ostringstream out; \
          out << "comparison assertion failed" << std::endl; \
          out << "\"" << (a) << "\" is not >= \"" << (b) << "\"" << std::endl; \
          CppUnit :: Asserter :: fail(out.str()); } } while(0)

class TagTest : public CppUnit::TestFixture {
  CPPUNIT_TEST_SUITE(TagTest);

  CPPUNIT_TEST(testTagTraverse);
  CPPUNIT_TEST(testTagStr);
  CPPUNIT_TEST(testTagEqual);
  CPPUNIT_TEST(testTagOrder);
  CPPUNIT_TEST(testTagListTraverse);

  CPPUNIT_TEST_SUITE_END();
private:
  static tag parseTag(const std::string &s)
  {
    return tag(s.begin(), s.end());
  }

  // These should all parse to (tag1, tag2, tag3):
  static const char *basicTagEntries[];
  // Erroneous entries.  Should parse as having no tags at all.
  static const char *badEntry1;
  static const char *badEntry2;
  static const char *badEntry3;
  static const char *badEntry4;

  // Tag order checks.  The tag ordering function should say that
  // forall n . tagorder[n]<tagorder[n+1]
  static const char *tagorder[];
  // Tag list checks.
  static const char *tagLists[];
  // One more test.
  static const char *tagListLast;

  // Checks that the given tag is "tag1::tag2::tag3".
  void testTag123(const tag &t)
  {
    tag::const_iterator ti = t.begin();

    CPPUNIT_ASSERT(ti != t.end());
    CPPUNIT_ASSERT_EQUAL(string("tag1"), *ti);

    ++ti;


    CPPUNIT_ASSERT(ti != t.end());
    CPPUNIT_ASSERT_EQUAL(string("tag2"), *ti);

    ++ti;


    CPPUNIT_ASSERT(ti != t.end());
    CPPUNIT_ASSERT_EQUAL(string("tag3"), *ti);

    ++ti;

    CPPUNIT_ASSERT(ti == t.end());
  }

  void testTagTraverse()
  {
    const char **s=basicTagEntries;
    while(*s)
      {
	tag parsed = parseTag(*s);
	testTag123(parsed);
	++s;
      }
  }

  void testTagStr()
  {
    const char **s=basicTagEntries;
    while(*s)
      {
	tag parsed=parseTag(*s);
	CPPUNIT_ASSERT_EQUAL(string("tag1::tag2::tag3"), parsed.str());
	++s;
      }
  }

  void testTagEqual()
  {
    for(const char **s=basicTagEntries; *s; ++s)
      for(const char **t=basicTagEntries; *t; ++t)
	CPPUNIT_ASSERT_EQUAL(parseTag(*s), parseTag(*t));
  }

  void testTagOrder()
  {
    for(const char **s = tagorder; *s; ++s)
      {
	tag ps = parseTag(*s);

	CPPUNIT_ASSERT_EQUAL(ps, ps);

	for(const char **t = s; *t; ++t)
	  {
	    tag pt = parseTag(*t);

	    CPPUNIT_ASSERT_LTEQUAL(ps, pt);
	    CPPUNIT_ASSERT_GTEQUAL(pt, ps);

	    if(t != s)
	      {
		CPPUNIT_ASSERT_LT(ps, pt);
		CPPUNIT_ASSERT_GT(pt, ps);
	      }
	  }
      }
  }

  void testTagListTraverse()
  {
    for(const char **l = tagLists; *l; ++l)
      {
	tag_list lp(*l, (*l)+strlen(*l));

	tag_list :: const_iterator i = lp.begin();

	CPPUNIT_ASSERT(i != lp.end());
	CPPUNIT_ASSERT_EQUAL(string("a::b"), (*i).str());

	++i;

	CPPUNIT_ASSERT(i != lp.end());
	CPPUNIT_ASSERT_EQUAL(string("c::d"), (*i).str());

	++i;

	CPPUNIT_ASSERT(i != lp.end());
	CPPUNIT_ASSERT_EQUAL(string("e::f"), (*i).str());

	++i;

	CPPUNIT_ASSERT(i == lp.end());
      }
  }
};

const char *TagTest::basicTagEntries[]={
  "tag1::tag2::tag3",
  "  tag1::tag2::tag3",
  "tag1::tag2::tag3  ",
  "  tag1::tag2::tag3  ",
  0
};

// Erroneous entries.  Should parse as having no tags at all.
const char *TagTest::badEntry1="";
const char *TagTest::badEntry2=0;
const char *TagTest::badEntry3="   ";
const char *TagTest::badEntry4="   ::    ";

// Tag order checks.  The tag ordering function should say that
// forall n . tagorder[n]<tagorder[n+1]
const char *TagTest::tagorder[]={
  "tag1::tag2::tag3  ",
  "  tag1::tag3::tag3",
  "tag2::tag1::tag1",
  " tag3::tag2::tag2",
  "tag3::tag2::tag3",
  0
};

// Tag list checks.
const char *TagTest::tagLists[] = {
  "a::b,c::d,e::f",
  "a::b, c::d, e::f",
  "  a::b,c::d,    e::f",
  "a::b   ,   c::d   ,   e::f",
  0
};

// One more test.
const char *TagTest::tagListLast="  a  :: b";

CPPUNIT_TEST_SUITE_REGISTRATION(TagTest);

#endif
