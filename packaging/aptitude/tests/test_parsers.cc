// test_parsers.cc
//
// Copyright (C) 2010 Daniel Burrows
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

#include <generic/util/parsers.h>

#include <cppunit/extensions/HelperMacros.h>

#include <boost/fusion/container/generation/make_vector.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/numeric/conversion/cast.hpp>

using namespace parsers;

typedef std::string::const_iterator::difference_type iter_difftype;

template<typename T>
std::ostream &operator<<(std::ostream &out, const boost::optional<T> &o)
{
  if(o)
    return out << "Just " << *o;
  else
    return out << "Nothing";
}

template<typename T>
std::ostream &operator<<(std::ostream &out, const std::vector<T> &v)
{
  out << "[";

  bool first = true;
  for(typename std::vector<T>::const_iterator it = v.begin(); it != v.end(); ++it)
    {
      if(first)
        first = false;
      else
        out << ", ";

      out << *it;
    }

  out << "]";

  return out;
}

// Used to make it easier to compare results that are vectors.
class strip_shared_ptrs_result
{
  template<typename T>
  struct apply
  {
    typedef T type;
  };

  template<typename T>
  struct apply<boost::shared_ptr<T> >
  {
    typedef typename apply<T>::type type;
  };

  template<typename T>
  struct apply<std::vector<T> >
  {
    typedef std::vector<typename apply<T>::type > type;
  };
};

BOOST_STATIC_ASSERT( (boost::is_same<std::vector<std::vector<int> >,
                      strip_shared_ptrs_result::apply<boost::shared_ptr<std::vector<std::vector<boost::shared_ptr<int> > > > >::type>::value) );

template<typename T>
T strip_shared_ptrs(const T &t)
{
  return t;
}

// Without this forward declaration, the mutual recursion between the
// two specializations below doesn't work (the shared_ptr
// specialization invokes the default specialization instead of the
// std::vector one).
template<typename T>
typename std::vector<typename strip_shared_ptrs_result::apply<T>::type>
strip_shared_ptrs(const std::vector<T> &v);

template<typename T>
typename strip_shared_ptrs_result::apply<T>::type
strip_shared_ptrs(const boost::shared_ptr<T> &t)
{
  return strip_shared_ptrs(*t);
}

template<typename T>
typename std::vector<typename strip_shared_ptrs_result::apply<T>::type>
strip_shared_ptrs(const std::vector<T> &v)
{
  std::vector<typename strip_shared_ptrs_result::apply<T>::type> rval;

  for(typename std::vector<T>::const_iterator it = v.begin(); it != v.end(); ++it)
    rval.push_back(strip_shared_ptrs(*it));

  return rval;
}

template<typename P>
void verify_success(const std::string &input,
                    const P &parser,
                    const typename strip_shared_ptrs_result::apply<typename P::result_type>::type &expected)
{
  boost::variant<typename P::result_type, ParseException> result =
    parse(input, parser);

  if(boost::get<ParseException>(&result) != NULL)
    CPPUNIT_FAIL(boost::get<ParseException>(result).what());

  CPPUNIT_ASSERT(boost::get<typename P::result_type>(&result) != NULL);

  typename strip_shared_ptrs_result::apply<typename P::result_type>::type
    actual_result(strip_shared_ptrs(boost::get<typename P::result_type>(result)));
  CPPUNIT_ASSERT_EQUAL(expected, actual_result);
}

template<typename P>
void verify_failure(const std::string &input,
                    const P &parser)
{
  boost::variant<typename P::result_type, ParseException> result =
    parse(input, parser);

  CPPUNIT_ASSERT(boost::get<ParseException>(&result) != NULL);
}

class ParsersTest : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(ParsersTest);

  CPPUNIT_TEST(testParseChar);
  CPPUNIT_TEST(testParseAnyChar);
  CPPUNIT_TEST(testWhitespace);
  CPPUNIT_TEST(testInteger);
  CPPUNIT_TEST(testIntegerInvalid);
  CPPUNIT_TEST(testEof);
  CPPUNIT_TEST(testStr);
  CPPUNIT_TEST(testVal);
  CPPUNIT_TEST(testAndThenBothMatch);
  CPPUNIT_TEST(testAndThenBothMatchChained);
  CPPUNIT_TEST(testAndThenOnlyFirstMatches);
  CPPUNIT_TEST(testAndThenOnlyFirstMatchesChained);
  CPPUNIT_TEST(testAndThenFirstFailsAndConsumesInput);
  CPPUNIT_TEST(testAndThenFirstFailsWithoutConsumingInput);
  CPPUNIT_TEST(testAndThenFirstFailsWithoutConsumingInputChained);
  CPPUNIT_TEST(testAndFirstBothMatch);
  CPPUNIT_TEST(testAndFirstBothMatchChained);
  CPPUNIT_TEST(testAndFirstOnlyFirstMatches);
  CPPUNIT_TEST(testAndFirstOnlyFirstMatchesChained);
  CPPUNIT_TEST(testAndFirstFirstFailsAndConsumesInput);
  CPPUNIT_TEST(testAndFirstFirstFailsWithoutConsumingInput);
  CPPUNIT_TEST(testAndFirstFirstFailsWithoutConsumingInputChained);
  CPPUNIT_TEST(testSetExpected);
  CPPUNIT_TEST(testForeachEmptyEndNotEOF);
  CPPUNIT_TEST(testForeachEmptyEndEOF);
  CPPUNIT_TEST(testForeachNotEmptyEndNotEOF);
  CPPUNIT_TEST(testForeachNotEmptyEndEOF);
  CPPUNIT_TEST(testForeachFailure);
  CPPUNIT_TEST(testSkipManyEmptyEndNotEOF);
  CPPUNIT_TEST(testSkipManyEmptyEndEOF);
  CPPUNIT_TEST(testSkipManyNotEmptyEndNotEOF);
  CPPUNIT_TEST(testSkipManyNotEmptyEndEOF);
  CPPUNIT_TEST(testSkipManyPlusEmptyEndNotEOF);
  CPPUNIT_TEST(testSkipManyPlusEmptyEndEOF);
  CPPUNIT_TEST(testSkipManyPlusNotEmptyEndNotEOF);
  CPPUNIT_TEST(testSkipManyPlusNotEmptyEndEOF);
  CPPUNIT_TEST(testManyEmptyEndNotEOF);
  CPPUNIT_TEST(testManyEmptyEndEOF);
  CPPUNIT_TEST(testManyNotEmptyEndNotEOF);
  CPPUNIT_TEST(testManyNotEmptyEndEOF);
  CPPUNIT_TEST(testManyFailure);
  CPPUNIT_TEST(testOrFirstBranchMatches);
  CPPUNIT_TEST(testOrFirstBranchFailsAndConsumesInput);
  CPPUNIT_TEST(testOrSecondBranchMatches);
  CPPUNIT_TEST(testOrSecondBranchFailsAndConsumesInput);
  CPPUNIT_TEST(testOrSecondBranchFailsAndConsumesNoInput);
  CPPUNIT_TEST(testOrChainedSuccess);
  CPPUNIT_TEST(testOrChainedFailure);
  CPPUNIT_TEST(testOrChainedCollapsesParsers);
  CPPUNIT_TEST(testMaybeFailure);
  CPPUNIT_TEST(testMaybeSuccess);
  CPPUNIT_TEST(testMaybeValue);
  CPPUNIT_TEST(testTupleSuccess);
  CPPUNIT_TEST(testTupleFailureWithoutConsumingInput);
  CPPUNIT_TEST(testTupleFailureWithConsumingInput);
  CPPUNIT_TEST(testApplySuccess);
  CPPUNIT_TEST(testApplyFailure);
  CPPUNIT_TEST(testFollowedBySuccess);
  CPPUNIT_TEST(testFollowedByFailure);
  CPPUNIT_TEST(testNotFollowedBySuccess);
  CPPUNIT_TEST(testNotFollowedByFailure);
  CPPUNIT_TEST(testPostAssertSuccess);
  CPPUNIT_TEST(testPostAssertFailure);
  CPPUNIT_TEST(testManyPlusSuccess);
  CPPUNIT_TEST(testManyPlusFailure);
  CPPUNIT_TEST(testOptionalSuccess);
  CPPUNIT_TEST(testOptionalFailure);
  CPPUNIT_TEST(testSepBySuccessEmpty);
  CPPUNIT_TEST(testSepBySuccessNonempty);
  CPPUNIT_TEST(testSepByFailureInFirstElement);
  CPPUNIT_TEST(testSepByFailureInSeparator);
  CPPUNIT_TEST(testSepByFailureInSecondElement);
  CPPUNIT_TEST(testSepByTrailingSeparator);
  CPPUNIT_TEST(testLexemeSuccess);
  CPPUNIT_TEST(testLexemeFailure);
  CPPUNIT_TEST(testBetweenSuccess);
  CPPUNIT_TEST(testBetweenFailure);
  CPPUNIT_TEST(testErrorPositionInformation);
  CPPUNIT_TEST(testContainerSuccess);
  CPPUNIT_TEST(testContainerOr);
  CPPUNIT_TEST(testConcatenateInts);
  CPPUNIT_TEST(testConcatenateMany);
  CPPUNIT_TEST(testConcatenateSepBy);
  CPPUNIT_TEST(testConcatenateOptional);

  CPPUNIT_TEST_SUITE_END();

public:

  void testParseChar()
  {
    ch_p<char> comma(','), semicolon(';');

    std::string input(",;;,,,;a");

    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_THROW(semicolon.parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());

    CPPUNIT_ASSERT_EQUAL(',', comma.parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());

    CPPUNIT_ASSERT_THROW(comma.parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());

    CPPUNIT_ASSERT_EQUAL(';', semicolon.parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)2, begin - input.begin());

    CPPUNIT_ASSERT_EQUAL(';', semicolon.parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)3, begin - input.begin());

    CPPUNIT_ASSERT_EQUAL(',', comma.parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)4, begin - input.begin());

    CPPUNIT_ASSERT_THROW(semicolon.parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)4, begin - input.begin());

    CPPUNIT_ASSERT_EQUAL(',', comma.parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)5, begin - input.begin());

    CPPUNIT_ASSERT_EQUAL(',', comma.parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)6, begin - input.begin());

    CPPUNIT_ASSERT_EQUAL(';', semicolon.parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)7, begin - input.begin());

    CPPUNIT_ASSERT_THROW(comma.parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)7, begin - input.begin());

    CPPUNIT_ASSERT_THROW(semicolon.parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)7, begin - input.begin());
  }

  void testParseAnyChar()
  {
    anychar_p<char> any;
    std::string input = "abcdefg";
    std::string::const_iterator begin = input.begin(), end = input.end();

    for(std::string::size_type i = 0; i < input.size(); ++i)
      {
        CPPUNIT_ASSERT_EQUAL(i, boost::numeric_cast<std::string::size_type>((begin - input.begin())));
        CPPUNIT_ASSERT_EQUAL(input[i], any.parse(begin, end));
      }

    CPPUNIT_ASSERT_EQUAL(input.size(), boost::numeric_cast<std::string::size_type>(begin - input.begin()));
  }

  void testWhitespace()
  {
    space_p sp(space());

    std::string input = " b";

    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_EQUAL(' ', sp.parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());

    CPPUNIT_ASSERT_THROW(sp.parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());
  }

  void testInteger()
  {
    integer_p integer;

    for(int i = -1000; i <= 1000; ++i)
      {
        std::string input = boost::lexical_cast<std::string>(i) + "Q";

        std::string::const_iterator begin = input.begin(), end = input.end();
        CPPUNIT_ASSERT_EQUAL(i, integer.parse(begin, end));
        CPPUNIT_ASSERT_EQUAL((iter_difftype)1, input.end() - begin);
      }

    {
      std::string input = boost::lexical_cast<std::string>(INT_MIN) + "Q";
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_EQUAL(INT_MIN, integer.parse(begin, end));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)1, input.end() - begin);
    }

    {
      std::string input = boost::lexical_cast<std::string>(INT_MAX) + "Q";
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_EQUAL(INT_MAX, integer.parse(begin, end));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)1, input.end() - begin);
    }
  }

  void testIntegerInvalid()
  {
    integer_p integer;

    // Try something that's just not an integer at all (but has
    // integer bits later in the string).
    {
      std::string input = "abc123";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW(integer.parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
    }

    // If we see a lone hyphen, we should eat it and fail.
    {
      std::string input = "-abc123";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW(integer.parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());
    }

    // The only other failure mode would be an integer that's too
    // large or too small.  Ideally we would test INT_MIN-1 and
    // INT_MAX+1, but computing that would require some complexity.
    // Easier to just append zeroes.

    {
      std::string input = boost::lexical_cast<std::string>(INT_MIN) + "0";
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_THROW(integer.parse(begin, end), ParseException);
      CPPUNIT_ASSERT(begin != input.begin());
    }

    {
      std::string input = boost::lexical_cast<std::string>(INT_MAX) + "0";
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_THROW(integer.parse(begin, end), ParseException);
      CPPUNIT_ASSERT(begin != input.begin());
    }
  }

  void testEof()
  {
    std::string input = "abc";

    std::string::const_iterator begin = input.begin(), end = input.end();

    eof e;

    CPPUNIT_ASSERT_THROW(e.parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());

    ++begin;
    CPPUNIT_ASSERT_THROW(e.parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());

    ++begin;
    CPPUNIT_ASSERT_THROW(e.parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)2, begin - input.begin());

    ++begin;
    CPPUNIT_ASSERT_NO_THROW(e.parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)3, begin - input.begin());
  }

  void testFail()
  {
    std::string input = "abc def ghi";
    std::string::const_iterator begin = input.begin(), end = input.end();

    boost::optional<ParseException> thrown;
    try
      {
        ( (maybe(str("def")) >> fail("This is an ex-parser") >> val(boost::shared_ptr<std::vector<boost::shared_ptr<std::string> > >()))
          | many(lexeme(container(std::string(), many(alpha()))))).parse(begin, end);
      }
    catch(ParseException &ex)
      {
        thrown = ex;
      }

    CPPUNIT_ASSERT_MESSAGE("Expected exception not thrown.", thrown);
    CPPUNIT_ASSERT_EQUAL(std::string("This is an ex-parser"), thrown->get_raw_msg());
    CPPUNIT_ASSERT_EQUAL((iter_difftype)7, begin - input.begin());
  }

  void testStr()
  {
    std::string input = "abcdef";

    std::string::const_iterator begin = input.begin(), end = input.end();

    str abc("abc");
    str da("da");
    str xyz("xyz");

    CPPUNIT_ASSERT_NO_THROW(abc.parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)3, begin - input.begin());

    CPPUNIT_ASSERT_THROW(da.parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)4, begin - input.begin());

    CPPUNIT_ASSERT_THROW(xyz.parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)4, begin - input.begin());
  }

  void testVal()
  {
    std::string input = "o3qithkje5hgkjh";
    std::string::const_iterator begin = input.begin(), end = input.begin();

    val_p<std::string> v = val("abcdefg");

    CPPUNIT_ASSERT_EQUAL(std::string("abcdefg"), v.parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
  }

  // Cases to test for the "and then" parser:
  //
  //  1. Both parsers match.  Check that "begin" is advanced
  //     past the second match and that the second value (not
  //     the first) is returned.
  //  2. Only the first parser matches.  Check that "begin" is
  //     advanced and an exception is thrown.
  //  3. The first parser fails after consuming input.
  //     Check that "begin" is advanced and an exception is
  //     thrown.
  //  4. The first parser fails without consuming input.
  //     Check that "begin" is NOT advanced and that an
  //     exception is thrown.

  // Case 1: both parsers match.
  void testAndThenBothMatch()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    str abc("abc");
    ch_p<char> d('d');

    CPPUNIT_ASSERT_EQUAL('d', (abc >> d).parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)4, begin - input.begin());
  }

  // Case 1.5: chain together multiple parsers.
  void testAndThenBothMatchChained()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    ch_p<char> a('a'), b('b'), c('c'), d('d');

    CPPUNIT_ASSERT_EQUAL('d', (a >> b >> c >> d).parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)4, begin - input.begin());
  }

  // Case 2: only the first parser matches.
  void testAndThenOnlyFirstMatches()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    str ab("ab"), yz("yz");

    CPPUNIT_ASSERT_THROW((ab >> yz).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)2, begin - input.begin());
  }

  // Case 2.5: only the first two parsers match, with several chained together.
  void testAndThenOnlyFirstMatchesChained()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    ch_p<char> a('a'), b('b'), y('y'), z('z');

    CPPUNIT_ASSERT_THROW((a >> b >> y >> z).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)2, begin - input.begin());
  }

  // Case 3: the first parser fails after consuming input.
  void testAndThenFirstFailsAndConsumesInput()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    str ax("ax"), yz("yz");

    CPPUNIT_ASSERT_THROW((ax >> yz).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());
  }

  // Case 4: the first parser fails without consuming input.
  void testAndThenFirstFailsWithoutConsumingInput()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    str wx("wx"), yz("yz");

    CPPUNIT_ASSERT_THROW((wx >> yz).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
  }

  // Case 4.5: the first parser fails without consuming input (chained).
  void testAndThenFirstFailsWithoutConsumingInputChained()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    ch_p<char> w('w'), x('x'), y('y'), z('z');

    CPPUNIT_ASSERT_THROW((w >> x >> y >> z).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
  }

  // Cases to test for the "and first" parser:
  //
  //  1. Both parsers match.  Check that "begin" is advanced
  //     past the second match and that the first value is
  //     returned.
  //  2. Only the first parser matches.  Check that "begin" is
  //     advanced and an exception is thrown.
  //  3. The first parser fails after consuming input.
  //     Check that "begin" is advanced and an exception is
  //     thrown.
  //  4. The first parser fails without consuming input.
  //     Check that "begin" is NOT advanced and that an
  //     exception is thrown.

  // Case 1: both parsers match.
  void testAndFirstBothMatch()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    ch_p<char> a('a');
    str bcd("bcd");

    CPPUNIT_ASSERT_EQUAL('a', (a << bcd).parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)4, begin - input.begin());
  }

  // Case 1.5: chain together multiple parsers.
  void testAndFirstBothMatchChained()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    ch_p<char> a('a'), b('b'), c('c'), d('d');

    CPPUNIT_ASSERT_EQUAL('a', (a << b << c << d).parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)4, begin - input.begin());
  }

  // Case 2: only the first parser matches.
  void testAndFirstOnlyFirstMatches()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    str ab("ab"), yz("yz");

    CPPUNIT_ASSERT_THROW((ab << yz).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)2, begin - input.begin());
  }

  // Case 2.5: only the first two parsers match, with several chained together.
  void testAndFirstOnlyFirstMatchesChained()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    ch_p<char> a('a'), b('b'), y('y'), z('z');

    CPPUNIT_ASSERT_THROW((a << b << y << z).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)2, begin - input.begin());
  }

  // Case 3: the first parser fails after consuming input.
  void testAndFirstFirstFailsAndConsumesInput()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    str ax("ax"), yz("yz");

    CPPUNIT_ASSERT_THROW((ax << yz).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());
  }

  // Case 4: the first parser fails without consuming input.
  void testAndFirstFirstFailsWithoutConsumingInput()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    str wx("wx"), yz("yz");

    CPPUNIT_ASSERT_THROW((wx << yz).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
  }

  // Case 4.5: the first parser fails without consuming input (chained).
  void testAndFirstFirstFailsWithoutConsumingInputChained()
  {
    std::string input = "abcd";
    std::string::const_iterator begin = input.begin(), end = input.end();

    ch_p<char> w('w'), x('x'), y('y'), z('z');

    CPPUNIT_ASSERT_THROW((w << x << y << z).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
  }

  void testSetExpected()
  {
    ch_p<char> a('a');

    std::string new_expected("My hovercraft is full of eels");

    set_expected_p<ch_p<char> > a2 = a[new_expected];

    std::stringstream msg1;
    a.get_expected_description(msg1);
    CPPUNIT_ASSERT( msg1.str() != new_expected );

    std::stringstream msg2;
    a2.get_expected_description(msg2);
    CPPUNIT_ASSERT_EQUAL(new_expected, msg2.str());

    // Test that we can parse, too.
    std::string input("aba");
    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_EQUAL('a', a2.parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());

    CPPUNIT_ASSERT_THROW(a2.parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());
  }

  void testForeachEmptyEndNotEOF()
  {
    alpha_p letter = alpha();

    std::string input = "   abcde   ";
    std::string::const_iterator begin = input.begin(), end = input.end();

    std::string result;

    (foreach(letter, push_back_a(result))).parse(begin, end);

    CPPUNIT_ASSERT_EQUAL(std::string(""), result);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
  }

  void testForeachEmptyEndEOF()
  {
    alpha_p letter = alpha();

    std::string input = "";
    std::string::const_iterator begin = input.begin(), end = input.end();

    std::string result;

    (foreach(letter, push_back_a(result))).parse(begin, end);

    CPPUNIT_ASSERT_EQUAL(std::string(""), result);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
  }

  void testForeachNotEmptyEndNotEOF()
  {
    alpha_p letter = alpha();

    std::string input = "abcde   ";
    std::string::const_iterator begin = input.begin(), end = input.end();

    std::string result;

    (foreach(letter, push_back_a(result))).parse(begin, end);

    CPPUNIT_ASSERT_EQUAL(std::string("abcde"), result);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)5, begin - input.begin());
  }

  void testForeachNotEmptyEndEOF()
  {
    alpha_p letter = alpha();

    std::string input = "abcde";
    std::string::const_iterator begin = input.begin(), end = input.end();

    std::string result;

    (foreach(letter, push_back_a(result))).parse(begin, end);

    CPPUNIT_ASSERT_EQUAL(std::string("abcde"), result);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)5, begin - input.begin());
  }

  void testForeachFailure()
  {
    std::string input = "2345 1234 234jf ";
    std::string::const_iterator begin = input.begin(), end = input.end();

    std::vector<int> result;

    CPPUNIT_ASSERT_THROW(foreach(integer() >> ch(' '),
                                 push_back_a(result)).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)13, begin - input.begin());
  }


  void testSkipManyEmptyEndNotEOF()
  {
    alpha_p letter = alpha();

    std::string input = "   abcde   ";
    std::string::const_iterator begin = input.begin(), end = input.end();

    (skipMany(letter)).parse(begin, end);

    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
  }

  void testSkipManyEmptyEndEOF()
  {
    alpha_p letter = alpha();

    std::string input = "";
    std::string::const_iterator begin = input.begin(), end = input.end();

    (skipMany(letter)).parse(begin, end);

    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
  }

  void testSkipManyNotEmptyEndNotEOF()
  {
    alpha_p letter = alpha();

    std::string input = "abcde   ";
    std::string::const_iterator begin = input.begin(), end = input.end();

    (skipMany(letter)).parse(begin, end);

    CPPUNIT_ASSERT_EQUAL((iter_difftype)5, begin - input.begin());
  }

  void testSkipManyNotEmptyEndEOF()
  {
    alpha_p letter = alpha();

    std::string input = "abcde";
    std::string::const_iterator begin = input.begin(), end = input.end();

    (skipMany(letter)).parse(begin, end);

    CPPUNIT_ASSERT_EQUAL((iter_difftype)5, begin - input.begin());
  }

  void testSkipManyPlusEmptyEndNotEOF()
  {
    alpha_p letter = alpha();

    std::string input = "   abcde   ";
    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_THROW((skipManyPlus(letter)).parse(begin, end), ParseException);

    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
  }

  void testSkipManyPlusEmptyEndEOF()
  {
    alpha_p letter = alpha();

    std::string input = "";
    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_THROW((skipManyPlus(letter)).parse(begin, end), ParseException);

    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
  }

  void testSkipManyPlusNotEmptyEndNotEOF()
  {
    alpha_p letter = alpha();

    std::string input = "abcde   ";
    std::string::const_iterator begin = input.begin(), end = input.end();

    (skipManyPlus(letter)).parse(begin, end);

    CPPUNIT_ASSERT_EQUAL((iter_difftype)5, begin - input.begin());
  }

  void testSkipManyPlusNotEmptyEndEOF()
  {
    alpha_p letter = alpha();

    std::string input = "abcde";
    std::string::const_iterator begin = input.begin(), end = input.end();

    (skipManyPlus(letter)).parse(begin, end);

    CPPUNIT_ASSERT_EQUAL((iter_difftype)5, begin - input.begin());
  }

  void testManyEmptyEndNotEOF()
  {
    alpha_p letter = alpha();

    std::string input = "   abcde   ";
    std::string::const_iterator begin = input.begin(), end = input.end();

    boost::shared_ptr<std::string> result = (container(std::string(), many(letter))).parse(begin, end);

    CPPUNIT_ASSERT_EQUAL(std::string(""), *result);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
  }

  void testManyEmptyEndEOF()
  {
    alpha_p letter = alpha();

    std::string input = "";
    std::string::const_iterator begin = input.begin(), end = input.end();

    boost::shared_ptr<std::string> result = (container(std::string(), many(letter))).parse(begin, end);

    CPPUNIT_ASSERT_EQUAL(std::string(""), *result);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
  }

  void testManyNotEmptyEndNotEOF()
  {
    alpha_p letter = alpha();

    std::string input = "abcde   ";
    std::string::const_iterator begin = input.begin(), end = input.end();

    boost::shared_ptr<std::string> result = (container(std::string(), many(letter))).parse(begin, end);

    CPPUNIT_ASSERT_EQUAL(std::string("abcde"), *result);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)5, begin - input.begin());
  }

  void testManyNotEmptyEndEOF()
  {
    alpha_p letter = alpha();

    std::string input = "abcde";
    std::string::const_iterator begin = input.begin(), end = input.end();

    boost::shared_ptr<std::string> result = (container(std::string(), many(letter))).parse(begin, end);

    CPPUNIT_ASSERT_EQUAL(std::string("abcde"), *result);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)5, begin - input.begin());
  }

  void testManyFailure()
  {
    std::string input = "2345 1234 234jf ";
    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_THROW(many(integer() >> ch(' ')).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)13, begin - input.begin());
  }

  void testOrFirstBranchMatches()
  {
    str ab("ab"), a("a");

    std::string input = "abskrj";
    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_NO_THROW((ab | a).parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)2, begin - input.begin());
  }

  void testOrFirstBranchFailsAndConsumesInput()
  {
    str ab("ab"), a("a");

    std::string input = "acskrj";
    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_THROW((ab | a).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());
  }

  void testOrSecondBranchMatches()
  {
    str ab("ab"), cd("cd");

    std::string input = "cdlkwrj";
    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_NO_THROW((ab | cd).parse(begin, end));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)2, begin - input.begin());
  }

  void testOrSecondBranchFailsAndConsumesInput()
  {
    str ab("ab"), cd("cd");

    std::string input = "cyzablksdfj";
    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_THROW((ab | cd).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());
  }

  void testOrSecondBranchFailsAndConsumesNoInput()
  {
    str ab("ab"), cd("cd");

    std::string input = "yzablksdfj";
    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_THROW((ab | cd).parse(begin, end), ParseException);
    CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
  }

  void testOrChainedSuccess()
  {
    str ab("ab"), cd("cd"), ef("ef");

    {
      std::string input = "abcdef";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_NO_THROW((ab | cd | ef).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)2, begin - input.begin());
    }

    {
      std::string input = "cd";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_NO_THROW((ab | cd | ef).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)2, begin - input.begin());
    }

    {
      std::string input = "ef";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_NO_THROW((ab | cd | ef).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)2, begin - input.begin());
    }
  }

  void testOrChainedFailure()
  {
    str ab("ab"), cd("cd"), ef("ef"), a("a");

    {
      std::string input = "axyz";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW((ab | cd | ef).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());
    }

    {
      std::string input = "axyz";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW((ab | a | a | a | a | cd | ef).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());
    }

    {
      std::string input = "cxyz";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW((ab | cd | ef).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());
    }

    {
      std::string input = "exyz";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW((ab | cd | ef).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());
    }

    {
      std::string input = "xyz";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW((ab | cd | ef).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
    }
  }

  void testOrChainedCollapsesParsers()
  {
    ch_p<char> a('a'), b('b'), c('c'), d('d'), e('e'), f('f');

    CPPUNIT_ASSERT_EQUAL(6, (int)boost::fusion::size((a | b | c | d | e | f).get_values()));
  }

  void testMaybeSuccess()
  {
    std::string input("abcdefg");
    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_EQUAL(std::string("ab"),
                         std::string((maybe(str("ab") >> val("ab")) | (str("a") >> val("a"))).parse(begin, end)));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)2, begin - input.begin());
  }

  void testMaybeFailure()
  {
    std::string input("abcdefg");
    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_EQUAL(std::string("a"),
                         std::string((maybe(str("ad") >> val("ad")) | (str("a") >> val("a"))).parse(begin, end)));
    CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());
  }

  void testMaybeValue()
  {
    std::string input("12345");
    std::string::const_iterator begin = input.begin(), end = input.end();

    CPPUNIT_ASSERT_EQUAL(12345, integer().parse(begin, end));
  }

  void testTupleSuccess()
  {
    {
      std::string input("765");
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_EQUAL(boost::fusion::make_vector(765),
                           parsers::tuple(integer()).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)3, begin - input.begin());
    }

    {
      std::string input("123,456,789");
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_EQUAL(boost::fusion::make_vector(123, 456, 789),
                           (integer(), ch(',') >> integer(), ch(',') >> integer()).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)11, begin - input.begin());

    }
  }

  void testTupleFailureWithoutConsumingInput()
  {
    {
      std::string input("abcdefg");
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_THROW((integer(), str("abc"), str("def")).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
    }
  }

  void testTupleFailureWithConsumingInput()
  {
    // Cases tested: in a 3-tuple, check what happens if the first
    // parser fails, if the second parser fails, and if the third
    // fails, with input consumed in each case.

    {
      std::string input("123,456,789");
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_THROW((digit() >> alpha(), integer(), integer()).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)1, begin - input.begin());
    }

    {
      std::string input("123,456,789");
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_THROW((integer(), ch(',') >> alpha(), integer()).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)4, begin - input.begin());
    }

    {
      std::string input("123,456,789");
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_THROW((integer(), ch(',') >> integer(), ch(',') >> integer() >> alpha()).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)11, begin - input.begin());
    }
  }

  // Simple function object usable with apply().
  struct addOne
  {
    typedef int result_type;

    result_type operator()(int input) const
    {
      return input + 1;
    }
  };

  struct makeIntPair
  {
    typedef boost::fusion::vector<int, int> result_type;

    result_type operator()(int n1, int n2) const
    {
      return boost::fusion::make_vector(n1, n2);
    }
  };

  // Used to ensure we get a vector of the expected type (which is not
  // what make_vector returns in this version of Boost).
  boost::fusion::vector<int, int> makeVector2(int a, int b)
  {
    return boost::fusion::make_vector(a, b);
  }

  void testApplySuccess()
  {
    // Unary test via apply().
    {
      std::string input("123,456");
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_EQUAL(124, apply(addOne(), integer()).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)3, begin - input.begin());
    }

    // Unary test using a unary tuple.
    {
      std::string input("123,456");
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_EQUAL(124, ::parsers::apply(addOne(), tuple(integer())).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)3, begin - input.begin());
    }

    // Unary test without the apply() convenience routine..
    {
      std::string input("123,456");
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_EQUAL(124, (apply_p<addOne, tuple_p<boost::fusion::vector<integer_p> > >(addOne(), tuple(integer())).parse(begin, end)));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)3, begin - input.begin());
    }

    // Binary test.
    {
      std::string input("123,456");
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_EQUAL(makeVector2(123, 456), (apply(makeIntPair(), (integer(), ch(',') >> integer())).parse(begin, end)));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)7, begin - input.begin());
    }

    // Test that parsers::construct_f works as expected.
    {
      std::string input("123,456");
      std::string::const_iterator begin = input.begin(), end = input.end();
      CPPUNIT_ASSERT_EQUAL(makeVector2(123, 456), (apply(construct_f<boost::fusion::vector<int, int> >(),
                                                         (integer(), ch(',') >> integer())).parse(begin, end)));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)7, begin - input.begin());
    }
  }

  void testApplyFailure()
  {
    {
      std::string input("123,456");
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW(apply(makeIntPair(), (integer(), integer())).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)3, begin - input.begin());
    }
  }

  void testFollowedBySuccess()
  {
    {
      std::string input("123,456");
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_EQUAL(456, (integer() >> ch(',') >> followedBy(integer())).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)4, begin - input.begin());
    }
  }

  void testFollowedByFailure()
  {
    {
      std::string input("123,abc");
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW((integer() >> ch(',') >> followedBy(integer())).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)4, begin - input.begin());
    }

    {
      std::string input("123,axy");
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW((integer() >> ch(',') >> followedBy(str("abc"))).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)4, begin - input.begin());
    }
  }

  void testNotFollowedBySuccess()
  {
    {
      std::string input("123,456");
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_NO_THROW((integer() >> notFollowedBy(str(",abc"))).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)3, begin - input.begin());
    }
  }

  void testNotFollowedByFailure()
  {
    {
      std::string input("123,456");
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW((integer() >> ch(',') >> notFollowedBy(integer())).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)4, begin - input.begin());
    }

    {
      std::string input("123,abc");
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW((integer() >> ch(',') >> notFollowedBy(str("abc"))).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)4, begin - input.begin());
    }
  }

  class lessThan_f
  {
    int x;

  public:
    lessThan_f(int _x) : x(_x) { }

    bool operator()(int y) const { return y < x; }
  };

  void testPostAssertSuccess()
  {
    {
      std::string input("123456");
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_EQUAL(123456, postAssert(integer(), "integer below 500000", lessThan_f(500000)).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)6, begin - input.begin());
    }
  }

  void testPostAssertFailure()
  {
    {
      std::string input("123456");
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW(postAssert(integer(), "integer below 10", lessThan_f(10)).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)6, begin - input.begin());
    }
  }

  void testManyPlusSuccess()
  {
    {
      std::string input("57482adfb");
      std::string::const_iterator begin = input.begin(), end = input.end();

      boost::shared_ptr<std::string> ptr;
      CPPUNIT_ASSERT_NO_THROW(ptr = (container(std::string(), manyPlus(digit()))).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL(std::string("57482"), *ptr);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)5, begin - input.begin());
    }

    {
      std::string input("a34b15c999");
      std::string::const_iterator begin = input.begin(), end = input.end();

      typedef tuple_p<boost::fusion::vector<anychar_p<char>, integer_p> >::result_type charint_vector;
      typedef std::vector<charint_vector> result_type;
      result_type expected;
      expected.push_back(charint_vector('a', 34));
      expected.push_back(charint_vector('b', 15));
      expected.push_back(charint_vector('c', 999));

      boost::shared_ptr<result_type> result;
      CPPUNIT_ASSERT_NO_THROW(result = manyPlus( (anychar(), integer()) ).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL((iter_difftype)10, begin - input.begin());

      CPPUNIT_ASSERT_EQUAL(expected.size(), result->size());
      for(result_type::size_type i = 0; i < expected.size(); ++i)
        {
          std::ostringstream msg;

          msg << "At index " << i;
          CPPUNIT_ASSERT_EQUAL(expected[i], (*result)[i]);
        }
    }
  }

  void testManyPlusFailure()
  {
    {
      std::string input("abdsfa");
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW(container(std::string(), manyPlus(digit())).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
    }

    {
      std::string input("a34b15c");
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW( manyPlus( (anychar(), integer()) ).parse(begin, end), ParseException );

      CPPUNIT_ASSERT_EQUAL((iter_difftype)7, begin - input.begin());
    }
  }

  void testOptionalSuccess()
  {
    {
      std::string input("123478zs");
      std::string::const_iterator begin = input.begin(), end = input.end();

      boost::optional<int> result;
      CPPUNIT_ASSERT_NO_THROW( result = optional(integer()).parse(begin, end) );
      CPPUNIT_ASSERT_EQUAL(boost::optional<int>(123478), result);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)6, begin - input.begin());
    }

    {
      std::string input("alsdkfj");
      std::string::const_iterator begin = input.begin(), end = input.end();

      boost::optional<int> result;
      CPPUNIT_ASSERT_NO_THROW( result = optional(integer()).parse(begin, end) );
      CPPUNIT_ASSERT_EQUAL(boost::optional<int>(), result);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
    }
  }

  void testOptionalFailure()
  {
    {
      std::string input("234,abc");
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW(optional( (integer(), integer()) ).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)3, begin - input.begin());
    }
  }

  void testSepBySuccessEmpty()
  {
    {
      std::string input = "abcde";
      std::string::const_iterator begin = input.begin(), end = input.end();

      boost::shared_ptr<std::vector<int> > result;
      CPPUNIT_ASSERT_NO_THROW(result = sepBy(str(","), integer()).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL(0, (int)result->size());
      CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
    }
  }

  void testSepBySuccessNonempty()
  {
    {
      std::string input = "3094,124498,34saflk";
      std::string::const_iterator begin = input.begin(), end = input.end();

      boost::shared_ptr<std::vector<int> > result;
      CPPUNIT_ASSERT_NO_THROW(result = sepBy(str(","), integer()).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL(3, (int)result->size());
      CPPUNIT_ASSERT_EQUAL(3094, (*result)[0]);
      CPPUNIT_ASSERT_EQUAL(124498, (*result)[1]);
      CPPUNIT_ASSERT_EQUAL(34, (*result)[2]);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)14, begin - input.begin());
    }
  }

  void testSepByFailureInFirstElement()
  {
    {
      std::string input = "abxyz,abcde,abcde";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW(sepBy(str(","), str("abcde")).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)2, begin - input.begin());
    }
  }

  void testSepByFailureInSeparator()
  {
    {
      std::string input = "ab,,cd,ef";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW(sepBy(str(",,"), many(alpha())).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)7, begin - input.begin());
    }
  }

  void testSepByFailureInSecondElement()
  {
    // Check that if we see a separator, we have to see an element
    // after it (i.e., the parser doesn't accept dangling separators
    // at the end of the list).
    {
      std::string input = "ab,cd,,ef";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW(sepBy(str(","), manyPlus(alpha())).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)6, begin - input.begin());
    }
  }

  void testSepByTrailingSeparator()
  {
    {
      std::string input = "ab,cd,";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW(sepBy(str(","), manyPlus(alpha())).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)6, begin - input.begin());
    }
  }

  void testLexemeSuccess()
  {
    {
      std::string input = "abcd    ef";
      std::string::const_iterator begin = input.begin(), end = input.end();

      boost::shared_ptr<std::string> result;
      CPPUNIT_ASSERT_NO_THROW(result = lexeme(container(std::string(), many(alpha()))).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL(std::string("abcd"), *result);
      // Unlike in many parsers, this test is NOT purely cosmetic; it
      // verifies that we really consumed the whitespace (which is,
      // y'know, the whole point).
      CPPUNIT_ASSERT_EQUAL((iter_difftype)8, begin - input.begin());
    }
  }

  void testLexemeFailure()
  {
    // Just check that a failure really is propagated out of the
    // lexeme parser.
    {
      std::string input = "ab43   ";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW(lexeme(alpha() >> alpha() >> alpha()).parse(begin, end), ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)2, begin - input.begin());
    }
  }

  void testBetweenSuccess()
  {
    {
      std::string input = "( 12345 )asdflj";
      std::string::const_iterator begin = input.begin(), end = input.end();

      int result;
      CPPUNIT_ASSERT_NO_THROW(result = between(lexeme(ch('(')),
                                               lexeme(integer()),
                                               lexeme(ch(')'))).parse(begin, end));
      CPPUNIT_ASSERT_EQUAL(12345, result);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)9, begin - input.begin());
    }
  }

  void testBetweenFailure()
  {
    {
      std::string input = "12345 )asdflj";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW(between(lexeme(ch('(')),
                                   lexeme(integer()),
                                   lexeme(ch(')'))).parse(begin, end),
                           ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)0, begin - input.begin());
    }

    {
      std::string input = "( asdlfkj )asdflj";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW(between(lexeme(ch('(')),
                                   lexeme(integer()),
                                   lexeme(ch(')'))).parse(begin, end),
                           ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)2, begin - input.begin());
    }

    {
      std::string input = "( 12345 asdflj";
      std::string::const_iterator begin = input.begin(), end = input.end();

      CPPUNIT_ASSERT_THROW(between(lexeme(ch('(')),
                                   lexeme(integer()),
                                   lexeme(ch(')'))).parse(begin, end),
                           ParseException);
      CPPUNIT_ASSERT_EQUAL((iter_difftype)8, begin - input.begin());
    }
  }

  void testErrorPositionInformation()
  {
    {
      std::string input = "123,abc";

      boost::variant<boost::shared_ptr<std::vector<int> >, ParseException>
        result = parse(input, sepBy(ch(','), integer()));

      ParseException *ex = boost::get<ParseException>(&result);
      CPPUNIT_ASSERT(ex != NULL);

      CPPUNIT_ASSERT_EQUAL(1, ex->get_line_number());
      CPPUNIT_ASSERT_EQUAL(5, ex->get_column_number());
    }

    {
      std::string input = "123  \n  ,  456  \n\n\n\n\n,\n  abc";

      boost::variant<boost::shared_ptr<std::vector<int> >, ParseException>
        result = parse(input, sepBy(lexeme(ch(',')), lexeme(integer())));

      ParseException *ex = boost::get<ParseException>(&result);
      CPPUNIT_ASSERT(ex != NULL);

      CPPUNIT_ASSERT_EQUAL(8, ex->get_line_number());
      CPPUNIT_ASSERT_EQUAL(3, ex->get_column_number());
    }
  }

  void testContainerSuccess()
  {
    {
      std::string input = "123abc";
      std::vector<std::string> expected;
      expected.push_back("123");
      expected.push_back("abc");

      verify_success(input,
                     many(container(std::string(), manyPlus(alpha()) | manyPlus(digit()))),
                     expected);
    }

    {
      std::string input = "123abc";
      std::vector<std::string> expected;
      expected.push_back("x123");
      expected.push_back("xabc");

      verify_success(input,
                     many(container(std::string("x"), manyPlus(alpha()) | manyPlus(digit()))),
                     expected);
    }
  }

  void testContainerOr()
  {
    {
      std::string input = "2hj4j46g4jhg";
      std::vector<char> expected(input.begin(), input.end());

      verify_success(input,
                     many(alpha() | digit()),
                     expected);
    }
  }

  void testConcatenateInts()
  {
    {
      std::string input = "123 456";
      std::vector<int> expected;
      expected.push_back(123);
      expected.push_back(456);

      verify_success(input,
                     lexeme(integer()) + lexeme(integer()),
                     expected);
    }

    {
      std::string input = "123 456 789";
      std::vector<int> expected;
      expected.push_back(123);
      expected.push_back(456);
      expected.push_back(789);

      verify_success(input,
                     lexeme(integer()) + lexeme(integer()) + lexeme(integer()),
                     expected);
    }
  }

  void testConcatenateMany()
  {
    {
      std::string input = "abc123xyz";
      std::string expected = "abc123";

      verify_success(input,
                     container(std::string(), many(alpha()) + many(digit())),
                     expected);
    }
  }

  void testConcatenateSepBy()
  {
    {
      std::string input = "abc,def123,456xyz";
      std::vector<std::string> expected;
      expected.push_back("abc");
      expected.push_back("def");
      expected.push_back("123");
      expected.push_back("456");

      verify_success(input,
                     sepBy(str(","),
                           container(std::string(),
                                     manyPlus(alpha()))) +
                     sepBy(str(","),
                           container(std::string(),
                                     manyPlus(digit()))),
                     expected);
    }
  }

  void testConcatenateOptional()
  {
    {
      std::string input = "abc3j2k3h";
      std::string expected = input;

      verify_success(input,
                     container(std::string(),
                               many(alpha() + optional(digit()))),
                     expected);
    }

    {
      std::string input = "abc32j2k3h";
      std::string expected = "abc3";
      verify_success(input,
                     container(std::string(),
                               many(alpha() + optional(digit()))),
                     expected);
    }
  }
};

CPPUNIT_TEST_SUITE_REGISTRATION(ParsersTest);
