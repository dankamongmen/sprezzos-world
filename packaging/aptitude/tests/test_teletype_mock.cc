/** \file test_teletype_mock */

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



// Local includes:
#include <cmdline/mocks/teletype.h>
#include <cmdline/mocks/terminal.h>

// System includes:
#include <cwidget/generic/util/transcode.h>

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <locale.h>

namespace mocks = aptitude::cmdline::mocks;

using aptitude::cmdline::mocks::StrTrimmedEq;
using aptitude::cmdline::mocks::StrTrimmedRightEq;
using boost::shared_ptr;
using testing::InSequence;
using testing::Not;
using testing::Return;
using testing::StrEq;
using testing::Test;

namespace
{
  // Note that I use a character that is *never* multi-column under
  // normal circumstances, to verify that the locale object is
  // actually being used.
  const wchar_t two_column_char = L'Z';

  struct TeletypeTest : public Test
  {
    // An arbitrary Unicode codepoint that I happen to know will take
    // two columns.  (TODO: does this make the test locale-dependent?
    // Although using some sort of locale mock is kind of a scary idea)
    const std::wstring widechar;

    // Stores the previous value of LC_CTYPE before we modified it for
    // the test.
    //
    // I really, really don't like having to do this.  But mocking out
    // the whole locale system would be utterly insane, and I can't
    // find a Unicode character that I can test regardless of the
    // value of CTYPE.
    std::string previous_lc_ctype;

    shared_ptr<mocks::terminal_locale> term_locale;
    shared_ptr<mocks::terminal_metrics> term_metrics;
    shared_ptr<mocks::terminal_with_combined_output> term_output;
    shared_ptr<mocks::teletype> teletype;

    static std::string safe_string(const char *c)
    {
      if(c == NULL)
        return std::string();
      else
        return c;
    }

    // narrow -> wide conversion function that doesn't depend on the
    // current locale.
    static std::wstring transcode(const std::string &s)
    {
      return cwidget::util::transcode(s, "UTF-8");
    }

    // wide -> narrow conversion function that doesn't depend on the
    // current locale.
    static std::string transcode(const std::wstring &s)
    {
      return cwidget::util::transcode(s, "UTF-8");
    }

    TeletypeTest()
      : widechar(1, two_column_char),
        term_locale(mocks::terminal_locale::create_strict()),
        term_metrics(mocks::terminal_metrics::create_strict()),
        term_output(mocks::terminal_with_combined_output::create_strict()),
        teletype(mocks::create_strict_teletype(term_locale, term_metrics, term_output))
    {
      EXPECT_CALL(*term_locale, wcwidth(two_column_char))
        .WillRepeatedly(Return(2));

      EXPECT_CALL(*term_metrics, get_screen_width())
        .WillRepeatedly(Return(80));
    }

    void SetUp()
    {
      // Sanity-check the widechar string.
      EXPECT_EQ(1, widechar.size());
      if(widechar.size() >= 0)
        {
          EXPECT_EQ(two_column_char, widechar[0]);
          EXPECT_EQ(2, term_locale->wcwidth(widechar[0]));
        }
    }
  };
}

TEST_F(TeletypeTest, testOutputPartialLine)
{
  EXPECT_CALL(*teletype, set_last_line(StrEq(L"abc")));

  term_output->output(L"abc");
}

TEST_F(TeletypeTest, testOutputLine)
{
  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrEq(L"abc")));
    EXPECT_CALL(*teletype, newline());
  }


  term_output->output(L"abc\n");
}

TEST_F(TeletypeTest, NewlineAfterFlush)
{
  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrEq(L"abc")));
    EXPECT_CALL(*teletype, newline());
  }

  term_output->output(L"abc");
  term_output->output(L"\n");
}

TEST_F(TeletypeTest, SuppressDuplicateWrites)
{
  EXPECT_CALL(*teletype, set_last_line(StrEq(L"abc")));

  term_output->output(L"abc");
  term_output->output(L"\r");
  term_output->output(L"abc");
}

// Imitates what the transient message does, to be sure that it will
// behave as expected if it outputs the right thing.
TEST_F(TeletypeTest, OverwriteABCWithA)
{
  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrEq(L"abc")));
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"a  ")));
  }

  term_output->output(L"abc");
  term_output->output(L"\r   \ra");
}

TEST_F(TeletypeTest, testOverwriteOneCharAtATime)
{
  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrEq(L"abc")));
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"xbc")));
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"xyc")));
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"xyz")));
  }

  term_output->output(L"abc\r");
  term_output->output(L"x");
  term_output->output(L"y");
  term_output->output(L"z");
}

TEST_F(TeletypeTest, OverwriteNarrowCharWithWideChar)
{
  {
    InSequence dummy;
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"abc")));
    EXPECT_CALL(*teletype, set_last_line(StrEq(widechar + L"c")));
  }

  term_output->output(L"abc\r");
  term_output->output(widechar);
}

TEST_F(TeletypeTest, OverwriteWideCharWithNarrowChar)
{
  {
    InSequence dummy;
    EXPECT_CALL(*teletype, set_last_line(StrEq(widechar + L"c")));
    // NB: this behavior isn't quite what a real terminal will do, but
    // it's close enough for testing. (the terminal would have a blank
    // space without an actual character, but it would be too
    // complicated to simulate that, and it doesn't really matter from
    // the point of view of seeing what the output looks like, which
    // is what this is for)
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"a c")));
  }

  term_output->output(widechar + L"c\r");
  term_output->output(L"a");
}

TEST_F(TeletypeTest, OverwriteWideCharWithNarrowChars)
{
  {
    InSequence dummy;
    EXPECT_CALL(*teletype, set_last_line(StrEq(widechar + L"c")));
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"abc")));
  }

  term_output->output(widechar + L"c\r");
  term_output->output(L"ab");
}

TEST_F(TeletypeTest, overwriteEverything)
{
  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrEq(L"abc")));
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"xyz")));
  }

  term_output->output(L"abc\r");
  term_output->output(L"xyz");
}

TEST_F(TeletypeTest, overwritePastEverything)
{
  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrEq(L"abc")));
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"xyzw")));
  }

  term_output->output(L"abc\r");
  term_output->output(L"xyzw");
}

TEST_F(TeletypeTest, testWritePastEOL)
{
  EXPECT_CALL(*term_metrics, get_screen_width())
    .WillRepeatedly(Return(5));

  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrEq(L"abcde")));
    EXPECT_CALL(*teletype, newline());
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"fghij")));
  }

  term_output->output(L"abcdefghij");
}

TEST_F(TeletypeTest, WritePastEOLAfterWideChar)
{
  EXPECT_CALL(*term_metrics, get_screen_width())
    .WillRepeatedly(Return(4));

  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrEq(widechar + L"bc")));
    EXPECT_CALL(*teletype, newline());
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"def")));
  }

  term_output->output(widechar + L"bcdef");
}

TEST_F(TeletypeTest, WriteWideCharPastEOL)
{
  EXPECT_CALL(*term_metrics, get_screen_width())
    .WillRepeatedly(Return(4));

  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrEq(widechar + widechar)));
    EXPECT_CALL(*teletype, newline());
    EXPECT_CALL(*teletype, set_last_line(StrEq(widechar)));
  }

  term_output->output(widechar + widechar + widechar);
}

TEST_F(TeletypeTest, WriteWideCharPastEOLWithSplit)
{
  EXPECT_CALL(*term_metrics, get_screen_width())
    .WillRepeatedly(Return(4));

  {
    InSequence dummy;
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"a" + widechar)));
    EXPECT_CALL(*teletype, newline());
    EXPECT_CALL(*teletype, set_last_line(StrEq(widechar + L"a")));
  }

  term_output->output(L"a" + widechar + widechar + L"a");
}

TEST_F(TeletypeTest, testOverwritePastEOL)
{
  EXPECT_CALL(*term_metrics, get_screen_width())
    .WillRepeatedly(Return(5));

  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrEq(L"12345")));
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"abcde")));
    EXPECT_CALL(*teletype, newline());
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"fghij")));
  }

  term_output->output(L"12345");
  term_output->output(L"\rabcdefghij");
}

TEST_F(TeletypeTest, TeletypeDoesNotBreakTerminalMock)
{
  shared_ptr<mocks::combining_terminal_output> real_term_output =
    mocks::combining_terminal_output::create_strict();
  shared_ptr<mocks::teletype> teletype =
    mocks::create_strict_teletype(term_locale, term_metrics, real_term_output);

  EXPECT_CALL(*term_metrics, get_screen_width())
    .WillRepeatedly(Return(80));

  {
    InSequence dummy;
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"abc")));
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"a  ")));
  }

  real_term_output->write_text(L"abc");
  real_term_output->flush();

  real_term_output->move_to_beginning_of_line();
  real_term_output->write_text(L"   ");
  real_term_output->move_to_beginning_of_line();
  real_term_output->write_text(L"a");
  real_term_output->flush();
}


TEST(TrimmedEqTest, testTrimmedEqExact)
{
  EXPECT_THAT("abc", StrTrimmedEq("abc"));
}

TEST(TrimmedEqTest, testTrimmedEqFirstLeftPadded)
{
  EXPECT_THAT("  abc", StrTrimmedEq("abc"));
}

TEST(TrimmedEqTest, testTrimmedEqFirstRightPadded)
{
  EXPECT_THAT("abc  ", StrTrimmedEq("abc"));
}

TEST(TrimmedEqTest, testTrimmedEqFirstBothPadded)
{
  EXPECT_THAT("  abc  ", StrTrimmedEq("abc"));
}

TEST(TrimmedEqTest, testTrimmedEqSecondLeftPadded)
{
  EXPECT_THAT("abc", StrTrimmedEq("  abc"));
}

TEST(TrimmedEqTest, testTrimmedEqSecondRightPadded)
{
  EXPECT_THAT("abc", StrTrimmedEq("abc  "));
}

TEST(TrimmedEqTest, testTrimmedEqSecondBothPadded)
{
  EXPECT_THAT("abc", StrTrimmedEq("  abc  "));
}

TEST(TrimmedEqTest, testTrimmedEqBothBothPadded)
{
  EXPECT_THAT(" abc  ", StrTrimmedEq("   abc    "));
}

TEST(TrimmedEqTest, TrimmedEqWide)
{
  EXPECT_THAT(L" abc  ", StrTrimmedEq(L"   abc     "));
}

TEST(TrimmedEqTest, TrimmedEqWideAndNarrow)
{
  EXPECT_THAT(" abc  ", StrTrimmedEq(L"   abc     "));
}





TEST(TrimmedEqTest, testTrimmedRightEqExact)
{
  EXPECT_THAT(" abc", StrTrimmedRightEq(" abc"));
}

TEST(TrimmedEqTest, testTrimmedRightEqFirstLeftPadded)
{
  EXPECT_THAT("  abc", Not(StrTrimmedRightEq("abc")));
}

TEST(TrimmedEqTest, testTrimmedRightEqFirstRightPadded)
{
  EXPECT_THAT("abc  ", StrTrimmedRightEq("abc"));
}

TEST(TrimmedEqTest, testTrimmedRightEqFirstBothPadded)
{
  EXPECT_THAT("  abc  ", Not(StrTrimmedRightEq("abc")));
}

TEST(TrimmedEqTest, testTrimmedRightEqSecondLeftPadded)
{
  EXPECT_THAT("abc", Not(StrTrimmedRightEq("  abc")));
}

TEST(TrimmedEqTest, testTrimmedRightEqSecondRightPadded)
{
  EXPECT_THAT("abc", StrTrimmedRightEq("abc  "));
}

TEST(TrimmedEqTest, testTrimmedRightEqSecondBothPadded)
{
  EXPECT_THAT("abc", Not(StrTrimmedRightEq("  abc  ")));
}

TEST(TrimmedEqTest, testTrimmedRightEqBothBothPadded)
{
  EXPECT_THAT(" abc  ", Not(StrTrimmedRightEq("   abc    ")));
}

TEST(TrimmedEqTest, TrimmedRightEqWide)
{
  EXPECT_THAT(L" abc  ", Not(StrTrimmedRightEq(L"   abc     ")));
}

TEST(TrimmedEqTest, TrimmedRightEqWideAndNarrow)
{
  EXPECT_THAT(" abc  ", Not(StrTrimmedRightEq(L"   abc     ")));
}
