/** \file test_transient_message.cc */


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
#include <cmdline/terminal.h>
#include <cmdline/transient_message.h>

// Global includes:
#include <gmock/gmock.h>
#include <gtest/gtest.h>

namespace mocks = aptitude::cmdline::mocks;

using aptitude::cmdline::create_transient_message;
using aptitude::cmdline::mocks::StrTrimmedRightEq;
using aptitude::cmdline::transient_message;
using boost::shared_ptr;
using testing::InSequence;
using testing::Return;
using testing::StrEq;
using testing::Test;
using testing::_;

namespace
{
  // We'll pretend that '=' is two columns wide.
  const wchar_t two_column_char = L'=';

  struct TransientMessage : public Test
  {
    shared_ptr<mocks::terminal_locale> term_locale;
    shared_ptr<mocks::terminal_metrics> term_metrics;
    shared_ptr<mocks::combining_terminal_output> term_output;
    shared_ptr<mocks::teletype> teletype;
    shared_ptr<transient_message> message;
    std::wstring widechar;

    // I need to set up expectations on the terminal during member
    // initialization, since some of the other member initializers
    // cause methods to be invoked on it.
    static shared_ptr<mocks::combining_terminal_output> create_terminal_output()
    {
      shared_ptr<mocks::combining_terminal_output> rval =
        mocks::combining_terminal_output::create_strict();

      EXPECT_CALL(*rval, output_is_a_terminal())
        .WillRepeatedly(Return(true));

      return rval;
    }

    static shared_ptr<mocks::terminal_metrics> create_terminal_metrics()
    {
      shared_ptr<mocks::terminal_metrics> rval =
        mocks::terminal_metrics::create_strict();

      EXPECT_CALL(*rval, get_screen_width())
        .WillRepeatedly(Return(80));

      return rval;
    }

    TransientMessage()
      : term_locale(mocks::terminal_locale::create_strict()),
        term_metrics(create_terminal_metrics()),
        term_output(create_terminal_output()),
        teletype(mocks::create_strict_teletype(term_locale, term_metrics, term_output)),
        message(create_transient_message(term_locale, term_metrics, term_output)),
        widechar(1, two_column_char)
    {
      EXPECT_CALL(*term_locale, wcwidth(two_column_char))
        .WillRepeatedly(Return(2));
    }
  };
}

TEST_F(TransientMessage, SetText)
{
  EXPECT_CALL(*teletype, set_last_line(StrTrimmedRightEq(L"abc")));

  message->set_text(L"abc");
}

TEST_F(TransientMessage, DisplayAndAdvanceBasic)
{
  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrEq(L"abcdefghi")));
    EXPECT_CALL(*teletype, newline());
  }

  message->display_and_advance(L"abcdefghi");
}

TEST_F(TransientMessage, DisplayAndAdvanceWrapping)
{
  EXPECT_CALL(*term_metrics, get_screen_width())
    .WillRepeatedly(Return(4));

  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrEq(L"abcd")));
    EXPECT_CALL(*teletype, newline());
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"efgh")));
    EXPECT_CALL(*teletype, newline());
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"ij")));
    EXPECT_CALL(*teletype, newline());
  }

  message->display_and_advance(L"abcdefghij");
}

TEST_F(TransientMessage, DisplayAndAdvanceClearsExistingText)
{
  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrTrimmedRightEq(L"xyzw")));
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"abcd")));
    EXPECT_CALL(*teletype, newline());
  }

  message->set_text(L"xyzw");
  message->display_and_advance(L"abcd");
}

TEST_F(TransientMessage, DisplayAndAdvanceWithoutTerminal)
{
  EXPECT_CALL(*term_output, output_is_a_terminal())
    .WillRepeatedly(Return(false));
  EXPECT_CALL(*term_metrics, get_screen_width())
    .WillRepeatedly(Return(4));

  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrEq(L"xyzw")));
    EXPECT_CALL(*teletype, newline());
    EXPECT_CALL(*teletype, set_last_line(StrEq(L"abcd")));
    EXPECT_CALL(*teletype, newline());
  }

  // Need to create a new message object since it reads and caches the
  // value of output_is_a_terminal() when it's created.
  const shared_ptr<transient_message> requiring_message =
    create_transient_message(term_locale, term_metrics, term_output);

  requiring_message->display_and_advance(L"xyzwabcd");
}

TEST_F(TransientMessage, ClearText)
{
  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrTrimmedRightEq("abc")));
    EXPECT_CALL(*teletype, set_last_line(StrTrimmedRightEq("")));
  }

  message->set_text(L"abc");
  message->set_text(L"");
}

TEST_F(TransientMessage, ReplaceTextWithShorter)
{
  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrTrimmedRightEq(L"abc")));
    EXPECT_CALL(*teletype, set_last_line(StrTrimmedRightEq(L"a")));
  }

  message->set_text(L"abc");
  message->set_text(L"a");
}

TEST_F(TransientMessage, ReplaceTextWithSameLength)
{
  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrTrimmedRightEq(L"abc")));
    EXPECT_CALL(*teletype, set_last_line(StrTrimmedRightEq(L"xyz")));
  }

  message->set_text(L"abc");
  message->set_text(L"xyz");
}

TEST_F(TransientMessage, ReplaceTextWithLonger)
{
  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrTrimmedRightEq(L"abc")));
    EXPECT_CALL(*teletype, set_last_line(StrTrimmedRightEq(L"xyzw")));
  }

  message->set_text(L"abc");
  message->set_text(L"xyzw");
}

TEST_F(TransientMessage, ReplaceWideCharTextWithShorter)
{
  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrTrimmedRightEq(widechar + widechar + widechar)));
    EXPECT_CALL(*teletype, set_last_line(StrTrimmedRightEq(L"xyz")));
  }

  message->set_text(widechar + widechar + widechar);
  message->set_text(L"xyz");
}

TEST_F(TransientMessage, ReplaceWideCharTextWithSameLength)
{
  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrTrimmedRightEq(widechar + widechar)));
    EXPECT_CALL(*teletype, set_last_line(StrTrimmedRightEq(L"abcd")));
  }

  message->set_text(widechar + widechar);
  message->set_text(L"abcd");
}

TEST_F(TransientMessage, ReplaceWideCharTextWithLonger)
{
  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrTrimmedRightEq(widechar)));
    EXPECT_CALL(*teletype, set_last_line(StrTrimmedRightEq(L"abc")));
  }

  message->set_text(widechar);
  message->set_text(L"abc");
}

TEST_F(TransientMessage, TruncateLongLine)
{
  EXPECT_CALL(*term_metrics, get_screen_width())
    .WillRepeatedly(Return(4));

  EXPECT_CALL(*teletype, set_last_line(StrTrimmedRightEq("abcd")));

  message->set_text(L"abcdefghijklmnopqrstuvwxyz");
}

TEST_F(TransientMessage, ReplaceTruncatedLongLineWithNonTruncated)
{
  EXPECT_CALL(*term_metrics, get_screen_width())
    .WillRepeatedly(Return(4));

  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrTrimmedRightEq("abcd")));
    EXPECT_CALL(*teletype, set_last_line(StrTrimmedRightEq("xyz")));
  }

  message->set_text(L"abcdefghijklmnopqrstuvwxyz");
  message->set_text(L"xyz");
}

TEST_F(TransientMessage, ReplaceTruncatedLongLineWithTruncated)
{
  EXPECT_CALL(*term_metrics, get_screen_width())
    .WillRepeatedly(Return(4));

  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrTrimmedRightEq("abcd")));
    EXPECT_CALL(*teletype, set_last_line(StrTrimmedRightEq("zyxw")));
  }

  message->set_text(L"abcdefghijklmnopqrstuvwxyz");
  message->set_text(L"zyxwvuts");
}

TEST_F(TransientMessage, TruncateWideCharLine)
{
  EXPECT_CALL(*term_metrics, get_screen_width())
    .WillRepeatedly(Return(4));

  EXPECT_CALL(*teletype, set_last_line(StrTrimmedRightEq(L"ab" + widechar)));

  message->set_text(L"ab" + widechar + L"cdef");
}

TEST_F(TransientMessage, TruncateWideCharLineWithSplit)
{
  EXPECT_CALL(*term_metrics, get_screen_width())
    .WillRepeatedly(Return(4));

  EXPECT_CALL(*teletype, set_last_line(StrTrimmedRightEq(L"abc")));

  message->set_text(L"abc" + widechar + L"def");
}

TEST_F(TransientMessage, ReplaceTruncatedWideCharLine)
{
  EXPECT_CALL(*term_metrics, get_screen_width())
    .WillRepeatedly(Return(4));

  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrTrimmedRightEq(widechar + widechar)));
    EXPECT_CALL(*teletype, set_last_line(StrTrimmedRightEq(L"z")));
  }

  message->set_text(widechar + widechar + L"abcdef");
  message->set_text(L"z");
}

TEST_F(TransientMessage, RequireTtyDecorationsWithTty)
{
  EXPECT_CALL(*term_output, output_is_a_terminal())
    .WillRepeatedly(Return(true));

  {
    InSequence dummy;

    EXPECT_CALL(*teletype, set_last_line(StrTrimmedRightEq(L"abc")));
    EXPECT_CALL(*teletype, set_last_line(StrTrimmedRightEq(L"xyz")));
  }

  // Need to create a new message object since it reads and caches the
  // value of output_is_a_terminal() when it's created.
  const shared_ptr<transient_message> requiring_message =
    create_transient_message(term_locale, term_metrics, term_output);

  requiring_message->set_text(L"abc");
  requiring_message->set_text(L"xyz");
}

TEST_F(TransientMessage, RequireTtyDecorationsWithoutTty)
{
  EXPECT_CALL(*term_output, output_is_a_terminal())
    .WillRepeatedly(Return(false));

  EXPECT_CALL(*teletype, set_last_line(_))
    .Times(0);

  // Need to create a new message object since it reads and caches the
  // value of output_is_a_terminal() when it's created.
  const shared_ptr<transient_message> requiring_message =
    create_transient_message(term_locale, term_metrics, term_output);

  requiring_message->set_text(L"abc");
  requiring_message->set_text(L"xyz");
}
