/** \file test_terminal_mock.cc */

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


#include <cmdline/mocks/terminal.h>
#include <cmdline/terminal.h>

namespace mocks = aptitude::cmdline::mocks;

using boost::shared_ptr;
using testing::InSequence;
using testing::StrEq;
using testing::Test;
using testing::_;

namespace
{
  struct TerminalMock : public Test
  {
    boost::shared_ptr<mocks::combining_terminal_output> terminal;

  public:
    TerminalMock()
      : terminal(mocks::combining_terminal_output::create_strict())
    {
    }
  };
}


// The only behavior that we need to test on the mock is that it
// correctly combines and splits writes using the usual flushing
// behavior.


TEST_F(TerminalMock, WriteEmptyStringDoesNotOutput)
{
  EXPECT_CALL(*terminal, output(_))
    .Times(0);

  terminal->write_text(L"");
  terminal->flush();
}

TEST_F(TerminalMock, WritesMustBeFlushed)
{
  EXPECT_CALL(*terminal, output(_))
    .Times(0);

  // Nothing should be called by this:
  terminal->write_text(L"abc");
}

TEST_F(TerminalMock, MoveToBeginningOfLineMustBeFlushed)
{
  EXPECT_CALL(*terminal, output(_))
    .Times(0);

  terminal->move_to_beginning_of_line();
}

TEST_F(TerminalMock, WriteAndFlush)
{
  EXPECT_CALL(*terminal, output(StrEq(L"abc")));

  terminal->write_text(L"abc");
  terminal->flush();
}

TEST_F(TerminalMock, MoveToBeginningOfLineAndFlush)
{
  EXPECT_CALL(*terminal, output(StrEq(L"\r")));

  terminal->move_to_beginning_of_line();
  terminal->flush();
}

TEST_F(TerminalMock, NewlineIsImplicitFlush)
{
  EXPECT_CALL(*terminal, output(StrEq(L"abc\n")));

  terminal->write_text(L"abc\n");
}

TEST_F(TerminalMock, DoubleFlushDoesNotOutput)
{
  EXPECT_CALL(*terminal, output(StrEq(L"def")));

  terminal->write_text(L"def");
  terminal->flush();
  terminal->flush();
}

TEST_F(TerminalMock, DoubleNewlineOutputsTwice)
{
  {
    InSequence dummy;

    EXPECT_CALL(*terminal, output(StrEq(L"def\n")));
    EXPECT_CALL(*terminal, output(StrEq(L"\n")));
  }

  terminal->write_text(L"def\n\n");
}

TEST_F(TerminalMock, MultipleNewlines)
{
  {
    InSequence dummy;

    EXPECT_CALL(*terminal, output(StrEq(L"abc\n")));
    EXPECT_CALL(*terminal, output(StrEq(L"I like\n")));
    EXPECT_CALL(*terminal, output(StrEq(L"bunnies!\n")));
    EXPECT_CALL(*terminal, output(StrEq(L" -- Burble")));
  }

  terminal->write_text(L"abc\nI like\nbunnies!\n -- Burble");
  terminal->flush();
}

TEST_F(TerminalMock, FlushAfterNewlineDoesNotOutput)
{
  EXPECT_CALL(*terminal, output(StrEq(L"xyz\n")));

  terminal->write_text(L"xyz\n");
  terminal->flush();
}



TEST_F(TerminalMock, FlushCombinesWrites)
{
  EXPECT_CALL(*terminal, output(StrEq(L"abcdef")));

  terminal->write_text(L"abc");
  terminal->write_text(L"def");
  terminal->flush();
}

TEST_F(TerminalMock, FlushCombinesWritesWithMoveToBeginningOfLine)
{
  EXPECT_CALL(*terminal, output(StrEq(L"abc\rdef\rghi")));

  terminal->write_text(L"abc");
  terminal->move_to_beginning_of_line();
  terminal->write_text(L"def");
  terminal->move_to_beginning_of_line();
  terminal->write_text(L"ghi");
  terminal->flush();
}

TEST_F(TerminalMock, NewlineCombinesWrites)
{
  EXPECT_CALL(*terminal, output(StrEq(L"xyzzy\n")));

  terminal->write_text(L"xyz");
  terminal->write_text(L"zy\n");
}

TEST_F(TerminalMock, newlineCombinesWritesWithMoveToBeginningOfLine)
{
  EXPECT_CALL(*terminal, output(StrEq(L"abc\rdef\n")));

  terminal->write_text(L"abc");
  terminal->move_to_beginning_of_line();
  terminal->write_text(L"def");
  terminal->write_text(L"\n");
}

// Check that there's no weirdness when you need to combine and split
// at the same time.
TEST_F(TerminalMock, CombineAndSplit)
{
  {
    InSequence dummy;

    EXPECT_CALL(*terminal, output(StrEq(L"ab\n")));
    EXPECT_CALL(*terminal, output(StrEq(L"de\rfg\n")));
    EXPECT_CALL(*terminal, output(StrEq(L"hijklmn\n")));
    EXPECT_CALL(*terminal, output(StrEq(L"op")));
  }

  terminal->write_text(L"a");
  terminal->write_text(L"b\nde");
  terminal->move_to_beginning_of_line();
  terminal->write_text(L"fg\nhijk");
  terminal->write_text(L"lmn\nop");
  terminal->flush();
}

// Check that the transient message will work properly.
TEST_F(TerminalMock, SimulatedTransientMessage)
{
  {
    InSequence dummy;

    EXPECT_CALL(*terminal, output(StrEq(L"\r\rabc")));
    EXPECT_CALL(*terminal, output(StrEq(L"\r   \ra")));
  }

  terminal->move_to_beginning_of_line();
  terminal->move_to_beginning_of_line();
  terminal->write_text(L"abc");
  terminal->flush();

  terminal->move_to_beginning_of_line();
  terminal->write_text(L"   ");
  terminal->move_to_beginning_of_line();
  terminal->write_text(L"a");
  terminal->flush();
}
