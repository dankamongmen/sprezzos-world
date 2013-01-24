/** \file test_cmdline_progress_display.cc */


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
#include <cmdline/cmdline_progress_display.h>
#include <cmdline/mocks/transient_message.h>

#include <generic/util/progress_info.h>
#include <generic/views/progress.h>

// System includes:
#include <boost/make_shared.hpp>
#include <boost/shared_ptr.hpp>

#include <gmock/gmock.h>
#include <gtest/gtest.h>

using aptitude::cmdline::create_progress_display;
using aptitude::util::progress_info;
using boost::make_shared;
using boost::shared_ptr;
using testing::AnyNumber;
using testing::AtMost;
using testing::Expectation;
using testing::HasSubstr;
using testing::StrEq;
using testing::StrNe;
using testing::TestWithParam;
using testing::Values;
using testing::_;

namespace mocks = aptitude::cmdline::mocks;
namespace views = aptitude::views;

namespace
{
  class CmdlineProgressDisplayParams
  {
    bool old_style_percentage;
    bool retain_completed;

  public:
    CmdlineProgressDisplayParams(bool _old_style_percentage,
                                 bool _retain_completed)
      : old_style_percentage(_old_style_percentage),
        retain_completed(_retain_completed)
    {
    }

    bool get_old_style_percentage() const { return old_style_percentage; }
    bool get_retain_completed() const { return retain_completed; }
  };

  std::ostream &operator<<(std::ostream &out,
                           const CmdlineProgressDisplayParams &params)
  {
    return out << "("
               << "old_style_percentage = "
               << (params.get_old_style_percentage() ? "true" : "false")
               << ", "
               << "retain_completed = "
               << (params.get_retain_completed() ? "true" : "false")
               << ")";
  }

  struct CmdlineProgressDisplayTest
    : public TestWithParam<CmdlineProgressDisplayParams>
  {
    shared_ptr<mocks::transient_message> msg;
    shared_ptr<views::progress> progress;

    bool get_old_style_percentage() const
    {
      return GetParam().get_old_style_percentage();
    }

    bool get_retain_completed() const
    {
      return GetParam().get_retain_completed();
    }

    CmdlineProgressDisplayTest()
      : msg(mocks::transient_message::create_strict())
    {
    }

    // It's not documented anywhere, but from the examples and my
    // intuition about how TEST_P is probably implemented, I bet that
    // we can't access GetParm() until SetUp:
    void SetUp()
    {
      progress = create_progress_display(msg,
                                         get_old_style_percentage(),
                                         get_retain_completed());
    }

    // Define shorter names for the progress_info constructors.
    progress_info none()
    {
      return progress_info::none();
    }

    progress_info pulse(const std::string &msg)
    {
      return progress_info::pulse(msg);
    }

    progress_info bar(double fraction, const std::string &msg)
    {
      return progress_info::bar(fraction, msg);
    }
  };
}

#define EXPECT_NO_RETAIN_COMPLETED()            \
  EXPECT_CALL(*msg, display_and_advance(_)).Times(0)

TEST_P(CmdlineProgressDisplayTest, InitialShowNoneHasNoEffect)
{
  EXPECT_CALL(*msg, set_text(_))
    .Times(0);
  EXPECT_NO_RETAIN_COMPLETED();

  progress->set_progress(none());
}

TEST_P(CmdlineProgressDisplayTest, ShowNoneAfterPulse)
{
  EXPECT_NO_RETAIN_COMPLETED();

  EXPECT_CALL(*msg, set_text(StrNe(L"")));
  EXPECT_CALL(*msg, set_text(StrEq(L"")));

  progress->set_progress(pulse("xyzzy"));
  progress->set_progress(none());
}

TEST_P(CmdlineProgressDisplayTest, ShowPulse)
{
  if(get_old_style_percentage())
    EXPECT_CALL(*msg, set_text(StrEq(L"Hello world...")));
  else
    EXPECT_CALL(*msg, set_text(StrEq(L"[----] Hello world")));

  EXPECT_NO_RETAIN_COMPLETED();

  progress->set_progress(pulse("Hello world"));
}

TEST_P(CmdlineProgressDisplayTest, ShowBarNegative)
{
  if(get_old_style_percentage())
    EXPECT_CALL(*msg, set_text(StrEq(L"Before the beginning of the world... 0%")));
  else
    EXPECT_CALL(*msg, set_text(StrEq(L"[  0%] Before the beginning of the world")));

  EXPECT_NO_RETAIN_COMPLETED();

  progress->set_progress(bar(-1, "Before the beginning of the world"));
}

TEST_P(CmdlineProgressDisplayTest, ShowBarZero)
{
  if(get_old_style_percentage())
    EXPECT_CALL(*msg, set_text(StrEq(L"Beginning world... 0%")));
  else
    EXPECT_CALL(*msg, set_text(StrEq(L"[  0%] Beginning world")));

  EXPECT_NO_RETAIN_COMPLETED();

  progress->set_progress(bar(0, "Beginning world"));
}

TEST_P(CmdlineProgressDisplayTest, ShowBarRoundUp)
{
  if(get_old_style_percentage())
    EXPECT_CALL(*msg, set_text(StrEq(L"Almost middle world... 50%")));
  else
    EXPECT_CALL(*msg, set_text(StrEq(L"[ 50%] Almost middle world")));

  EXPECT_NO_RETAIN_COMPLETED();

  progress->set_progress(bar(0.499, "Almost middle world"));
}

TEST_P(CmdlineProgressDisplayTest, ShowBarRoundUpInMiddle)
{
  if(get_old_style_percentage())
    EXPECT_CALL(*msg, set_text(StrEq(L"Almost middle world... 50%")));
  else
    EXPECT_CALL(*msg, set_text(StrEq(L"[ 50%] Almost middle world")));

  EXPECT_NO_RETAIN_COMPLETED();

  progress->set_progress(bar(0.495, "Almost middle world"));
}

TEST_P(CmdlineProgressDisplayTest, ShowBarMiddleExact)
{
  if(get_old_style_percentage())
    EXPECT_CALL(*msg, set_text(StrEq(L"Middle world... 50%")));
  else
    EXPECT_CALL(*msg, set_text(StrEq(L"[ 50%] Middle world")));
  EXPECT_NO_RETAIN_COMPLETED();

  progress->set_progress(bar(0.50, "Middle world"));
}

TEST_P(CmdlineProgressDisplayTest, ShowBarRoundDown)
{
  if(get_old_style_percentage())
    EXPECT_CALL(*msg, set_text(StrEq(L"Just past middle world... 50%")));
  else
    EXPECT_CALL(*msg, set_text(StrEq(L"[ 50%] Just past middle world")));
  EXPECT_NO_RETAIN_COMPLETED();

  progress->set_progress(bar(0.501, "Just past middle world"));
}

TEST_P(CmdlineProgressDisplayTest, ShowBarComplete)
{
  if(get_old_style_percentage())
    EXPECT_CALL(*msg, set_text(StrEq(L"Finished world... 100%")));
  else
    EXPECT_CALL(*msg, set_text(StrEq(L"[100%] Finished world")));
  EXPECT_NO_RETAIN_COMPLETED();

  progress->set_progress(bar(1, "Finished world"));
}

TEST_P(CmdlineProgressDisplayTest, FractionTooLarge)
{
  if(get_old_style_percentage())
    EXPECT_CALL(*msg, set_text(StrEq(L"Past the end of the world... 100%")));
  else
    EXPECT_CALL(*msg, set_text(StrEq(L"[100%] Past the end of the world")));
  EXPECT_NO_RETAIN_COMPLETED();

  progress->set_progress(bar(100, "Past the end of the world"));
}

TEST_P(CmdlineProgressDisplayTest, SuppressDuplicateNones)
{
  EXPECT_CALL(*msg, set_text(StrNe(L"")));
  EXPECT_CALL(*msg, set_text(StrEq(L"")));
  EXPECT_NO_RETAIN_COMPLETED();

  progress->set_progress(pulse("Irrelevant data"));
  progress->set_progress(none());
  progress->set_progress(none());
}

TEST_P(CmdlineProgressDisplayTest, PulseSuppressesDuplicateMessage)
{
  if(get_old_style_percentage())
    EXPECT_CALL(*msg, set_text(StrEq(L"Some message...")));
  else
    EXPECT_CALL(*msg, set_text(StrEq(L"[----] Some message")));
  EXPECT_NO_RETAIN_COMPLETED();

  progress->set_progress(pulse("Some message"));
  progress->set_progress(pulse("Some message"));
}

TEST_P(CmdlineProgressDisplayTest, BarSuppressesDuplicateSettings)
{
  if(get_old_style_percentage())
    EXPECT_CALL(*msg, set_text(StrEq(L"Some message... 66%")));
  else
    EXPECT_CALL(*msg, set_text(StrEq(L"[ 66%] Some message")));
  EXPECT_NO_RETAIN_COMPLETED();

  progress->set_progress(bar(0.66, "Some message"));
  progress->set_progress(bar(0.66, "Some message"));
}

TEST_P(CmdlineProgressDisplayTest, BarSuppressesAlmostDuplicateSettings)
{
  if(get_old_style_percentage())
    EXPECT_CALL(*msg, set_text(StrEq(L"Some message... 66%")));
  else
    EXPECT_CALL(*msg, set_text(StrEq(L"[ 66%] Some message")));
  EXPECT_NO_RETAIN_COMPLETED();

  progress->set_progress(bar(0.661, "Some message"));
  progress->set_progress(bar(0.662, "Some message"));
}

TEST_P(CmdlineProgressDisplayTest, CanSwitchFromNoneToPulse)
{
  if(get_old_style_percentage())
    EXPECT_CALL(*msg, set_text(StrEq(L"Pulse...")));
  else
    EXPECT_CALL(*msg, set_text(StrEq(L"[----] Pulse")));
  EXPECT_NO_RETAIN_COMPLETED();

  progress->set_progress(none());
  progress->set_progress(pulse("Pulse"));
}

TEST_P(CmdlineProgressDisplayTest, CanSwitchFromNoneToEmptyBar)
{
  if(get_old_style_percentage())
    EXPECT_CALL(*msg, set_text(StrEq(L"... 0%")));
  else
    EXPECT_CALL(*msg, set_text(StrEq(L"[  0%] ")));
  EXPECT_NO_RETAIN_COMPLETED();

  progress->set_progress(none());
  progress->set_progress(bar(0, ""));
}

TEST_P(CmdlineProgressDisplayTest, CanSwitchFromNoneToNonEmptyBar)
{
  if(get_old_style_percentage())
    EXPECT_CALL(*msg, set_text(StrEq(L"Pelagic argosy... 33%")));
  else
    EXPECT_CALL(*msg, set_text(StrEq(L"[ 33%] Pelagic argosy")));
  EXPECT_NO_RETAIN_COMPLETED();

  progress->set_progress(none());
  progress->set_progress(bar(0.33, "Pelagic argosy"));
}

TEST_P(CmdlineProgressDisplayTest, CanSwitchFromPulseToNone)
{
  if(get_old_style_percentage())
    EXPECT_CALL(*msg, set_text(StrEq(L"Beware the leopard...")));
  else
    EXPECT_CALL(*msg, set_text(StrEq(L"[----] Beware the leopard")));
  EXPECT_CALL(*msg, set_text(StrEq(L"")));
  EXPECT_NO_RETAIN_COMPLETED();

  progress->set_progress(pulse("Beware the leopard"));
  progress->set_progress(none());
}

TEST_P(CmdlineProgressDisplayTest, CanSwitchFromPulseToZeroBar)
{
  // Check that it doesn't get confused and think that the second
  // setting is identical to the first.
  if(get_old_style_percentage())
    {
      EXPECT_CALL(*msg, set_text(StrEq(L"Wobbly Weasel...")));
      EXPECT_CALL(*msg, set_text(StrEq(L"Wobbly Weasel... 0%")));
    }
  else
    {
      EXPECT_CALL(*msg, set_text(StrEq(L"[----] Wobbly Weasel")));
      EXPECT_CALL(*msg, set_text(StrEq(L"[  0%] Wobbly Weasel")));
    }
  EXPECT_NO_RETAIN_COMPLETED();

  progress->set_progress(pulse("Wobbly Weasel"));
  progress->set_progress(bar(0, "Wobbly Weasel"));
}

TEST_P(CmdlineProgressDisplayTest, CanSwitchFromZeroBarToNone)
{
  if(get_old_style_percentage())
    {
      EXPECT_CALL(*msg, set_text(StrEq(L"Bar None... 0%")));
      EXPECT_CALL(*msg, set_text(StrEq(L"")));
    }
  else
    {
      EXPECT_CALL(*msg, set_text(StrEq(L"[  0%] Bar None")));
      EXPECT_CALL(*msg, set_text(StrEq(L"")));
    }

  EXPECT_NO_RETAIN_COMPLETED();

  progress->set_progress(bar(0, "Bar None"));
  progress->set_progress(none());
}

TEST_P(CmdlineProgressDisplayTest, CanSwitchFromNonZeroBarToNone)
{
  if(get_old_style_percentage())
    EXPECT_CALL(*msg, set_text(StrEq(L"Albatross... 21%")));
  else
    EXPECT_CALL(*msg, set_text(StrEq(L"[ 21%] Albatross")));

  EXPECT_CALL(*msg, set_text(StrEq(L"")));
  EXPECT_NO_RETAIN_COMPLETED();

  progress->set_progress(bar(0.21, "Albatross"));
  progress->set_progress(none());
}

TEST_P(CmdlineProgressDisplayTest, CanSwitchFromZeroBarToPulse)
{
  if(get_old_style_percentage())
    {
      EXPECT_CALL(*msg, set_text(StrEq(L"All I've got is this bloody albatross... 0%")));
      EXPECT_CALL(*msg, set_text(StrEq(L"All I've got is this bloody albatross...")));
    }
  else
    {
      EXPECT_CALL(*msg, set_text(StrEq(L"[  0%] All I've got is this bloody albatross")));
      EXPECT_CALL(*msg, set_text(StrEq(L"[----] All I've got is this bloody albatross")));
    }
  EXPECT_NO_RETAIN_COMPLETED();

  progress->set_progress(bar(0, "All I've got is this bloody albatross"));
  progress->set_progress(pulse("All I've got is this bloody albatross"));
}

TEST_P(CmdlineProgressDisplayTest, CanSwitchFromNonZeroBarToPulse)
{
  if(get_old_style_percentage())
    {
      EXPECT_CALL(*msg, set_text(StrEq(L"Bop... 11%")));
      EXPECT_CALL(*msg, set_text(StrEq(L"Bop...")));
    }
  else
    {
      EXPECT_CALL(*msg, set_text(StrEq(L"[ 11%] Bop")));
      EXPECT_CALL(*msg, set_text(StrEq(L"[----] Bop")));
    }
  EXPECT_NO_RETAIN_COMPLETED();

  progress->set_progress(bar(0.11, "Bop"));
  progress->set_progress(pulse("Bop"));
}

TEST_P(CmdlineProgressDisplayTest, CanSwitchFromNonZeroBarToDifferentPulse)
{
  if(get_old_style_percentage())
    {
      EXPECT_CALL(*msg, set_text(StrEq(L"Zip... 11%")));
      EXPECT_CALL(*msg, set_text(StrEq(L"Bop...")));
    }
  else
    {
      EXPECT_CALL(*msg, set_text(StrEq(L"[ 11%] Zip")));
      EXPECT_CALL(*msg, set_text(StrEq(L"[----] Bop")));
    }
  EXPECT_NO_RETAIN_COMPLETED();

  progress->set_progress(bar(0.11, "Zip"));
  progress->set_progress(pulse("Bop"));
}

TEST_P(CmdlineProgressDisplayTest, DoneAfterNoneHasNoEffect)
{
  // This behavior is tested elsewhere:
  EXPECT_CALL(*msg, set_text(StrEq(L"")))
    .Times(AtMost(1));

  EXPECT_NO_RETAIN_COMPLETED();

  progress->set_progress(none());
  progress->done();
  // Test multiple done()s too.
  progress->done();
  progress->done();
}

TEST_P(CmdlineProgressDisplayTest, DoneAfterPulse)
{
  // Expect one call to set_text() up-front, to set up the pre-done
  // display.
  Expectation text_set;
  if(get_old_style_percentage())
    text_set = EXPECT_CALL(*msg, set_text(StrEq(L"Marvelous Monkey...")));
  else
    text_set = EXPECT_CALL(*msg, set_text(StrEq(L"[----] Marvelous Monkey")));

  Expectation maybe_done = text_set;

  // First, we should get a Done if we're retaining completed tasks.
  if(get_retain_completed())
    {
      if(get_old_style_percentage())
        maybe_done =
          EXPECT_CALL(*msg, display_and_advance(StrEq(L"Marvelous Monkey... Done")))
          .After(text_set);
      else
        maybe_done =
          EXPECT_CALL(*msg, display_and_advance(StrEq(L"[DONE] Marvelous Monkey")))
          .After(text_set);
    }
  else
    EXPECT_CALL(*msg, set_text(StrEq(L"")));

  progress->set_progress(pulse("Marvelous Monkey"));
  progress->done();
}

TEST_P(CmdlineProgressDisplayTest, DoneAfterBar)
{
  // Expect one call to set_text() up-front, to set up the pre-done display.
  Expectation text_set;

  if(get_old_style_percentage())
    text_set = EXPECT_CALL(*msg, set_text(StrEq(L"Ack... 98%")));
  else
    text_set = EXPECT_CALL(*msg, set_text(StrEq(L"[ 98%] Ack")));

  Expectation maybe_done = text_set;

  // First, we should get a Done if we're retaining completed tasks.
  if(get_retain_completed())
    {
      if(get_old_style_percentage())
        maybe_done =
          EXPECT_CALL(*msg, display_and_advance(StrEq(L"Ack... Done")))
          .After(text_set);
      else
        maybe_done =
          EXPECT_CALL(*msg, display_and_advance(StrEq(L"[DONE] Ack")))
          .After(text_set);
    }
  else
    EXPECT_CALL(*msg, set_text(StrEq(L"")));

  progress->set_progress(bar(0.98, "Ack"));
  progress->done();
}

TEST_P(CmdlineProgressDisplayTest, DoneAfterDone)
{
  // This test is exactly identical to the pulse() test, except that
  // we invoke done() twice.  The point is that the second done() has
  // no effect.


  // Expect one call to set_text() up-front, to set up the pre-done
  // display.
  Expectation text_set;
  if(get_old_style_percentage())
    text_set = EXPECT_CALL(*msg, set_text(StrEq(L"Marvelous Monkey...")));
  else
    text_set = EXPECT_CALL(*msg, set_text(StrEq(L"[----] Marvelous Monkey")));

  Expectation maybe_done = text_set;

  // First, we should get a Done if we're retaining completed tasks.
  if(get_retain_completed())
    {
      if(get_old_style_percentage())
        maybe_done =
          EXPECT_CALL(*msg, display_and_advance(StrEq(L"Marvelous Monkey... Done")))
          .After(text_set);
      else
        maybe_done =
          EXPECT_CALL(*msg, display_and_advance(StrEq(L"[DONE] Marvelous Monkey")))
          .After(text_set);
    }
  else
    EXPECT_CALL(*msg, set_text(StrEq(L"")));

  progress->set_progress(pulse("Marvelous Monkey"));
  progress->done();
  progress->done();
}

TEST_P(CmdlineProgressDisplayTest, NoneAfterDone)
{
  // This test is exactly identical to the pulse() test, except that
  // we send none() after invoking done().  The point is that the
  // none() has no effect even though the previously set progress
  // value was not none().


  // Expect one call to set_text() up-front, to set up the pre-done
  // display.
  Expectation text_set;
  if(get_old_style_percentage())
    text_set = EXPECT_CALL(*msg, set_text(StrEq(L"Marvelous Monkey...")));
  else
    text_set = EXPECT_CALL(*msg, set_text(StrEq(L"[----] Marvelous Monkey")));

  Expectation maybe_done = text_set;

  // First, we should get a Done if we're retaining completed tasks.
  if(get_retain_completed())
    {
      if(get_old_style_percentage())
        maybe_done =
          EXPECT_CALL(*msg, display_and_advance(StrEq(L"Marvelous Monkey... Done")))
          .After(text_set);
      else
        maybe_done =
          EXPECT_CALL(*msg, display_and_advance(StrEq(L"[DONE] Marvelous Monkey")))
          .After(text_set);
    }
  else
    EXPECT_CALL(*msg, set_text(StrEq(L"")));

  progress->set_progress(pulse("Marvelous Monkey"));
  progress->done();
  progress->set_progress(none());
  // See what happens if a few more none()s are set.
  progress->set_progress(none());
  progress->set_progress(none());
}

INSTANTIATE_TEST_CASE_P(CmdlineProgressDisplayTestOldStyleAndRetainCompleted,
                        CmdlineProgressDisplayTest,
                        Values(CmdlineProgressDisplayParams(true, true)));

INSTANTIATE_TEST_CASE_P(CmdlineProgressDisplayTestOldStyleAndNoRetainCompleted,
                        CmdlineProgressDisplayTest,
                        Values(CmdlineProgressDisplayParams(true, false)));

INSTANTIATE_TEST_CASE_P(CmdlineProgressDisplayTestNewStyleAndRetainCompleted,
                        CmdlineProgressDisplayTest,
                        Values(CmdlineProgressDisplayParams(false, true)));

INSTANTIATE_TEST_CASE_P(CmdlineProgressDisplayTestNewStyleAndNoRetainCompleted,
                        CmdlineProgressDisplayTest,
                        Values(CmdlineProgressDisplayParams(false, false)));
