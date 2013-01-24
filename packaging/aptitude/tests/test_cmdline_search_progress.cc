/** \file test_cmdline_search_progress.cc */


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
#include <cmdline/cmdline_search_progress.h>

#include <generic/util/progress_info.h>
#include <generic/util/mocks/throttle.h>
#include <generic/views/mocks/progress.h>

// System includes:
#include <boost/make_shared.hpp>
#include <boost/shared_ptr.hpp>

#include <gmock/gmock.h>
#include <gtest/gtest.h>

using aptitude::cmdline::create_search_progress;
using aptitude::util::progress_info;
using boost::make_shared;
using boost::shared_ptr;
using testing::AnyNumber;
using testing::Expectation;
using testing::Mock;
using testing::Return;
using testing::Sequence;
using testing::Test;
using testing::_;

namespace mocks
{
  using namespace aptitude::util::mocks;
  using namespace aptitude::views::mocks;
}

namespace views = aptitude::views;

namespace
{
  struct CmdlineSearchProgressTest : public Test
  {
    const shared_ptr<mocks::progress> progress;
    const shared_ptr<mocks::throttle> throttle;

    /** \brief The search pattern string used to create the search
     *  progress object.
     */
    const std::string search_pattern;
    const shared_ptr<views::progress> search_progress;

    CmdlineSearchProgressTest()
      : progress(make_shared<mocks::progress>()),
        throttle(make_shared<mocks::throttle>()),
        search_pattern("?name(aptitude)"),
        search_progress(create_search_progress(search_pattern,
                                               progress,
                                               throttle))
    {
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

    void never_throttle()
    {
      EXPECT_CALL(*throttle, update_required())
        .WillRepeatedly(Return(true));
      EXPECT_CALL(*throttle, reset_timer())
        .Times(AnyNumber());
    }

    void always_throttle()
    {
      EXPECT_CALL(*throttle, update_required())
        .WillRepeatedly(Return(false));
      // No call to reset_timer() expected since it's throttled --
      // calling it would, in fact, be wrong.
      EXPECT_CALL(*throttle, reset_timer())
        .Times(0);
    }
  };
}

TEST_F(CmdlineSearchProgressTest, SetProgressNone)
{
  never_throttle();

  EXPECT_CALL(*progress, set_progress(none()));

  search_progress->set_progress(none());
}

TEST_F(CmdlineSearchProgressTest, SetProgressPulse)
{
  never_throttle();

  const std::string msg = "Blip";
  const progress_info info = pulse(search_pattern + ": " + msg);

  EXPECT_CALL(*progress, set_progress(info));

  search_progress->set_progress(pulse(msg));
}

TEST_F(CmdlineSearchProgressTest, SetProgressBar)
{
  never_throttle();

  const double fraction = 0.3;
  const std::string msg = "Blorp";
  const progress_info info = bar(fraction, search_pattern + ": " + msg);

  EXPECT_CALL(*progress, set_progress(info));

  search_progress->set_progress(bar(fraction, msg));
}

TEST_F(CmdlineSearchProgressTest, ThrottledProgressNone)
{
  always_throttle();

  EXPECT_CALL(*progress, set_progress(_))
    .Times(0);

  search_progress->set_progress(none());


  // Check that a second set_progress() call goes through, since
  // throttling is no longer enabled:
  Mock::VerifyAndClearExpectations(throttle.get());
  Mock::VerifyAndClearExpectations(progress.get());

  never_throttle();

  EXPECT_CALL(*progress, set_progress(none()));

  search_progress->set_progress(none());
}

TEST_F(CmdlineSearchProgressTest, ThrottledProgressPulse)
{
  const std::string msg = "The caged whale knows not the mighty deeps";
  const progress_info info = pulse(msg);

  always_throttle();

  EXPECT_CALL(*progress, set_progress(_))
    .Times(0);

  search_progress->set_progress(info);


  // Check that a second set_progress() call goes through, since
  // throttling is no longer enabled:
  Mock::VerifyAndClearExpectations(throttle.get());
  Mock::VerifyAndClearExpectations(progress.get());

  never_throttle();

  EXPECT_CALL(*progress,
              set_progress(pulse(search_pattern + ": " + msg)));

  search_progress->set_progress(info);
}

TEST_F(CmdlineSearchProgressTest, ThrottledProgressBar)
{
  const std::string msg = "Reversing polarity";
  const double fraction = 0.8;
  const progress_info info = bar(fraction, msg);

  always_throttle();

  EXPECT_CALL(*progress, set_progress(_))
    .Times(0);

  search_progress->set_progress(info);


  // Check that a second set_progress() call goes through, since
  // throttling is no longer enabled:
  Mock::VerifyAndClearExpectations(throttle.get());
  Mock::VerifyAndClearExpectations(progress.get());

  never_throttle();

  EXPECT_CALL(*progress,
              set_progress(bar(fraction, search_pattern + ": " + msg)));

  search_progress->set_progress(info);
}

TEST_F(CmdlineSearchProgressTest, Done)
{
  never_throttle();
  EXPECT_CALL(*progress, done());

  search_progress->done();
}

// Test that Done ignores throttling.
TEST_F(CmdlineSearchProgressTest, DoneThrottled)
{
  always_throttle();
  EXPECT_CALL(*progress, done());

  search_progress->done();
}
