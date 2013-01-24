/** \file test_cmdline_download_status_display.cc */

// Copyright (C) 2010-2011 Daniel Burrows
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

#include <cmdline/cmdline_download_progress_display.h>
#include <cmdline/mocks/terminal.h>
#include <cmdline/mocks/transient_message.h>

#include <generic/views/download_progress.h>

// System includes:
#include <boost/make_shared.hpp>
#include <boost/shared_ptr.hpp>

#include <cwidget/generic/util/transcode.h>

#include <gmock/gmock.h>
#include <gtest/gtest.h>

using aptitude::cmdline::create_cmdline_download_status_display;
using aptitude::cmdline::download_status_display;
using aptitude::views::download_progress;
using boost::make_shared;
using boost::optional;
using boost::shared_ptr;
using testing::Return;
using testing::StrEq;
using testing::TestWithParam;
using testing::Values;
using testing::_;

namespace cw = cwidget;

namespace mocks
{
  using namespace aptitude::cmdline::mocks;
}

namespace
{
  // We pretend this character occupies two cells and all the rest
  // occupy only one.
  const wchar_t two_column_char = L'-';

  // Locally alias transcode to always use UTF-8.
  std::wstring transcode(const std::string &s)
  {
    return cw::util::transcode(s, "UTF-8");
  }

  std::string transcode(const std::wstring &s)
  {
    return cw::util::transcode(s, "UTF-8");
  }

  // The test parameter hides the status if it's "true"; it's used to
  // verify that suppressing messages actually works.
  struct CmdlineDownloadStatusDisplayTest : public TestWithParam<bool>
  {
    shared_ptr<mocks::transient_message> msg;
    shared_ptr<mocks::terminal_locale> term_locale;
    shared_ptr<mocks::terminal_metrics> term_metrics;

    shared_ptr<download_status_display> status_display;

    std::wstring widechstr;

    typedef download_progress::file_progress file_progress;
    typedef download_progress::status status;
    typedef download_progress::status::worker_status worker_status;
    file_progress no_progress;
    file_progress almost_half_complete_round_down;
    file_progress almost_half_complete_round_down_with_complete_flag;
    file_progress almost_half_complete_round_up;
    file_progress half_complete;
    file_progress full_progress;
    file_progress no_progress_out_of_zero;
    file_progress overcomplete_progress;
    file_progress overcomplete_progress_out_of_zero;
    file_progress overcomplete_progress_out_of_zero_with_complete_flag;

    file_progress no_progress_with_mode_no_id_no_description;
    file_progress no_progress_with_mode_with_id_no_description;
    file_progress no_progress_with_mode_no_id_with_description;
    file_progress no_progress_with_mode_with_id_with_description;
    file_progress no_progress_out_of_zero_with_mode;

    CmdlineDownloadStatusDisplayTest()
      : msg(make_shared<mocks::transient_message>()),
        term_locale(mocks::terminal_locale::create_strict()),
        term_metrics(mocks::terminal_metrics::create_strict()),
        no_progress(0, 10, false, "no progress", optional<unsigned long>(), ""),
        almost_half_complete_round_down(454, 1000, false, "round down", 1, ""),
        almost_half_complete_round_down_with_complete_flag(454, 1000, true, "round down", 1, ""),
        almost_half_complete_round_up(455, 1000, false, "round up", 2, ""),
        half_complete(5, 10, false, "half complete", 3, ""),
        full_progress(10, 10, false, "complete", 4, ""),
        no_progress_out_of_zero(0, 0, false, "no progress zero", 5, ""),
        overcomplete_progress(15, 10, false, "overcomplete progress", 6, ""),
        overcomplete_progress_out_of_zero(15, 0, false, "progress zero", 7, ""),
        overcomplete_progress_out_of_zero_with_complete_flag(15, 0, true, "progress zero", 7, ""),
        no_progress_with_mode_no_id_no_description(0, 10, false, "", optional<unsigned long>(), "the mode"),
        no_progress_with_mode_with_id_no_description(0, 10, false, "", 5, "the mode"),
        no_progress_with_mode_no_id_with_description(0, 10, false, "no progress", optional<unsigned long>(), "the mode"),
        no_progress_with_mode_with_id_with_description(0, 10, false, "no progress", 5, "the mode"),
        no_progress_out_of_zero_with_mode(0, 0, false, "no progress zero", 5, "another mode")
    {
      widechstr.push_back(two_column_char);

      // Make the terminal 40 characters wide by default.
      set_screen_width(40);


      // Set up the locale to claim that the two-column character
      // occupies two columns.
      EXPECT_CALL(*term_locale, wcwidth(two_column_char))
        .WillRepeatedly(Return(2));
    }

    void SetUp()
    {
      status_display =
        create_cmdline_download_status_display(msg,
                                               term_locale,
                                               term_metrics,
                                               get_hide_status());
    }

    bool get_hide_status() const
    {
      return GetParam();
    }

    void set_screen_width(int width)
    {
      EXPECT_CALL(*term_metrics, get_screen_width())
        .WillRepeatedly(Return(width));
    }

    std::vector<worker_status> make_files()
    {
      return std::vector<worker_status>();
    }

    static std::vector<worker_status> make_files(const worker_status &f1)
    {
      std::vector<worker_status> rval;
      rval.push_back(f1);
      return rval;
    }

    static std::vector<worker_status> make_files(const worker_status &f1,
                                                 const worker_status &f2)
    {
      std::vector<worker_status> rval;
      rval.push_back(f1);
      rval.push_back(f2);
      return rval;
    }

    static std::vector<worker_status> make_files(const worker_status &f1,
                                                 const worker_status &f2,
                                                 const worker_status &f3)
    {
      std::vector<worker_status> rval;
      rval.push_back(f1);
      rval.push_back(f2);
      rval.push_back(f3);
      return rval;
    }
  };
}

// Expect the given message if the status isn't hidden, and nothing if
// it is.
#define EXPECT_MSG(__the_message_to_expect)                             \
  do                                                                    \
    {                                                                   \
      if(!get_hide_status())                                            \
        EXPECT_CALL(*msg, set_text(StrEq(__the_message_to_expect)));    \
      else                                                              \
        EXPECT_CALL(*msg, set_text(_))                                  \
          .Times(0);                                                    \
    } while(0)

TEST_P(CmdlineDownloadStatusDisplayTest, CreateDoesNothing)
{
  // Test that there are no unexpected calls just from creating the
  // download progress display.
}

// Test that we get correct results when there are no files being
// downloaded, no current progress and the download is stalled.
TEST_P(CmdlineDownloadStatusDisplayTest, NoCpsNoFilesNoProgressNoTimeEstimate)
{
  EXPECT_MSG(L"0% [Working]");

  status_display->display_status(status(0, make_files(), 0, 0));
}

// Test displaying the overall progress at 49.4%.
TEST_P(CmdlineDownloadStatusDisplayTest, NoCpsNoFilesAlmostHalfProgressRoundDownNoTimeEstimate)
{
  EXPECT_MSG(L"49% [Working]");

  status_display->display_status(status(0, make_files(), 0.494, 0));
}

// Test displaying the overall progress at 49.5%.
TEST_P(CmdlineDownloadStatusDisplayTest, NoCpsNoFilesAlmostHalfProgressRoundUpNoTimeEstimate)
{
  EXPECT_MSG(L"50% [Working]");

  status_display->display_status(status(0, make_files(), 0.495, 0));
}

// Test displaying the overall progress at 50%.
TEST_P(CmdlineDownloadStatusDisplayTest, NoCpsNoFilesHalfProgressNoTimeEstimate)
{
  EXPECT_MSG(L"50% [Working]");

  status_display->display_status(status(0, make_files(), 0.5, 0));
}

// Test displaying the overall progress at 100%.
TEST_P(CmdlineDownloadStatusDisplayTest, NoCpsNoFilesFullProgressNoTimeEstimate)
{
  EXPECT_MSG(L"100% [Working]");

  status_display->display_status(status(0, make_files(), 1, 0));
}

// Test what happens if the progress is negative.
TEST_P(CmdlineDownloadStatusDisplayTest, NoCpsNoFilesNegativeProgressNoTimeEstimate)
{
  EXPECT_MSG(L"0% [Working]");

  status_display->display_status(status(0, make_files(), -0.5, 0));
}

// Test what happens if the progress is above 100%.
TEST_P(CmdlineDownloadStatusDisplayTest, NoCpsNoFilesOvercompleteProgressNoTimeEstimate)
{
  EXPECT_MSG(L"100% [Working]");

  status_display->display_status(status(0, make_files(), 1.5, 0));
}

// I don't test the time and download rate meters extensively -- they
// come from apt functions that are assumed to produce reasonable
// results.

// Test that updating the progress produces correct results with a
// download rate indicator and no time estimate.
TEST_P(CmdlineDownloadStatusDisplayTest, HasCpsNoFilesNoProgressNoTimeEstimate)
{
  EXPECT_MSG(L"0% [Working]                20.0 kB/s 0s");

  status_display->display_status(status(20 * 1000,
                                        make_files(),
                                        0,
                                        0));
}

// Test that updating the progress produces correct results with a
// time estimate.
TEST_P(CmdlineDownloadStatusDisplayTest, NoCpsNoFilesNoProgressHasTimeEstimate)
{
  EXPECT_MSG(L"0% [Working]                         30s");

  status_display->display_status(status(0,
                                        make_files(),
                                        0,
                                        30));
}

// Test that updating the progress produces correct results with a
// time estimate and a download rate indicator.
TEST_P(CmdlineDownloadStatusDisplayTest, HasCpsNoFilesNoProgressHasTimeEstimate)
{
  EXPECT_MSG(L"0% [Working]               40.0 kB/s 30s");

  status_display->display_status(status(40 * 1000,
                                        make_files(),
                                        0,
                                        30));
}

// Test that updating the progress produces correct results with one
// file at 0%.
TEST_P(CmdlineDownloadStatusDisplayTest, OneFileWithNoProgress)
{
  EXPECT_MSG(L"0% [no progress 0 B/10 B 0%]");

  status_display->display_status(status(0, make_files(no_progress), 0, 0));
}

// Test that updating the progress produces correct results with one
// file at 45.4%.
TEST_P(CmdlineDownloadStatusDisplayTest, OneFileAlmostHalfCompleteRoundDown)
{
  EXPECT_MSG(L"0% [1 round down 454 B/1000 B 45%]");

  status_display->display_status(status(0, make_files(almost_half_complete_round_down), 0, 0));
}

// Test that updating the progress produces correct results with one
// file at 45.4% if the complete flag is set.
TEST_P(CmdlineDownloadStatusDisplayTest, OneFileAlmostHalfCompleteRoundDownWithCompleteFlag)
{
  EXPECT_MSG(L"0% [1 round down 454 B]");

  status_display->display_status(status(0, make_files(almost_half_complete_round_down_with_complete_flag), 0, 0));
}

// Test that updating the progress produces correct results with one
// file at 45.5%.
TEST_P(CmdlineDownloadStatusDisplayTest, OneFileAlmostHalfCompleteRoundUp)
{
  EXPECT_MSG(L"0% [2 round up 455 B/1000 B 46%]");

  status_display->display_status(status(0, make_files(almost_half_complete_round_up), 0, 0));
}

// Test that updating the progress meter produces correct results with
// one file at 100%.
TEST_P(CmdlineDownloadStatusDisplayTest, OneFileFullyComplete)
{
  EXPECT_MSG(L"0% [4 complete 10 B/10 B 100%]");

  status_display->display_status(status(0, make_files(full_progress), 0, 0));
}

// Test that updating the progress meter produces correct results with
// one file with progress 0/0.
TEST_P(CmdlineDownloadStatusDisplayTest, OneFileNoProgressNoSize)
{
  EXPECT_MSG(L"0% [5 no progress zero]");

  status_display->display_status(status(0, make_files(no_progress_out_of_zero), 0, 0));
}

// Test test updating the progress meter produces correct results with
// one file that has progress but a total size of 0.
TEST_P(CmdlineDownloadStatusDisplayTest, OneFileWithProgressZeroSize)
{
  EXPECT_MSG(L"0% [7 progress zero 15 B]");

  status_display->display_status(status(0, make_files(overcomplete_progress_out_of_zero), 0, 0));
}

// Test test updating the progress meter produces correct results with
// one file that has progress but a total size of 0 if the complete
// flag is set.
TEST_P(CmdlineDownloadStatusDisplayTest, OneFileWithProgressZeroSizeWithCompleteFlag)
{
  EXPECT_MSG(L"0% [7 progress zero 15 B]");

  status_display->display_status(status(0, make_files(overcomplete_progress_out_of_zero_with_complete_flag), 0, 0));
}

// Test that updating the progress meter produces correct results with
// one file above 100%.
TEST_P(CmdlineDownloadStatusDisplayTest, OneFileOvercomplete)
{
  EXPECT_MSG(L"0% [6 overcomplete progress 15 B/10 B 100%]");

  status_display->display_status(status(0, make_files(overcomplete_progress), 0, 0));
}

// Various tests of cases involving the mode string:
TEST_P(CmdlineDownloadStatusDisplayTest, OneFileNoProgressWithModeNoIdNoDescription)
{
  EXPECT_MSG(L"0% [the mode 0 B/10 B 0%]");

  status_display->display_status(status(0, make_files(no_progress_with_mode_no_id_no_description), 0, 0));
}

TEST_P(CmdlineDownloadStatusDisplayTest, OneFileNoProgressWithModeWithIdNoDescription)
{
  EXPECT_MSG(L"0% [5 the mode 0 B/10 B 0%]");

  status_display->display_status(status(0, make_files(no_progress_with_mode_with_id_no_description), 0, 0));
}

TEST_P(CmdlineDownloadStatusDisplayTest, OneFileNoProgressWithModeNoIdWithDescription)
{
  EXPECT_MSG(L"0% [no progress the mode 0 B/10 B 0%]");

  status_display->display_status(status(0, make_files(no_progress_with_mode_no_id_with_description), 0, 0));
}

TEST_P(CmdlineDownloadStatusDisplayTest, OneFileNoProgressWithModeWithIdWithDescription)
{
  EXPECT_MSG(L"0% [5 no progress the mode 0 B/10 B 0%]");

  status_display->display_status(status(0, make_files(no_progress_with_mode_with_id_with_description), 0, 0));
}

TEST_P(CmdlineDownloadStatusDisplayTest, OneFileNoProgressOutOfZeroWithMode)
{
  EXPECT_MSG(L"0% [5 no progress zero another mode]");

  status_display->display_status(status(0, make_files(no_progress_out_of_zero_with_mode), 0, 0));
}


// Test that displaying two files works as expected.
TEST_P(CmdlineDownloadStatusDisplayTest, TwoFiles)
{
  EXPECT_MSG(L"0% [no progress 0 B/10 B 0%] [3 half complete 5 B/10 B 50%]");

  status_display->display_status(status(0, make_files(no_progress, half_complete), 0, 0));
}


//////////// Formatting tests ////////////

// The only specialized formatting that we do in the download code is
// to display the download rate and/or time justified to the
// right-hand side of the screen, overwriting the rest of the progress
// if it's too long.  Other than that, we rely on transient_message to
// clip properly.

TEST_P(CmdlineDownloadStatusDisplayTest, ClipOverwideDisplayWithDownloadRateIndicator)
{
  set_screen_width(17);

  EXPECT_MSG(L"0% [Wor 30 B/s 0s");

  status_display->display_status(status(30, make_files(), 0, 0));
}

TEST_P(CmdlineDownloadStatusDisplayTest, ClipOverwideDisplayWithTimeRemainingAndWideCharOverlapped)
{
  set_screen_width(14);

  EXPECT_MSG(L"0% [abcd" + widechstr + L" 20s");

  file_progress file_with_wide_char(50, 100, false,
                                    "abcd" + transcode(widechstr) + "efghi",
                                    optional<unsigned long>(), "");
  status_display->display_status(status(0, make_files(file_with_wide_char), 0, 20));
}

// Verify that if a wide character is split, it's replaced with spaces.
TEST_P(CmdlineDownloadStatusDisplayTest, ClipOverwideDisplayWithTimeRemainingAndSplitWideChar)
{
  set_screen_width(13);

  EXPECT_MSG(std::wstring(L"0% [abcd  20s"));

  file_progress file_with_wide_char(50, 100, false,
                                    "abcd" + transcode(widechstr) + "efghi",
                                    optional<unsigned long>(), "");
  status_display->display_status(status(0, make_files(file_with_wide_char), 0, 20));
}

TEST_P(CmdlineDownloadStatusDisplayTest, ClipOverwideDisplayWithTimeRemainingAndWideCharNotOverlapped)
{
  set_screen_width(15);

  EXPECT_MSG(L"0% [abcd" + widechstr + L"e 20s");

  file_progress file_with_wide_char(50, 100, false,
                                    "abcd" + transcode(widechstr) + "efghi",
                                    optional<unsigned long>(), "");
  status_display->display_status(status(0, make_files(file_with_wide_char), 0, 20));
}

TEST_P(CmdlineDownloadStatusDisplayTest, ClipOverwideDisplayWithTimeRemaining)
{
  set_screen_width(14);

  EXPECT_MSG(L"0% [Workin 20s");

  status_display->display_status(status(0, make_files(), 0, 20));
}

TEST_P(CmdlineDownloadStatusDisplayTest, ClipOverwideDisplayWithDownloadRateIndicatorAndTimeRemaining)
{
  set_screen_width(14);

  EXPECT_MSG(L"0%  30 B/s 20s");

  status_display->display_status(status(30, make_files(), 0, 20));
}

TEST_P(CmdlineDownloadStatusDisplayTest, ClipOverwideDisplayWithDownloadRateIndicatorAndTimeRemainingTooNarrow)
{
  set_screen_width(9);

  EXPECT_MSG(L" 30 B/s 20s");

  status_display->display_status(status(30, make_files(), 0, 20));
}

TEST_P(CmdlineDownloadStatusDisplayTest, ZeroTerminalWidth)
{
  // Just test that something sane happens (i.e., we don't crash).
  set_screen_width(0);

  if(!get_hide_status())
    EXPECT_CALL(*msg, set_text(_));
  else
    EXPECT_CALL(*msg, set_text(_)).Times(0);

  status_display->display_status(status(300, make_files(), 0, 20));
}

////////// End formatting tests //////////

INSTANTIATE_TEST_CASE_P(WithStatus,
                        CmdlineDownloadStatusDisplayTest,
                        Values(false));

INSTANTIATE_TEST_CASE_P(WithoutStatus,
                        CmdlineDownloadStatusDisplayTest,
                        Values(true));
