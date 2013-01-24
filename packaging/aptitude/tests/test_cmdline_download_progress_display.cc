/** \file test_cmdline_download_progress_display.cc */

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
#include <cmdline/mocks/download_status_display.h>
#include <cmdline/mocks/terminal.h>
#include <cmdline/mocks/transient_message.h>

#include <generic/views/download_progress.h>
#include <generic/views/mocks/download_progress.h>

// System includes:
#include <boost/make_shared.hpp>
#include <boost/shared_ptr.hpp>

#include <gmock/gmock.h>
#include <gtest/gtest.h>

using aptitude::cmdline::create_download_progress_display;
using aptitude::views::download_progress;
using boost::make_shared;
using boost::optional;
using boost::shared_ptr;
using testing::InSequence;
using testing::Mock;
using testing::Return;
using testing::StrEq;
using testing::TestWithParam;
using testing::Throw;
using testing::Values;
using testing::_;

namespace mocks
{
  using namespace aptitude::cmdline::mocks;
  using namespace aptitude::views::mocks;
}

namespace views = aptitude::views;

namespace
{
  // The test parameter controls whether messages are to be displayed.
  struct CmdlineDownloadProgressDisplayTest : public TestWithParam<bool>
  {
    shared_ptr<mocks::transient_message> msg;
    shared_ptr<mocks::download_status_display> status_display;
    shared_ptr<mocks::terminal_input> term_input;

    shared_ptr<views::download_progress> progress;

    typedef download_progress::file_progress file_progress;
    typedef download_progress::status status;
    typedef download_progress::status::worker_status worker_status;
    // Two arbitrary status objects; the only requirement on them is
    // that they have to be different from each other (so we can
    // verify that arguments are being passed through as expected).
    status status1, status2;

    CmdlineDownloadProgressDisplayTest()
      : msg(mocks::transient_message::create_strict()),
        status_display(mocks::download_status_display::create_strict()),
        term_input(mocks::terminal_input::create_strict()),
        status1(5, std::vector<status::worker_status>(), 0.5, 10),
        status2(1, std::vector<status::worker_status>(), 0.2, 110)
    {
    }

    void SetUp()
    {
      progress = create_download_progress_display(msg,
                                                  status_display,
                                                  term_input,
                                                  get_display_messages());
    }

    bool get_display_messages() const
    {
      return GetParam();
    }
  };
}

// Expect the given string to be passed to display_and_advance() if
// get_display_messages() is true; otherwise expect no calls to that
// routine.
#define EXPECT_MSG(__the_message_to_expect)                             \
  do                                                                    \
    {                                                                   \
      if(get_display_messages())                                        \
        EXPECT_CALL(*msg, display_and_advance(StrEq(__the_message_to_expect))); \
      else                                                              \
        EXPECT_CALL(*msg, display_and_advance(_))                       \
          .Times(0);                                                    \
    } while(0)

// Test that there are no unexpected calls just from creating the
// download progress display.
TEST_P(CmdlineDownloadProgressDisplayTest, CreateDoesNothing)
{
}

// Test that equality comparisons of file_progress objects respect the
// current_size field.
TEST_P(CmdlineDownloadProgressDisplayTest, FileProgressEqualsRespectsCurrentSize)
{
  EXPECT_NE(file_progress(1, 0, true, "", optional<unsigned long>(0), ""),
            file_progress(2, 0, true, "", optional<unsigned long>(0), ""));
}

// Test that equality comparisons of file_progress objects respect the
// total_size field.
TEST_P(CmdlineDownloadProgressDisplayTest, FileProgressEqualsRespectsTotalSize)
{
  EXPECT_NE(file_progress(2, 1, true, "", optional<unsigned long>(0), ""),
            file_progress(2, 0, true, "", optional<unsigned long>(0), ""));
}

// Test that equality comparisons of file_progress objects respect the
// description field.
TEST_P(CmdlineDownloadProgressDisplayTest, FileProgressEqualsRespectsDescription)
{
  EXPECT_NE(file_progress(2, 0, true, "1", optional<unsigned long>(0), ""),
            file_progress(2, 0, true, "2", optional<unsigned long>(0), ""));
}

// Test that equality comparisons of file_progress objects respect the
// id field.
TEST_P(CmdlineDownloadProgressDisplayTest, FileProgressEqualsRespectsId)
{
  EXPECT_NE(file_progress(2, 0, true, "1", optional<unsigned long>(0), ""),
            file_progress(2, 0, true, "1", optional<unsigned long>(5), ""));
}

// Test that equality comparisons of file_progress objects respect the
// id field when one side is unspecified.
TEST_P(CmdlineDownloadProgressDisplayTest, FileProgressEqualsRespectsIdOneUnspecified)
{
  EXPECT_NE(file_progress(2, 0, true, "1", optional<unsigned long>(0), ""),
            file_progress(2, 0, true, "1", optional<unsigned long>(), ""));
}

// Test that equality comparisons of file_progress objects respect the
// mode field.
TEST_P(CmdlineDownloadProgressDisplayTest, FileProgressEqualsRespectsMode)
{
  EXPECT_NE(file_progress(2, 0, true, "1", optional<unsigned long>(0), "some mode"),
            file_progress(2, 0, true, "1", optional<unsigned long>(0), "another mode"));
}

// Test that equality comparisons of file_progress objects respect the
// complete field.
TEST_P(CmdlineDownloadProgressDisplayTest, FileProgressEqualsRespectsComplete)
{
  EXPECT_NE(file_progress(2, 0, true, "1", optional<unsigned long>(0), "some mode"),
            file_progress(2, 0, false, "1", optional<unsigned long>(0), "some mode"));
}

// Ensure that a pair of equal file_progress objects compares equal.
TEST_P(CmdlineDownloadProgressDisplayTest, EqualFileProgressEqual)
{
  EXPECT_EQ(file_progress(6, 4, true, "10", optional<unsigned long>(9), "a mode"),
            file_progress(6, 4, true, "10", optional<unsigned long>(9), "a mode"));
}

// Check that comparison of status objects respects the download_rate
// field.
TEST_P(CmdlineDownloadProgressDisplayTest, StatusEqualsRespectsDownloadRate)
{
  EXPECT_NE(status(0, std::vector<worker_status>(), 0, 0),
            status(1, std::vector<worker_status>(), 0, 0));
}

// Check that comparison of status objects respects the
// active_downloads field (empty vs nonempty).
TEST_P(CmdlineDownloadProgressDisplayTest, StatusEqualsRespectsActiveDownloadsEmptyVsNonempty)
{
  std::vector<worker_status> singleton;
  singleton.push_back(file_progress(0, 1, false, "2", 3, "4"));

  EXPECT_NE(status(0, singleton, 0, 0),
            status(0, std::vector<worker_status>(), 0, 0));
}

// Check that comparison of status objects respects the
// active_downloads field (one contains a file_progress, the other
// contains a string).
TEST_P(CmdlineDownloadProgressDisplayTest, StatusEqualsRespectsActiveDownloadsContentsFileProgressAndString)
{
  std::vector<worker_status> singleton1, singleton2;
  singleton1.push_back(file_progress(0, 1, false, "2", 3, "4"));
  singleton2.push_back(std::string("2"));

  EXPECT_NE(status(0, singleton1, 0, 0),
            status(0, singleton2, 0, 0));
}

// Check that comparison of status objects respects the
// active_downloads field (one contains a file_progress, the other
// contains a different file_progress).
TEST_P(CmdlineDownloadProgressDisplayTest, StatusEqualsRespectsActiveDownloadsContentsDifferentFileProgresses)
{
  std::vector<worker_status> singleton1, singleton2;
  singleton1.push_back(file_progress(0, 1, false, "2", 3, "4"));
  singleton2.push_back(file_progress(0, 1, false, "3", 3, "4"));

  EXPECT_NE(status(0, singleton1, 0, 0),
            status(0, singleton2, 0, 0));
}

// Check that comparison of status objects respects the
// active_downloads field (one contains a string, the other
// contains a different string).
TEST_P(CmdlineDownloadProgressDisplayTest, StatusEqualsRespectsActiveDownloadsContentsDifferentStrings)
{
  std::vector<worker_status> singleton1, singleton2;
  singleton1.push_back(std::string("1"));
  singleton2.push_back(std::string("2"));

  EXPECT_NE(status(0, singleton1, 0, 0),
            status(0, singleton2, 0, 0));
}

// Test that update_progress() passes through the status object and
// returns true.
TEST_P(CmdlineDownloadProgressDisplayTest, TestStatus1NeqStatus2)
{
  EXPECT_NE(status1, status2);
}

TEST_P(CmdlineDownloadProgressDisplayTest, UpdateProgress1)
{
  EXPECT_CALL(*status_display, display_status(status1));

  EXPECT_TRUE(progress->update_progress(status1));
}

TEST_P(CmdlineDownloadProgressDisplayTest, UpdateProgress2)
{
  EXPECT_CALL(*status_display, display_status(status2));

  EXPECT_TRUE(progress->update_progress(status2));
}

TEST_P(CmdlineDownloadProgressDisplayTest, FileStartedWithIdNoNameWithFileSize)
{
  EXPECT_MSG(L"Get: 4 [100 B]");

  progress->file_started("", 4, 100);
}

TEST_P(CmdlineDownloadProgressDisplayTest, FileStartedNoIdNoFileSize)
{
  EXPECT_MSG(L"Get: somefile");

  progress->file_started("somefile", optional<unsigned long>(), optional<unsigned long long>());
}

TEST_P(CmdlineDownloadProgressDisplayTest, FileStartedWithIdNoFileSize)
{
  EXPECT_MSG(L"Get: 0 filename");

  progress->file_started("filename", optional<unsigned long>(0), optional<unsigned long long>());
}

TEST_P(CmdlineDownloadProgressDisplayTest, FileStartedNoIdWithFileSize)
{
  EXPECT_MSG(L"Get: thefile [210 B]");

  progress->file_started("thefile", optional<unsigned long>(), 210);
}

TEST_P(CmdlineDownloadProgressDisplayTest, FileStartedWithIdWithFileSize)
{
  EXPECT_MSG(L"Get: 6 another file [210 B]");

  progress->file_started("another file", 6, 210);
}



TEST_P(CmdlineDownloadProgressDisplayTest, FileAlreadyDownloadedWithIdNoNameWithFileSize)
{
  EXPECT_MSG(L"Hit 4 [100 B]");

  progress->file_already_downloaded("", 4, 100);
}

TEST_P(CmdlineDownloadProgressDisplayTest, FileAlreadyDownloadedNoIdNoFileSize)
{
  EXPECT_MSG(L"Hit somefile");

  progress->file_already_downloaded("somefile", optional<unsigned long>(), optional<unsigned long long>());
}

TEST_P(CmdlineDownloadProgressDisplayTest, FileAlreadyDownloadedWithIdNoFileSize)
{
  EXPECT_MSG(L"Hit 0 filename");

  progress->file_already_downloaded("filename", optional<unsigned long>(0), optional<unsigned long long>());
}

TEST_P(CmdlineDownloadProgressDisplayTest, FileAlreadyDownloadedNoIdWithFileSize)
{
  EXPECT_MSG(L"Hit thefile [210 B]");

  progress->file_already_downloaded("thefile", optional<unsigned long>(), 210);
}

TEST_P(CmdlineDownloadProgressDisplayTest, FileAlreadyDownloadedWithIdWithFileSize)
{
  EXPECT_MSG(L"Hit 6 another file [210 B]");

  progress->file_already_downloaded("another file", 6, 210);
}


// Test what happens with ignored errors.  There are lots of cases
// here, but they boil down to "we ignore all the parameters except
// description" (and of course ignored).
TEST_P(CmdlineDownloadProgressDisplayTest, ErrorIgnoredNoIdNoErrorNoDescription)
{
  EXPECT_MSG(L"Ign");

  progress->error(true, "", "", optional<unsigned long>());
}

TEST_P(CmdlineDownloadProgressDisplayTest, ErrorIgnoredWithIdNoErrorNoDescription)
{
  EXPECT_MSG(L"Ign");

  progress->error(true, "", "", optional<unsigned long>(4));
}

TEST_P(CmdlineDownloadProgressDisplayTest, ErrorIgnoredNoIdWithErrorNoDescription)
{
  EXPECT_MSG(L"Ign");

  progress->error(true, "error message", "", optional<unsigned long>());
}

TEST_P(CmdlineDownloadProgressDisplayTest, ErrorIgnoredWithIdWithErrorNoDescription)
{
  EXPECT_MSG(L"Ign");

  progress->error(true, "error message", "", optional<unsigned long>(4));
}

TEST_P(CmdlineDownloadProgressDisplayTest, ErrorIgnoredNoIdNoErrorWithDescription)
{
  EXPECT_MSG(L"Ign description");

  progress->error(true, "", "description", optional<unsigned long>());
}

TEST_P(CmdlineDownloadProgressDisplayTest, ErrorIgnoredWithIdNoErrorWithDescription)
{
  EXPECT_MSG(L"Ign description");

  progress->error(true, "", "description", optional<unsigned long>(4));
}

TEST_P(CmdlineDownloadProgressDisplayTest, ErrorIgnoredNoIdWithErrorWithDescription)
{
  EXPECT_MSG(L"Ign description");

  progress->error(true, "error message", "description", optional<unsigned long>());
}

TEST_P(CmdlineDownloadProgressDisplayTest, ErrorIgnoredWithIdWithErrorWithDescription)
{
  EXPECT_MSG(L"Ign description");

  progress->error(true, "error message", "description", optional<unsigned long>(4));
}


// Test what happens with non-ignored errors.
TEST_P(CmdlineDownloadProgressDisplayTest, ErrorNotIgnoredNoIdNoErrorNoDescription)
{
  EXPECT_MSG(L"Err");

  progress->error(false, "", "", optional<unsigned long>());
}

TEST_P(CmdlineDownloadProgressDisplayTest, ErrorNotIgnoredWithIdNoErrorNoDescription)
{
  EXPECT_MSG(L"Err");

  progress->error(false, "", "", optional<unsigned long>(4));
}

TEST_P(CmdlineDownloadProgressDisplayTest, ErrorNotIgnoredNoIdWithErrorNoDescription)
{
  {
    InSequence dummy;

    EXPECT_MSG(L"Err");
    EXPECT_MSG(L"  error message");
  }

  progress->error(false, "error message", "", optional<unsigned long>());
}

TEST_P(CmdlineDownloadProgressDisplayTest, ErrorNotIgnoredWithIdWithErrorNoDescription)
{
  {
    InSequence dummy;

    EXPECT_MSG(L"Err");
    EXPECT_MSG(L"  error message 2");
  }

  progress->error(false, "error message 2", "", optional<unsigned long>(4));
}

TEST_P(CmdlineDownloadProgressDisplayTest, ErrorNotIgnoredNoIdNoErrorWithDescription)
{
  EXPECT_MSG(L"Err description");

  progress->error(false, "", "description", optional<unsigned long>());
}

TEST_P(CmdlineDownloadProgressDisplayTest, ErrorNotIgnoredWithIdNoErrorWithDescription)
{
  EXPECT_MSG(L"Err description");

  progress->error(false, "", "description", optional<unsigned long>(4));
}

TEST_P(CmdlineDownloadProgressDisplayTest, ErrorNotIgnoredNoIdWithErrorWithDescription)
{
  {
    InSequence dummy;

    EXPECT_MSG(L"Err description");
    EXPECT_MSG(L"  error message");
  }

  progress->error(false, "error message", "description", optional<unsigned long>());
}

TEST_P(CmdlineDownloadProgressDisplayTest, ErrorNotIgnoredWithIdWithErrorWithDescription)
{
  {
    InSequence dummy;

    EXPECT_MSG(L"Err description");
    EXPECT_MSG(L"  error message");
  }

  progress->error(false, "error message", "description", optional<unsigned long>(4));
}


TEST_P(CmdlineDownloadProgressDisplayTest, FileFinishedNoDescriptionNoId)
{
  EXPECT_CALL(*msg, display_and_advance(_)).Times(0);

  progress->file_finished("", optional<unsigned long>());
}

TEST_P(CmdlineDownloadProgressDisplayTest, FileFinishedWithDescriptionNoId)
{
  EXPECT_CALL(*msg, display_and_advance(_)).Times(0);

  progress->file_finished("medusa cascade", optional<unsigned long>());
}

TEST_P(CmdlineDownloadProgressDisplayTest, FileFinishedNoDescriptionWithId)
{
  EXPECT_CALL(*msg, display_and_advance(_)).Times(0);

  progress->file_finished("", optional<unsigned long>(5));
}

TEST_P(CmdlineDownloadProgressDisplayTest, FileFinishedWithDescriptionWithId)
{
  EXPECT_CALL(*msg, display_and_advance(_)).Times(0);

  progress->file_finished("medusa cascade", optional<unsigned long>(5));
}

TEST_P(CmdlineDownloadProgressDisplayTest, Done)
{
  EXPECT_MSG(L"Fetched 100 B in 5s (20 B/s)");

  progress->done(100, 5, 20);
}

class MockMediaChangeCallback
{
public:
  MOCK_METHOD1(media_change, void(bool));
};

TEST_P(CmdlineDownloadProgressDisplayTest, MediaChange)
{
  MockMediaChangeCallback callback;

  Mock::VerifyAndClearExpectations(term_input.get());
  {
    InSequence dummy;

    // This will happen even if display_messages is false:
    EXPECT_CALL(*msg, set_text(StrEq(L""))); // We have to clear the display first.
    EXPECT_CALL(*term_input, prompt_for_input(StrEq(L"Media change: Please insert the disc labeled 'banana' into the drive 'kiwi' and press [Enter].")))
      .WillOnce(Return(L"kumquat"));
    EXPECT_CALL(callback, media_change(true));
  }

  progress->media_change("banana", "kiwi",
                         sigc::mem_fun(callback, &MockMediaChangeCallback::media_change));
}

TEST_P(CmdlineDownloadProgressDisplayTest, MediaChangeEOF)
{
  MockMediaChangeCallback callback;

  Mock::VerifyAndClearExpectations(term_input.get());
  {
    InSequence dummy;

    // This will happen even if display_messages is false:
    EXPECT_CALL(*msg, set_text(StrEq(L""))); // We have to clear the display first.
    EXPECT_CALL(*term_input, prompt_for_input(StrEq(L"Media change: Please insert the disc labeled 'dev' into the drive 'null' and press [Enter].")))
      .WillOnce(Throw(StdinEOFException()));
    EXPECT_CALL(callback, media_change(false));
  }

  progress->media_change("dev", "null",
                         sigc::mem_fun(callback, &MockMediaChangeCallback::media_change));
}

TEST_P(CmdlineDownloadProgressDisplayTest, CompleteDoesNothing)
{
  progress->complete(4, 100, 3);
}

INSTANTIATE_TEST_CASE_P(WithMessages,
                        CmdlineDownloadProgressDisplayTest,
                        Values(true));

INSTANTIATE_TEST_CASE_P(WithoutMessages,
                        CmdlineDownloadProgressDisplayTest,
                        Values(false));
