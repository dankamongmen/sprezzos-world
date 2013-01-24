#ifndef BOOST_TEST_NO_MAIN
#define BOOST_TEST_NO_MAIN
#endif

#include <boost/test/unit_test.hpp>

#include <gmock/gmock.h>

#include <loggers.h>

#include <iostream>

using logging::TRACE_LEVEL;
using logging::WARN_LEVEL;

using logging::Logger;
using logging::LoggerPtr;
using logging::describe_log_level;
using logging::log_level;

// One dummy test so that this can be dropped in before the actual
// test suite is written.
BOOST_AUTO_TEST_CASE(dummy)
{
}

// The Google mock library emits Google Test failures; we need to be
// able to convert those to Boost.Test failures.
class gtest_to_boost_adaptor : public testing::EmptyTestEventListener
{
  void OnTestPartResult(const testing::TestPartResult &test_part_result)
  {
    if(test_part_result.nonfatally_failed())
      BOOST_ERROR(test_part_result.file_name()
                  << ":"
                  << test_part_result.line_number()
                  << ": "
                  << test_part_result.summary());
    else if(test_part_result.fatally_failed())
      BOOST_FAIL(test_part_result.file_name()
                 << ":"
                 << test_part_result.line_number()
                 << ": "
                 << test_part_result.summary());
  }
};

bool init_unit_test()
{
  return true;
}

char *argv0 = NULL;

namespace
{
  void log_to_stdout(const char *sourceFilename,
                     int sourceLineNumber,
                     log_level level,
                     LoggerPtr logger,
                     const std::string &msg)
  {
    std::cout << sourceFilename
              << ":" << sourceLineNumber
              << " " << describe_log_level(level)
              << " - " << msg << std::endl << std::flush;
  }
}

int main(int argc, char **argv)
{
  argv0 = argv[0];

  ::testing::InitGoogleMock(&argc, argv);
  {
    ::testing::TestEventListeners &listeners =
      ::testing::UnitTest::GetInstance()->listeners();
    listeners.Append(new gtest_to_boost_adaptor);
  }

  bool debug = false;
  for(int i = 1; i < argc; ++i)
    {
      if(!strcmp(argv[i], "--debug"))
	debug = true;
    }

  if(debug)
    Logger::getLogger("")->setLevel(TRACE_LEVEL);
  else
    Logger::getLogger("")->setLevel(WARN_LEVEL);
  Logger::getLogger("")->connect_message_logged(sigc::ptr_fun(&log_to_stdout));

  return boost::unit_test::unit_test_main(init_unit_test, argc, argv);
}
