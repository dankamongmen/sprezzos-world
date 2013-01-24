// Main test program for the generic aptitude code.
//
//   Copyright (C) 2005, 2009-2010 Daniel Burrows
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

#include <cppunit/extensions/TestFactoryRegistry.h>
#include <cppunit/ui/text/TestRunner.h>

#include <loggers.h>

#include <stdio.h>
#include <string.h>

#include <iostream>

using logging::TRACE_LEVEL;
using logging::WARN_LEVEL;

using logging::Logger;
using logging::LoggerPtr;
using logging::describe_log_level;
using logging::log_level;

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
  bool debug = false;
  for(int i = 1; i < argc; ++i)
    {
      if(!strcmp(argv[i], "--debug"))
	debug = true;
      else
	{
	  fprintf(stderr, "Invalid argument \"%s\".", argv[i]);
	  return -1;
	}
    }

  if(debug)
    Logger::getLogger("")->setLevel(TRACE_LEVEL);
  else
    Logger::getLogger("")->setLevel(WARN_LEVEL);
  Logger::getLogger("")->connect_message_logged(sigc::ptr_fun(&log_to_stdout));

  CppUnit::TextTestRunner runner;
  CppUnit::TestFactoryRegistry &registry =
    CppUnit::TestFactoryRegistry::getRegistry();

  runner.addTest(registry.makeTest());

  bool wasSuccessful = runner.run("", false);

  return wasSuccessful ? 0 : -255;
}
