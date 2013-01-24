// test_temp.cc
//
//   Copyright (C) 2005, 2007, 2009-2010 Daniel Burrows
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

#include <cppunit/extensions/HelperMacros.h>

#include <generic/util/temp.h>
#include <generic/util/util.h>

#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <libgen.h>

#include <boost/format.hpp>

#define ASSERT_STAT(s, buf) \
  do \
  { \
    if(stat((s), (buf)) != 0) \
      {									\
	int __errnum = errno;						\
	std::string __err = sstrerror(__errnum);			\
        CPPUNIT_FAIL(ssprintf("Can't stat %s: %s", (s), __err.c_str())); \
      }									\
  } while(0)


class TempTest : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TempTest);

  CPPUNIT_TEST(testTempDir);
  CPPUNIT_TEST(testTempName);
  CPPUNIT_TEST(testShutdown);
  CPPUNIT_TEST(testShutdownOnExit);

  CPPUNIT_TEST_SUITE_END();

public:
  void setUp()
  {
    temp::initialize("test");
  }

  void tearDown()
  {
    temp::shutdown();
  }

  void testTempDir()
  {
    std::string d1name, d2name;

    {
      temp::dir d1("tmp");
      temp::dir d2("tmp");

      d1name = d1.get_name();
      d2name = d2.get_name();

      int result = access(d1name.c_str(), F_OK);
      if(result != 0)
	CPPUNIT_FAIL(ssprintf("Unable to access %s: %s",
			      d1name.c_str(), sstrerror(errno).c_str()));

      result = access(d2.get_name().c_str(), F_OK);
      if(result != 0)
	CPPUNIT_FAIL(ssprintf("Unable to access %s: %s",
			      d2name.c_str(), sstrerror(errno).c_str()));

      char *d1namecopy = strdup(d1name.c_str());
      std::string base1 = basename(d1namecopy);
      free(d1namecopy);

      d1namecopy = strdup(d1name.c_str());
      std::string dir1 = dirname(d1namecopy);
      free(d1namecopy);

      d1namecopy = strdup(dir1.c_str());
      std::string dir1base = basename(d1namecopy);
      free(d1namecopy);
      d1namecopy = NULL;


      CPPUNIT_ASSERT_EQUAL((boost::format("%s-%s.%s:")
			    % "test" % get_username() % getpid()).str(),
			   std::string(dir1base, 0, dir1base.size() - 6));

      CPPUNIT_ASSERT_EQUAL(std::string("tmp"),
			   std::string(base1, 0, base1.size() - 6));


      struct stat stbuf;

      ASSERT_STAT(d1.get_name().c_str(), &stbuf);
      CPPUNIT_ASSERT(S_ISDIR(stbuf.st_mode));

      ASSERT_STAT(dir1.c_str(), &stbuf);
      CPPUNIT_ASSERT_EQUAL(0700, (int)(stbuf.st_mode & 0777));

      close(creat((d2name + "/" + "foo").c_str(), 0644));
    }

    int result = access(d1name.c_str(), F_OK);
    CPPUNIT_ASSERT(result != 0);
    CPPUNIT_ASSERT_EQUAL(ENOENT, errno);

    result = access(d2name.c_str(), F_OK);
    CPPUNIT_ASSERT(result != 0);
    CPPUNIT_ASSERT_EQUAL(ENOENT, errno);
  }

  void testTempName()
  {
    std::string fname;

    {
      temp::name f("tmpf");

      fname = f.get_name();

      char *fnamecopy = strdup(fname.c_str());
      std::string base = basename(fnamecopy);
      free(fnamecopy);

      fnamecopy = strdup(fname.c_str());
      std::string dir = dirname(fnamecopy);
      free(fnamecopy);

      fnamecopy = strdup(dir.c_str());
      std::string dirbase = basename(fnamecopy);
      free(fnamecopy);


      fnamecopy = NULL;



      CPPUNIT_ASSERT_EQUAL((boost::format("%s-%s.%s:")
			    % "test" % get_username() % getpid()).str(),
			   std::string(dirbase, 0, dirbase.size() - 6));

      CPPUNIT_ASSERT_EQUAL(std::string("tmpf"),
			   std::string(base, 0, 4));

      CPPUNIT_ASSERT(access(f.get_name().c_str(), F_OK) != 0);
      CPPUNIT_ASSERT_EQUAL(ENOENT, errno);

      // Create it.
      int fd = open(fname.c_str(), O_EXCL | O_CREAT | O_WRONLY, 0700);
      if(fd == -1)
	CPPUNIT_FAIL(ssprintf("Can't create \"%s\": %s",
			      fname.c_str(),
			      sstrerror(errno).c_str()));
      close(fd);

      CPPUNIT_ASSERT_EQUAL(0, access(f.get_name().c_str(), F_OK));
    }

    CPPUNIT_ASSERT(access(fname.c_str(), F_OK) != 0);
    CPPUNIT_ASSERT_EQUAL(ENOENT, errno);
  }

  class temporaryShutdown
  {
  public:
    temporaryShutdown()
    {
      temp::shutdown();
    }

    ~temporaryShutdown()
    {
      temp::initialize("test");
    }
  };

  void testShutdown()
  {
    // Check that shutting the system down deletes everything.

    std::string fname;
    std::string dname;

    temp::name f("tmpf");

    fname = f.get_name();

    {
      char *fnamecopy = strdup(fname.c_str());
      std::string base = basename(fnamecopy);
      free(fnamecopy);

      fnamecopy = strdup(fname.c_str());
      dname = dirname(fnamecopy);
      free(fnamecopy);

      fnamecopy = strdup(dname.c_str());
      std::string dirbase = basename(fnamecopy);
      free(fnamecopy);
      fnamecopy = NULL;



      CPPUNIT_ASSERT_EQUAL((boost::format("%s-%s.%s:")
			    % "test" % get_username() % getpid()).str(),
			   std::string(dirbase, 0, dirbase.size() - 6));

      CPPUNIT_ASSERT_EQUAL(std::string("tmpf"),
			   std::string(base, 0, 4));

      CPPUNIT_ASSERT(access(f.get_name().c_str(), F_OK) != 0);
      CPPUNIT_ASSERT_EQUAL(ENOENT, errno);
    }

    // Create it.
    int fd = open(fname.c_str(), O_EXCL | O_CREAT | O_WRONLY, 0700);
    if(fd == -1)
      CPPUNIT_FAIL(ssprintf("Can't create \"%s\": %s",
			    fname.c_str(),
			    sstrerror(errno).c_str()));
    close(fd);

    CPPUNIT_ASSERT_EQUAL(0, access(f.get_name().c_str(), F_OK));

    {
      temporaryShutdown x;

      CPPUNIT_ASSERT(access(dname.c_str(), F_OK) != 0);
      CPPUNIT_ASSERT_EQUAL(ENOENT, errno);

      CPPUNIT_ASSERT(access(fname.c_str(), F_OK) != 0);
      CPPUNIT_ASSERT_EQUAL(ENOENT, errno);
    }



    f = temp::name("tmpf");

    fname = f.get_name();

    {
      char *fnamecopy = strdup(fname.c_str());
      std::string base = basename(fnamecopy);
      free(fnamecopy);

      fnamecopy = strdup(fname.c_str());
      std::string dir = dirname(fnamecopy);
      free(fnamecopy);

      fnamecopy = strdup(dir.c_str());
      std::string dirbase = basename(fnamecopy);
      free(fnamecopy);
      fnamecopy = NULL;



      CPPUNIT_ASSERT_EQUAL((boost::format("%s-%s.%s:")
			    % "test" % get_username() % getpid()).str(),
			   std::string(dirbase, 0, dirbase.size() - 6));

      CPPUNIT_ASSERT_EQUAL(std::string("tmpf"),
			   std::string(base, 0, 4));
    }

    CPPUNIT_ASSERT(access(f.get_name().c_str(), F_OK) != 0);
    CPPUNIT_ASSERT_EQUAL(ENOENT, errno);

    // Create it.
    fd = open(fname.c_str(), O_EXCL | O_CREAT | O_WRONLY, 0700);
    if(fd == -1)
      CPPUNIT_FAIL(ssprintf("Can't create \"%s\": %s",
			    fname.c_str(),
			    sstrerror(errno).c_str()));
    close(fd);

    CPPUNIT_ASSERT_EQUAL(0, access(f.get_name().c_str(), F_OK));
  }

  void testShutdownOnExit()
  {
    temporaryShutdown x;

    temp::initialize("test");

    temp::name f("tmpf");

    std::string fname;
    std::string dname;

    fname = f.get_name();

    {
      char *fnamecopy = strdup(fname.c_str());
      std::string base = basename(fnamecopy);
      free(fnamecopy);

      fnamecopy = strdup(fname.c_str());
      dname = dirname(fnamecopy);
      free(fnamecopy);

      fnamecopy = strdup(dname.c_str());
      std::string dirbase = basename(fnamecopy);
      free(fnamecopy);
      fnamecopy = NULL;



      CPPUNIT_ASSERT_EQUAL((boost::format("%s-%s.%s:")
			    % "test" % get_username() % getpid()).str(),
			   std::string(dirbase, 0, dirbase.size() - 6));

      CPPUNIT_ASSERT_EQUAL(std::string("tmpf"),
			   std::string(base, 0, 4));

      CPPUNIT_ASSERT(access(f.get_name().c_str(), F_OK) != 0);
      CPPUNIT_ASSERT_EQUAL(ENOENT, errno);
    }

    // Create it.
    int fd = open(fname.c_str(), O_EXCL | O_CREAT | O_WRONLY, 0700);
    if(fd == -1)
      CPPUNIT_FAIL(ssprintf("Can't create \"%s\": %s",
			    fname.c_str(),
			    sstrerror(errno).c_str()));
    close(fd);

    CPPUNIT_ASSERT_EQUAL(0, access(f.get_name().c_str(), F_OK));

    pid_t pid = fork();
    switch(pid)
      {
      case -1:
	{
	  int errnum = errno;
	  CPPUNIT_FAIL(ssprintf("fork() failed: %s",
				sstrerror(errnum).c_str()));
	}
	return;

      case 0:
        exit(0);
        CPPUNIT_FAIL("exit() returned");

      default:
	{
	  int status;
	  if(waitpid(pid, &status, 0) < 0)
	    {
	      int errnum = errno;
	      CPPUNIT_FAIL(ssprintf("waitpid() failed: %s",
				    sstrerror(errnum).c_str()));
	      return;
	    }
	  else
	    {
	      CPPUNIT_ASSERT(access(dname.c_str(), F_OK) != 0);
	      CPPUNIT_ASSERT_EQUAL(ENOENT, errno);

	      CPPUNIT_ASSERT(access(fname.c_str(), F_OK) != 0);
	      CPPUNIT_ASSERT_EQUAL(ENOENT, errno);
	    }
	}
      }

    temp::shutdown();
  }
};

CPPUNIT_TEST_SUITE_REGISTRATION(TempTest);
