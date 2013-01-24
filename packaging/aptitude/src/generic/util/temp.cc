// temp.cc
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

#include "temp.h"

#include "util.h"

#include <aptitude.h>
#include <loggers.h>

#include <stdlib.h>
#include <string.h>

#include <apt-pkg/error.h>

#include <boost/format.hpp>
#include <boost/random.hpp>
#include <boost/scoped_array.hpp>

using aptitude::Loggers;
namespace cw = cwidget;

namespace temp
{
  const char *TemporaryCreationFailure::what() const throw()
  {
    try
      {
        return msg.c_str();
      }
    catch(...)
      {
        return "Error generating error message.";
      }
  }

  namespace
  {
    bool created_atexit_handler = false;
    // This mutex is deliberately allocated on the heap and leaked.
    // This ensures that it is still accessible when exit handlers
    // run.
    cw::threads::mutex *temp_state_mutex = new cw::threads::mutex;
    // This string is allocated when the temporary system is
    // initialized.
    std::string *temp_base = NULL;
    // A private RNG used below; created when the temporary system is
    // first used and controlled by temp_state_mutex.  Using
    // boost::random instead of random_r to make this code more
    // portable.
    boost::mt19937 *temp_rng = NULL;

    // Characters placed into the resulting file name.  A little
    // punctuation is sprinkled in, but only characters that have no
    // special meaning to the system shell, mostly out of politeness.
    const char characters[] =
      {
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
        'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
        'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
        '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '-', '_', '+',
        '=', ',', '.', ':', '%', '^'
      };
    const int num_characters = sizeof(characters) / sizeof(characters[0]);


    // A mktemp() replacement that is guaranteed to use lots of
    // randomness.  Only used once below, and it's only used to create
    // a name in a directory that we should control (i.e., attackers
    // shouldn't be able to create files in this directory *anyway*;
    // using a random name is just a defensive measure).  mktemp()
    // would be safe too for that reason, but it generates an annoying
    // linker warning because it obviously doesn't know it's being
    // used in a safe context.
    //
    // The reason for doing this instead of using mkstemp() is that
    // several parts of aptitude really just need a name, because
    // they're going to invoke external code that wants a name instead
    // of an open file, and some of that code fails if the file
    // exists.  Since mkstemp() creates the file implicitly, we'd have
    // to unlink it before invoking the other code.  But that just
    // puts back the race condition that mkstemp() was supposed to
    // fix, only more so, since we would *definitely* leak the
    // temporary file name.
    //
    // Returns an empty string if it can't find a unique name.
    std::string mymktemp(std::string prefix)
    {
      // Obviously arbitrary -- the main requirement is that it needs
      // to be big.
      static const int num_random_characters = 32;

      // Also arbitrary; just need a limit to avoid cycling if
      // something goes horribly wrong..
      static const int num_retries = 50;

      cw::threads::mutex::lock l(*temp_state_mutex);


      if(temp_rng == NULL)
        temp_rng = new boost::mt19937;

      boost::variate_generator<boost::mt19937&, boost::uniform_int<> >
        random_char_idx(*temp_rng, boost::uniform_int<>(0, num_characters - 1));


      for(int attempt = 0; attempt < num_retries; ++attempt)
        {
          std::string result = prefix;

          for(int i = 0; i < num_random_characters; ++i)
            result += characters[random_char_idx()];

          // Nested "if" statements for better debugging.
          if(access(result.c_str(), F_OK) != 0)
            {
              if(errno == ENOENT)
                // The usual race condition exists here; I rely on
                // both the fact that attackers can't write to the
                // directory where this file exists and on the fact
                // that it's a huge random number (but less on the
                // latter since it's only a PRNG).
                return result;
            }
        }

      return std::string();
    }
  }

  void initialize(const std::string &initial_prefix)
  {
    cw::threads::mutex::lock l(*temp_state_mutex);

    if(temp_base != NULL)
      {
	LOG_WARN(Loggers::getAptitudeTemp(),
		 "Ignoring the second attempt to initialize the temporary file module.");
	return;
      }

    LOG_TRACE(Loggers::getAptitudeTemp(),
	      "Initializing the temporary file module.");

    if(initial_prefix.size() > 0 && initial_prefix[0] == '/')
      {
	LOG_ERROR(Loggers::getAptitudeTemp(),
		  "Invalid attempt to create a rooted temporary directory.");
	return;
      }

    std::string prefix(initial_prefix);

    const char *tmpdir = getenv("TMPDIR");

    if(tmpdir == NULL)
      tmpdir = getenv("TMP");

    if(tmpdir == NULL)
      tmpdir = "/tmp";

    // User name and process information is encoded into the temporary
    // directory's name as a convenience for users, so they can
    // identify which aptitude created a given directory.  It's not
    // for security; that comes from mkdtemp().
    prefix = ( boost::format("%s/%s-%s.%d:")
	       % std::string(tmpdir)
	       % prefix.c_str()
	       % get_username()
	       % getpid() ).str();


    size_t bufsize = prefix.size() + 6 + 1;
    boost::scoped_array<char> tmpl(new char[bufsize]);
    strcpy(tmpl.get(), prefix.c_str());
    strcat(tmpl.get(), "XXXXXX");


    if(mkdtemp(tmpl.get()) == NULL)
      {
	int errnum = errno;
	std::string err = sstrerror(errnum);
	LOG_ERROR(Loggers::getAptitudeTemp(),
		  boost::format(_("Unable to create temporary directory from template \"%s\": %s"))
		  % tmpl % err);

	return;
      }

    if(!created_atexit_handler)
      {
	LOG_TRACE(Loggers::getAptitudeTemp(),
		  "Adding an atexit handler for the temporary directory shut-down routine.");
	atexit(&temp::shutdown);
	created_atexit_handler = true;
      }

    temp_base = new std::string(tmpl.get());
    LOG_INFO(Loggers::getAptitudeTemp(),
	     "Initialized the temporary file module using the base directory "
	     << *temp_base);
  }

  void shutdown()
  {
    // No log statements because this ran after log4cxx's global
    // destructors when we used log4cxx.
    cw::threads::mutex::lock l(*temp_state_mutex);

    if(temp_base == NULL)
      return;

    aptitude::util::recursive_remdir(*temp_base);
    delete temp_base;
    temp_base = NULL;
  }



  void dir::impl::init_dir(const std::string &initial_prefix)
  {
    cw::threads::mutex::lock l(*temp_state_mutex);

    if(temp_base == NULL)
      {
	const char * const msg = "Can't create a new temporary directory object when the temporary file module is not initialized.";
	LOG_ERROR(Loggers::getAptitudeTemp(),
		  msg);
	throw TemporaryCreationFailure(msg);
      }

    LOG_TRACE(Loggers::getAptitudeTemp(),
	      "Creating a temporary directory with prefix " << initial_prefix);

    std::string prefix(initial_prefix);

    if(prefix.size() > 0 && prefix[0] == '/')
      throw TemporaryCreationFailure("Invalid attempt to create an absolutely named temporary directory.");


    prefix = *temp_base + "/" + prefix;

    size_t bufsize = prefix.size() + 6 + 1;
    boost::scoped_array<char> tmpl(new char[bufsize]);
    strcpy(tmpl.get(), prefix.c_str());
    strcat(tmpl.get(), "XXXXXX");


    if(mkdtemp(tmpl.get()) == NULL)
      {
	int errnum = errno;
	std::string err = sstrerror(errnum);

	LOG_FATAL(Loggers::getAptitudeTemp(),
		  "Unable to create temporary directory from template \""
		  << tmpl.get() << "\": " << err);

	std::string errmsg = ssprintf(_("Unable to create temporary directory from template \"%s\": %s"),
				      tmpl.get(), err.c_str());

	throw TemporaryCreationFailure(errmsg);
      }

    dirname.assign(tmpl.get());

    LOG_INFO(Loggers::getAptitudeTemp(),
	     "Temporary directory created in " << dirname);
  }

  dir::impl::impl(const std::string &prefix)
    : refcount(1)
  {
    init_dir(prefix);
  }

  dir::impl::~impl()
  {
    aptitude::util::recursive_remdir(dirname);
  }


  name::impl::impl(const std::string &_filename)
    : refcount(1)
  {
    std::string parentdir;

    LOG_TRACE(Loggers::getAptitudeTemp(),
	      "Creating temporary file name with base name " << _filename);

    {
      cw::threads::mutex::lock l(*temp_state_mutex);
      if(temp_base == NULL)
	{
	  const char * const msg = "Can't create a temporary filename: the temporary filename system hasn't been initialized yet.";
	  LOG_FATAL(Loggers::getAptitudeTemp(), msg);
	  throw TemporaryCreationFailure(msg);
	}
      else
	parentdir = *temp_base;
    }

    // Warn early about bad filenames.
    if(_filename.find('/') != _filename.npos)
      {
	std::string msg = ssprintf("Invalid temporary filename (contains directory separator): \"%s\"",
				   _filename.c_str());
	LOG_FATAL(Loggers::getAptitudeTemp(), msg);
	throw TemporaryCreationFailure(msg);
      }


    std::string prefix = parentdir;
    prefix += "/";
    prefix += _filename;

    errno = 0;

    filename = mymktemp(prefix);
    if(filename.empty())
      {
	LOG_FATAL(Loggers::getAptitudeTemp(),
		  "Unable to create temporary filename from prefix \"" << prefix
		  << "\"");

	throw TemporaryCreationFailure(ssprintf(_("Unable to create temporary filename from prefix \"%s\""),
						prefix.c_str()));
      }
  }

  // We don't know if it's a filename or a directory, so try blowing
  // both away (ignoring errors).
  name::impl::~impl()
  {
    rmdir(filename.c_str());
    unlink(filename.c_str());
  }
}

