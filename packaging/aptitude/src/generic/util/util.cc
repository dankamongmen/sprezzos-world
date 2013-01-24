// util.cc
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

#include "util.h"

#include "dirent_safe.h"

#include <aptitude.h>

#include <ctype.h>
#include <errno.h>
#include <pwd.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#include <apt-pkg/error.h>

#include <cwidget/generic/util/eassert.h>

#include <sys/time.h>

using namespace std;

std::string backslash_escape_nonalnum(const std::string &s)
{
  std::string rval;
  for(std::string::const_iterator it = s.begin();
      it != s.end(); ++it)
    {
      if(isalnum(*it))
	rval.push_back(*it);
      else
	{
	  rval.push_back('\\');
	  rval.push_back(*it);
	}
    }

  return rval;
}

void stripws(string &s)
{
  size_t start = 0;
  while(start < s.size() && isspace(s[start]))
    ++start;

  size_t end = s.size();
  while(end > 0 && isspace(s[end-1]))
    --end;

  if(start >= end)
    s.clear();
  else
    s.assign(s, start, end-start);
}

void splitws(const string &s, vector<string> &output, int start, int length)
{
  while(start < length)
    {
      while(start < length && isspace(s[start]))
	++start;

      string tmp;
      // Could support quoting, etc?
      while(start < length && !isspace(s[start]))
	tmp += s[start++];

      if(!tmp.empty())
	output.push_back(tmp);
    }
}

void splitws(const string &s, vector<string> &output)
{
  return splitws(s, output, 0, s.size());
}

string ssprintf(const char *format, ...)
{
  va_list ap;

  va_start(ap, format);
  string rval = vssprintf(format, ap);
  va_end(ap);

  return rval;
}

const int initbufsize=512;

string vssprintf(const char *format, va_list ap)
{
  // We need to do this because you can't necessarily re-use a
  // va_list after stepping down it.
  va_list ap2;
  va_copy(ap2, ap);
  char buf[initbufsize];
  const int amt = vsnprintf(buf, initbufsize, format, ap);

  if(amt < initbufsize)
    return buf;
  else
    {
      const int buf2size = amt + 1;
      char *buf2 = new char[buf2size];

      const int amt2 = vsnprintf(buf2, buf2size, format, ap2);

      eassert(amt2 < buf2size);

      string rval(buf2, amt2);

      delete[] buf2;

      return rval;
    }
}

wstring swsprintf(const wchar_t *format, ...)
{
  va_list ap;

  va_start(ap, format);
  wstring rval = vswsprintf(format, ap);
  va_end(ap);

  return rval;
}

wstring vswsprintf(const wchar_t *format, va_list ap)
{
  wchar_t buf[initbufsize];
  int amt = vswprintf(buf, initbufsize, format, ap);

  if(amt < initbufsize)
    return buf;
  else
    {
      wchar_t *buf2 = new wchar_t[amt+1];

      int amt2 = vswprintf(buf2, initbufsize, format, ap);

      eassert(amt2 < amt+1);

      wstring rval(buf2, amt2);

      delete[] buf2;

      return rval;
    }
}

string sstrftime(const char *format, const tm *tm)
{
  size_t bufsize = 512;

  while(bufsize < 512 * 512)
    {
      char *buf = new char[bufsize];

      buf[0] = '\1';
      size_t result = strftime(buf, bufsize, format, tm);

      if(result == 0 && buf[0] != '\0')
	{
	  delete[] buf;
	  bufsize *= 2;
	}
      else
	{
	  // Could eliminate this with an "array smart pointer".
	  string rval(buf);
	  delete[] buf;
	  return rval;
	}
    }

  return "";
}

string sstrerror(int errnum)
{
  size_t bufsize = 512;

  while(bufsize < 512 * 512)
    {
      char *buf = new char[bufsize];

      char *result = strerror_r(errnum, buf, bufsize);

      if(result == NULL)
	{
	  int strerror_errno = errno;

	  delete[] buf;

	  if(strerror_errno == EINVAL)
	    return ssprintf("Invalid error code %d", errnum);
	  else if(strerror_errno != ERANGE)
	    return ssprintf("Unexpected error from strerror_r: %d", errnum);
	  else
	    bufsize *= 2;
	}
      else
	{
	  // We need to copy "result", not "buf", because some
	  // versions of strerror_r can return a static string and
	  // leave "buf" alone.
	  string rval(result);
	  delete[] buf;
	  return rval;
	}
    }

  return "";
}

string get_homedir()
{
  passwd pwbuf;
  passwd *useless;
  uid_t myuid = getuid();

#ifdef _SC_GETPW_R_SIZE_MAX
  long bufsize = sysconf(_SC_GETPW_R_SIZE_MAX);
  char *buf = new char[bufsize];

  if(getpwuid_r(myuid, &pwbuf, buf, bufsize, &useless) != 0)
    {
      delete[] buf;
      return "";
    }
  else
    {
      string rval = pwbuf.pw_dir;
      delete[] buf;
      return rval;
    }
#else
  long bufsize = 512;
  bool done = false;

  // The 512 * 512 is an arbitrary cutoff to avoid allocating
  // unbounded amounts of memory when the system doesn't support a way
  // to directly determine the largest possible password structure.
  while(bufsize < 512 * 512)
    {
      char *buf = new char[bufsize];

      if(getpwuid_r(myuid, &pwbuf, buf, bufsize, &useless) == 0)
	{
	  string rval = pwbuf.pw_dir;
	  delete[] buf;
	  return rval;
	}
      else
	{
	  delete[] buf;
	  bufsize *= 2;
	}
    }

  return "";
#endif
}

string get_username()
{
  passwd pwbuf;
  passwd *useless;
  uid_t myuid = getuid();

#ifdef _SC_GETPW_R_SIZE_MAX
  long bufsize = sysconf(_SC_GETPW_R_SIZE_MAX);
  char *buf = new char[bufsize];

  if(getpwuid_r(myuid, &pwbuf, buf, bufsize, &useless) != 0)
    {
      delete[] buf;
      return "";
    }
  else
    {
      string rval = pwbuf.pw_name;
      delete[] buf;
      return rval;
    }
#else
  long bufsize = 512;
  bool done = false;

  // The 512 * 512 is an arbitrary cutoff to avoid allocating
  // unbounded amounts of memory when the system doesn't support a way
  // to directly determine the largest possible password structure.
  while(bufsize < 512 * 512)
    {
      char *buf = new char[bufsize];

      if(getpwuid_r(myuid, &pwbuf, buf, bufsize, &useless) == 0)
	{
	  string rval = pwbuf.pw_name;
	  delete[] buf;
	  return rval;
	}
      else
	{
	  delete[] buf;
	  bufsize *= 2;
	}
    }

  return "";
#endif
}

namespace aptitude
{
  namespace util
  {
    timeval subtract_timevals(const timeval &a, const timeval &b)
    {
      timeval rval;

      rval.tv_sec = a.tv_sec - b.tv_sec;
      if(a.tv_usec < b.tv_usec)
        {
          --rval.tv_sec;
          rval.tv_usec = (a.tv_usec - b.tv_usec) + 1000000;
        }
      else
        rval.tv_usec = a.tv_usec - b.tv_usec;

      return rval;
    }

    bool recursive_remdir(const std::string &dirname)
    {
      struct stat stbuf;

      if(lstat(dirname.c_str(), &stbuf) != 0)
	_error->Errno("recursive_remdir", _("Unable to stat \"%s\""), dirname.c_str());

      if(S_ISLNK(stbuf.st_mode) || !S_ISDIR(stbuf.st_mode))
	{
	  if(unlink(dirname.c_str()) != 0)
	    {
	      _error->Errno("recursive_remdir", _("Unable to remove \"%s\""), dirname.c_str());
	      return false;
	    }
	  else
	    return true;
	}

      DIR *dir = opendir(dirname.c_str());
      if(dir == NULL)
	{
	  _error->Errno("recursive_remdir", _("Unable to list files in \"%s\""), dirname.c_str());
	  return false;
	}

      bool rval = true;

      dirent_safe dent;
      dirent *tmp;
      for(int dirent_result = readdir_r(dir, &dent.d, &tmp);
	  dirent_result == 0 && tmp != NULL;
	  dirent_result = readdir_r(dir, &dent.d, &tmp))
	if(strcmp(dent.d.d_name, ".") != 0 &&
	   strcmp(dent.d.d_name, "..") != 0)
	  rval = (rval && recursive_remdir(dirname + "/" + dent.d.d_name));

      if(closedir(dir) != 0)
	{
	  _error->Errno("recursive_remdir", _("Failure closing directory \"%s\""), dirname.c_str());
	  rval = false;
	}

      if(rmdir(dirname.c_str()) != 0)
	{
	  _error->Errno("recursive_remdir", _("Unable to remove directory \"%s\""), dirname.c_str());
	  rval = false;
	}

      return rval;
    }
  }
}
