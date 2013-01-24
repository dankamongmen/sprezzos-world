// temp.h                                 -*-c++-*-
//
//   Copyright (C) 2005, 2007-2010 Daniel Burrows
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
//

#ifndef TEMP_H
#define TEMP_H

#include <string>

#include <cwidget/generic/util/eassert.h>
#include <cwidget/generic/util/exception.h>
#include <cwidget/generic/threads/threads.h>

/** \brief Code to support safely creating files in a temporary
 *  directory and deleting the files when they are no longer needed.
 *
 *  This differs somewhat from tempnam() and friends in that it
 *  creates a private directory when the module is initialized and
 *  uses that for all operations to avoid race conditions.  The
 *  temporary directory is deleted when the program exits or invokes
 *  temp::shutdown().
 *
 *  \note The objects here are global because they model something
 *  global: we want to have a single, globally unique directory in
 *  which all temporary files are created.
 *
 *  \file temp.h
 */

namespace temp
{
  /** An exception thrown when a temporary object cannot be created. */
  class TemporaryCreationFailure : public std::exception
  {
    std::string msg;
  public:
    TemporaryCreationFailure(const std::string &_msg)
      : msg(_msg)
    {
    }

    ~TemporaryCreationFailure() throw()
    {
    }

    const char *what() const throw();
  };

  /** \brief Initialize the temporary name system, using the given
   *  string as part of the name of the temporary directory to create.
   *
   *  If the temporary directory can't be created for some reason,
   *  this logs an error and continues; any attempt to create a name
   *  object after this will throw.
   */
  void initialize(const std::string &prefix);

  /** \brief Shut down the temporary name system if it's initialized,
   *  recursively deleting the temporary directory and its contents.
   *
   *  This is automatically called when you invoke exit().
   */
  void shutdown();

  /** This object represents a directory in which temporary files can
   *  be created.  While you can extract the file name, it is
   *  recommended that you instead create temporary file(name)
   *  objects.
   */
  class dir
  {
    class impl;

    impl *real_dir;
  public:
    /** Create a temporary directory object with no backing directory. */
    dir()
      : real_dir(NULL)
    {
    }

    /** \brief Create a new temporary directory within the global
     *  temporary directory whose name begins with the text in prefix.
     *
     *  When all references to this directory are discarded, it will
     *  be recursively deleted.
     *
     *  \throws TemporaryCreationFailure
     */
    dir(const std::string &prefix);

    /** Create a new reference to an existing temporary directory. */
    dir(const dir &other);

    /** Copy a reference to an existing temporary directory. */
    dir &operator=(const dir &other);

    /** \return \b true if the directory is a valid reference. */
    bool valid() const;

    /** This method may be invoked only on valid directories.
     *
     *  \return the name of this directory.  It is recommended that
     *  you use this only for informational purposes; files should be
     *  created via (e.g.) temp::name.
     */
    std::string get_name() const;

    ~dir();
  };

  class dir::impl
  {
    /** The name of this directory. */
    std::string dirname;

    cwidget::threads::mutex m;

    int refcount;

    /** \brief If \b true, the directory will be deleted as if with rm
     *  -rf; otherwise it's deleted as if with rmdir.
     */
    bool forceful_delete;

    /** Set up a temporary directory with the given prefix.
     *
     *  Contains common code for the constructors.
     */
    void init_dir(const std::string &prefix);

  public:
    /** Create a new temporary directory whose name begins with the
     *  text in prefix.  For instance, passing "aptitude" might
     *  create "/tmp/aptitude45j2hs" or somesuch.
     *
     *  The initial refcount is 1.
     *
     *  \param prefix the prefix of this directory's name.  If
     *  prefix begins with a '/', then it is considered an absolute
     *  path; otherwise, it is considered a relative path within the
     *  system temporary directory.
     *
     *  \throws TemporaryCreationFailure
     */
    impl(const std::string &prefix);

    /** Attempt to remove the temporary directory. */
    ~impl();

    std::string get_name() const
    {
      return dirname;
    }

    /** Increment the reference count of this impl. */
    void incref()
    {
      cwidget::threads::mutex::lock l(m);
      ++refcount;
    }

    /** Decrement the reference count of this impl. */
    void decref()
    {
      // Be careful to release the lock before we delete ourselves and
      // destroy the mutex.
      int new_count;
      {
	cwidget::threads::mutex::lock l(m);

	eassert(refcount > 0);
	--refcount;
	new_count = refcount;
      }

      if(new_count == 0)
	delete this;
    }
  };

  inline dir::dir(const std::string &prefix)
    : real_dir(new impl(prefix))
  {
  }

  inline dir::dir(const dir &other)
    : real_dir(other.real_dir)
  {
    if(real_dir != NULL)
      real_dir->incref();
  }

  inline dir &dir::operator=(const dir &other)
  {
    if(other.real_dir != NULL)
      other.real_dir->incref();

    if(real_dir != NULL)
      real_dir->decref();

    real_dir = other.real_dir;

    return *this;
  }

  inline bool dir::valid() const
  {
    return real_dir != NULL;
  }

  inline std::string dir::get_name() const
  {
    return real_dir->get_name();
  }

  inline dir::~dir()
  {
    if(real_dir != NULL)
      real_dir->decref();
  }

  /** A temporary name -- at the moment of its creation it is
   *  guaranteed to be unique, but it is up to you to ensure that it
   *  is created uniquely.
   */
  class name
  {
    class impl;

    impl *real_name;

  public:
    /** Create a new temporary filename.
     *
     *  \param prefix the prefix of the new filename
     *
     *  \throws TemporaryCreationFailure if no temporary name can be
     *  reserved.
     */
    name(const std::string &prefix);

    /** Create an empty temporary name. */
    name();

    /** Create a new reference to an existing name. */
    name(const name &other);

    ~name();

    /** Copy a reference to an existing temporary name. */
    name &operator=(const name &other);


    bool valid() const;
    std::string get_name() const;
  };

  class name::impl
  {
    /** The name of this temporary object. */
    std::string filename;

    /** The mutex of this name's reference count. */
    cwidget::threads::mutex m;

    /** The reference count of this name. */
    int refcount;
  public:
    /** Create a new temporary filename.
     *
     *  \param filename the prefix of the temporary filename
     *
     *  \throws TemporaryCreationFailure if no temporary name can be
     *  reserved.
     */
    impl(const std::string &filename);

    /** Remove the filename associated with this temporary. */
    ~impl();

    /** \return the temporary's name. */
    std::string get_name() const
    {
      return filename;
    }

    /** Increment the reference count of this impl. */
    void incref()
    {
      cwidget::threads::mutex::lock l(m);
      ++refcount;
    }

    /** Decrement the reference count of this impl. */
    void decref()
    {
      int new_count;

      {
	cwidget::threads::mutex::lock l(m);

	eassert(refcount > 0);
	--refcount;
	new_count = refcount;
      }

      if(new_count == 0)
	delete this;
    }
  };

  inline name::name(const std::string &prefix)
    : real_name(new impl(prefix))
  {
  }

  inline name::name()
    : real_name(NULL)
  {
  }

  inline name::name(const name &other)
    : real_name(other.real_name)
  {
    if(real_name != NULL)
      real_name->incref();
  }

  inline name::~name()
  {
    if(real_name != NULL)
      real_name->decref();
  }

  inline name &name::operator=(const name &other)
  {
    if(other.real_name != NULL)
      other.real_name->incref();

    if(real_name != NULL)
      real_name->decref();

    real_name = other.real_name;

    return *this;
  }

  inline bool name::valid() const
  {
    return real_name != NULL;
  }

  inline std::string name::get_name() const
  {
    return real_name->get_name();
  }
};

#endif // TEMP_H
