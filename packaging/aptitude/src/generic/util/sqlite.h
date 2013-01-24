/** \file sqlite.h */   // -*-c++-*-

//   Copyright (C) 2009 Daniel Burrows

//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.

//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.

//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.

#ifndef SQLITE_H
#define SQLITE_H

#include <cwidget/generic/threads/threads.h>
#include <cwidget/generic/util/exception.h>

#include <sqlite3.h>

#include <boost/make_shared.hpp>
#include <boost/multi_index_container.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/multi_index/sequenced_index.hpp>
#include <boost/multi_index/member.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/unordered_set.hpp>

// C++ wrapper for sqlite to handle tracking and releasing resources.

namespace aptitude
{
  namespace sqlite
  {
    class exception : public cwidget::util::Exception
    {
      std::string msg;
      int error_code;

    public:
      exception(const std::string &_msg, int _error_code)
	: msg(_msg), error_code(_error_code)
      {
      }

      std::string errmsg() const { return msg; }
      int get_error_code() const { return error_code; }
    };

    class blob;
    class statement;

    /** \brief Wraps a single connection to an SQLite database.
     *
     *  If the database has been changed to multithreaded mode (as
     *  opposed to the default serialized mode), users should avoid
     *  simultaneous calls to database methods.  While it should be
     *  safe to access the database itself, the wrong error message
     *  might be returned from a database call in this case.
     */
    class db
    {
      sqlite3 *handle;

      friend class blob;
      friend class statement;

      // For safety's sake, we keep around a collection of weak
      // references to the active statements, in cooperation with the
      // constructor and destructor of the statement class.  When the
      // database itself is destroyed, this is used to invalidate all
      // the statement objects.
      boost::unordered_set<statement *> active_statements;

      // Similarly, a set of active blob objects.
      boost::unordered_set<blob *> active_blobs;



      // Used to cache statements for reuse.  Each statement in this
      // set is currently *unused*; when a statement is requested, the
      // requester effectively "checks out" a copy, removing it from
      // the set.  The statement is accessed through a smart pointer
      // wrapper that places it back in the set once it's no longer
      // used.  This is necessary because it's not safe to reuse
      // SQLite statements.
      struct statement_cache_entry
      {
	std::string sql;
	boost::shared_ptr<statement> stmt;

	statement_cache_entry(const std::string &_sql,
			      const boost::shared_ptr<statement> &_stmt)
	  : sql(_sql), stmt(_stmt)
	{
	}
      };

      typedef boost::multi_index_container<
	statement_cache_entry,
	boost::multi_index::indexed_by<
	  boost::multi_index::hashed_non_unique<
	    boost::multi_index::member<
	      statement_cache_entry,
	      std::string,
	      &statement_cache_entry::sql> >,
	  boost::multi_index::sequenced<>
	  >
	>  statement_cache_container;

      static const int statement_cache_hash_index_N = 0;
      static const int statement_cache_mru_N = 1;

      typedef statement_cache_container::nth_index<statement_cache_hash_index_N>::type statement_cache_hash_index;
      typedef statement_cache_container::nth_index<statement_cache_mru_N>::type statement_cache_mru;

      statement_cache_container statement_cache;
      unsigned int statement_cache_limit;
      // Synchronizes access to the statement cache.
      cwidget::threads::mutex statement_cache_mutex;

      statement_cache_hash_index &get_cache_hash_index()
      {
	return statement_cache.get<statement_cache_hash_index_N>();
      }

      statement_cache_mru &get_cache_mru()
      {
	return statement_cache.get<statement_cache_mru_N>();
      }

      void cache_statement(const statement_cache_entry &entry);


      /** \brief An intermediate data item used to track the use of
       *  a statement that was checked out from the cache.
       *
       *  When a statement_proxy is destroyed, it places its enclosed
       *  statement back into the database's statement cache.
       */
      class statement_proxy_impl
      {
	statement_cache_entry entry;

      public:
	statement_proxy_impl(const statement_cache_entry &_entry)
	  : entry(_entry)
	{
	}

	const boost::shared_ptr<statement> &get_statement() const { return entry.stmt; }
	const statement_cache_entry &get_entry() const { return entry; }

	~statement_proxy_impl();
      };

      db(const std::string &filename, int flags, const char *vfs);
    public:
      /** \brief Used to make the wrapper routines atomic.
       *
       *  When the database is opened in serialized mode (the
       *  default), this class provides an RAII way to acquire and
       *  release the global database lock.  This ensures, among other
       *  things, that error codes are properly matched to the call
       *  that triggered the error.
       */
      class lock
      {
	sqlite3 *handle;
      public:
	lock(db &parent);
	~lock();
      };

      /** \brief Open an SQLite database.
       *
       *  \param filename   The name of the database file to open.
       *  \param flags      The flags with which to open the database.
       *  \param vfs        The name of the VFS module that should be
       *                    used to access the database.
       *
       *  See the sqlite3_open documentation for details.
       */
      static boost::shared_ptr<db>
      create(const std::string &filename,
	     int flags = SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE,
	     const char *vfs = NULL)
      {
	return boost::shared_ptr<db>(new db(filename, flags, vfs));
      }

      /** \brief Close the encapsulated database. */
      ~db();

      /** \brief Change the maximum number of statements to cache.
       *
       *  If it is not set, the maximum number defaults to 100.
       */
      void set_statement_cache_limit(unsigned int new_limit)
      {
	statement_cache_limit = new_limit;
      }

      /** \brief Set the timeout used when database contention occurs.
       *
       *  Normally, the database throws an exception with the error
       *  code SQLITE_BUSY.  If a timeout is set, it will wait for at
       *  least the given amount of time before throwing an exception.
       *
       *  \param  timeout   The number of milliseconds to wait,
       *                    or 0 to throw an error immediately.
       */
      void set_busy_timeout(int timeout);

      /** \brief Retrieve the last error that was generated on this
       *  database.
       *
       *  If the database is not opened in the default "serialized"
       *  mode, the last error might be an error for an operation on
       *  another thread.
       */
      std::string get_error();

      /** \brief Get the last inserted row ID.
       *
       *  This can be modified by operations on this object from other
       *  threads -- read the documentation of
       *  sqlite3_last_insert_rowid for caveats.
       */
      sqlite3_int64 get_last_insert_rowid();

      /** \brief Represents a statement retrieved from the
       *  cache.
       *
       *  statement_proxy objects act as strong references to the
       *  particular statement that was retrieved.  When all the
       *  references to a statement expire, it is returned to the
       *  cache as the most recently used entry.
       *
       *  Because statements are not thread-safe, statement proxies
       *  should not be passed between threads (more specifically,
       *  they should not be dereferenced from multiple threads at
       *  once).  Instead, each thread should invoke
       *  get_cached_statement() separately.
       */
      class statement_proxy
      {
	boost::shared_ptr<statement_proxy_impl> impl;

	friend class db;

	statement_proxy(const boost::shared_ptr<statement_proxy_impl> &_impl)
	  : impl(_impl)
	{
	}

      public:
	statement &operator*() const { return *impl->get_statement(); }
	statement *operator->() const { return impl->get_statement().get(); }

	/** \brief Discard the reference to the implementation. */
	void reset() { impl.reset(); }
	/** \brief Test whether we have a valid pointer to the implementation. */
	bool valid() const { return impl.get() != NULL; }
      };

      /** \brief Retrieve a statement from this database's statement
       *  cache.
       *
       *  If the statement is not in the cache, it will be compiled
       *  and added.
       */
      statement_proxy get_cached_statement(const std::string &sql);

      /** \brief Execute multiple expressions from one string.
       *
       *  Useful for setting up a database initially.
       *
       *  \param sql   The SQL code to execute.
       *  \param callback  A callback invoked for every row
       *                   of data produced by the statement.
       *  \param data  Data to pass as the first argument to
       *               the callback.
       *  \param errmsg    A location in which to store any errors
       *                   generated by the SQL statements.
       */
      void exec(const std::string &sql,
		int (*callback)(void *, int, char **, char **) = NULL,
		void *data = NULL);
    };

    /** \brief Wraps a prepared sqlite3 statement.
     *
     *  This class is explicitly *not* thread-safe.  You should place
     *  locks around it if it's going to be accessed from multiple
     *  threads.
     */
    class statement
    {
    public:
      /** \brief RAII class that controls access to the results of a
       *  statement.
       *
       *  This is essentially "the right to invoke sqlite3_step".
       *  Using RAII ensures that statements are never left in an
       *  "executing" state.  The caller is still responsible for
       *  ensuring that sqlite3_step is not invoked on two statements
       *  at once (sqlite doesn't like that).
       */
      class execution
      {
	statement &parent;
	bool stepped;

      public:
	execution(statement &_parent)
	  : parent(_parent), stepped(false)
	{
	}

	~execution()
	{
	  if(stepped)
	    {
	      try
		{
		  parent.reset();
		}
	      catch(...)
		{
		}
	    }
	}

	/** \brief Step to the next result row of the statement.
	 *
	 *  Mirroring the underlying sqlite behavior, there is no
	 *  "result" object -- meaning that if multiple threads might
	 *  retrieve results from the same statement, they need to lock
	 *  each other out.
	 *
	 *  \return \b true if a new row of results was retrieved, \b
	 *  false otherwise.
	 */
	bool step()
	{
	  stepped = true;
	  return parent.step();
	}
      };

    private:
      db &parent;
      sqlite3_stmt *handle;
      /** \brief Set to \b true when results are available.
       *
       *  Used to sanity-check that the database is being used
       *  correctly (according to the rules laid down in the docs).
       *  Maybe sqlite does this already, but since it's not
       *  documented I don't want to rely on it.
       */
      bool has_data;

      friend class db;
      friend class db::statement_proxy_impl;

      statement(db &_parent, sqlite3_stmt *_handle);
      template<typename A, typename B, typename C>
      friend boost::shared_ptr<A> boost::make_shared(const B &, const C &);

      /** \brief Throw an exception if there isn't result data ready
       *  to be read.
       */
      void require_data()
      {
	if(!has_data)
	  throw exception("No data to retrieve.", SQLITE_MISUSE);
      }

      /** \brief Step to the next result row of the statement.
       *
       *  Mirroring the underlying sqlite behavior, there is no
       *  "result" object -- meaning that if multiple threads might
       *  retrieve results from the same statement, they need to lock
       *  each other out.
       *
       *  \return \b true if a new row of results was retrieved, \b
       *  false otherwise.
       */
      bool step();

    public:
      ~statement();

      /** \brief Prepare an SQL statement.
       */
      static boost::shared_ptr<statement>
      prepare(db &parent,
	      const std::string &sql);

      /** \brief Prepare an SQL statement.
       */
      static boost::shared_ptr<statement>
      prepare(db &parent,
	      const char *sql);

      /** \brief Return to the beginning of the statement's result set
       *  and discard parameter bindings.
       */
      void reset();

      /** \brief Parameter binding.
       *
       *  Note that parameter indices are one-based, while column
       *  indices are zero-based.  This is to minimize the abstraction
       *  distance from sqlite, which uses the same convention.
       */
      // @{

      /** \brief Bind a region of memory to a parameter as a BLOB.
       *
       *  \param parameter_idx   The one-based index of the parameter
       *  to set.
       *  \param blob A pointer to the memory region to input to the
       *  statement.
       *  \param size  The size of the memory that is to be stored.
       *  \param destructor   A method to be invoked on the BLOB when
       *  sqlite is done with it, or SQLITE_TRANSIENT if sqlite should
       *  make a temporary copy.
       */
      void bind_blob(int parameter_idx, const void *blob, int size, void (*destructor)(void *) = SQLITE_TRANSIENT);

      /** \brief Bind a double to a parameter.
       *
       *  \param parameter_idx  The one-based index of the parameter to set.
       *  \param value  The value to bind to this parameter.
       */
      void bind_double(int parameter_idx, double value);

      /** \brief Bind an integer to a parameter.
       *
       *  \param parameter_idx  The one-based index of the parameter to set.
       *  \param value  The value to bind to this parameter.
       */
      void bind_int(int parameter_idx, int value);


      /** \brief Bind a 64-bit integer to a parameter.
       *
       *  \param parameter_idx  The one-based index of the parameter to set.
       *  \param value  The value to bind to this parameter.
       */
      void bind_int64(int parameter_idx, sqlite3_int64 value);

      /** \brief Bind NULL to a parameter.
       *
       *  \param parameter_idx   The one-based index of the parameter to set.
       */
      void bind_null(int parameter_idx);

      /** \brief Bind a string to a parameter.
       *
       *  \param parameter_idx  The one-based index of the parameter to set.
       *  \param value  The value to bind to this parameter.
       *
       *  Makes a temporary copy of the string.
       */
      void bind_string(int parameter_idx, const std::string &value);

      /** \brief Bind a null-terminated string to a parameter.
       *
       *  \param parameter_idx  The one-based index of the parameter to set.
       *  \param value  The value to bind to this parameter.
       *  \param dx     How to handle memory management of the string (the
       *                default, SQLITE_TRANSIENT, says to make a private
       *                copy; see also SQLITE_STATIC to not manage the buffer
       *                at all).
       */
      void bind_string(int parameter_idx, const char *value, void (*dx)(void *) = SQLITE_TRANSIENT);

      /** \brief Bind a BLOB containing all zeroes to a parameter.
       *
       *  Typically used to initialize a BLOB that will be written
       *  incrementally (since incremental writes can't expand a BLOB,
       *  the initial size must be exactly the size that's needed).
       *
       *  \param parameter_idx  The one-based index of the parameter to set.
       *  \param size  The size in bytes of the zero-filled BLOB to insert.
       */
      void bind_zeroblob(int parameter_idx, int size);

      // @}

      /** \brief Execute a statement and discard its results.
       *
       *  This is equivalent to invoking step() until it returns \b
       *  false.  Useful for, e.g., side-effecting statements.
       */
      void exec()
      {
	while(step())
	  ; // Do nothing.
      }


      /** \brief Retrieve the value stored in a column as a BLOB.
       *
       *  The data block might be invalidated by any other method
       *  invoked on this statement.
       *
       *  \param column  The zero-based index of the column that is to be
       *                 retrieved.
       *  \param bytes   A location in which to store the size of the
       *                 result in bytes.
       *  \return A pointer to the block of data stored in the
       *  given column.
       */
      const void *get_blob(int column, int &bytes);

      /** \brief Retrieve the value stored in a column as a double.
       *
       *  \param column The zero-based index of the column that is to be
       *                retrieved.
       */
      double get_double(int column);

      /** \brief Retrieve the value stored in a column as an integer.
       *
       *  \param column The zero-based index of the column that is to be
       *                retrieved.
       */
      int get_int(int column);

      /** \brief Retrieve the value stored in a column as a 64-bit integer.
       *
       *  \param column The zero-based index of the column that is to be
       *                retrieved.
       */
      sqlite3_int64 get_int64(int column);

      /** \brief Retrieve the value stored in a column as a string. */
      std::string get_string(int column);

      /** \brief Retrieve the data type stored in the given column of
       *  the current row.
       */
      int get_column_type(int column);
    };

    /** \brief Represents a BLOB that has been opened for incremental
     *  I/O.
     */
    class blob
    {
      db &parent;
      sqlite3_blob *handle;

      friend class db;

      blob(db &_parent, sqlite3_blob *_handle);
      template<typename A, typename B, typename C>
      friend boost::shared_ptr<A> boost::make_shared(const B &, const C &);

    public:
      /** \brief Open an existing BLOB.
       *
       *  \param parent       The database object containing the BLOB.
       *  \param databaseName The name of the database to open (normally
       *                      "main", unless ATTACH has been used).
       *  \param table        The table containing the BLOB.
       *  \param column       The column name containing the BLOB.
       *  \param row          The ROWID of the row containing the BLOB.
       *  \param readWrite    If \b false, the BLOB will be opened
       *                      read-only; otherwise it will be opened
       *                      read-write.
       */
      static boost::shared_ptr<blob>
      open(db &parent,
	   const std::string &databaseName,
	   const std::string &table,
	   const std::string &column,
	   sqlite3_int64 row,
	   bool readWrite = true);

      /** \brief Retrieve the size of the BLOB in bytes. */
      int size();

      /** \brief Read some data from the BLOB.
       *
       *  \param offset   The byte offset at which to start reading.
       *  \param out      The memory location at which to begin storing
       *                  the data that was read.
       *  \param length   The number of bytes of data to read.
       *
       *  If there are fewer than "length" bytes following "offset" in
       *  the BLOB, this operation throws an exception.  If the BLOB's
       *  row has been modified, this operation throws an exception
       *  with the error code SQLITE_ABORT.
       */
      void read(int offset, void *out, int length);

      /** \brief Write some data into the BLOB.
       *
       *  \param offset The byte offset at which to start writing.
       *  \param in     The data to write into the BLOB.
       *  \param length The number of bytes of data to write.
       *
       *  If there are fewer than "length" bytes following "offset" in
       *  the BLOB, this operation throws an exception.  If the BLOB's
       *  row has been modified, this operation throws an exception
       *  with the error code SQLITE_ABORT.
       */
      void write(int offset, const void *in, int length);

      ~blob();
    };
  }
}

#endif
