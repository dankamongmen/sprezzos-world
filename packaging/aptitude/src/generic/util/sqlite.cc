/** \file sqlite.cc */


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

#include "sqlite.h"

#include <boost/make_shared.hpp>

#include <string.h>

namespace aptitude
{
  namespace sqlite
  {
    /** \brief The default maximum size of a database's statement
     *  cache.
     */
    const unsigned int default_statement_cache_limit = 100;

    db::lock::lock(db &parent)
      : handle(parent.handle)
    {
      sqlite3_mutex_enter(sqlite3_db_mutex(handle));
    }

    db::lock::~lock()
    {
      sqlite3_mutex_leave(sqlite3_db_mutex(handle));
    }

    db::db(const std::string &filename,
	   int flags,
	   const char *vfs)
      : statement_cache_limit(default_statement_cache_limit)
    {
      const int result =
	sqlite3_open_v2(filename.c_str(), &handle,
			flags, vfs);
      if(result != SQLITE_OK)
	{
	  std::string msg(get_error());
	  // sqlite3_open will allocate a database object even on
	  // error, probably to ensure we can retrieve the message (as
	  // above).  To avoid a resource leak, it's necessary to
	  // close the object after retrieving the message.
	  if(handle != NULL)
	    sqlite3_close(handle);
	  throw exception(msg, result);
	}
    }

    db::~db()
    {
      // NULL out the handles of all outstanding statements, to avoid
      // trying to access freed objects.
      for(boost::unordered_set<statement *>::const_iterator
	    it = active_statements.begin();
	  it != active_statements.end(); ++it)
	{
	  (*it)->handle = NULL;
	}

      // Close all active statements on the database.
      while(1)
	{
	  sqlite3_stmt *stmt(sqlite3_next_stmt(handle, NULL));

	  if(stmt == NULL)
	    break;
	  else
	    sqlite3_finalize(stmt);
	}

      // Close all active blobs on the database.
      for(boost::unordered_set<blob *>::const_iterator
	    it = active_blobs.begin();
	  it != active_blobs.end(); ++it)
	{
	  sqlite3_blob_close((*it)->handle);
	  (*it)->handle = NULL;
	}

      sqlite3_close(handle);
    }

    void db::set_busy_timeout(int timeout)
    {
      lock l(*this);

      int result = sqlite3_busy_timeout(handle, timeout);
      if(result != SQLITE_OK)
	throw exception(get_error(), result);
    }

    std::string db::get_error()
    {
      lock l(*this);

      // Note that we're careful to copy the error message into a
      // local string while holding the lock.
      std::string rval(sqlite3_errmsg(handle));

      return rval;
    }

    sqlite3_int64 db::get_last_insert_rowid()
    {
      return sqlite3_last_insert_rowid(handle);
    }

    void db::cache_statement(const statement_cache_entry &entry)
    {
      cwidget::threads::mutex::lock l(statement_cache_mutex);

      statement_cache_mru &mru(get_cache_mru());
      mru.push_back(entry);

      // Drop old entries from the cache if it's too large.
      while(mru.size() > statement_cache_limit)
	mru.pop_front();
    }

    db::statement_proxy_impl::~statement_proxy_impl()
    {
      // Careful here: the database might have been deleted while the
      // proxy is active.  WE RELY ON THE FACT THAT DELETING THE
      // DATABASE NULLS OUT THE STATEMENT HANDLE.  If you delete the
      // database from a separate thread while statement proxies are
      // still active ... then sorry, but you're screwed.
      if(entry.stmt->handle == NULL)
	return; // The database is dead; nothing to do.
      else
	entry.stmt->parent.cache_statement(entry);
    }

    db::statement_proxy db::get_cached_statement(const std::string &sql)
    {
      cwidget::threads::mutex::lock l(statement_cache_mutex);

      // Check whether the statement exists in the cache.
      statement_cache_hash_index &index(get_cache_hash_index());

      statement_cache_hash_index::const_iterator found =
	index.find(sql);

      if(found != index.end())
	{
	  // Extract the element from the set and return it.
	  statement_cache_entry entry(*found);
	  entry.stmt->reset();

	  index.erase(sql);

	  return statement_proxy(boost::make_shared<statement_proxy_impl>(entry));
	}
      else
	{
	  // Prepare a new SQL statement and return a proxy to it.  It
	  // won't be added to the cache until the caller is done with
	  // it.
	  boost::shared_ptr<statement> stmt(statement::prepare(*this, sql));

	  statement_cache_entry entry(sql, stmt);
	  return statement_proxy(boost::make_shared<statement_proxy_impl>(entry));
	}
    }

    namespace
    {
      struct free_on_destroy
      {
	char * &arr;

	free_on_destroy(char * &_arr) : arr(_arr) { }
	~free_on_destroy() { if(arr != NULL) sqlite3_free(arr); }
      };
    }

    void db::exec(const std::string &sql,
		  int (*callback)(void *, int, char **, char **),
		  void *data)
    {
      char *msg = NULL;
      free_on_destroy msg_free(msg);
      int result = sqlite3_exec(handle, sql.c_str(),
				callback, data, &msg);

      std::string errmsg("internal error: no sqlite error");
      if(msg != NULL)
	errmsg = msg;

      if(result != SQLITE_OK || msg != NULL)
	throw exception(errmsg, result);
    }



    statement::statement(db &_parent, sqlite3_stmt *_handle)
      : parent(_parent),
	handle(_handle),
	has_data(false)
    {
      parent.active_statements.insert(this);
    }

    statement::~statement()
    {
      if(handle != NULL)
	sqlite3_finalize(handle);
      parent.active_statements.erase(this);
    }

    boost::shared_ptr<statement>
    statement::prepare(db &parent,
		       const std::string &sql)
    {
      sqlite3_stmt *handle = NULL;

      // Serialize access to the database for this call.
      db::lock l(parent);

      const int result =
	sqlite3_prepare_v2(parent.handle,
			   sql.c_str(),
			   static_cast<int>(sql.size()),
			   &handle,
			   NULL);

      if(result != SQLITE_OK)
	{
	  // Paranoia: the docs say that "handle" is now NULL, but
	  // just in case...
	  if(handle != NULL)
	    sqlite3_finalize(handle);

	  throw exception(parent.get_error(), result);
	}
      else
	return boost::make_shared<statement>(boost::ref(parent), handle);
    }

    boost::shared_ptr<statement>
    statement::prepare(db &parent,
		       const char *sql)
    {
      sqlite3_stmt *handle = NULL;

      // Serialize access to the database for this call.
      db::lock l(parent);

      const int result =
	sqlite3_prepare_v2(parent.handle,
			   sql,
			   -1,
			   &handle,
			   NULL);

      if(result != SQLITE_OK)
	{
	  // Paranoia: the docs say that "handle" is now NULL, but
	  // just in case...
	  if(handle != NULL)
	    sqlite3_finalize(handle);

	  throw exception(parent.get_error(), result);
	}
      else
	return boost::make_shared<statement>(boost::ref(parent), handle);
    }

    void statement::reset()
    {
      sqlite3_reset(handle);
      has_data = false;
    }

    bool statement::step()
    {
      int result = sqlite3_step(handle);
      if(result == SQLITE_ROW)
	{
	  has_data = true;
	  return true;
	}
      else if(result == SQLITE_DONE)
	{
	  has_data = false;
	  return false;
	}
      else
	throw exception(parent.get_error(), result);
    }

    int statement::get_column_type(int column)
    {
      return sqlite3_column_type(handle, column);
    }

    const void *statement::get_blob(int column, int &bytes)
    {
      require_data();
      const void *rval = sqlite3_column_blob(handle, column);
      bytes = sqlite3_column_bytes(handle, column);

      return rval;
    }

    double statement::get_double(int column)
    {
      require_data();
      return sqlite3_column_double(handle, column);
    }

    int statement::get_int(int column)
    {
      require_data();
      return sqlite3_column_int(handle, column);
    }

    sqlite3_int64 statement::get_int64(int column)
    {
      require_data();
      return sqlite3_column_int64(handle, column);
    }

    std::string statement::get_string(int column)
    {
      require_data();

      const unsigned char * const rval = sqlite3_column_text(handle, column);
      const int bytes = sqlite3_column_bytes(handle, column);

      return std::string(rval, rval + bytes);
    }

    void statement::bind_blob(int parameter_idx, const void *blob, int size, void (*destructor)(void *))
    {
      const int result = sqlite3_bind_blob(handle, parameter_idx, blob, size, destructor);
      if(result != SQLITE_OK)
	{
	  throw exception(parent.get_error(), result);
	}
    }

    void statement::bind_double(int parameter_idx, double value)
    {
      const int result = sqlite3_bind_double(handle, parameter_idx, value);
      if(result != SQLITE_OK)
	{
	  throw exception(parent.get_error(), result);
	}
    }

    void statement::bind_int(int parameter_idx, int value)
    {
      const int result = sqlite3_bind_int(handle, parameter_idx, value);
      if(result != SQLITE_OK)
	{
	  throw exception(parent.get_error(), result);
	}
    }

    void statement::bind_int64(int parameter_idx, sqlite3_int64 value)
    {
      const int result = sqlite3_bind_int64(handle, parameter_idx, value);
      if(result != SQLITE_OK)
	{
	  throw exception(parent.get_error(), result);
	}
    }

    void statement::bind_null(int parameter_idx)
    {
      const int result = sqlite3_bind_null(handle, parameter_idx);
      if(result != SQLITE_OK)
	{
	  throw exception(parent.get_error(), result);
	}
    }

    void statement::bind_string(int parameter_idx, const std::string &value)
    {
      const int result = sqlite3_bind_text(handle, parameter_idx, value.c_str(), value.size(), SQLITE_TRANSIENT);
      if(result != SQLITE_OK)
	{
	  throw exception(parent.get_error(), result);
	}
    }

    void statement::bind_string(int parameter_idx, const char *value, void (*dx)(void *))
    {
      const int result = sqlite3_bind_text(handle, parameter_idx, value, strlen(value), dx);
      if(result != SQLITE_OK)
	{
	  throw exception(parent.get_error(), result);
	}
    }

    void statement::bind_zeroblob(int parameter_idx, int size)
    {
      const int result = sqlite3_bind_zeroblob(handle, parameter_idx, size);
      if(result != SQLITE_OK)
	{
	  throw exception(parent.get_error(), result);
	}
    }

    blob::blob(db &_parent, sqlite3_blob *_handle)
      : parent(_parent),
	handle(_handle)
    {
      parent.active_blobs.insert(this);
    }

    boost::shared_ptr<blob>
    blob::open(db &parent,
	       const std::string &databaseName,
	       const std::string &table,
	       const std::string &column,
	       sqlite3_int64 row,
	       bool readWrite)
    {
      sqlite3_blob *handle = NULL;

      const int result = sqlite3_blob_open(parent.handle,
					   databaseName.c_str(),
					   table.c_str(),
					   column.c_str(),
					   row,
					   readWrite ? 1 : 0,
					   &handle);

      if(result != SQLITE_OK)
	{
	  std::string msg(parent.get_error());

	  // Paranoia: handle should always be NULL, but free it if it
	  // isn't.
	  if(handle != NULL)
	    sqlite3_blob_close(handle);
	  throw exception(msg, result);
	}
      else
	return boost::make_shared<blob>(boost::ref(parent), handle);
    }

    blob::~blob()
    {
      if(handle != NULL)
	sqlite3_blob_close(handle);
      parent.active_blobs.erase(this);
    }

    int blob::size()
    {
      return sqlite3_blob_bytes(handle);
    }

    void blob::read(int offset, void *out, int length)
    {
      const int result = sqlite3_blob_read(handle, out, length, offset);
      if(result != SQLITE_OK)
	{
	  std::string msg(parent.get_error());
	  throw exception(msg, result);
	}
    }

    void blob::write(int offset, const void *out, int length)
    {
      const int result = sqlite3_blob_write(handle, out, length, offset);
      if(result != SQLITE_OK)
	{
	  std::string msg(parent.get_error());
	  throw exception(msg, result);
	}
    }
  }
}
