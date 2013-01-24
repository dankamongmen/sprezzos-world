/** \file file_cache.cc */     // -*-c++-*-

//   Copyright (C) 2009 Daniel Burrows
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

#include "file_cache.h"

#include "sqlite.h"
#include "util.h"

#include <apt-pkg/fileutl.h>

#include <cwidget/generic/threads/threads.h>
#include <cwidget/generic/util/exception.h>
#include <cwidget/generic/util/ssprintf.h>

#include <boost/format.hpp>
#include <boost/iostreams/copy.hpp>
#include <boost/iostreams/device/file.hpp>
#include <boost/iostreams/filter/gzip.hpp>
#include <boost/iostreams/filtering_stream.hpp>
#include <boost/iostreams/invert.hpp>
#include <boost/iostreams/operations.hpp>
#include <boost/iostreams/read.hpp>
#include <boost/iostreams/seek.hpp>
#include <boost/iostreams/stream.hpp>
#include <boost/iostreams/write.hpp>
#include <boost/make_shared.hpp>

#include <loggers.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

using namespace aptitude::sqlite;
namespace cw = cwidget;
namespace io = boost::iostreams;

namespace aptitude
{
  namespace util
  {
    namespace
    {
      // Upgrade routines.  Each routine moves from one database
      // version to the next version, and combining all these routines
      // should produce a database of the most recent version.  (this
      // is checked by a unit test) Failure is indicated by throwing
      // an exception.
      namespace upgrade
      {
	// Version 3 added the ModificationTime column to the cache,
	// which is the time of the item's last modification in
	// seconds-since-the-epoch.
	void version_2_to_version_3(const boost::shared_ptr<db> &store)
	{
	  LOG_INFO(Loggers::getAptitudeDownloadCache(),
		   "Upgrading the cache from version 2 to version 3.");

	  store->exec("savepoint upgrade23");

	  boost::shared_ptr<statement> get_version_statement =
	    statement::prepare(*store, "select version from format");
	  {
	    statement::execution get_version_execution(*get_version_statement);
	    if(!get_version_execution.step())
	      throw FileCacheException("Can't read the cache version number.");
	    else
	      {
		int database_version = get_version_statement->get_int(0);
		if(database_version != 2)
		  throw FileCacheException("Wrong database version number for this upgrade.");
	      }
	  }

	  const char * const sql = "                                    \
alter table cache							\
add column ModificationTime  numeric   default 0  not null;		\
									\
update format								\
set version = 3;							\
									\
release upgrade23;							\
";

	  store->exec(sql);
	}
      }


      /** \brief An SQLite-backed cache.
       *
       *  The cache schema is defined below as a gigantic string
       *  constant.  The reason for splitting blobs out from the main
       *  table is primarily that the incremental blob-reading
       *  functions cause trouble if someone else updates the row
       *  containing the blob; each row in the blob table above will
       *  be immutable until it's removed.
       *
       *  Note that referential integrity is only checked on changes
       *  to the main cache table.  If an entry in the main cache
       *  table is deleted, we zap the corresponding blob entry
       *  automatically.
       *
       *  The "format" table supports upgrades to the cache format; it
       *  contains a single row, which contains a single column
       *  holding the current version of the database schema.
       */
      class file_cache_sqlite : public file_cache
      {
	boost::shared_ptr<db> store;
	std::string filename; // Used to report errors.
	/** \brief The maximum size of the cache, in bytes.
	 *
	 *  \todo What's the best way to compute the current size?  I
	 *  could store it in the database and update it as part of
	 *  the transaction that inserts / removes blobs.  Or I could
	 *  use triggers to keep it up-to-date.  Or perhaps there's an
	 *  sqlite query that will return the number of data pages
	 *  used by a table/column?
	 *
	 *  Also, I need to decide when to vacuum the database: after
	 *  each update, after deletes, or what?
	 */
	int max_size;

	// Used to ensure that only one thread is accessing the
	// database connection at once.  Several sqlite3 functions
	// (e.g., sqlite3_last_insert_rowid()) are not threadsafe.
	cw::threads::mutex store_mutex;

	static const int current_version_number = 3;

	void create_new_database()
	{
	  // Note that I rely on the fact that integer primary key
	  // columns autoincrement.  If the largest one is chosen,
	  // then (according to the sqlite documentation) this will
	  // fail gracefully; cache contents will end up in a random
	  // order.  Since the largest key is on the order of 10^18, I
	  // only really care that something halfway sensible happens
	  // in that case.
	  std::string schema = "                                        \
begin transaction;							\
create table format ( version integer );				\
									\
create table globals ( TotalBlobSize integer not null );		\
									\
create table cache ( CacheId integer primary key,			\
                     ModificationTime datetime not null,                \
                     BlobSize integer not null,				\
                     BlobId integer not null,				\
                     Key text not null );				\
									\
create table blobs ( BlobId integer primary key,			\
                     Data blob not null );				\
									\
create index cache_by_blob_id on cache (BlobId);			\
create unique index cache_by_key on cache (Key);			\
									\
									\
create trigger i_cache_blob_size					\
before insert on cache							\
for each row begin							\
    update globals set TotalBlobSize = TotalBlobSize + NEW.BlobSize;	\
end;									\
									\
create trigger u_cache_blob_size					\
before update on cache							\
for each row begin							\
    update globals set TotalBlobSize = TotalBlobSize + NEW.BlobSize - OLD.BlobSize; \
end;									\
									\
create trigger d_cache_blob_size					\
before delete on cache							\
for each row begin							\
    update globals set TotalBlobSize = TotalBlobSize - OLD.BlobSize;	\
end;									\
									\
									\
									\
create trigger fki_cache_blob_id					\
before insert on cache							\
for each row begin							\
    select raise(rollback, 'Insert on table \"cache\" violates foreign key constraint \"fki_cache_blob_id\"') \
    where (select 1 from blobs where blobs.BlobId = NEW.BlobId) is NULL; \
end;									\
									\
create trigger fku_cache_blob_id					\
before update of BlobId on cache					\
for each row begin							\
    select raise(rollback, 'Insert on table \"cache\" violates foreign key constraint \"fki_cache_blob_id\"') \
    where (select 1 from blobs where blobs.BlobId = NEW.BlobId) is NULL; \
end;									\
									\
create trigger fkd_cache_blob_id					\
before delete on cache							\
for each row begin							\
    delete from blobs							\
       where blobs.BlobId = OLD.BlobId					\
        and (select 1 from cache where cache.BlobId = OLD.BlobId) is NULL; \
end;									\
									\
									\
insert into globals(TotalBlobSize) values(0);				\
";

	  store->exec(schema);

	  try
	    {
	      boost::shared_ptr<statement> set_version_statement =
		statement::prepare(*store, "insert into format(version) values(?)");

	      set_version_statement->bind_int(1, current_version_number);
	      set_version_statement->exec();

	      store->exec("commit");

	      sanity_check_database();
	    }
	  catch(...)
	    {
	      try
		{
		  store->exec("rollback");
		}
	      catch(...)
		{
		}

	      throw;
	    }
	}

	void sanity_check_database()
	{
	  try
	    {
	      store->exec("begin transaction");

	      try
		{
		  // Check the version number, transparently upgrading
		  // the database if we know how to.
		  boost::shared_ptr<statement> get_version_statement =
		    statement::prepare(*store, "select version from format");
		  {
		    statement::execution get_version_execution(*get_version_statement);
		    if(!get_version_execution.step())
		      throw FileCacheException("Can't read the cache version number.");
		    else
		      {
			int database_version = get_version_statement->get_int(0);

			// Run the upgrade scripts, if any.
			switch(database_version)
			  {
			  case 2:
			    upgrade::version_2_to_version_3(store);
			    // Fallthrough.
			  case 3:
			    break;

			  }
		      }
		  }

		  // Check the version number again.
		  {
		    statement::execution get_version_execution(*get_version_statement);
		    if(!get_version_execution.step())
		      throw FileCacheException("Can't read the cache version number.");
		    else
		      {
			int database_version = get_version_statement->get_int(0);
			if(database_version != current_version_number)
			  throw FileCacheException("Unsupported version number.");
		      }
		  }

		  boost::shared_ptr<statement> get_total_size_statement =
		    statement::prepare(*store, "select TotalBlobSize from globals");
		  sqlite3_int64 total_size = -1;

		  {
		    statement::execution get_total_size_execution(*get_total_size_statement);
		    if(!get_total_size_execution.step())
		      throw FileCacheException("Can't read the total size of all the files in the database.");
		    total_size = get_total_size_statement->get_int64(0);
		  }

		  boost::shared_ptr<statement> compute_total_size_statement =
		    statement::prepare(*store, "select sum(BlobSize) from cache");
		  sqlite3_int64 computed_total_size = -1;

		  {
		    statement::execution compute_total_size_execution(*compute_total_size_statement);
		    if(!compute_total_size_execution.step())
		      throw FileCacheException("Can't compute the total size of all the files in the database.");
		    computed_total_size = compute_total_size_statement->get_int64(0);
		  }

		  if(computed_total_size != total_size)
		    {
		      LOG_WARN(Loggers::getAptitudeDownloadCache(),
			       boost::format("Inconsistent cache state: the stored total size %d does not match the actual total size %d.  Fixing it.")
			       % total_size % computed_total_size);

		      boost::shared_ptr<statement> fix_total_size_statement =
			statement::prepare(*store, "update globals set TotalSize = ?");
		      fix_total_size_statement->bind_int64(1, computed_total_size);
		      fix_total_size_statement->exec();
		    }

		  store->exec("commit");
		}
	      catch(...)
		{
		  try
		    {
		      store->exec("rollback");
		    }
		  catch(...)
		    {
		    }

		  // TODO: maybe if something fails we should just
		  // delete and re-create the database?

		  throw;
		}
	    }
	  catch(sqlite::exception &ex)
	    {
	      throw FileCacheException("Can't sanity-check the database: " + ex.errmsg());
	    }
	}

      public:
	file_cache_sqlite(const std::string &_filename, int _max_size)
	  : store(db::create(_filename)),
	    filename(_filename),
	    max_size(_max_size)
	{
	  // Set up the database.  First, check the format:
	  sqlite::db::statement_proxy check_for_format_statement =
	    store->get_cached_statement("select 1 from sqlite_master where name = 'format'");
	  bool has_format = false;
	  {
	    statement::execution check_for_format_execution(*check_for_format_statement);
	    has_format = check_for_format_execution.step();
	  }

	  // If we collide with an ongoing insert, allow it a half
	  // second to clear out.
	  store->set_busy_timeout(500);

	  if(!has_format)
	    create_new_database();
	  else
	    sanity_check_database();
	}

	void putItem(const std::string &key,
		     const std::string &path,
		     time_t mtime)
	{
	  // NOTE: We wait until we've finished compressing the input
	  // file to take the store mutex.
	  try
	    {
	      // Before anything else, we need to compress the input
	      // file to a temporary location.  Without this step,
	      // there's no way to know the size of the compressed
	      // data, but we need that size in order to insert it
	      // into the cache database.
	      temp::name tn("cacheContentCompressed");

	      const std::string compressed_path(tn.get_name());

	      // The size of the input file -- used only for logging
	      // so we can see how well it was compressed.
	      std::streamsize input_size = -1;

	      // Compress the input file.
	      //
	      // TODO: make the compression level an option.  Maybe
	      // also support other algorithms, although that needs a
	      // schema change (an extra column in the blobs table
	      // giving the compressor that was used).
	      {
		io::filtering_ostream compressed_out(io::zlib_compressor(9) | io::file_sink(compressed_path));

		input_size = io::copy(io::file(path), compressed_out);
	      }

	      if(input_size < 0)
		throw FileCacheException((boost::format("Unable to compress \"%s\" to \"%s\".")
					  % path % compressed_path).str());

	      LOG_TRACE(Loggers::getAptitudeDownloadCache(),
			"Compressed \"" << path << "\" to \"" << compressed_path << "\"");

	      // Here's the plan:
	      //
	      // 1) Open the compressed file and get its size.
	      // 2) If the file is too large to ever cache, return
	      //    immediately (don't cache it).
	      // 3) In an sqlite transaction:
	      //    3.a) Retrieve and save the keys of entries,
	      //         starting with the oldest, until removing
	      //         all the stored keys would create enough
	      //         space for the new entry.
	      //    3.b) Delete the entries that were saved.
	      //    3.c) Place the new entry into the cache.


	      // Step 1)
	      //
	      // Using Unix I/O instead of Boost IOStreams or
	      // std::ifstream because my attempts to use the latter
	      // two failed utterly: it seems to be impossible to
	      // determine a file's size using a high-level interface.
	      FileFd fd;
	      if(!fd.Open(compressed_path, FileFd::ReadOnly))
		{
		  const int err = errno;
		  throw FileCacheException((boost::format("Can't open \"%s\" to store it in the cache: %s")
					    % compressed_path % cw::util::sstrerror(err)).str());
		}

	      struct stat buf;
	      if(fstat(fd.Fd(), &buf) != 0)
		{
		  const int err = errno;
		  throw FileCacheException((boost::format("Can't determine the size of \"%s\" to store it in the cache: %s.")
					    % compressed_path % cw::util::sstrerror(err)).str());
		}

	      off_t compressed_size = buf.st_size;

	      if(compressed_size == 0 && input_size > 0)
		throw FileCacheException("Sanity-check failed: a non-empty file was compressed to zero bytes!.");

	      // Step 2)
	      if(compressed_size > max_size)
		{
		  LOG_INFO(Loggers::getAptitudeDownloadCache(),
			   "Refusing to cache \"" << compressed_path << "\" as \"" << key
			   << "\": its size " << compressed_size
			   << " is greater than the cache size limit " << max_size);
		  return;
		}

	      cw::threads::mutex::lock l(store_mutex);

	      LOG_INFO(Loggers::getAptitudeDownloadCache(),
		       "Caching " << compressed_path << " as " << key
		       << " (size: " << input_size << " -> "
		       << compressed_size << " ["
		       << (compressed_size == 0
			   ? (input_size == 0 ? boost::format("100%%") : boost::format("VANISHED?"))
			   : (boost::format("%.2f%%") % (((double)(100 * compressed_size)) / input_size)))
		       << "])");

	      store->exec("begin transaction");

	      // Step 3)
	      try
		{
		  sqlite::db::statement_proxy get_total_size_statement =
		    store->get_cached_statement("select TotalBlobSize from globals");

		  sqlite3_int64 total_size = -1;
		  {
		    statement::execution get_total_size_execution(*get_total_size_statement);
		    if(!get_total_size_execution.step())
		      throw FileCacheException("Can't read the total size of all the files in the database.");

		    total_size = get_total_size_statement->get_int64(0);
		  }

		  if(total_size + compressed_size > max_size)
		    {
		      LOG_TRACE(Loggers::getAptitudeDownloadCache(),
				boost::format("The new cache size %ld exceeds the maximum size %ld; dropping old entries.")
				% (total_size + compressed_size) % max_size);

		      bool first = true;
		      sqlite3_int64 last_cache_id_dropped = -1;
		      sqlite3_int64 amount_dropped = 0;
		      int num_dropped = 0;

		      // Step 3.a)
		      db::statement_proxy read_entries_statement =
			store->get_cached_statement("select CacheId, BlobSize from cache order by CacheId");
		      {
			statement::execution read_entries_execution(*read_entries_statement);

			while(total_size + compressed_size - amount_dropped > max_size &&
			      read_entries_execution.step())
			  {
			    first = false;
			    last_cache_id_dropped = read_entries_statement->get_int64(0);
			    amount_dropped += read_entries_statement->get_int64(1);
			    ++num_dropped;
			  }

			if(first)
			  throw FileCacheException("Internal error: no cached files, but the total size is nonzero.");
		      }

		      LOG_TRACE(Loggers::getAptitudeDownloadCache(),
				boost::format("Deleting %d entries from the cache for a total of %ld bytes saved")
				% num_dropped % amount_dropped);

		      // Step 3.b)
		      {
			sqlite::db::statement_proxy delete_old_statement =
			  store->get_cached_statement("delete from cache where CacheId <= ?");
			delete_old_statement->bind_int64(1, last_cache_id_dropped);
			delete_old_statement->exec();
		      }
		    }

		  // Step 3.c)
		  {
		    LOG_TRACE(Loggers::getAptitudeDownloadCache(),
			      boost::format("Inserting \"%s\" into the blobs table.") % compressed_path);

		    // The blob has to be inserted before the
		    // cache entry, so the foreign key constraints
		    // are maintained.

		    // Insert a zeroblob first, so we can write it
		    // incrementally.
		    {
		      sqlite::db::statement_proxy insert_blob_statement =
			store->get_cached_statement("insert into blobs (Data) values (zeroblob(?))");
		      insert_blob_statement->bind_int64(1, compressed_size);
		      insert_blob_statement->exec();
		    }

		    sqlite3_int64 inserted_blob_row =
		      store->get_last_insert_rowid();

		    // Delete any existing entries for the same
		    // key.  This hopefully avoids any trouble due
		    // to duplicate keys.
		    {
		      sqlite::db::statement_proxy delete_key_statement =
			store->get_cached_statement("delete from cache where Key = ?");
		      delete_key_statement->bind_string(1, key);
		      delete_key_statement->exec();
		    }

		    // Insert the corresponding entry in the cache
		    // table.
		    {
		      LOG_TRACE(Loggers::getAptitudeDownloadCache(),
				boost::format("Inserting \"%s\" into the cache table.") % key);

		      sqlite::db::statement_proxy insert_cache_statement =
			store->get_cached_statement("insert into cache (BlobId, BlobSize, Key, ModificationTime) values (?, ?, ?, ?)");
		      insert_cache_statement->bind_int64(1, inserted_blob_row);
		      insert_cache_statement->bind_int64(2, compressed_size);
		      insert_cache_statement->bind_string(3, key);
		      insert_cache_statement->bind_int64(4, mtime);
		      insert_cache_statement->exec();
		    }

		    boost::shared_ptr<blob> blob_data =
		      sqlite::blob::open(*store,
					 "main",
					 "blobs",
					 "Data",
					 inserted_blob_row);

		    int amount_to_write(compressed_size);
		    int blob_offset = 0;
		    static const int block_size = 16384;
		    char buf[block_size];
		    while(amount_to_write > 0)
		      {
			int curr_amt;
			if(amount_to_write < block_size)
			  curr_amt = static_cast<int>(amount_to_write);
			else
			  curr_amt = block_size;

			int amt_read = read(fd.Fd(), buf, curr_amt);
			if(amt_read == 0)
			  throw FileCacheException((boost::format("Unexpected end of file while reading %s into the cache.") % compressed_path).str());
			else if(amt_read < 0)
			  {
			    std::string errmsg(cw::util::sstrerror(errno));
			    throw FileCacheException((boost::format("Error while reading %s into the cache: %s.") % compressed_path % errmsg).str());
			  }

			blob_data->write(blob_offset, buf, curr_amt);
			blob_offset += amt_read;
			amount_to_write -= amt_read;
		      }
		  }


		  store->exec("commit");
		  LOG_INFO(Loggers::getAptitudeDownloadCache(),
			   boost::format("Cached \"%s\" as \"%s\"") % path % key);
		}
	      catch(...)
		{
		  // Try to roll back, but don't throw a new exception if
		  // that fails too.
		  try
		    {
		      store->exec("rollback");
		    }
		  catch(...)
		    {
		    }

		  // TODO: maybe instead of rethrowing the exception,
		  // we should delete the cache tables and recreate
		  // them?  But this should only happen if there was a
		  // *database* error rather than an error, e.g.,
		  // reading the file data to cache.
		  throw;
		}
	    }
	  catch(cw::util::Exception &ex)
	    {
	      LOG_WARN(Loggers::getAptitudeDownloadCache(),
		       boost::format("Can't cache \"%s\" as \"%s\": %s")
		       % path % key % ex.errmsg());
	    }
	  catch(std::exception &ex)
	    {
	      LOG_WARN(Loggers::getAptitudeDownloadCache(),
		       boost::format("Can't cache \"%s\" as \"%s\": %s")
		       % path % key % ex.what());
	    }
	}

	temp::name getItem(const std::string &key, time_t &mtime)
	{
	  cw::threads::mutex::lock l(store_mutex);

	  LOG_TRACE(Loggers::getAptitudeDownloadCache(),
		    boost::format("Looking up \"%s\" in the cache.") % key);

	  // Here's the plan.
	  //
	  // 1) In an sqlite transaction:
	  //    1.a) Look up the cache entry corresponding
	  //         to this key.
	  //    1.a.i)  If there is no entry, return an invalid name.
	  //    1.a.ii) If there is an entry,
	  //        1.a.ii.A) Update its last use field.
	  //        1.a.ii.B) Extract it to a temporary file
	  //                  and return it.
	  try
	    {
	      store->exec("begin transaction");

	      try
		{
		  sqlite::db::statement_proxy find_cache_entry_statement =
		    store->get_cached_statement("select CacheId, BlobId, ModificationTime from cache where Key = ?");

		  bool found = false;
		  sqlite3_int64 oldCacheId = -1;
		  sqlite3_int64 blobId = -1;
		  find_cache_entry_statement->bind_string(1, key);
		  {
		    statement::execution find_cache_entry_execution(*find_cache_entry_statement);
		    found = find_cache_entry_execution.step();

		    if(found)
		      {
			oldCacheId = find_cache_entry_statement->get_int64(0);
			blobId     = find_cache_entry_statement->get_int64(1);
			mtime      = find_cache_entry_statement->get_int64(2);
		      }
		    else
		      // 1.a.i: no matching entry
		      {
			LOG_TRACE(Loggers::getAptitudeDownloadCache(),
				  boost::format("No entry for \"%s\" found in the cache.") % key);

			store->exec("rollback");
			return temp::name();
		      }
		  }

		  // 1.a.ii.A: update the last use field.
		  {
		    // WARNING: this might fail if the largest
		    // cache ID has been used.  That should never
		    // happen in aptitude (you'd need 10^18 get or
		    // put calls), and trying to avoid it seems
		    // like it would cause a lot of trouble.
		    sqlite::db::statement_proxy update_last_use_statement =
		      store->get_cached_statement("update cache set CacheId = (select max(CacheId) from cache) + 1 where CacheId = ?");
		    update_last_use_statement->bind_int64(1, oldCacheId);
		    update_last_use_statement->exec();
		  }

		  // TODO: I should consolidate the temporary
		  // directories aptitude creates.
		  temp::name rval("cacheExtracted");

		  int extracted_size = -1;
		  {
		    // Decompress the data as it's written to the
		    // output file.
		    io::filtering_ostream outfile(io::zlib_decompressor() | io::file_sink(rval.get_name()));
		    if(!outfile.good())
		      throw FileCacheException(((boost::format("Can't open \"%s\" for writing"))
						% rval.get_name()).str());

		    boost::shared_ptr<sqlite::blob> blob_data =
		      sqlite::blob::open(*store,
					 "main",
					 "blobs",
					 "Data",
					 blobId,
					 false);

		    static const int block_size = 16384;
		    char buf[block_size];

		    int amount_to_read = blob_data->size();
		    int blob_offset = 0;

		    LOG_TRACE(Loggers::getAptitudeDownloadCache(),
			      boost::format("Extracting %d bytes to \"%s\".") % amount_to_read % rval.get_name());

		    // Copy the blob into the temporary file.
		    while(amount_to_read > 0)
		      {
			int curr_amt;

			if(amount_to_read < block_size)
			  curr_amt = amount_to_read;
			else
			  curr_amt = block_size;

			blob_data->read(blob_offset, buf, curr_amt);
			std::streamsize amt_written = io::write(outfile, buf, curr_amt);

			blob_offset += amt_written;
			amount_to_read -= amt_written;
		      }

		    extracted_size = blob_data->size();
		  }

		  LOG_INFO(Loggers::getAptitudeDownloadCache(),
			   boost::format("Extracted %d bytes corresponding to \"%s\" to \"%s\".")
			   % extracted_size % key % rval.get_name());

		  store->exec("commit");
		  return rval;
		}
	      catch(...)
		{
		  // Try to roll back, but don't throw a new exception if
		  // that fails too.
		  try
		    {
		      store->exec("rollback");
		    }
		  catch(...)
		    {
		    }

		  // TODO: maybe instead of rethrowing the exception,
		  // we should delete the cache tables and recreate
		  // them?  But this should only happen if there was a
		  // *database* error rather than an error, e.g.,
		  // reading the file data to cache.
		  throw;
		}
	    }
	  catch(cw::util::Exception &ex)
	    {
	      LOG_WARN(Loggers::getAptitudeDownloadCache(),
		       boost::format("Can't get the cache entry for \"%s\": %s")
		       % key % ex.errmsg());
	      return temp::name();
	    }
	  catch(std::exception &ex)
	    {
	      LOG_WARN(Loggers::getAptitudeDownloadCache(),
		       boost::format("Can't get the cache entry for \"%s\": %s")
		       % key % ex.what());
	      return temp::name();
	    }
	}
      };

      /** \brief A multilevel cache.
       *
       *  "get" requests are serviced from each sub-cache in turn,
       *  failing if the object isn't found in any cache.
       *
       *  "put" requests are forwarded to all sub-caches.
       */
      class file_cache_multilevel : public file_cache
      {
	std::vector<boost::shared_ptr<file_cache> > caches;

      public:
	file_cache_multilevel()
	{
	}

	void push_back(const boost::shared_ptr<file_cache> &cache)
	{
	  caches.push_back(cache);
	}


	void putItem(const std::string &key, const std::string &path,
		     time_t mtime)
	{
	  for(std::vector<boost::shared_ptr<file_cache> >::const_iterator
		it = caches.begin(); it != caches.end(); ++it)
	    (*it)->putItem(key, path, mtime);
	}

	temp::name getItem(const std::string &key, time_t &mtime)
	{
	  for(std::vector<boost::shared_ptr<file_cache> >::const_iterator
		it = caches.begin(); it != caches.end(); ++it)
	    {
	      temp::name found = (*it)->getItem(key, mtime);
	      if(found.valid())
		return found;
	    }

	  return temp::name();
	}
      };
    }

    boost::shared_ptr<file_cache> file_cache::create(const std::string &filename,
						     int memory_size,
						     int disk_size)
    {
      boost::shared_ptr<file_cache_multilevel> rval = boost::make_shared<file_cache_multilevel>();

      if(memory_size > 0)
	{
	  try
	    {
	      // \note A boost::multi_index_container might be more
	      // efficient for the in-memory cache.  OTOH, it would
	      // require more code.
	      rval->push_back(boost::make_shared<file_cache_sqlite>(":memory:", memory_size));
	    }
	  catch(const cw::util::Exception &ex)
	    {
	      LOG_WARN(Loggers::getAptitudeDownloadCache(),
		       "Unable to create the in-memory cache: " << ex.errmsg());
	    }
	}
      else
	LOG_INFO(Loggers::getAptitudeDownloadCache(),
		 "In-memory cache disabled.");


      if(disk_size > 0)
	{
	  try
	    {
	      rval->push_back(boost::make_shared<file_cache_sqlite>(filename, disk_size));
	    }
	  catch(const cw::util::Exception &ex)
	    {
	      LOG_WARN(Loggers::getAptitudeDownloadCache(),
		       "Unable to open the on-disk cache \"" << filename << "\": " << ex.errmsg());
	    }
	}
      else
	LOG_INFO(Loggers::getAptitudeDownloadCache(),
		 "On-disk cache disabled.");

      return rval;
    }

    file_cache::~file_cache()
    {
    }
  }
}
