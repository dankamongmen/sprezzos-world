// pkg_changelog.cc
//
//  Copyright 2000, 2004-2005, 2008-2009 Daniel Burrows
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; see the file COPYING.  If not, write to
//  the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//  Boston, MA 02111-1307, USA.

#include "pkg_changelog.h"

#include "apt.h"
#include "download_queue.h"

#include <generic/util/job_queue_thread.h>

#include <aptitude.h>
#include <config.h>

#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/stat.h>

#include <deque>

#include <sigc++/bind.h>

#include <apt-pkg/sourcelist.h>
#include <apt-pkg/fileutl.h>
#include <apt-pkg/strutl.h>

#include <cwidget/generic/util/ssprintf.h>

#include <boost/enable_shared_from_this.hpp>
#include <boost/make_shared.hpp>
#include <boost/scoped_ptr.hpp>

#include <loggers.h>

using namespace std;
namespace cw = cwidget;

namespace aptitude
{
  namespace apt
  {
    namespace
    {
      // Hack to attempt to suppress an unnecessary apt error in the case
      // that there aren't any source records.
      bool source_lines_exist()
      {
	for (pkgSourceList::const_iterator sourceListIt =
	       apt_source_list->begin();
	     sourceListIt != apt_source_list->end();
	     ++sourceListIt)
	  {
	    // Partly cribbed from srcrecords.cc in apt.
	    std::vector<pkgIndexFile *> *indexes = (*sourceListIt)->GetIndexFiles();
	    for (std::vector<pkgIndexFile *>::const_iterator indexesIt =
		   indexes->begin(); indexesIt != indexes->end(); ++indexesIt)
	      {
		boost::scoped_ptr<pkgSrcRecords::Parser>
		  parser((*indexesIt)->CreateSrcParser());

		if(parser.get() != NULL)
		  return true;
	      }
	  }

	return false;
      }
    }

    std::string get_changelog_uri(const std::string &path)
    {
      string server = _config->Find("APT::Changelogs::Server",
                                    "http://packages.debian.org/changelogs");

      return cw::util::ssprintf("%s/%s/changelog",
                                server.c_str(),
                                path.c_str());
    }

    /* Construct a changelog file path for third party sites that do
     * not use packages.debian.org/changelogs
     * This simply uses the ArchiveURI() of the source pkg and looks for
     * a .changelog file there.
     */
    bool guess_third_party_changelog_uri(pkgCache::VerIterator ver,
                                         const std::string &path,
                                         std::string &out_uri)
    {
      // guess uri for third-party packages
      pkgCache::VerFileIterator vf = ver.FileList();
      pkgCache::PkgFileIterator pf = vf.File();
      pkgIndexFile *index;
      if(apt_source_list->FindIndex(pf, index) == false)
        return false;

      out_uri = index->ArchiveURI(path + ".changelog");
      return true;
    }

    boost::shared_ptr<changelog_info>
    changelog_info::create(const std::string &source_package,
			   const std::string &source_version,
                           const std::vector<std::string> &uri_list,
			   const std::string &display_name)
    {
      return boost::make_shared<changelog_info>(source_package,
						source_version,
                                                uri_list,
						display_name);
    }

    boost::shared_ptr<changelog_info>
    changelog_info::guess(const std::string &source_package,
			  const std::string &source_version,
                          const std::string &section,
			  const std::string &display_name)
    {
      string realsection;
      const string::size_type slashfound = section.find('/');
      if(slashfound != section.npos)
        realsection.assign(section, 0, slashfound);
      else
        realsection.assign("main");

      string prefix;
      if(source_package.size() > 3
         && source_package.compare(0, 3, "lib") == 0)
        prefix.assign(source_package, 0, 4);
      else
        prefix.assign(source_package, 0, 1);

      string realver = StripEpoch(source_version);

      vector<string> uri_list;

      string path;
      path = cw::util::ssprintf("pool/%s/%s/%s/%s_%s",
                                realsection.c_str(),
                                prefix.c_str(),
                                source_package.c_str(),
                                source_package.c_str(),
                                realver.c_str());

      uri_list.push_back(get_changelog_uri(path));

      LOG_TRACE(Loggers::getAptitudeChangelog(),
		"Getting the changelog of the source package "
		<< source_package << " " << source_version);

      return boost::make_shared<changelog_info>(source_package,
						source_version,
                                                uri_list,
						display_name);
    }

    boost::shared_ptr<changelog_info>
    changelog_info::create(const pkgCache::VerIterator &ver)
    {
      if(ver.end())
	return boost::shared_ptr<changelog_info>();

      if(ver.FileList().end())
	{
	  LOG_TRACE(Loggers::getAptitudeChangelog(),
		    "No changelog information for " << ver.ParentPkg().Name()
		    << ": it isn't available from any archive.");
	  return boost::shared_ptr<changelog_info>();
	}

      pkgRecords::Parser &rec =
	apt_package_records->Lookup(ver.FileList());
      string source_package =
	rec.SourcePkg().empty() ? ver.ParentPkg().Name() : rec.SourcePkg();

      const string source_version =
	rec.SourceVer().empty() ? ver.VerStr() : rec.SourceVer();

      string path;
      path = flNotFile(rec.FileName());
      path += source_package + "_" + StripEpoch(source_version);

      vector<string> uri_list;

      uri_list.push_back(get_changelog_uri(path));

      string third_party_uri;
      if(guess_third_party_changelog_uri(ver, path, third_party_uri))
        uri_list.push_back(third_party_uri);

      LOG_TRACE(Loggers::getAptitudeChangelog(),
		"For " << ver.ParentPkg().Name()
		<< " " << ver.VerStr() << ", getting the changelog of the source package "
		<< source_package << " " << source_version);

      return boost::make_shared<changelog_info>(source_package,
						source_version,
                                                uri_list,
						ver.ParentPkg().Name());
    }

    namespace
    {
      /** \brief Callback proxy for changelog requests.
       *
       *  This is responsible for switching to fallback URIs when a
       *  download fails.
       */
      class changelog_download : public boost::enable_shared_from_this<changelog_download>, public download_request, public download_callbacks
      {
	// The callbacks the client code wants us to use for reporting
	// the state of the download.  All callbacks are passed through
	// to here, except for failures when there are more uris
	// available.
	//
	// We rely in several places on the fact that this member is
	// read-only (so we don't need to hold a lock to access it).
	boost::shared_ptr<download_callbacks> parent;

	// The current download, if any.
	boost::shared_ptr<download_request> current_download;

	// The URIs to fetch.
	std::deque<std::string> uris;

	// True if this download has been started.
	bool started : 1;

	// True if this download has completed, failed, or been canceled.
	bool finished : 1;

	// A mutex managing access to the above members (other than
	// parent).  Necessary because the download_request interface
	// states that cancel() may be called from any thread.
	cw::threads::mutex state_mutex;

	// How to send thunks to the main thread.
	post_thunk_f post_thunk;

	// A brief description of the changelog being downloaded by this
	// object.
	std::string short_description;

      public:
	changelog_download(const boost::shared_ptr<download_callbacks> &_parent,
			   post_thunk_f _post_thunk,
			   const std::string &_short_description)
	  : parent(_parent),
	    started(false), finished(false),
	    post_thunk(_post_thunk),
	    short_description(_short_description)
	{
	}

	void push_back(const std::string &uri)
	{
	  cw::threads::mutex::lock l(state_mutex);

	  if(finished)
	    LOG_WARN(Loggers::getAptitudeChangelog(),
		     "Not adding " << uri << " to the queue for " << short_description
		     << ": the item is no longer active.");
	  else
	    {
	      LOG_INFO(Loggers::getAptitudeChangelog(),
		       "Adding " << uri << " to the queue for " << short_description);

	      uris.push_back(uri);
	    }
	}

	/** \brief Enqueue the first URI in this object. */
	void start()
	{
	  cw::threads::mutex::lock l(state_mutex);

	  if(started)
	    LOG_TRACE(Loggers::getAptitudeChangelog(),
		      "Not starting to download " << short_description
		      << ": it is already started.");
	  else if(finished)
	    LOG_TRACE(Loggers::getAptitudeChangelog(),
		      "Not starting to download " << short_description
		      << ": it already finished downloading.");
	  else
	    {
	      const std::string &uri(uris.front());

	      LOG_INFO(Loggers::getAptitudeChangelog(),
		       "Enqueuing the first URI for "
		       << short_description << ": " << uri);

	      current_download = queue_download(uri, short_description,
						shared_from_this(),
						post_thunk);
	      started = true;
	    }
	}

	void success(const temp::name &filename)
	{
	  cw::threads::mutex::lock l(state_mutex);

	  if(finished)
	    LOG_TRACE(Loggers::getAptitudeChangelog(),
		      "Not signaling success for the download to "
		      << filename.get_name()
		      << " for " << short_description << ": the item is no longer active.");
	  else
	    {
	      LOG_TRACE(Loggers::getAptitudeChangelog(),
			"Signaling success for the download to "
			<< filename.get_name()
			<< " for " << short_description);

	      current_download.reset();
	      finished = true;

	      l.release(); // Release the lock in case the parent wants
	      // to call back into this object (not that
	      // they should).

	      parent->success(filename);
	    }
	}

	void failure(const std::string &msg)
	{
	  cw::threads::mutex::lock l(state_mutex);

	  if(finished)
	    {
	      LOG_TRACE(Loggers::getAptitudeChangelog(),
			"Not signaling failure for the download of "
			<< short_description
			<< ": the item is no longer active.");

	      return;
	    }

	  LOG_TRACE(Loggers::getAptitudeChangelog(),
		    "Downloading " << short_description
		    << " failed: " << msg);

	  if(uris.empty())
	    {
	      LOG_ERROR(Loggers::getAptitudeChangelog(),
			"Fatal error downloading " << short_description
			<< ": all URIs exhausted, with last error "
			<< msg);

	      current_download.reset();
	      finished = true;
	      l.release(); // Release the lock in case the parent wants
	      // to call back into this object.
	      parent->failure(msg);
	    }
	  else
	    {
	      {
		const std::string &uri(uris.front());

		LOG_INFO(Loggers::getAptitudeChangelog(),
			 "Falling back to the next URI for "
			 << short_description << ": " << uri);

		current_download = queue_download(uri, short_description,
						  shared_from_this(),
						  post_thunk);
	      }

	      uris.pop_front();

	      l.release();
	    }
	}

	void partial_download(const temp::name &filename,
			      unsigned long long currentSize,
			      unsigned long long totalSize)
	{
	  parent->partial_download(filename, currentSize, totalSize);
	}

	/** \brief Invoke failure() in the main thread.
	 */
	void post_failure(const std::string &msg)
	{
	  sigc::slot<void, std::string> failure_slot(sigc::mem_fun(*this, &changelog_download::failure));
	  sigc::slot<void> thunk(sigc::bind(failure_slot, msg));

	  boost::shared_ptr<changelog_download> this_ptr(shared_from_this());

	  post_thunk(make_keepalive_slot(thunk, this_ptr));
	}

	void cancel()
	{
	  cw::threads::mutex::lock l(state_mutex);

	  if(finished)
	    {
	      LOG_TRACE(Loggers::getAptitudeChangelog(),
			"Not canceling the download of "
			<< short_description
			<< ": the item is no longer active.");

	      return;
	    }
	  else
	    {
	      LOG_TRACE(Loggers::getAptitudeChangelog(),
			"Canceling the download of " << short_description);

	      if(current_download.get() != NULL)
		{
		  current_download->cancel();
		  current_download.reset();
		}

	      finished = true;
	    }
	}
      };

      class preprocess_changelogs_request
      {
	// Information about the download to perform.
	boost::shared_ptr<changelog_info> info;
	// The download object to use.
	boost::shared_ptr<changelog_download > download;

      public:
	preprocess_changelogs_request(const boost::shared_ptr<changelog_info> &_info,
				      const boost::shared_ptr<changelog_download> &_download)
	  : info(_info),
	    download(_download)
	{
	}

	const boost::shared_ptr<changelog_info> &get_info() const
	{
	  return info;
	}

	const boost::shared_ptr<changelog_download> &get_download() const
	{
	  return download;
	}
      };

      std::ostream &operator<<(std::ostream &out,
			       const preprocess_changelogs_request &req)
      {
	return out << "download(source_package = "
		   << req.get_info()->get_source_package()
		   << ", source_version = "
		   << req.get_info()->get_source_version()
		   << ", display_name = "
		   << req.get_info()->get_display_name()
		   << ", download = 0x"
		   << cw::util::ssprintf("%p", req.get_download().get());
      }

      /** \brief This thread is responsible for preprocessing requests
       *  for changelogs.
       *
       *  This thread checks whether each changelog already exists on
       *  the system; if so, it includes a file:// URI in the list of
       *  URIs to check.  Jobs must be added in the foreground thread.
       */
      class preprocess_changelogs_thread
	: public util::job_queue_thread<preprocess_changelogs_thread,
					preprocess_changelogs_request>
      {
	static bool signals_connected;

      public:
	preprocess_changelogs_thread()
	{
	  // This is why jobs have to be added from the foreground
	  // thread: we need to know that nothing is invoking
	  // cache_closed() or cache_reloaded() while we add to their
	  // slot lists.

	  if(!signals_connected)
	    {
	      cache_closed.connect(sigc::ptr_fun(&preprocess_changelogs_thread::stop));
	      cache_reloaded.connect(sigc::ptr_fun(&preprocess_changelogs_thread::start));
	      signals_connected = true;
	    }
	}

	static logging::LoggerPtr get_log_category()
	{
	  return Loggers::getAptitudeChangelog();
	}

	void process_job(const preprocess_changelogs_request &req)
	{
	  logging::LoggerPtr logger(get_log_category());

	  const changelog_info &info = *req.get_info();
	  changelog_download &download = *req.get_download();

	  const string source_package(info.get_source_package());
	  const string source_version(info.get_source_version());
          const vector<string> uri_list(info.get_uri_list());
	  const string name(info.get_display_name());
	  const string short_description = cw::util::ssprintf(_("Changelog of %s"), name.c_str());

	  try
	    {
	      if(source_lines_exist())
		{
		  // Try to find a changelog that's already on the
		  // system, first.  Check each binary package in the
		  // source package; for any package that's unpacked,
		  // check that the version on the system corresponds
		  // to the requested source version, and if it passes
		  // look for a changelog.
		  pkgSrcRecords source_records(*apt_source_list);
		  source_records.Restart();
		  pkgSrcRecords::Parser *source_rec = source_records.Find(source_package.c_str());

		  if(source_rec != NULL)
		    for(const char **binaryIt = source_rec->Binaries();
			binaryIt != NULL && *binaryIt != NULL; ++binaryIt)
		      {
			pkgCache::PkgIterator pkg = (*apt_cache_file)->FindPkg(*binaryIt);
			if(!pkg.end() &&
			   !pkg.CurrentVer().end() &&
			   !pkg.CurrentVer().FileList().end() &&
			   pkg->CurrentState != pkgCache::State::NotInstalled &&
			   pkg->CurrentState != pkgCache::State::ConfigFiles)
			  {
			    pkgRecords::Parser &rec(apt_package_records->Lookup(pkg.CurrentVer().FileList()));
			    std::string rec_sourcepkg = rec.SourcePkg();
			    if(rec_sourcepkg.empty())
			      rec_sourcepkg = pkg.Name();
			    std::string rec_sourcever = rec.SourceVer();
			    if(rec_sourcever.empty())
			      rec_sourcever = pkg.CurrentVer().VerStr();

			    if(rec_sourcepkg == source_package &&
			       rec_sourcever == source_version)
			      {
				// Everything passed.  Now test to see whether
				// the changelog exists by trying to stat it.
				struct stat buf;

				std::string changelog_file = "/usr/share/doc/";
				changelog_file += pkg.Name();
				changelog_file += "/changelog.Debian";

				if(stat(changelog_file.c_str(), &buf) == 0)
				  {
				    LOG_TRACE(logger,
					      "Adding " << "file://" << changelog_file
					      << " as a URI for the changelog of " << source_package << " " << source_version);
				    download.push_back("file://" + changelog_file);
				  }

				changelog_file += ".gz";

				if(stat(changelog_file.c_str(), &buf) == 0)
				  {
				    LOG_TRACE(logger,
					      "Adding " << "gzip://" << changelog_file
					      << " as a URI for the changelog of " << source_package << " " << source_version);
				    download.push_back("gzip://" + changelog_file);
				  }

				// Beware the races here -- ideally we
				// should parse the returned changelog
				// and check that the first version it
				// contains is what we expect.  This
				// could return the wrong file in the
				// case that the changelog on the system
				// doesn't match the installed package
				// version, or the package was upgraded
				// under us before the changelog could
				// be fetched.  These are both fairly
				// corner cases and it might not be
				// worth bogging down the logic here in
				// order to exclude them anyway.
			      }
			  }
		      }
		}

              for(vector<string>::const_iterator uri = uri_list.begin();
                  uri != uri_list.end();
                  ++uri)
                {
                  LOG_TRACE(logger,
                            "Adding " << (*uri)
                            << " as a URI for the changelog of " << source_package
                            << " " << source_version);

                  download.push_back(*uri);
                }

	      LOG_TRACE(logger,
			"Starting to download " << short_description);
	      download.start();
	    }
	  catch(cw::util::Exception &ex)
	    {
	      LOG_FATAL(logger, "Failed to download changelogs: " << ex.errmsg());
	      download.post_failure(cw::util::ssprintf(_("Failed to download changelogs: %s"), ex.errmsg().c_str()));
	    }
	  catch(std::exception &ex)
	    {
	      LOG_FATAL(logger, "Failed to download changelogs: " << ex.what());
	      download.post_failure(cw::util::ssprintf(_("Failed to download changelogs: %s"), ex.what()));
	    }
	  catch(...)
	    {
	      LOG_FATAL(logger, "Failed to download changelogs: unexpected exception type");
	      download.post_failure(cw::util::ssprintf(_("Failed to download changelogs: unexpected exception.")));
	    }
	}
      };

      bool preprocess_changelogs_thread::signals_connected = false;
    }

  boost::shared_ptr<download_request>
  get_changelog(const boost::shared_ptr<changelog_info> &info,
		const boost::shared_ptr<download_callbacks> &callbacks,
		post_thunk_f post_thunk)
  {
    const std::string short_description =
      cw::util::ssprintf(_("Changelog of %s"), info->get_display_name().c_str());

    boost::shared_ptr<changelog_download> rval =
      boost::make_shared<changelog_download>(callbacks, post_thunk, short_description);

    preprocess_changelogs_thread::add_job(preprocess_changelogs_request(info, rval));

    return rval;
  }

namespace
{
  class slot_callbacks : public download_callbacks
  {
    sigc::slot<void, temp::name> success_slot;
    sigc::slot<void, std::string> failure_slot;

  public:
    slot_callbacks(const sigc::slot<void, temp::name> &_success_slot,
		   const sigc::slot<void, std::string> &_failure_slot)
      : success_slot(_success_slot),
	failure_slot(_failure_slot)
    {
    }

    void success(const temp::name &n)
    {
      success_slot(n);
    }

    void failure(const std::string &msg)
    {
      failure_slot(msg);
    }
  };
}

boost::shared_ptr<download_request>
get_changelog(const pkgCache::VerIterator &ver,
	      post_thunk_f post_thunk,
	      const sigc::slot<void, temp::name> &success,
	      const sigc::slot<void, std::string> &failure)
{
  return get_changelog(changelog_info::create(ver),
		       boost::make_shared<slot_callbacks>(success, failure),
		       post_thunk);
}
}
}
