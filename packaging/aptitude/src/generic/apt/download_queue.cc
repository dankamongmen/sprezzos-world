/** \file download_queue.cc */


// Copyright (C) 2009-2011 Daniel Burrows
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

#include "download_queue.h"

#include <loggers.h>

#include <generic/apt/apt.h>
#include <generic/util/file_cache.h>
#include <generic/util/job_queue_thread.h>

#include <apt-pkg/acquire.h>
#include <apt-pkg/acquire-item.h>
#include <apt-pkg/acquire-worker.h>
#include <apt-pkg/strutl.h>

#include <boost/enable_shared_from_this.hpp>
#include <boost/format.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/unordered_map.hpp>
#include <boost/weak_ptr.hpp>

#include <list>

#include <sigc++/bind.h>

namespace cw = cwidget;

namespace aptitude
{
  namespace
  {
    /** \brief Information about what a job is downloading and how to
     *  respond when it's complete.
     *
     *  Used by both active_download_info and the Item class.
     *  active_download_info also includes information on how to
     *  destroy the corresponding Item.
     */
    class download_job
    {
      std::string uri;
      std::string short_description;
      temp::name filename;
      temp::name cached_filename;
      // The last-modified-time of the cached value.
      time_t last_modified_time;

      typedef std::pair<boost::shared_ptr<download_callbacks>, post_thunk_f> listener;
      // The registered listeners on this job.  When one is canceled,
      // it's pulled out of this list.  This is threadsafe: remember
      // that the actual cancel process takes place in the download
      // thread, as do the processes of adding a new listener and
      // invoking all the listeners.
      std::list<listener> listeners;

    public:
      typedef std::list<listener>::iterator listener_connection;

      download_job(const std::string &_uri,
		   const std::string &_short_description,
		   const temp::name &_filename,
		   const temp::name &_cached_filename,
		   time_t _last_modified_time)
	: uri(_uri),
	  short_description(_short_description),
	  filename(_filename),
	  cached_filename(_cached_filename),
	  last_modified_time(_last_modified_time)
      {
      }

      const std::string &get_uri() const { return uri; }
      const std::string &get_short_description() const { return short_description; }
      const temp::name &get_filename() const { return filename; }
      const temp::name &get_cached_filename() const { return cached_filename; }
      time_t get_last_modified_time() const { return last_modified_time; }

      /** \brief Return \b true if there are no listeners on this job. */
      bool listeners_empty() const { return listeners.empty(); }

      listener_connection add_listener(const boost::shared_ptr<download_callbacks> &callbacks,
				       post_thunk_f post_thunk)
      {
	return listeners.insert(listeners.end(), listener(callbacks, post_thunk));
      }

      void remove_listener(listener_connection conn)
      {
	listeners.erase(conn);
      }

      /** \brief Removes this job from the queue's dictionary of
       *  active jobs and drops all listeners.  Must be invoked from
       *  the background download thread.
       */
      void mark_finished();

      /** \brief Invoke the success callback on each listener. */
      void invoke_success(const temp::name &filename) const
      {
	for(std::list<listener>::const_iterator
	      it = listeners.begin(); it != listeners.end(); ++it)
	  {
	    sigc::slot<void> success_slot =
	      sigc::bind(sigc::mem_fun(*it->first, &download_callbacks::success),
			 filename);

	    // Note that we use a keepalive slot to ensure that the
	    // callback object doesn't get deleted before the thunk
	    // fires off!  We need to do this because the last
	    // reference to the callback could be dropped from any
	    // thread.
	    it->second(make_keepalive_slot(success_slot, it->first));
	  }
      }

      /** \brief Invoke the failure callback on each listener. */
      void invoke_failure(const std::string &msg) const
      {
	for(std::list<listener>::const_iterator
	      it = listeners.begin(); it != listeners.end(); ++it)
	  {
	    sigc::slot<void> failure_slot =
	      sigc::bind(sigc::mem_fun(*it->first, &download_callbacks::failure),
			 msg);

	    // Note that we use a keepalive slot to ensure that the
	    // callback object doesn't get deleted before the thunk
	    // fires off!  We need to do this because the last
	    // reference to the callback could be dropped from any
	    // thread.
	    it->second(make_keepalive_slot(failure_slot, it->first));
	  }
      }

      /** \brief Invoke the partial download callback on each listener. */
      void invoke_partial_download(const temp::name &filename,
				   unsigned long long currentSize,
				   unsigned long long totalSize) const
      {
	for(std::list<listener>::const_iterator
	      it = listeners.begin(); it != listeners.end(); ++it)
	  {
	    sigc::slot<void> partial_download_slot =
	      sigc::bind(sigc::mem_fun(*it->first, &download_callbacks::partial_download),
			 filename, currentSize, totalSize);

	    // Note that we use a keepalive slot to ensure that the
	    // callback object doesn't get deleted before the thunk
	    // fires off!  We need to do this because the last
	    // reference to the callback could be dropped from any
	    // thread.
	    it->second(make_keepalive_slot(partial_download_slot, it->first));
	  }
      }

      /** \brief Invoke the canceled callback on each listener. */
      void invoke_canceled() const
      {
	for(std::list<listener>::const_iterator
	      it = listeners.begin(); it != listeners.end(); ++it)
	  {
	    sigc::slot<void> canceled_slot =
	      sigc::mem_fun(*it->first, &download_callbacks::canceled);

	    // Note that we use a keepalive slot to ensure that the
	    // callback object doesn't get deleted before the thunk
	    // fires off!  We need to do this because the last
	    // reference to the callback could be dropped from any
	    // thread.
	    it->second(make_keepalive_slot(canceled_slot, it->first));
	  }
      }
    };

    // Responsible for downloading a file and informing the listener
    // that it has been downloaded.
    //
    // This is trackable so we can tell from outside whether it's been
    // destroyed yet (and thus removed from its owner's queue).
    class AcqQueuedFile : public pkgAcqFile, public sigc::trackable
    {
      boost::shared_ptr<download_job> job;

      // Deal with failure situations.  Necessary since we want to
      // fall back to cached values if they're available.
      void handle_failure(const std::string &msg)
      {
	if(job->get_cached_filename().valid())
	  {
	    LOG_INFO(Loggers::getAptitudeDownloadQueue(),
		     "Failed to download " << job->get_short_description()
		     << " from the URI " << job->get_uri()
		     << " (" << msg << "), falling back to cached values.");

	    job->invoke_success(job->get_cached_filename());
	  }
	else
	  {
	    LOG_WARN(Loggers::getAptitudeDownloadQueue(),
		     "Failed to download " << job->get_short_description()
		     << " from the URI " << job->get_uri()
		     << ": " << msg);

	    job->invoke_failure(msg);
	  }
      }

    public:
      AcqQueuedFile(pkgAcquire *Owner,
		    const boost::shared_ptr<download_job> &_job)
	: pkgAcqFile(Owner, _job->get_uri(), "", 0,
		     "", _job->get_short_description(), "",
		     _job->get_filename().get_name()),
	  job(_job)
      {
	LOG_TRACE(Loggers::getAptitudeDownloadQueue(),
		  "Setting up a download of " << job->get_short_description());
      }

      ~AcqQueuedFile()
      {
	if(job.get() != NULL)
	  {
	    LOG_WARN(Loggers::getAptitudeDownloadQueue(),
		     "Download of " << job->get_short_description()
		     << " is being prematurely destroyed.");
	    job->invoke_failure("Download queue destroyed.");
	    job.reset();
	  }
      }

      const download_job &get_job() const { return *job; }

      /** \brief Delete this object.
       *
       *  Provided so it can be the target of a slot.
       */
      void destroy()
      {
	delete this;
      }

      void Failed(std::string Message, pkgAcquire::MethodConfig *Cnf)
      {
	pkgAcqFile::Failed(Message, Cnf);

	if(job.get() == NULL)
	  {
	    LOG_TRACE(Loggers::getAptitudeDownloadQueue(),
		      "Not signaling failure on "
		      << DescURI()
		      << ": its job has already been cleared.");
	    return;
	  }

	handle_failure(ErrorText);

	// We have to be careful here because mark_finished() might
	// destroy this object.  Because of that, we can't access
	// "this" after invoking it, so if we want to reset the job
	// pointer, we need to work on a copy.
	boost::shared_ptr<download_job> job_copy(job);
	job.reset();
	job_copy->mark_finished();
      }

      void Done(std::string Message,
		unsigned long long Size,
		std::string CalcHash,
		pkgAcquire::MethodConfig *Cnf)
      {
	pkgAcqFile::Done(Message, Size, CalcHash, Cnf);

	if(job.get() == NULL)
	  {
	    LOG_TRACE(Loggers::getAptitudeDownloadQueue(),
		      "Not handling completion on "
		      << DescURI()
		      << ": its job has already been cleared.");
	    return;
	  }

	if(Status != pkgAcquire::Item::StatDone)
	  {
	    LOG_ERROR(Loggers::getAptitudeDownloadQueue(),
		      boost::format("Failed to fetch %s from the URI %s: %s")
		      % job->get_short_description()
		      % job->get_uri().c_str()
		      % ErrorText.c_str());

	    LOG_ERROR(Loggers::getAptitudeDownloadQueue(),
		      "Failed to download " << job->get_short_description()
		      << ": "
		      << LookupTag(Message, "Message") << "[" << ErrorText << "]");
	  }
	else
	  {
	    const std::string lastModifiedTimeStr = LookupTag(Message, "Last-Modified");
	    time_t lastModifiedTime = 0;
	    if(!RFC1123StrToTime(lastModifiedTimeStr.c_str(), lastModifiedTime))
	      lastModifiedTime = 0;

	    LOG_INFO(Loggers::getAptitudeDownloadQueue(),
		     "Successfully downloaded " << job->get_short_description()
		     << " (size = "
		     << Size << ", last modified time = "
		     << lastModifiedTime << " ["
		     << lastModifiedTimeStr << "]) : "
		     << LookupTag(Message, "Message"));

	    download_cache->putItem(job->get_uri(), job->get_filename().get_name(), lastModifiedTime);
	    job->invoke_success(job->get_filename());
	  }

	// We have to be careful here because mark_finished() might
	// destroy this object.  Because of that, we can't access
	// "this" after invoking it, so if we want to reset the job
	// pointer, we need to work on a copy.
	boost::shared_ptr<download_job> job_copy(job);
	job.reset();
	job_copy->mark_finished();
      }

      std::string Custom600Headers()
      {
	if(job.get() == NULL || job->get_last_modified_time() == 0)
	  return "";
	else
	  return "Last-Modified: " + TimeRFC1123(job->get_last_modified_time());
      }
    };

    /** \brief Tracks information about an active download and a
     *  callback saying how to destroy it.
     */
    class active_download_info
    {
      boost::shared_ptr<download_job> job;
      sigc::slot<void> destroy;

    public:
      active_download_info(const boost::shared_ptr<download_job> &_job,
			   const sigc::slot<void> &_destroy)
	: job(_job),
	  destroy(_destroy)
      {
      }

      const boost::shared_ptr<download_job> &get_job() const { return job; }
      void destroy_item() { destroy(); }
    };

    /** \brief Identifies a single download request; used to allow
     *	requests to be dynamically removed from the queue.
     *
     *  This stores the information we need to hook into the
     *  background thread, find all the associated connections and the
     *  associated queue item, and destroy them.
     *
     *  When a new download is requested, a blank request object is
     *  created and returned immediately.  The request object is then
     *  placed into a queue of requests that should be considered for
     *  addition to the background thread (paired with the job
     *  details).  When the URI is added to the queue or hooked into
     *  an existing download, the request is filled in appropriately
     *  by the background thread.
     *
     *  When the request is canceled, the request object is placed
     *  into a queue of requests to cancel.  On each Pulse() call, the
     *  background thread first processes requests that are being
     *  added to the queue (as noted above), then processes cancel
     *  requests.
     *
     *  The important thing about this protocol is that all the
     *  manipulations of the download queue happen in a single thread;
     *  the frontend routines that are exposed to other modules just
     *  place requests into queues and return.  This is important
     *  because the Acquire code is not thread-safe, so we have to
     *  ensure that it always runs in the same thread.
     */
    class download_request_impl : public download_request,
				  public boost::enable_shared_from_this<download_request_impl>
    {
      boost::weak_ptr<download_job> parent;

      // The handle used to cancel listening to events on this item.
      download_job::listener_connection connection;

      // True if this request has been canceled.  Used to avoid a race
      // condition if something cancels a request before it has been
      // filled in and added to the download queue.
      bool canceled;

    public:
      /** \brief Create an unconnected download request. */
      download_request_impl()
	: canceled(false)
      {
      }

      /** \brief Associate this request with a particular active
       *  download.
       */
      void bind(const boost::shared_ptr<download_job> &_parent,
		download_job::listener_connection _connection)
      {
	parent = _parent;
	connection = _connection;
      }

      /** \brief Actually cancel this request.
       *
       *  To be safe, this must be invoked from the thread that owns
       *  the download objects.
       */
      void do_cancel();

      /** \brief Place this request into the parent queue's cancel
       *  queue.
       */
      void cancel();
    };

    /** \brief Manages a collection of currently-running downloads and
     *  a single background thread in which the downloads run.
     *
     *  A second background thread is used to retrieve URIs from the
     *  download cache.
     */
    class download_thread
    {
      /** \brief Stores a request to start downloading a URI.
       *
       *  One of these is generated when the frontend routine is
       *  invoked.
       */
      class start_request
      {
	std::string uri;
	std::string short_description;
	temp::name filename;
	// A location where the last cached data is stored, or an
	// invalid pointer if there isn't cached data.  (note: it's
	// tempting to only store the last modification time here so
	// as to avoid extracting files unnecessarily -- but if the
	// file expires from the cache between when we check for it
	// the first time and when we check again, you could have
	// trouble)
	temp::name cached_filename;

	boost::shared_ptr<download_callbacks> callbacks;
	post_thunk_f post_thunk;

	// When the cached file was last modified, or 0 to not set the
	// last modified time in the HTTP header.  This member is
	// initially 0 and is updated if the file is found in the
	// download cache.
	time_t last_modified_time;

	/** \brief A blank request that should be bound to the new
	 *  download object.
	 */
	boost::shared_ptr<download_request_impl> request;

      public:
	start_request(const std::string &_uri,
		      const std::string &_short_description,
		      const temp::name &_filename,
		      const boost::shared_ptr<download_callbacks> &_callbacks,
		      post_thunk_f _post_thunk,
		      const boost::shared_ptr<download_request_impl> &_request)
	  : uri(_uri),
	    short_description(_short_description),
	    filename(_filename),
	    callbacks(_callbacks),
	    post_thunk(_post_thunk),
	    last_modified_time(0),
	    request(_request)
	{
	}

	const std::string &get_uri() const { return uri; }
	const std::string &get_short_description() const { return short_description; }
	const temp::name &get_filename() const { return filename; }
	const temp::name &get_cached_filename() const { return cached_filename; }
	time_t get_last_modified_time() const { return last_modified_time; }
	const boost::shared_ptr<download_callbacks> &get_callbacks() const { return callbacks; }
	post_thunk_f get_post_thunk() const { return post_thunk; }
	const boost::shared_ptr<download_request_impl> &get_request() const { return request; }

	void update_from_cache(const temp::name &new_filename,
			       time_t new_last_modified_time)
	{
	  cached_filename = new_filename;
	  last_modified_time = new_last_modified_time;
	}
      };

      /** \brief A background thread that looks up files in the cache.
       *
       *  Requests are passed along to the main download thread after
       *  this thread finishes with them.
       *
       *  If a file is found, the request object is updated with its
       *  last modified time.
       */
      class cache_lookup_thread :
	public util::job_queue_thread<cache_lookup_thread,
				      boost::shared_ptr<start_request> >
      {
	static bool signals_connected;
      public:
	static logging::LoggerPtr get_log_category()
	{
	  return Loggers::getAptitudeDownloadQueueCache();
	}

	cache_lookup_thread()
	{
	  // Since the download cache goes away when the apt cache is
	  // closed, we need to stop working while that's happening.
	  if(!signals_connected)
	    {
	      cache_closed.connect(sigc::ptr_fun(&cache_lookup_thread::stop));
	      cache_reloaded.connect(sigc::ptr_fun(&cache_lookup_thread::start));
	      signals_connected = true;
	    }
	}

	void process_job(const boost::shared_ptr<start_request> &job)
	{
	  if(download_cache)
	    {
	      time_t mtime;
	      temp::name filename =
		download_cache->getItem(job->get_uri(), mtime);
	      if(filename.valid())
		job->update_from_cache(filename, mtime);
	    }

	  download_thread::queue_job(job);
	}
      };

      /** \brief Hook into the download process; used to add new
       *  downloads into the Acquire object.
       */
      class download_callback : public pkgAcquireStatus
      {
	// Invoked when the cached item for a job is confirmed to be
	// up-to-date.
	void IMSHit(pkgAcquire::ItemDesc &item)
	{
	  // apt doesn't invoke any methods on the item itself in this
	  // case, so we have to signal the hit manually.
	  cw::threads::mutex::lock l(state_mutex);

	  boost::unordered_map<std::string, boost::shared_ptr<active_download_info> >::iterator
	    found = active_downloads.find(item.URI);
	  if(found != active_downloads.end())
	    {
	      temp::name cached_filename =
		found->second->get_job()->get_cached_filename();
	      found->second->get_job()->invoke_success(cached_filename);
	    }
	}

	bool Pulse(pkgAcquire *Owner)
	{
	  cw::threads::mutex::lock l(state_mutex);

	  if(shutdown_queue)
	    {
	      LOG_INFO(Loggers::getAptitudeDownloadQueue(),
		       "The download queue is shutting down: stopping the background download.");
	      return false;
	    }

	  for(std::deque<boost::shared_ptr<start_request> >::const_iterator it =
		start_requests.begin();
	      it != start_requests.end(); ++it)
	    {
	      const start_request &req(**it);

	      process_start_request(req, *Owner);
	    }
	  start_requests.clear();

	  for(std::deque<boost::shared_ptr<download_request_impl> >::const_iterator it =
		cancel_requests.begin(); it != cancel_requests.end(); ++it)
	    {
	      (*it)->do_cancel();
	    }
	  cancel_requests.clear();

	  for(pkgAcquire::Worker *w = Owner->WorkersBegin();
	      w != NULL; w = Owner->WorkerStep(w))
	    {
	      if(w->CurrentItem == NULL)
		continue;

	      const std::string &uri = w->CurrentItem->URI;
	      boost::unordered_map<std::string, boost::shared_ptr<active_download_info> >::iterator
		found = active_downloads.find(uri);

	      if(found != active_downloads.end())
		{
		  const download_job &job = *found->second->get_job();

		  LOG_TRACE(Loggers::getAptitudeDownloadQueue(),
			    "Noting a partial download of "
			    << found->first << " to " << job.get_filename().get_name()
			    << " (" << w->CurrentSize << " of "
			    << w->TotalSize << " bytes)");

		  job.invoke_partial_download(job.get_filename(),
					      w->CurrentSize,
					      w->TotalSize);
		}
	    }

	  return true;
	}

	bool MediaChange(std::string, std::string)
	{
	  // Media changes will always abort.
	  return false;
	}
      };

      // All these members are static because they are used by the
      // singleton download thread, but might need to be populated
      // before it starts processing items.

      // A mutex that serializes access to all of the state below.
      static cw::threads::mutex state_mutex;

      // A queue of requests for the background thread to start
      // downloading URIs.
      static std::deque<boost::shared_ptr<start_request> > start_requests;

      // A queue of requests for the background thread to stop
      // downloading URIs.
      static std::deque<boost::shared_ptr<download_request_impl> > cancel_requests;

      // Tracks the active downloads, if any, for various URIs.
      static boost::unordered_map<std::string, boost::shared_ptr<active_download_info> > active_downloads;

      // The single instance of this object (or NULL if there is no
      // active thread).
      //
      // The thread itself isn't stored because we never need to
      // join() it: it's perfectly safe for it to keep downloading
      // while the cache is closed, for instance.
      static boost::shared_ptr<download_thread> instance;

      // Similarly, if the instance is still running, this is a thread
      // object for the thread it's running in.  Users of this member
      // should make a strong copy while holding the state lock, since
      // the background thread clears it upon exit.
      static boost::shared_ptr<cw::threads::thread> instancet;

      // Set to true to cancel any pending downloads in preparation
      // for shutting down the program.
      //
      // This doesn't attempt to leave things in any sort of "nice"
      // state; it just asks the Acquire process to stop, nukes
      // everything in all the queues, and refuses to start a new
      // queue runner.
      static bool shutdown_queue;

      /** \brief Used to invoke the download queue's run() from the
       *  background thread.
       *
       *  Note that because this object lives as long as operator()()
       *  is executing, we know that the instance won't be destroyed
       *  until run() completes.
       */
      class bootstrap
      {
	boost::shared_ptr<download_thread> instance;

      public:
	bootstrap(const boost::shared_ptr<download_thread> &_instance)
	  : instance(_instance)
	{
	}

	void operator()() const
	{
	  instance->run();
	}
      };

      /** \brief Start the background thread if it isn't running. */
      static void ensure_background_thread()
      {
	cw::threads::mutex::lock l(state_mutex);

	if(instance.get() == NULL)
	  {
	    instance = boost::make_shared<download_thread>();
	    instancet = boost::make_shared<cw::threads::thread>(bootstrap(instance));
	  }
      }

      /** \brief Insert a job into the list of jobs to add.
       *
       *  By the time it gets here, the job has been preprocessed to
       *  check whether the URI it references is in the download
       *  cache.
       */
      static void queue_job(const boost::shared_ptr<start_request> &job)
      {
	cw::threads::mutex::lock l(state_mutex);

	start_requests.push_back(job);
	ensure_background_thread();
      }

      /** \brief Actually process a start request and add it to the
       *  Acquire queue.
       *
       *  This creates a new download item, sets up the appropriate
       *  callbacks, and inserts the item into the set of active
       *  downloads.
       */
      static void process_start_request(const start_request &req,
					pkgAcquire &acquireQueue)
      {
	cw::threads::mutex::lock l(state_mutex);

	LOG_TRACE(Loggers::getAptitudeDownloadQueue(),
		  "Creating a new download item for " << req.get_uri());

	boost::shared_ptr<download_job> job =
	  boost::make_shared<download_job>(req.get_uri(),
					   req.get_short_description(),
					   req.get_filename(),
					   req.get_cached_filename(),
					   req.get_last_modified_time());

	// The next couple lines are only safe because we're
	// holding a lock (otherwise someone could sneak in and
	// delete the item in between them).
	AcqQueuedFile *item = new AcqQueuedFile(&acquireQueue, job);

	boost::shared_ptr<active_download_info> download =
	  boost::make_shared<active_download_info>(job, sigc::mem_fun(*item, &AcqQueuedFile::destroy));

	active_downloads[req.get_uri()] = download;

	req.get_request()->bind(job,
				job->add_listener(req.get_callbacks(),
						  req.get_post_thunk()));
      }

    public:
      download_thread()
      {
      }

      // The main frontend routine.
      static boost::shared_ptr<download_request>
      start_download_job(const std::string &uri,
			 const std::string &short_description,
			 const boost::shared_ptr<download_callbacks> &callbacks,
			 post_thunk_f post_thunk)
      {
	cw::threads::mutex::lock l(state_mutex);

	boost::shared_ptr<download_request_impl> rval =
	  boost::make_shared<download_request_impl>();

	if(shutdown_queue)
	  {
	    LOG_WARN(Loggers::getAptitudeDownloadQueue(),
		     "Not starting a job to download " << uri << ": the queue is shut down.");
	    // Note that a newly instantiated request object is safe
	    // to return: canceling it will just be a NOP.
	    return rval;
	  }

	boost::shared_ptr<start_request> start =
	  boost::make_shared<start_request>(uri, short_description,
					    temp::name("aptitudeDownload"),
					    callbacks,
					    post_thunk,
					    rval);

	cache_lookup_thread::add_job(start);

	ensure_background_thread();

	return rval;
      }

      /** \brief Insert a request to cancel the given
       *  job into the cancel queue.
       */
      static void cancel_job(const boost::shared_ptr<download_request_impl> &req)
      {
	cw::threads::mutex::lock l(state_mutex);

	if(shutdown_queue)
	  {
	    LOG_WARN(Loggers::getAptitudeDownloadQueue(),
		     "Not canceling a job: the queue is shut down.");
	    return;
	  }

	cancel_requests.push_back(req);

	ensure_background_thread();
      }

      /** \brief Shut down the background thread and clear its data
       *  structures; used to abort all processing when the program is
       *  terminating.
       *
       *  We need to do this because otherwise, the objects in the
       *  queue might be destroyed when global destructors are called,
       *  and some of them access things that might be destroyed at
       *  the same time. (log4cxx used to be a problem here; I haven't
       *  checked whether anything else is.  Deterministic destruction
       *  is a good idea anyway, though)
       */
      static void shutdown()
      {
	cw::threads::mutex::lock l(state_mutex);

	LOG_INFO(Loggers::getAptitudeDownloadQueue(),
		 "Shutting down the download queue.");

	shutdown_queue = true;

	if(instance.get() != NULL)
	  {
	    LOG_TRACE(Loggers::getAptitudeDownloadQueue(),
		      "Waiting for the background download thread to terminate.");
	    // Take a strong copy in case the instance thread is
	    // destroyed while we're working on it.
	    boost::shared_ptr<cw::threads::thread> instancet_copy(instancet);

	    l.release();
	    instancet_copy->join();
	    l.acquire();

	    LOG_TRACE(Loggers::getAptitudeDownloadQueue(),
		      "The background thread has exited.");
	  }

	LOG_TRACE(Loggers::getAptitudeDownloadQueue(),
		  "Clearing the start request list.");
	start_requests.clear();

	LOG_TRACE(Loggers::getAptitudeDownloadQueue(),
		  "Clearing the cancel request list.");
	cancel_requests.clear();

	LOG_TRACE(Loggers::getAptitudeDownloadQueue(),
		  "Clearing the active download map.");
	active_downloads.clear();

	LOG_INFO(Loggers::getAptitudeDownloadQueue(),
		 "The download queue is now shut down.");
      }

      /** \brief Stop any download for the given URI.
       *
       *  Use by do_cancel() to remove jobs with no listeners from the
       *  download queue.  Must run in the background thread.
       */
      static void remove_job_by_uri(const std::string &uri)
      {
	cw::threads::mutex::lock l(state_mutex);

	if(shutdown_queue)
	  {
	    LOG_WARN(Loggers::getAptitudeDownloadQueue(),
		     "Not removing the job that downloads " << uri
		     << ": the queue is shut down.");
	    return;
	  }

	boost::unordered_map<std::string, boost::shared_ptr<active_download_info> >::iterator
	  found = active_downloads.find(uri);

	if(found != active_downloads.end())
	  {
	    found->second->destroy_item();
	    active_downloads.erase(found);
	  }
      }

      void run()
      {
	cw::threads::mutex::lock l(state_mutex);

	LOG_TRACE(Loggers::getAptitudeDownloadQueue(), "Background download queue starting.");

	while(!start_requests.empty() && !shutdown_queue)
	  {
	    LOG_TRACE(Loggers::getAptitudeDownloadQueue(),
		      "Setting up the download process for the background download queue.");

	    download_callback cb;
	    pkgAcquire downloader;
            downloader.Setup(&cb);

	    for(std::deque<boost::shared_ptr<start_request> >::const_iterator it =
		  start_requests.begin();
		it != start_requests.end(); ++it)
	      {
		const start_request &request = **it;

		process_start_request(request, downloader);
	      }

	    start_requests.clear();

	    LOG_TRACE(Loggers::getAptitudeDownloadQueue(),
		      "Running the current download queue.");

	    l.release();

	    downloader.Run();

	    l.acquire();
	  }

	LOG_TRACE(Loggers::getAptitudeDownloadQueue(),
		  "No more download start requests, shutting down thread.");

	// If we finished running and there were no more start
	// requests, any cancel requests hanging out in the queue are
	// useless.
	cancel_requests.clear();

	instance.reset();
	instancet.reset();
      }
    };

    bool download_thread::cache_lookup_thread::signals_connected = false;

    cw::threads::mutex download_thread::state_mutex((cw::threads::mutex::attr(PTHREAD_MUTEX_RECURSIVE)));

    std::deque<boost::shared_ptr<download_thread::start_request> > download_thread::start_requests;

    std::deque<boost::shared_ptr<download_request_impl> > download_thread::cancel_requests;

    boost::unordered_map<std::string, boost::shared_ptr<active_download_info> > download_thread::active_downloads;

    bool download_thread::shutdown_queue = false;

    boost::shared_ptr<download_thread> download_thread::instance;
    boost::shared_ptr<cw::threads::thread> download_thread::instancet;


    void download_job::mark_finished()
    {
      // Note that remove_job_by_uri() could trigger destroying this
      // object, so we need to ensure that it's the last thing we
      // invoke.
      listeners.clear();
      download_thread::remove_job_by_uri(uri);
    }


    void download_request_impl::cancel()
    {
      download_thread::cancel_job(shared_from_this());
    }

    void download_request_impl::do_cancel()
    {
      // It's important to note that this runs in the download thread,
      // so manipulating its structures (e.g., disconnecting signals)
      // is OK.
      if(canceled)
	return;


      boost::shared_ptr<download_job> job(parent.lock());
      if(job.get() != NULL)
	{
	  job->remove_listener(connection);
	  if(job->listeners_empty())
	    download_thread::remove_job_by_uri(job->get_uri());
	  job->invoke_canceled();
	}

      canceled = true;
    }
  }

  download_request::~download_request()
  {
  }

  boost::shared_ptr<download_request>
  queue_download(const std::string &uri,
		 const std::string &short_description,
		 const boost::shared_ptr<download_callbacks> &callbacks,
		 post_thunk_f post_thunk)
  {
    return download_thread::start_download_job(uri, short_description,
					       callbacks, post_thunk);
  }

  void shutdown_download_queue()
  {
    download_thread::shutdown();
  }
}
