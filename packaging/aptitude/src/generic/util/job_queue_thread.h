/** \file job_queue.h */    // -*-c++-*-


// Copyright (C) 2009-2010 Daniel Burrows
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

#include <deque>

#include <boost/make_shared.hpp>

#include <cwidget/generic/threads/threads.h>

#include <loggers.h>

namespace aptitude
{
  namespace util
  {
    /** \brief Base class for threads that work by processing a single
     *  job at a time.
     *
     *  \tparam Subclass The class that will be derived from
     *  job_queue.  Must be default-constructable and must define a
     *  static method get_log_category() returning the category under
     *  which messages should be logged.
     *
     *  \tparam Job The type that represents jobs in the queue.  Must
     *  be copy-constructable, default-constructable, and support
     *  output to ostreams via operator<<.
     *
     *  \todo Add support for running up to a fixed number of jobs at
     *  the same time?
     */
    template<typename Subclass, typename Job>
    class job_queue_thread
    {
      // The jobs waiting to be run.
      static std::deque<Job> jobs;

      // The single instance of this class.
      static boost::shared_ptr<job_queue_thread> active_instance;

      // The active thread, or NULL if there isn't one.
      static boost::shared_ptr<cwidget::threads::thread> active_thread;

      // Set to true if the thread is currently stopped.  This causes
      // the job-processing loop to exit and prevents the thread from
      // being started if a new job is added to the queue.
      static bool stopped;

      // This mutex protects accesses to all the static state of the
      // job.
      static cwidget::threads::mutex state_mutex;

      class bootstrap
      {
	boost::shared_ptr<job_queue_thread> target;

      public:
	bootstrap(const boost::shared_ptr<job_queue_thread> &_target)
	  : target(_target)
	{
	}

	void operator()() const
	{
	  target->run();
	}
      };

    public:
      // Instance members first:
      job_queue_thread()
      {
      }

      /** \brief Test whether there are more jobs in the thread's
       *  input queue.
       */
      static bool empty()
      {
	cwidget::threads::mutex::lock l(state_mutex);

	return jobs.empty();
      }

      /** \brief Test whether the queue has been stopped by a call to
       *	stop().
       */
      bool get_stopped()
      {
	cwidget::threads::mutex::lock l(state_mutex);

	return stopped;
      }

      /** \brief Add a new job to the queue of jobs for this thread to
       *  run.
       *
       *  If the background thread isn't stopped, starts it.
       */
      static void add_job(const Job &job)
      {
	cwidget::threads::mutex::lock l(state_mutex);

	LOG_TRACE(Subclass::get_log_category(),
		  "Adding a job to the queue: " << job);

	jobs.push_back(job);

	if(!stopped)
	  start();
      }

      /** \brief Stop the active thread if there is one.
       *
       *  The background thread will only be stopped between jobs.
       *
       *  Blocks until the thread exits.  Until start() is invoked, no
       *  jobs will be processed.
       */
      static void stop()
      {
	cwidget::threads::mutex::lock l(state_mutex);

	LOG_TRACE(Subclass::get_log_category(),
		  "Pausing the background thread.");

	stopped = true;

	// Copy this since it'll be zeroed out when the thread exits,
	// which can happen as soon as the lock is released below.
	boost::shared_ptr<cwidget::threads::thread> active_thread_copy(active_thread);

	l.release();

	if(active_thread_copy.get() != NULL)
	  active_thread_copy->join();
      }

      /** \brief Start the background thread if it has jobs to
       *  process.
       *
       *  Has no effect if there are no jobs or if a thread is already
       *  running.
       */
      static void start()
      {
	cwidget::threads::mutex::lock l(state_mutex);

	stopped = false;

	if(active_thread != NULL)
	  LOG_TRACE(Subclass::get_log_category(),
		    "Not starting the background thread: it's already running.");
	else if(empty())
	  LOG_TRACE(Subclass::get_log_category(),
		    "Not starting the background thread: it has no jobs.");
	else
	  {
	    LOG_TRACE(Subclass::get_log_category(), "Starting the background thread.");

	    active_instance = boost::make_shared<Subclass>();
	    active_thread = boost::make_shared<cwidget::threads::thread>(bootstrap(active_instance));
	  }
      }

      /** \brief Process a single job serially. */
      virtual void process_job(const Job &job) = 0;

    private:
      /** \brief Dequeue and process jobs until the queue is empty or
       *  the thread is stopped.
       */
      void run()
      {
	try
	  {
	    cwidget::threads::mutex::lock l(state_mutex);

	    while(!jobs.empty() && !stopped)
	      {
		Job next(jobs.front());
		jobs.pop_front();

		// Unlock the state mutex, so that jobs can be
		// inserted without blocking while this job is being
		// processed.
		l.release();

		try
		  {
		    process_job(next);
		  }
		catch(const std::exception &ex)
		  {
		    LOG_WARN(Subclass::get_log_category(), "Background thread: got std::exception: " << ex.what());
		  }
		catch(const cwidget::util::Exception &ex)
		  {
		    LOG_WARN(Subclass::get_log_category(), "Background thread: got cwidget::util::Exception: " << ex.errmsg());
		  }
		catch(...)
		  {
		    LOG_WARN(Subclass::get_log_category(), "Background thread: got an unknown exception.");
		  }

		l.acquire();
	      }

	    active_thread.reset();
	    active_instance.reset();
	    return; // Unless there's an unlikely error, we exit here;
	            // otherwise we try some last-chance error
	            // handling below.
	  }
	catch(const std::exception &ex)
	  {
	    LOG_FATAL(Subclass::get_log_category(), "Background thread aborting with std::exception: " << ex.what());
	  }
	catch(const cwidget::util::Exception &ex)
	  {
	    LOG_FATAL(Subclass::get_log_category(), "Background thread aborting with cwidget::util::Exception: " << ex.errmsg());
	  }
	catch(...)
	  {
	    LOG_FATAL(Subclass::get_log_category(), "Background thread aborting with an unknown exception.");
	  }

	// Do what we can to recover, although there might be jobs
	// that can't be processed!
	{
	  cwidget::threads::mutex::lock l(state_mutex);
	  active_thread.reset();
	}
      }
    };

    // Instantiate static members:
    template<typename Subclass, typename Job>
    std::deque<Job> job_queue_thread<Subclass, Job>::jobs;

    template<typename Subclass, typename Job>
    boost::shared_ptr<job_queue_thread<Subclass, Job> > job_queue_thread<Subclass, Job>::active_instance;

    template<typename Subclass, typename Job>
    boost::shared_ptr<cwidget::threads::thread> job_queue_thread<Subclass, Job>::active_thread;

    template<typename Subclass, typename Job>
    bool job_queue_thread<Subclass, Job>::stopped = false;

    template<typename Subclass, typename Job>
    cwidget::threads::mutex job_queue_thread<Subclass, Job>::state_mutex((cwidget::threads::mutex::attr(PTHREAD_MUTEX_RECURSIVE)));
  }
}
