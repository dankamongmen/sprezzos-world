// download_thread                              -*-c++-*-
//
//   Copyright (C) 2005, 2008-2009 Daniel Burrows
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

#ifndef DOWNLOAD_THREAD_H
#define DOWNLOAD_THREAD_H

#include <aptitude.h>

#include <apt-pkg/acquire.h>
#include <apt-pkg/error.h>

#include <cwidget/generic/threads/threads.h>

#include <generic/util/safe_slot.h>

#include <sigc++/slot.h>

#include <boost/shared_ptr.hpp>

/** \file download_thread.h
 */

class download_manager;
class download_signal_log;

/** \brief The type of a callback that posts a thunk to the
 *  main thread.
 *
 *  We use a function pointer and not a slot to avoid any unexpected
 *  problems with slots and threads.
 */
typedef void (*post_thunk_func)(const safe_slot0<void> &thunk);

/** A proxy status object that posts messages to the main thread.
 *  Each message "blocks" the download until the user deals with it,
 *  just as if it was running in the main thread (I'm not sure
 *  anything else is safe).
 *
 *  This object does NOT own the encapsulated status object and will
 *  NOT delete it.
 */
class background_status : public pkgAcquireStatus
{
  download_signal_log *real_status;
  post_thunk_func post_thunk;

public:
  void Fetched(unsigned long long Size, unsigned long long ResumePoint);
  bool MediaChange(std::string Media, std::string Drive);
  void IMSHit(pkgAcquire::ItemDesc &);
  void Fetch(pkgAcquire::ItemDesc &);
  void Done(pkgAcquire::ItemDesc &);
  void Fail(pkgAcquire::ItemDesc &);
  bool Pulse(pkgAcquire *Owner);
  void Start();
  void Stop();
  void Complete();

  /** \brief Create a background-status object.
   *
   *  This should be created in the foreground thread.
   *
   *  \param _real_status  The status object that will be used to
   *                       broadcast messages in the foreground thread.
   *
   *  \param _post_thunk   How to schedule a thunk for execution in the
   *                       foreground thread.
   */
  background_status(download_signal_log *_real_status,
		    post_thunk_func _post_thunk)
    : real_status(_real_status),
      post_thunk(_post_thunk)
  {
  }
};

/** The thread that performs the download. */
class download_thread
{
  cwidget::threads::box<bool> cancelled;

  /** A callback that posts thunks to be run in the main thread. */
  post_thunk_func post_thunk;

  /** The bundled download_manager object.  It should have been
   *  initialized using a background_status wrapper as above, and you
   *  should join() this thread before deleting it.  (it is also OK
   *  to join() the thread first thing in the object's destructor)
   */
  boost::shared_ptr<download_manager> m;

  /** The continuation of this download, invoked in the main thread
   *  with this thread and the result of the run as parameters.  The
   *  thread will be automatically join()ed before the continuation is
   *  deleted.
   */
  safe_slot2<void, download_thread *, pkgAcquire::RunResult> continuation;

  cwidget::threads::thread *t;

  download_thread(const download_thread &other);
  download_thread &operator=(const download_thread &other);
public:
  download_thread(const boost::shared_ptr<download_manager> &manager,
		  post_thunk_func _post_thunk,
		  const safe_slot2<void, download_thread *, pkgAcquire::RunResult> &_continuation)
    : cancelled(false), post_thunk(_post_thunk),
      m(manager), continuation(_continuation), t(NULL)
  {
  }

  ~download_thread()
  {
    delete t;
  }

  void operator()();

  void start()
  {
    if(t != NULL)
      _error->Error(_("Attempt to start a download thread twice!"));
    else
      t = new cwidget::threads::thread(cwidget::threads::noncopy_bootstrap<download_thread>(*this));
  }

  void join()
  {
    t->join();
  }
};

#endif // DOWNLOAD_THREAD_H
