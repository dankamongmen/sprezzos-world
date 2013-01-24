
// ui_download_manager.h                           -*-c++-*-
//
//   Copyright (C) 2005, 2007-2009 Daniel Burrows
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

#ifndef UI_DOWNLOAD_MANAGER_H
#define UI_DOWNLOAD_MANAGER_H

#include "download_thread.h"

#include <apt-pkg/acquire.h>
#include <apt-pkg/progress.h>

#include <cwidget/generic/util/ref_ptr.h>

#include <generic/apt/download_manager.h>

#include <generic/util/refcounted_base.h>

#include <sigc++/signal.h>
#include <sigc++/trackable.h>

#include <boost/shared_ptr.hpp>

/** \brief  Glue code to go between the UI and the download manager/thread stuff
 * 
 *  \file ui_download_manager.h
 */

class download_signal_log;
class download_thread;
namespace cwidget
{
  namespace widgets
  {
    class widget;
  }
}

/** A base class for objects that provide the OpProgress interface and
 *  that can be refcounted.
 *
 *  Note that this class is not abstract: you can instantiate it to
 *  get a NOP progress object.
 */
class refcounted_progress : public OpProgress,
			    virtual public aptitude::util::refcounted_base_threadsafe
{
protected:
  refcounted_progress()
  {
  }

public:
  static cwidget::util::ref_ptr<refcounted_progress> create()
  {
    return new refcounted_progress;
  }

  static std::pair<cwidget::util::ref_ptr<refcounted_progress>,
		   sigc::slot0<void> >
  make()
  {
    return std::make_pair(create(), sigc::slot0<void>());
  }
};
typedef cwidget::util::ref_ptr<refcounted_progress> refcounted_progress_ref;

/** \brief A progress bar paired with a slot that tells
 *  us how to destroy it.
 */
typedef std::pair<refcounted_progress_ref,
		  sigc::slot0<void> > progress_with_destructor;

/** \brief A slot that creates a progress bar and
 *  tells us how to hide it.
 *
 *  The first returned value is a new progress reporting
 *  object; the second value is a slot to be invoked when
 *  any widget associated with the progress object
 *  should be cleaned up.
 */
typedef sigc::slot0<progress_with_destructor>
make_refcounted_progress_slot;

/** Represents the UI end of a download process.  This object
 *  completely handles its own memory management -- you don't have to
 *  delete it and you shouldn't try.
 */
class ui_download_manager : public sigc::trackable
{
  /** Used to indicate that a download was cancelled. */
  class aborter : public sigc::trackable
  {
    bool aborted;
  public:
    aborter() : aborted(false)
    {
    }

    void abort()
    {
      aborted = true;
    }

    bool get_aborted()
    {
      return aborted;
    }
  };

  boost::shared_ptr<download_manager> manager;

  aborter abort_state;

  download_signal_log *log;

  background_status *st;

  // Used to hide the details of how we make a progress bar.  Invoked
  // every time the ui_download_manager needs a progress bar to do
  // something.
  make_refcounted_progress_slot make_progress_bar;

  // How to post a thunk to the main thread.
  post_thunk_func post_thunk;

  // Wrapper so we can use generic refcounted stuff.
  //
  // All we want is a pointer that will drop a reference on the target
  // when we destroy it.
  //
  // This seems a little icky; it seems like there should be a better
  // way of handling these lifetime issues.
  template<typename T>
  class generic_refcounted : public aptitude::util::refcounted_base_threadsafe
  {
    cwidget::util::ref_ptr<T> ptr;

  public:
    generic_refcounted(const cwidget::util::ref_ptr<T> &_ptr)
      : ptr(_ptr)
    {
    }
  };

  /** Used to keep the download status widget alive until the download
   *  completes.
   */
  cwidget::util::ref_ptr<aptitude::util::refcounted_base_threadsafe> download_status;

  /** \brief A progress object, used to display progrss in done().
   *
   *  This is stored here to handle lifetime issues sensibly: we
   *  create it in done() and free it in finish_done(), and storing
   *  refcounted values in slot bindings is dangerous.
   *
   *  \todo Would it be reasonable to somehow just use two progress
   *  bars instead of passing one around?  The current scheme is the
   *  result of blindly refactoring the old done() and seems likely to
   *  become unmaintainable.
   */
  refcounted_progress_ref done_progress;

  /** A thunk that makes done_progress invisible and prepares
   *  to free its memory.
   */
  sigc::slot0<void> done_progress_destructor;

  void done(download_thread *t, pkgAcquire::RunResult res);

  /** \brief Finishes the work of done() after the post-download
   *  actions of the download manager have run.
   *
   *  \param run_res  The result of download_manager::finish().
   */
  void finish_done(download_manager::result run_res);

public:
  /** \brief Create a ui_download_manager.
   *
   *  \param _manager The download_manager object to wrap.
   *
   *  \param _log     How to report the download status (the
   *                  signals will be invoked in a foreground
   *                  thread).
   *
   *  \param _download_status   An arbitrary object that
   *                            should be kept around until
   *                            the download completes (its
   *                            references will be adjusted
   *                            in the foreground thread).
   *
   *  \param _make_progress_bar  A callback that will be invoked
   *                             whenever the download process
   *                             needs a fresh progress bar.
   *
   *  param _post_thunk          A callback that schedules a
   *                             thunk for execution in the
   *                             foreground thread.
   */
  template<typename T>
  ui_download_manager(const boost::shared_ptr<download_manager> &_manager,
		      download_signal_log *_log,
		      const cwidget::util::ref_ptr<T> &_download_status,
		      const make_refcounted_progress_slot &_make_progress_bar,
		      post_thunk_func _post_thunk)
    : manager(_manager),
      abort_state(),
      log(_log),
      st(new background_status(_log, _post_thunk)),
      make_progress_bar(_make_progress_bar),
      post_thunk(_post_thunk),
      download_status(new generic_refcounted<T>(_download_status))
  {
  }

  ~ui_download_manager();

  /** \brief Invoke this method if the download is cancelled.
   *
   *  This doesn't cancel the download itself, but it changes
   *  the flow-of-control appropriately: for instance, dpkg
   *  won't be invoked for an aborted install run.
   */
  void aborted()
  {
    abort_state.abort();
  }

  void start();

  /** \brief A signal emitted when the download is about to start. */
  sigc::signal<void> download_starts;

  /** \brief A signal emitted when the download finishes,
   *  after Complete() is invoked on the log object.
   */
  sigc::signal<void> download_stops;

  /** \brief A signal emitted when the download and any post-download
   *  actions are finished.
   *
   *  The parameter is \b true if the download was sucessful and \b
   *  false otherwise.  The signal is always emitted in the foreground
   *  thread.
   */
  sigc::signal<void, bool> download_complete;
};


#endif
