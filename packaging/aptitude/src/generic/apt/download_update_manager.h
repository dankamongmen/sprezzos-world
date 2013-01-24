// download_update_manager.h                   -*-c++-*-
//
//   Copyright (C) 2005, 2008, 2010 Daniel Burrows
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

#ifndef DOWNLOAD_UPDATE_MANAGER_H
#define DOWNLOAD_UPDATE_MANAGER_H

#include "download_manager.h"

#include <apt-pkg/sourcelist.h>

#include <sigc++/signal.h>

/** \file download_update_manager.h
 */

class download_signal_log;

/** A class that handles updating the package lists. */
class download_update_manager : public download_manager
{
  /** A signal log object upon which Complete() should be called. */
  download_signal_log *log;

  pkgSourceList src_list;

public:
  /** Create a new manager.  Note that acqlog and signallog may or may
   *  not be the same object (for instance, acqlog may be a log object
   *  that runs in a background thread and forwards messages to
   *  signallog in the foreground thread).
   *
   *  \param _cache_progress the progress bar which should monitor the
   *  cache save/reload.  This is /not/ owned by the manager object;
   *  it will not be deleted in the destructor.
   *
   */
  download_update_manager();
  ~download_update_manager();

  /** Set up the update run.  The class does not take ownership of any
   *  of the pointers passed to this method.
   *
   *  \param progress a progress bar used to display the progress in
   *         saving the cache
   *
   *  \param acqlog a pkgAcqStatus that should be directly invoked by
   *  the download process.
   *
   *  \param signallog a signal log object upon which Complete()
   *  should be called.
   *
   *  \return \b true iff the preparation succeeded.
   */
  bool prepare(OpProgress &progress,
	       pkgAcquireStatus &acqlog,
	       download_signal_log *signallog);

  void finish(pkgAcquire::RunResult res,
	      OpProgress *progress,
	      const sigc::slot1<void, result> &k);

  /** A signal that is invoked after an automatic 'forget new'
   *  operation.
   */
  sigc::signal0<void> post_forget_new_hook;

  /** A signal that is invoked prior to performing an autoclean.
   */
  sigc::signal0<void> pre_autoclean_hook;

  /** A signal that is invoked after performing an autoclean (it is
   *  always invoked once per invocation of pre_autoclean_signal).
   */
  sigc::signal0<void> post_autoclean_hook;
};


#endif
