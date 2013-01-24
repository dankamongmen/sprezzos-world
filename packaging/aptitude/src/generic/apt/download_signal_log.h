// download_signal_log.h            -*-c++-*-
//
//   Copyright (C) 2001, 2005 Daniel Burrows
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

#ifndef DOWNLOAD_SIGNAL_LOG_H
#define DOWNLOAD_SIGNAL_LOG_H

#include <apt-pkg/acquire.h>

#include <sigc++/signal.h>

#include "aptitude.h"
#include <cwidget/generic/util/ref_ptr.h>

/** \brief A class that sits there emitting signals about the current
 *  download status, and can be queried for specific info about that
 *  status.
 *
 *  \file download_signal_log.h
 */

/** A download-signal log translates AcquireStatus calls into signals.
 *  Typically only one slot will be attached to each signal, but this
 *  allows a degree of separation to be introduced between the
 *  back-end progress object and the higher-level objects that display
 *  the current progress.
 *
 *  The signals emitted by this class can be divided into groups.
 *  With most of the signals, the download class will block until the
 *  signal emission is complete.  However, several signals pass an
 *  extra slot argument called the "continuation"; this should be
 *  called (with the "result", if any, of the signal) when the task
 *  identified by the signal is completed.  The purpose here is to
 *  allow the non-blocking display of prompts such as the Media Change
 *  dialog.
 *
 *  The signals which take a continuation are currently MediaChange,
 *  Pulse, and Stop.  They also provide an implementation that is not
 *  continuation-passing; this version should be used only when no
 *  cross-thread calls are being performed and the connectee of the
 *  signals is known to not defer its response (i.e., it calls the
 *  continuation before returning).  Mainly this means the
 *  command-line download mode.
 */
class download_signal_log : public pkgAcquireStatus
{
public:
  download_signal_log();
  virtual ~download_signal_log();

  struct timeval &get_time() {return Time;}
  struct timeval &get_start_time() {return StartTime;}
  unsigned long long get_last_bytes() {return LastBytes;}
  unsigned long long get_currentCPS() {return CurrentCPS;}
  unsigned long long get_current_bytes() {return CurrentBytes;}
  unsigned long long get_total_bytes() {return TotalBytes;}
  unsigned long long get_fetched_bytes() {return FetchedBytes;}
  unsigned long long get_elapsed_time() {return ElapsedTime;}
  unsigned long get_total_items() {return TotalItems;}
  unsigned long get_current_items() {return CurrentItems;}

  void set_update(bool _Update) {Update=_Update;}

  sigc::signal3<void, unsigned long long, unsigned long long,
		download_signal_log &> Fetched_sig;
  sigc::signal4<void, std::string, std::string,
		download_signal_log &, const sigc::slot1<void, bool> &> MediaChange_sig;
  sigc::signal2<void, pkgAcquire::ItemDesc &, download_signal_log &> IMSHit_sig;
  sigc::signal2<void, pkgAcquire::ItemDesc &, download_signal_log &> Fetch_sig;
  sigc::signal2<void, pkgAcquire::ItemDesc &, download_signal_log &> Done_sig;
  sigc::signal2<void, pkgAcquire::ItemDesc &, download_signal_log &> Fail_sig;
  sigc::signal3<void, pkgAcquire *, download_signal_log &,
		const sigc::slot1<void, bool> &> Pulse_sig;
  sigc::signal1<void, download_signal_log &> Start_sig;
  sigc::signal2<void, download_signal_log &,
		const sigc::slot0<void> &> Stop_sig;
  sigc::signal1<void, download_signal_log &> Complete_sig;

  void Fetched(unsigned long long Size, unsigned long long ResumePoint);
  void MediaChange(const std::string &Media, const std::string &Drive,
		   const sigc::slot1<void, bool> &k);
  bool MediaChange(std::string Media, std::string Drive);
  void IMSHit(pkgAcquire::ItemDesc &item);
  void Fetch(pkgAcquire::ItemDesc &item);
  void Done(pkgAcquire::ItemDesc &item);
  void Fail(pkgAcquire::ItemDesc &item);
  void Pulse(pkgAcquire *Owner, const sigc::slot1<void, bool> &k);
  bool Pulse(pkgAcquire *Owner);
  void Start();
  void Stop(const sigc::slot0<void> &k);
  void Stop();

  // Called when EVERYTHING is over.  "Stop" is not sufficient, since
  // it is potentially called multiple times (eg, for installs spread across
  // several CDs)
  void Complete();
};

#endif
