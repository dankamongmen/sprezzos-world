// download_signal_log.cc
//
//   Copyright (C) 2001, 2005, 2008 Daniel Burrows
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

#include "download_signal_log.h"

#include <sigc++/adaptors/bind.h>

download_signal_log::download_signal_log()
{
}

download_signal_log::~download_signal_log()
{
}

/** If out is 1, do nothing; otherwise, set out to either 1 or 0,
 *  depending on whether val is \b true or \b false.
 */
static void set_bool(bool val, int *out)
{
  if(*out == 1)
    return;

  *out = val ? 1 : 0;
}

void download_signal_log::Fetched(unsigned long long Size, unsigned long long ResumePoint)
{
  pkgAcquireStatus::Fetched(Size, ResumePoint);

  Fetched_sig(Size, ResumePoint, *this);
}

void download_signal_log::MediaChange(const string &Media, const string &Drive,
				   const sigc::slot1<void, bool> &k)
{
  MediaChange_sig(Media, Drive, *this, k);
}

bool download_signal_log::MediaChange(string Media, string Drive)
{
  int rval = -1;

  MediaChange(Media, Drive, sigc::bind(sigc::ptr_fun(set_bool), &rval));

  // Sanity-check against broken slots.  Something ought to have
  // called this.
  if(!MediaChange_sig.empty())
    eassert(rval != -1);
  return rval != 0;
}

void download_signal_log::IMSHit(pkgAcquire::ItemDesc &item)
{
  IMSHit_sig(item, *this);
}

void download_signal_log::Fetch(pkgAcquire::ItemDesc &item)
{
  Fetch_sig(item, *this);
}

void download_signal_log::Done(pkgAcquire::ItemDesc &item)
{
  Done_sig(item, *this);
}

void download_signal_log::Fail(pkgAcquire::ItemDesc &item)
{
  Fail_sig(item, *this);
}

void download_signal_log::Pulse(pkgAcquire *Owner,
			     const sigc::slot1<void, bool> &k)
{
  pkgAcquireStatus::Pulse(Owner);

  Pulse_sig(Owner, *this, k);
}

bool download_signal_log::Pulse(pkgAcquire *Owner)
{
  int rval = -1;

  Pulse(Owner, sigc::bind(sigc::ptr_fun(set_bool), &rval));

  if(!Pulse_sig.empty())
    eassert(rval != -1);
  return rval != 0;
}

void download_signal_log::Start()
{
  pkgAcquireStatus::Start();

  Start_sig(*this);
}

void download_signal_log::Stop(const sigc::slot0<void> &k)
{
  pkgAcquireStatus::Stop();

  Stop_sig(*this, k);
}

static void nop()
{
}

void download_signal_log::Stop()
{
  Stop(sigc::ptr_fun(nop));
}

void download_signal_log::Complete()
{
  Complete_sig(*this);
}
