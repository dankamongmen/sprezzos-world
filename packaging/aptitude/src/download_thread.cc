// download_thread.cc
//
//   Copyright (C) 2005, 2008 Daniel Burrows
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

#include "download_thread.h"

#include <generic/apt/download_manager.h>
#include <generic/apt/download_signal_log.h>

#include <cwidget/toplevel.h>

#include <sigc++/bind.h>
#include <sigc++/slot.h>
#include <sigc++/functors/mem_fun.h>

namespace cw = cwidget;

template<typename RVal>
void background_execute(sigc::slot0<RVal> slot,
			cw::threads::box<RVal> *return_box)
{
  return_box->put(slot());
}

template<>
void background_execute<void>(sigc::slot0<void> slot,
			      cw::threads::box<void> *return_box)
{
  slot();
  return_box->put();
}

// Note: all these do_foreground_execute() overloads are thread-safe
// only because C is download_signal_log and that's not a trackable
// object.  Furthermore, they're safe only under the assumption that
// the signal log is not deleted 

/** Run the given method call in the foreground and return its value. */
template<typename C, typename RVal>
static
RVal do_foreground_execute(C *inst,
			   RVal (C::* fun) (),
			   post_thunk_func post_thunk)
{
  cw::threads::box<RVal> return_box;

  sigc::slot0<void> background_execute_slot =
    sigc::bind(sigc::ptr_fun(&background_execute<RVal>),
	       sigc::mem_fun(inst, fun),
	       &return_box);

  post_thunk(make_safe_slot(background_execute_slot));

  return return_box.take();
}

/** Run the given method call in the foreground and return its value. */
template<typename C, typename RVal, typename Arg0>
static
RVal do_foreground_execute(C *inst,
			   Arg0 arg0,
			   RVal (C::* fun) (Arg0),
			   post_thunk_func post_thunk)
{
  cw::threads::box<RVal> return_box;

  sigc::slot0<void> background_execute_slot =
    sigc::bind(sigc::ptr_fun(&background_execute<RVal>),
	       sigc::bind(sigc::mem_fun(inst, fun), arg0),
	       &return_box);

  post_thunk(make_safe_slot(background_execute_slot));

  return return_box.take();
}

/** Run the given method call in the foreground and return its value. */
template<typename C, typename RVal, typename Arg0, typename Arg1>
static
RVal do_foreground_execute(C *inst,
			   Arg0 arg0,
			   Arg1 arg1,
			   RVal (C::* fun) (Arg0, Arg1),
			   post_thunk_func post_thunk)
{
  cw::threads::box<RVal> return_box;

  sigc::slot0<void> background_execute_slot =
    sigc::bind(sigc::ptr_fun(&background_execute<RVal>),
	       sigc::bind(sigc::mem_fun(inst, fun), arg0, arg1),
	       &return_box);

  post_thunk(make_safe_slot(background_execute_slot));

  return return_box.take();
}

/** Run the given function call in the foreground and return its value. */
template<typename C, typename RVal, typename Arg0, typename Arg1, typename Arg2>
static
RVal do_foreground_execute(C *inst,
			   Arg0 arg0,
			   Arg1 arg1,
			   Arg2 arg2,
			   RVal (C::* fun) (Arg0, Arg1, Arg2),
			   post_thunk_func post_thunk)
{
  cw::threads::box<RVal> return_box;

  sigc::slot0<void> background_execute_slot =
    sigc::bind(sigc::ptr_fun(&background_execute<RVal>),
	       sigc::bind(sigc::mem_fun(inst, fun), arg0, arg1, arg2),
	       &return_box);

  post_thunk(make_safe_slot(background_execute_slot));

  return return_box.take();
}

void background_status::Fetched(unsigned long long Size,
				unsigned long long ResumePoint)
{
  do_foreground_execute(real_status, Size, ResumePoint,
			&download_signal_log::Fetched,
			post_thunk);
}

bool background_status::MediaChange(std::string Media, std::string Drive)
{
  cw::threads::box<bool> return_box;

  do_foreground_execute<download_signal_log,
    void, const std::string &, const std::string &,
    const sigc::slot1<void, bool> &>  (real_status, Media, Drive,
				       sigc::mem_fun(return_box,
						     &cw::threads::box<bool>::put),
				       &download_signal_log::MediaChange,
				       post_thunk);

  return return_box.take();
}

void background_status::IMSHit(pkgAcquire::ItemDesc &item)
{
  do_foreground_execute<download_signal_log, void, pkgAcquire::ItemDesc &>(real_status, item, &download_signal_log::IMSHit, post_thunk);
}

void background_status::Fetch(pkgAcquire::ItemDesc &item)
{
  do_foreground_execute<download_signal_log, void, pkgAcquire::ItemDesc &>(real_status, item, &download_signal_log::Fetch, post_thunk);
}

void background_status::Done(pkgAcquire::ItemDesc &item)
{
  do_foreground_execute<download_signal_log, void, pkgAcquire::ItemDesc &>(real_status, item, &download_signal_log::Done, post_thunk);
}

void background_status::Fail(pkgAcquire::ItemDesc &item)
{
  do_foreground_execute<download_signal_log, void, pkgAcquire::ItemDesc &>(real_status, item, &download_signal_log::Fail, post_thunk);
}

bool background_status::Pulse(pkgAcquire *Owner)
{
  cw::threads::box<bool> return_box;

  do_foreground_execute<download_signal_log, void,
    pkgAcquire *,
    const sigc::slot1<void, bool> &>(real_status, Owner,
				     sigc::mem_fun(&return_box,
						   &cw::threads::box<bool>::put),
				     &download_signal_log::Pulse,
				     post_thunk);

  return return_box.take();
}

void background_status::Start()
{
  do_foreground_execute(real_status, &download_signal_log::Start, post_thunk);
}

void background_status::Stop()
{
  cw::threads::box<void> return_box;

  do_foreground_execute<download_signal_log,
    void,
    const sigc::slot0<void> &>(real_status,
			       sigc::mem_fun(&return_box,
					     &cw::threads::box<void>::put),
			       &download_signal_log::Stop,
			       post_thunk);

  return_box.take();
}

namespace
{
  void do_download_complete(download_thread *t,
			    pkgAcquire::RunResult res,
			    safe_slot2<void, download_thread *, pkgAcquire::RunResult> continuation)
  {
    t->join();
    continuation.get_slot()(t, res);
  }
}

void download_thread::operator()()
{
  pkgAcquire::RunResult res = m->do_download();

  sigc::slot0<void> download_complete_slot =
    sigc::bind(sigc::ptr_fun(&do_download_complete),
	       this, res,
	       continuation);
  post_thunk(make_safe_slot(download_complete_slot));
}
