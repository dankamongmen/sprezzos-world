// ui_download_manager.cc
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

#include "ui_download_manager.h"

#include "aptitude.h"
#include "ui.h"
#include "progress.h"

#include <generic/apt/apt.h>
#include <generic/apt/download_manager.h>
#include <generic/apt/download_signal_log.h>

#include <sigc++/functors/mem_fun.h>

#include <cwidget/widgets/widget.h> // For cw::widget_ref

namespace cw = cwidget;
namespace cwidget
{
  using namespace widgets;
}

ui_download_manager::~ui_download_manager()
{
  log->Complete();

  download_stops();

  abort_state.abort();

  delete log;
  delete st;
}

void ui_download_manager::done(download_thread *t, pkgAcquire::RunResult res)
{
  delete t;

  progress_with_destructor pair = make_progress_bar();
  done_progress = pair.first;
  done_progress_destructor = pair.second;

  if(!abort_state.get_aborted())
    {
      manager->finish(res, done_progress.unsafe_get_ref(),
		      sigc::mem_fun(*this, &ui_download_manager::finish_done));
      return;
    }
  else
    {
      finish_done(download_manager::failure);
      return;
    }
}

void ui_download_manager::finish_done(download_manager::result run_res)
{
  apt_load_cache(done_progress.unsafe_get_ref(), true);

  done_progress_destructor();
  done_progress = NULL;

  if(run_res == download_manager::do_again && !abort_state.get_aborted())
    {
      sigc::slot2<void, download_thread *, pkgAcquire::RunResult>
	done_slot = sigc::mem_fun(this, &ui_download_manager::done);
      (new download_thread(manager,
			   post_thunk,
			   make_safe_slot(done_slot)))->start();
    }
  else
    {
      download_complete(run_res == download_manager::success);
      delete this;
    }
}

void ui_download_manager::start()
{
  download_starts();

  progress_with_destructor pair = make_progress_bar();
  refcounted_progress_ref p = pair.first;
  sigc::slot0<void> p_destructor = pair.second;

  bool ok = manager->prepare(*p.unsafe_get_ref(), *st, log);

  p_destructor();

  if(ok)
    {
      sigc::slot2<void, download_thread *, pkgAcquire::RunResult>
	done_slot = sigc::mem_fun(this, &ui_download_manager::done);
      (new download_thread(manager,
			   post_thunk,
			   make_safe_slot(done_slot)))->start();
    }
  else
    delete this;
}
