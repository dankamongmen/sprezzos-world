// cmdline_main_loop.cc
//
// Copyright (C) 2009 Daniel Burrows
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

#include "cmdline_main_loop.h"

#include <boost/make_shared.hpp>
#include <boost/shared_ptr.hpp>

#include <cwidget/generic/threads/threads.h>

#include <generic/util/safe_slot.h>

namespace aptitude
{
  namespace cmdline
  {
    namespace
    {
      class event_loop
      {
	boost::shared_ptr<cwidget::threads::mutex> running_mutex;
	bool running;

	bool exited;

	cwidget::threads::event_queue<safe_slot0<void> > eventq;

	void do_stop()
	{
	  exited = true;
	}

	class cancel_running_on_leave_scope
	{
	  event_loop &parent;
	public:
	  cancel_running_on_leave_scope(event_loop &_parent)
	    : parent(_parent)
	  {
	  }

	  ~cancel_running_on_leave_scope()
	  {
	    cwidget::threads::mutex::lock l(*parent.running_mutex);
	    parent.running = false;
	  }
	};

      public:
	event_loop()
	  : running_mutex(boost::make_shared<cwidget::threads::mutex>()),
	    running(false),
	    exited(false)
	{
	}

	void run()
	{
	  {
	    cwidget::threads::mutex::lock l(*running_mutex);
	    if(running)
	      return;

	    running = true;
	    exited = false;
	  }

	  cancel_running_on_leave_scope canceler(*this);

	  while(!exited)
	    {
	      safe_slot0<void> thunk(eventq.get());
	      thunk.get_slot()();
	    }
	}

	void post(const safe_slot0<void> &thunk)
	{
	  eventq.put(thunk);
	}

	void stop()
	{
	  sigc::slot<void> do_stop_slot(sigc::mem_fun(*this, &event_loop::do_stop));
	  post(make_safe_slot(do_stop_slot));
	}
      };

      event_loop global_event_loop;
    }

    void main_loop()
    {
      global_event_loop.run();
    }

    void exit_main()
    {
      global_event_loop.stop();
    }

    void post_thunk(const sigc::slot<void> &thunk)
    {
      global_event_loop.post(make_safe_slot(thunk));
    }
  }
}
