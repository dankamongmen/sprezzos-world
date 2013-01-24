/** \file post_event.cc */

// Copyright (C) 2010 Daniel Burrows
// Copyright 2008-2009 Obey Arthur Liu
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

#include "post_event.h"

#include <cwidget/generic/threads/threads.h>

#include <glibmm/dispatcher.h>

#include <deque>

namespace gui
{
  namespace globals
  {
    namespace
    {
      // Code related to injecting events into the main loop:

      // The Glib::dispatch mechanism only allows us to wake the main
      // thread up; it doesn't allow us to pass actual information
      // across the channel.  To hack this together, I borrow the
      // event_queue abstraction from cwidget, and use a dispatcher
      // for the sole purpose of waking the main thread up.
      //
      // I believe that putting sigc++ slots on this list should be
      // OK: from the sigc++ code, it looks like they get passed by
      // value, not by reference.  Once the slot is safely stuck into
      // the list, everything should be OK.

      cwidget::threads::mutex background_events_mutex;
      // Set to "true" if the background events function is currently
      // executing a thunk, in which case we don't need to invoke the
      // dispatcher.  This avoids trouble caused by the thunk invoking
      // post_event() and filling up the queue, so it deadlocks
      // itself.
      bool run_background_events_active = false;
      bool background_events_dispatcher_signalled = false;
      std::deque<safe_slot0<void> > background_events;
      Glib::Dispatcher background_events_dispatcher;

      // Used to ensure that run_background_events_active is true only
      // while the inner portion of the "while" loop below is running.
      class set_bool_in_scope
      {
        bool &target;

      public:
        set_bool_in_scope(bool &_target)
          : target(_target)
        {
          target = true;
        }

        ~set_bool_in_scope()
        {
          target = false;
        }
      };

      void run_background_events()
      {
        cwidget::threads::mutex::lock l(background_events_mutex);
        while(!background_events.empty())
          {
            set_bool_in_scope setter(run_background_events_active);
            safe_slot0<void> f = background_events.front();
            background_events.pop_front();

            l.release();
            f.get_slot()();
            l.acquire();
          }
        background_events_dispatcher_signalled = false;
      }
    }

    // Interface routines to the background event code.
    void post_event(const safe_slot0<void> &event)
    {
      // Ensure that the dispatcher is only invoked once.
      cwidget::threads::mutex::lock l(background_events_mutex);
      background_events.push_back(event);
      if(!run_background_events_active && !background_events_dispatcher_signalled)
        {
          background_events_dispatcher();
          background_events_dispatcher_signalled = true;
        }
    }

    void post_thunk(const sigc::slot<void> &thunk)
    {
      post_event(make_safe_slot(thunk));
    }

    void init_post_event()
    {
      // Defensive programming: ensure that there's only one
      // connection even if we're called multiple times.
      static sigc::connection background_events_connection;

      background_events_connection.disconnect();
      background_events_connection =
        background_events_dispatcher.connect(sigc::ptr_fun(&run_background_events));
    }
  }
}
