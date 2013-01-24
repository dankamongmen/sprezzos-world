/** \file post_event.h */     // -*-c++-*-

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

#ifndef POST_EVENT_H
#define POST_EVENT_H

#include <generic/util/safe_slot.h>

namespace gui
{
  namespace globals
  {
    // This code is not in globals.{cc,h] because it's more involved
    // than the other stuff in that file.

    /** \brief Dispatch the given safe thunk to the main loop.
     *
     *  Global because there is only one main loop and IT DOES NOT
     *  SHARE POWER^W^W^W^W^Wit has only one event queue.
     */
    void post_event(const safe_slot0<void> &thunk);

    /** \brief Post an event to the main loop's event queue,
     *  thread-safely.
     *
     *  Global because there is only one main loop and it has only one
     *  event queue.
     */
    void post_thunk(const sigc::slot<void> &thunk);

    /** \brief Initialize support for post_thunk() and
     *  post_event().
     */
    void init_post_event();
  }
}

#endif // POST_EVENT_H
