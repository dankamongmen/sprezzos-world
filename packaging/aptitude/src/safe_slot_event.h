// safe_slot_event.h               -*-c++-*-
//
//   Copyright (C) 2008 Daniel Burrows
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

#ifndef SAFE_SLOT_EVENT_H
#define SAFE_SLOT_EVENT_H

#include <generic/util/safe_slot.h>
#include <cwidget/toplevel.h>

namespace aptitude
{
  class safe_slot_event : public cwidget::toplevel::event
  {
    safe_slot0<void> slot;

  public:
    safe_slot_event(const safe_slot0<void> &_slot)
      : slot(_slot)
    {
    }

    void dispatch()
    {
      slot.get_slot()();
    }
  };
}

#endif // SAFE_SLOT_EVENT_H
