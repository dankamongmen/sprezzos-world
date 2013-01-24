// safe_slot.h                                   -*-c++-*-
//
//   Copyright (C) 2008-2009 Daniel Burrows
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

#ifndef SAFE_SLOT_H
#define SAFE_SLOT_H

#include <sigc++/bind.h>
#include <sigc++/slot.h>

#include <cwidget/generic/threads/threads.h>

/** \file safe_slot.h
 *
 *  This file defines "thread-safe" slot-like objects.  I place that
 *  in quote because these slots can \e not be invoked in a
 *  thread-safe way: they suffer from the usual slot caveats (for
 *  instance, if an object mentioned in the slot is deleted while the
 *  slot is executing, the results are unpredictable).  However, the
 *  slots may be freely copied without creating thread-related
 *  problems, as long as the last copy of a slot is not thrown away in
 *  any thread but one where it's safe to invoke the slot.
 */

/** \brief The class used in the background to implement safe slots.
 *
 *  This creates a slot on the heap that's reference-counted in a
 *  thread-safe way; instances of the class are actually
 *  reference-counting handles.  Be careful not to create reference
 *  cycles!
 *
 *  \typeparam T    The slot type to wrap.
 */
template<typename T>
class safe_slot_wrapper
{
  class implementation
  {
    T slot;

    int refcount;

    cwidget::threads::mutex m;

  public:
    explicit implementation(const T &_slot)
      : slot(_slot),
	refcount(0)
    {
    }

    void disconnect()
    {
      slot.disconnect();
    }

    const T &get_slot() const
    {
      return slot;
    }

    void incref()
    {
      cwidget::threads::mutex::lock l(m);
      ++refcount;
    }

    void decref()
    {
      int new_count;
      {
	cwidget::threads::mutex::lock l(m);
	--refcount;
	new_count = refcount;
      }

      if(new_count == 0)
	delete this;
    }
  };

  implementation *impl;

public:
  safe_slot_wrapper(const T &_slot)
    : impl(new implementation(_slot))
  {
    impl->incref();
  }

  safe_slot_wrapper()
    : impl(NULL)
  {
  }

  safe_slot_wrapper(const safe_slot_wrapper &other)
    : impl(other.impl)
  {
    if(impl != NULL)
      impl->incref();
  }

  ~safe_slot_wrapper()
  {
    if(impl != NULL)
      impl->decref();
  }

  safe_slot_wrapper &operator=(const safe_slot_wrapper &other)
  {
    implementation *old_impl = impl;
    impl = other.impl;

    if(impl != NULL)
      impl->incref();
    if(old_impl != NULL)
      old_impl->decref();

    return *this;
  }

  void disconnect()
  {
    if(impl != NULL)
      impl->disconnect();
  }

  /** \brief Return the stored slot, or a default-constructed slot if there is none. */
  T get_slot() const
  {
    if(impl != NULL)
      return impl->get_slot();
    else
      return T();
  }
};

template<typename Ret>
class safe_slot0
{
  safe_slot_wrapper<sigc::slot0<Ret> > real_slot;

public:
  safe_slot0()
  {
  }

  explicit safe_slot0(const sigc::slot0<Ret> &slot)
    : real_slot(slot)
  {
  }

  void disconnect() const { real_slot.disconnect(); }

  sigc::slot0<Ret> get_slot() const { return real_slot.get_slot(); }
};

template<typename Ret>
safe_slot0<Ret> make_safe_slot(const sigc::slot0<Ret> &slot)
{
  return safe_slot0<Ret>(slot);
}

template<typename Ret, typename A1>
class safe_slot1
{
  safe_slot_wrapper<sigc::slot1<Ret, A1> > real_slot;

public:
  safe_slot1()
  {
  }

  explicit safe_slot1(const sigc::slot1<Ret, A1> &slot)
    : real_slot(slot)
  {
  }

  void disconnect() { real_slot.disconnect(); }

  sigc::slot1<Ret, A1> get_slot() const { return real_slot.get_slot(); }
};

template<typename Ret, typename A1>
safe_slot1<Ret, A1> make_safe_slot(const sigc::slot1<Ret, A1> &slot)
{
  return safe_slot1<Ret, A1>(slot);
}

template<typename Ret, typename A1>
Ret safe_bind_invoke(safe_slot1<Ret, A1> slot, A1 a1)
{
  return slot.get_slot()(a1);
}

template<typename Ret, typename A1>
safe_slot0<Ret> safe_bind(const safe_slot1<Ret, A1> &slot, A1 a1)
{
  sigc::slot0<Ret> rval =
    sigc::bind(sigc::ptr_fun(&safe_bind_invoke<Ret, A1>),
	       slot, a1);
  return make_safe_slot(rval);
}

template<typename Ret, typename A1, typename A2>
class safe_slot2
{
  safe_slot_wrapper<sigc::slot2<Ret, A1, A2> > real_slot;

public:
  safe_slot2()
  {
  }

  explicit safe_slot2(const sigc::slot2<Ret, A1, A2> &slot)
    : real_slot(slot)
  {
  }

  void disconnect() { real_slot.disconnect(); }

  sigc::slot2<Ret, A1, A2> get_slot() const { return real_slot.get_slot(); }
};

template<typename Ret, typename A1, typename A2>
safe_slot2<Ret, A1, A2> make_safe_slot(const sigc::slot2<Ret, A1, A2> &slot)
{
  return safe_slot2<Ret, A1, A2>(slot);
}

template<typename Ret, typename A1, typename A2>
Ret safe_bind_invoke(safe_slot2<Ret, A1, A2> slot, A1 a1, A2 a2)
{
  return slot.get_slot()(a1, a2);
}

template<typename Ret, typename A1, typename A2>
safe_slot0<Ret> safe_bind(const safe_slot2<Ret, A1, A2> &slot, A1 a1, A2 a2)
{
  sigc::slot0<Ret> rval = sigc::bind(sigc::ptr_fun(&safe_bind_invoke<Ret, A1, A2>),
				     slot, a1, a2);
  return make_safe_slot(rval);
}

template<typename Ret, typename A1, typename A2>
safe_slot1<Ret, A1> safe_bind(const safe_slot2<Ret, A1, A2> &slot, A2 a2)
{
  sigc::slot1<Ret, A1> rval = sigc::bind(sigc::ptr_fun(&safe_bind_invoke<Ret, A1, A2>),
					 slot, a2);
  return make_safe_slot(rval);
}

template<typename Ret, typename A1, typename A2, typename A3>
class safe_slot3
{
  safe_slot_wrapper<sigc::slot3<Ret, A1, A2, A3> > real_slot;

public:
  safe_slot3()
  {
  }

  explicit safe_slot3(const sigc::slot3<Ret, A1, A2, A3> &slot)
    : real_slot(slot)
  {
  }

  void disconnect() { real_slot.disconnect(); }

  sigc::slot3<Ret, A1, A2, A3> get_slot() const { return real_slot.get_slot(); }
};

template<typename Ret, typename A1, typename A2, typename A3>
safe_slot3<Ret, A1, A2, A3> make_safe_slot(const sigc::slot3<Ret, A1, A2, A3> &slot)
{
  return safe_slot3<Ret, A1, A2, A3>(slot);
}

template<typename Ret, typename A1, typename A2, typename A3>
Ret safe_bind_invoke(safe_slot3<Ret, A1, A2, A3> slot, A1 a1, A2 a2, A3 a3)
{
  return slot.get_slot()(a1, a2, a3);
}

template<typename Ret, typename A1, typename A2, typename A3>
safe_slot0<Ret> safe_bind(const safe_slot3<Ret, A1, A2, A3> &slot, A1 a1, A2 a2, A3 a3)
{
  sigc::slot0<Ret> rval = sigc::bind(sigc::ptr_fun(&safe_bind_invoke<Ret, A1, A2, A3>),
				     slot, a1, a2, a3);
  return make_safe_slot(rval);
}

template<typename Ret, typename A1, typename A2, typename A3>
safe_slot1<Ret, A1> safe_bind(const safe_slot3<Ret, A1, A2, A3> &slot, A2 a2, A3 a3)
{
  sigc::slot1<Ret, A1> rval = sigc::bind(sigc::ptr_fun(&safe_bind_invoke<Ret, A1, A2, A3>),
					 slot, a2, a3);

  return make_safe_slot(rval);
}

template<typename Ret, typename A1, typename A2, typename A3>
safe_slot2<Ret, A1, A2> safe_bind(const safe_slot3<Ret, A1, A2, A3> &slot, A3 a3)
{
  sigc::slot2<Ret, A1, A2> rval = sigc::bind(sigc::ptr_fun(&safe_bind_invoke<Ret, A1, A2, A3>),
					     slot, a3);

  return make_safe_slot(rval);
}

// I don't need more than 3 arguments, as it turns out.

#endif // SAFE_SLOT_H
