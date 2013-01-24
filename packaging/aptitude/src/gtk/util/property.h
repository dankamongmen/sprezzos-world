// property.h              -*-c++-*-
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

#ifndef GTK_PROPERTY_H
#define GTK_PROPERTY_H

/** \file property.h
 *
 * Semi-type-safe get/set property templates for GTK-- widgets.  They
 * can't be fully type-safe because you can always try to use a string
 * with a different type, but they should be about as typesafe as
 * possible.
 *
 * Any value that has copy-construction and can be allocated on the
 * heap can be stored in a GTK+ property.  A value of type T is stored
 * by allocating a new value via "new T(x)" and then placing that
 * pointer in a GTK+ property.  If the widget is destroyed, the
 * pointer will be deleted as a pointer to T.
 */

namespace gui
{
  /** \brief A typed property name.
   *
   *  The string used to store the property should never be used
   *  anywhere else.
   */
  template<typename T>
  class property
  {
    Glib::Quark q;

    static void destroy_notify(gpointer p)
    {
      T *t = static_cast<T *>(p);
      delete t;
    }

  public:
    property(const Glib::ustring &name)
      : q(name)
    {
    }

    bool exists_on(Gtk::Widget &w) const
    {
      return w.get_data(q) != NULL;
    }

    bool exists_on(Gtk::Widget *w) const
    {
      return exists_on(*w);
    }

    T get_from(Gtk::Widget &w) const
    {
      gpointer val = w.get_data(q);

      T *t = static_cast<T *>(val);
      return *t;
    }

    T get_from(Gtk::Widget *w) const
    {
      return get_from(*w);
    }

    T get_from(Gtk::Widget &w, T dflt) const
    {
      gpointer val = w.get_data(q);

      if(val == NULL)
	return dflt;
      else
	{
	  T *t = static_cast<T *>(val);
	  return *t;
	}
    }

    T get_from(Gtk::Widget *w, T dflt) const
    {
      return get_from(*w, dflt);
    }

    void set_on(Gtk::Widget &w, T val) const
    {
      gpointer new_val = static_cast<gpointer>(new T(val));
      w.set_data(q, new_val, &property::destroy_notify);
    }

    void set_on(Gtk::Widget *w, T val) const
    {
      set_on(*w, val);
    }
  };
};

#endif
