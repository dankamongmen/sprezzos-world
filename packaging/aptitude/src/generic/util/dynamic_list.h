/** \file dynamic_list.h */   // -*-c++-*-

#ifndef TOPLEVEL_DYNAMIC_LIST_H
#define TOPLEVEL_DYNAMIC_LIST_H

#include <boost/shared_ptr.hpp>

#include <sigc++/signal.h>

namespace aptitude
{
  namespace util
  {
    /** \brief An abstract interface for a collection of objects that
     *  reports changes via signals.
     *
     *  This interface is read-only because it supports both actual
     *  lists, and synthetic views into lists for which modification
     *  is poorly defined.  If clients should be able to modify a list
     *  themselves, use writable_dynamic_list.
     *
     *  In addition to the usual requirements of containers, T must
     *  provide equality comparison and a hash function.
     *
     *  The advantages relative to TreeModel are that this is a simpler
     *  and narrower interface, and that it doesn't require GTK+.
     */
    template<typename T>
    class dynamic_list : public sigc::trackable
    {
    public:
      virtual ~dynamic_list();

      /** \brief Get the number of elements in this list. */
      virtual std::size_t size() = 0;

      /** \brief Retrieve the element at the given position in the
       *  list.
       *
       *  The position must be greater than or equal to zero, and
       *  strictly less than size().
       */
      virtual T get_at(std::size_t position) = 0;


      /** \brief Signals */
      // @{

      /** \brief Emitted after a value is added to the list.
       *
       *  The size_t parameter is the index of the new element.  The
       *  index of each element formerly at or above the new element's
       *  index has been incremented by one.
       */
      sigc::signal<void, T, std::size_t> signal_inserted;

      /** \brief Emitted after a value is removed from the list.
       *
       *  The parameters are the item, and its former position within
       *  the list.
       */
      sigc::signal<void, T, std::size_t> signal_removed;

      /** \brief Emitted when a value is moved from one location
       *  in the list to another.
       *
       *  This is provided because, for instance, moving a tab around
       *  in the list might mean something different from closing it
       *  and reopening it.  (we shouldn't trigger events that happen
       *  on close, the tab object shouldn't be destroyed, etc)
       *
       *  The size_t parameters are the previous index of the object
       *  and its new index.
       */
      sigc::signal<void, T, std::size_t, std::size_t> signal_moved;

      // @}
    };

    /** \brief An abstract description of a dynamic collection of
     *  objects that allows client code to insert and remove elements.
     */
    template<typename T>
    class writable_dynamic_list : public dynamic_list<T>
    {
    public:
      /** \brief Add a value to this list at the given position.
       *
       *  \param position The index of the new element (0 to insert at
       *  the front, size() to insert at the back).
       *
       *  signal_inserted is invoked after the value is inserted.
       */
      virtual void insert(const T &value, std::size_t position) = 0;

      /** \brief Remove a value from this list.
       *
       *  signal_removed() is invoked after the object is removed.
       */
      virtual void remove(std::size_t position) = 0;

      /** \brief Move an object to a new location.
       *
       *  from and to must be integers between 0 and size() - 1,
       *  inclusive.
       *
       *  If from == to, nothing happens and signal_moved is NOT
       *  emitted.
       *
       *  If from < to, then the intervening elements (between from+1
       *  and to, inclusive) are moved towards the beginning of the
       *  list to compensate.
       *
       *  If from > to, then the intervening elements (between to and
       *  from-1, inclusive) are moved towards the end of the list to
       *  compensate.
       *
       *  signal_moved is invoked after the element is moved, if from
       *  != to.
       */
      virtual void move(std::size_t from, std::size_t to) = 0;
    };

    template<typename T>
    dynamic_list<T>::~dynamic_list()
    {
    }
  }
}

#endif // TOPLEVEL_DYNAMIC_LIST_H
