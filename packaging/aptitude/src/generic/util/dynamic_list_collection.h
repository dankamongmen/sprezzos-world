/** \file dynamic_list_collection.h */ // -*-c++-*-

// Copyright (C) 2010 Daniel Burrows
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

#ifndef DYNAMIC_LIST_COLLECTION_H
#define DYNAMIC_LIST_COLLECTION_H

#include "dynamic_list.h"
#include "dynamic_list_impl.h"

#include <boost/functional/hash.hpp>
#include <boost/make_shared.hpp>
#include <boost/multi_index_container.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/multi_index/member.hpp>
#include <boost/multi_index/mem_fun.hpp>
#include <boost/multi_index/random_access_index.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/unordered_map.hpp>

#include <cwidget/generic/util/eassert.h>

#include <sigc++/bind.h>
#include <sigc++/connection.h>

namespace aptitude
{
  namespace util
  {
    /** \brief Assembles multiple dynamic lists into a single dynamic list.
     *
     *  The elements of any single list are placed in order, but the
     *  elements from different lists are interleaved according to the
     *  order in which they were appended.  If an element occurs in
     *  two separate lists, it will be added to this list twice.
     */
    template<typename T>
    class dynamic_list_collection : public dynamic_list<T>
    {
      typedef boost::unordered_multimap<boost::shared_ptr<dynamic_list<T> >,
                                        sigc::connection>
      sub_list_connections;

      /** \brief A dual-purpose collection.  It keeps the contained
       *  lists alive, and holds signal connections that need to be
       *  deleted when each sub-list is removed.
       */
      sub_list_connections connections_by_list;

      class cell
      {
        boost::shared_ptr<dynamic_list<T> > parent_list;
        std::size_t index_within_parent_list;
        T value;

      public:
        cell(const boost::shared_ptr<dynamic_list<T> > &_parent_list,
             std::size_t _index_within_parent_list,
             const T &_value);

        const boost::shared_ptr<dynamic_list<T> > &get_parent_list() const
        {
          return parent_list;
        }

        std::size_t get_index_within_parent_list() const
        {
          return index_within_parent_list;
        }

        const T &get_value() const { return value; }

        bool operator==(const cell &other) const
        {
          return
            parent_list == other.parent_list &&
            index_within_parent_list == other.index_within_parent_list &&
            value == other.value;
        }

        std::size_t hash() const
        {
          std::size_t rval = 1836;
          boost::hash_combine(rval, parent_list);
          boost::hash_combine(rval, index_within_parent_list);
          boost::hash_combine(rval, value);

          return rval;
        }
      };

      // The cells are stored in a multi-indexed container.
      //
      // The indices are:
      //   concrete_view  -> the cells in the order that they appear to
      //                     clients.
      //   by_parent_list -> the cells grouped by their parent list.

      class concrete_view_tag;
      class by_parent_list_tag;

      typedef boost::multi_index_container<
        cell,
        boost::multi_index::indexed_by<
          boost::multi_index::random_access<boost::multi_index::tag<concrete_view_tag> >,
          boost::multi_index::hashed_non_unique<
            boost::multi_index::tag<by_parent_list_tag>,
            boost::multi_index::const_mem_fun<cell,
                                              const boost::shared_ptr<dynamic_list<T> > &,
                                              &cell::get_parent_list> > > >
      cells_collection;

      typedef typename cells_collection::template index<concrete_view_tag>::type concrete_view_index;
      typedef typename cells_collection::template index<by_parent_list_tag>::type by_parent_list_index;

      cells_collection cells;

      void handle_insert(const T &value, std::size_t idx,
                         const boost::shared_ptr<dynamic_list<T> > &list);
      void handle_remove(const T &value, std::size_t idx,
                         const boost::shared_ptr<dynamic_list<T> > &list);
      void handle_move(const T &value, std::size_t from, std::size_t to,
                       const boost::shared_ptr<dynamic_list<T> > &list);

    public:
      // Only public for make_shared.
      dynamic_list_collection();

      std::size_t size();
      T get_at(std::size_t idx);

      void add_list(const boost::shared_ptr<dynamic_list<T> > &lst);
      void remove_list(const boost::shared_ptr<dynamic_list<T> > &lst);

      static boost::shared_ptr<dynamic_list_collection> create();
    };

    template<typename T>
    dynamic_list_collection<T>::cell::cell(const boost::shared_ptr<dynamic_list<T> > &_parent_list,
                                           std::size_t _index_within_parent_list,
                                           const T &_value)
      : parent_list(_parent_list),
        index_within_parent_list(_index_within_parent_list),
        value(_value)
    {
    }

    template<typename T>
    dynamic_list_collection<T>::dynamic_list_collection()
      : cells()
    {
    }

    template<typename T>
    boost::shared_ptr<dynamic_list_collection<T> >
    dynamic_list_collection<T>::create()
    {
      return boost::make_shared<dynamic_list_collection>();
    }

    template<typename T>
    std::size_t dynamic_list_collection<T>::size()
    {
      return cells.size();
    }

    template<typename T>
    T dynamic_list_collection<T>::get_at(std::size_t idx)
    {
      concrete_view_index &concrete_view = cells.template get<concrete_view_tag>();
      return concrete_view[idx].get_value();
    }

    template<typename T>
    void dynamic_list_collection<T>::handle_insert(const T &value, std::size_t idx,
                                                   const boost::shared_ptr<dynamic_list<T> > &list)
    {
      concrete_view_index &concrete_view = cells.template get<concrete_view_tag>();

      // If it was inserted at the end, insert into the end of this
      // list too.
      //
      // If it was inserted at another position, insert into the
      // corresponding slot in this list.


      // The procedure:
      //
      // 1. Grab the cells belonging to this list.
      // 2. Increment cell indices to account for the new entry (all
      //    cells at or above idx will have their within-parent index
      //    incremented).
      //    2.a. When we see the element in the target location, save
      //         its iterator.
      // 3. Insert at the iterator saved in 2.a.
      //
      // Note the reliance on multi_index's iterator stability in 2.a.
      //
      // Note that it's step 2 that forces a linear scan -- if we have
      // to save indices (and we do), then we have to update indices
      // when an item is dropped.  This could be avoided by passing
      // opaque cookies around that are unique over the lifetime of a
      // list to get O(1) operations in all cases...but that's
      // overengineering for lists that have a few dozen entries, max.
      // (I write this note in case it becomes justifiable)

      by_parent_list_index &by_parent_list = cells.template get<by_parent_list_tag>();

      // 1. Find the parent list's cells.
      std::pair<
        typename by_parent_list_index::iterator,
        typename by_parent_list_index::iterator > parent_range =
        by_parent_list.equal_range(list);

      // 2.a. If no insert position exists, insert at end.
      typename concrete_view_index::const_iterator
        insert_location = concrete_view.end();
      for(typename by_parent_list_index::iterator it =
            parent_range.first; it != parent_range.second; ++it)
        {
          const std::size_t it_idx = it->get_index_within_parent_list();

          // 2.a. Save the insert position.
          if(it_idx == idx)
            insert_location = cells.template project<concrete_view_tag>(it);

          // 2. Update the parent-index of each old cell.
          //
          // Note that for the iteration to be correct, I rely on
          // the multi-index guaranteeing that the element isn't
          // reordered if its key doesn't change.  Since only the
          // index changes and that isn't part of the key, we
          // should be OK.
          if(it_idx >= idx)
            by_parent_list.replace(it, cell(it->get_parent_list(),
                                            it_idx + 1,
                                            it->get_value()));
        }

      const std::size_t insert_idx = insert_location - concrete_view.begin();
      concrete_view.insert(insert_location, cell(list, idx, value));
      this->signal_inserted(value, insert_idx);
    }

    template<typename T>
    void dynamic_list_collection<T>::handle_remove(const T &value, std::size_t idx,
                                                   const boost::shared_ptr<dynamic_list<T> > &list)
    {
      concrete_view_index &concrete_view = cells.template get<concrete_view_tag>();

      // The procedure:
      //
      // 1. Grab the cells belonging to this list.
      // 2. Decrement cell indices to account for the removed entry (all
      //    cells above idx will have their within-parent index
      //    decremented).
      //    2.a. When we see the element in the target location, save
      //         that iterator.
      // 3. Remove the iterator saved in 2.a.
      //
      // Note the reliance on multi_index's iterator stability in 2.a.

      by_parent_list_index &by_parent_list = cells.template get<by_parent_list_tag>();

      // 1. Find the parent list's cells.
      std::pair<
        typename by_parent_list_index::iterator,
        typename by_parent_list_index::iterator > parent_range =
        by_parent_list.equal_range(list);

      // 2.a. Initialize the remove location to an invalid value.
      typename concrete_view_index::iterator remove_location =
        concrete_view.end();
      for(typename by_parent_list_index::iterator it =
            parent_range.first; it != parent_range.second; ++it)
        {
          const std::size_t it_idx = it->get_index_within_parent_list();

          // 2.a. Save the insert position.
          if(it_idx == idx)
            remove_location = cells.template project<concrete_view_tag>(it);

          // 2. Update the parent-index of each old cell.
          //
          // Note that for the iteration to be correct, I rely on
          // the multi-index guaranteeing that the element isn't
          // reordered if its key doesn't change.  Since only the
          // index changes and that isn't part of the key, we
          // should be OK.
          if(it_idx > idx)
            by_parent_list.replace(it, cell(it->get_parent_list(),
                                            it_idx - 1,
                                            it->get_value()));
        }

      if(remove_location == concrete_view.end())
        ; // TODO: log an error.
      else
        {
          // 3. Remove the old cell.  Compute the location of the
          // removal BEFORE removing it!
          const std::size_t remove_idx = remove_location - concrete_view.begin();
          concrete_view.erase(remove_location);
          this->signal_removed(value, remove_idx);
        }
    }

    template<typename T>
    void dynamic_list_collection<T>::handle_move(const T &value,
                                                 std::size_t from, std::size_t to,
                                                 const boost::shared_ptr<dynamic_list<T> > &list)
    {
      if(from == to)
        return; // TODO: log a warning?  This should not happen.

      concrete_view_index &concrete_view = cells.template get<concrete_view_tag>();

      // The procedure:
      //
      // 1. Grab the cells belonging to this list.
      // 2. Adjust cell indices to account for the moved entry (all
      //    cells between from and to will be either decremented or
      //    incremented).
      //    2.a. When we see the elements with indices "from" and "to",
      //         save their iterators.
      // 3. Move the iterators saved in 2.a.
      //
      // Note the reliance on multi_index's iterator stability in 2.a.

      by_parent_list_index &by_parent_list = cells.template get<by_parent_list_tag>();

      // 1. Find the parent list's cells.
      std::pair<
        typename by_parent_list_index::iterator,
        typename by_parent_list_index::iterator > parent_range =
        by_parent_list.equal_range(list);

      // 2.a. Prepare locations to store the from and to locations.
      //      Note that both locations should exist!
      typename concrete_view_index::iterator
        from_location = concrete_view.end(),
        to_location = concrete_view.end();
      for(typename by_parent_list_index::iterator it =
            parent_range.first; it != parent_range.second; ++it)
        {
          const std::size_t it_idx = it->get_index_within_parent_list();

          if(it_idx == from)
            from_location = cells.template project<concrete_view_tag>(it);

          if(it_idx == to)
            to_location = cells.template project<concrete_view_tag>(it);

          // 2. Update the parent-index of each old cell.
          //
          // Note that for the iteration to be correct, I rely on
          // the multi-index guaranteeing that the element isn't
          // reordered if its key doesn't change.  Since only the
          // index changes and that isn't part of the key, we
          // should be OK.
          //
          // If the current index is above "from" and less than or
          // equal to "to", shift left (decrement the index).
          //
          // If the current index is below "from" and greater than or
          // equal to "to", shift right (increment the index).
          //
          // If the current index *is* from, set it to "to".
          if(it_idx == from)
            by_parent_list.replace(it, cell(it->get_parent_list(),
                                            to,
                                            it->get_value()));
          else if(it_idx > from && it_idx <= to)
            by_parent_list.replace(it, cell(it->get_parent_list(),
                                            it_idx - 1,
                                            it->get_value()));
          else if(it_idx < from && it_idx >= to)
            by_parent_list.replace(it, cell(it->get_parent_list(),
                                            it_idx + 1,
                                            it->get_value()));
        }

      // 3. Move the cell.
      const std::size_t
        from_idx = from_location - concrete_view.begin(),
        to_idx   = to_location - concrete_view.begin();

      // If we're moving an element to the right, we need to relocate
      // just past the target, since relocate() inserts the entry
      // being moved just *before* the target iterator.  If we're
      // moving to the left, on the other hand, the behavior is
      // exactly correct.
      typename concrete_view_index::iterator relocate_target;
      if(to_idx > from_idx)
        relocate_target = to_location + 1;
      else
        relocate_target = to_location;

      concrete_view.relocate(relocate_target, from_location);
      this->signal_moved(value, from_idx, to_idx);
    }

    template<typename T>
    void dynamic_list_collection<T>::add_list(const boost::shared_ptr<dynamic_list<T> > &lst)
    {
      // Insert all the list's items at the end of this collection.
      //
      // Could do this more efficiently, but normally new lists are
      // empty and/or very small.
      for(std::size_t idx = 0; idx < lst->size(); ++idx)
        handle_insert(lst->get_at(idx), idx, lst);

      const sigc::connection inserted_connection =
        lst->signal_inserted.connect(sigc::bind(sigc::mem_fun(*this,
                                                              &dynamic_list_collection::handle_insert),
                                                lst));

      const sigc::connection removed_connection =
        lst->signal_removed.connect(sigc::bind(sigc::mem_fun(*this,
                                                             &dynamic_list_collection::handle_remove),
                                               lst));

      const sigc::connection moved_connection =
        lst->signal_moved.connect(sigc::bind(sigc::mem_fun(*this,
                                                           &dynamic_list_collection::handle_move),
                                             lst));

      connections_by_list.insert(std::make_pair(lst, inserted_connection));
      connections_by_list.insert(std::make_pair(lst, removed_connection));
      connections_by_list.insert(std::make_pair(lst, moved_connection));
    }

    template<typename T>
    void dynamic_list_collection<T>::remove_list(const boost::shared_ptr<dynamic_list<T> > &lst)
    {
      // First remove all the list's entries from the concrete view.
      // I'm avoiding the temptation to optimize this, with
      // difficulty. (we don't need to do all the reindexing, so a
      // linear scan would be O(n) instead of O(n**2) -- same with
      // add_list)
      for(std::size_t idx = 0; idx < lst->size(); ++idx)
        // Pretend the list was removed from the front, so each
        // element has index 0 when it dies.
        handle_remove(lst->get_at(idx), 0, lst);

      // Disconnect all the list's connections.
      std::pair<
        typename sub_list_connections::iterator,
        typename sub_list_connections::iterator>
        connections = connections_by_list.equal_range(lst);

      for(typename sub_list_connections::iterator it = connections.first;
          it != connections.second; ++it)
        it->second.disconnect();

      // Drop the list from the connections map.
      connections_by_list.erase(connections.first, connections.second);
    }
  }
}

#endif // DYNAMIC_LIST_COLLECTION_H
