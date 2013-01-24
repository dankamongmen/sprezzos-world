/** \file dynamic_set_union.h */     // -*-c++-*-


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

#ifndef DYNAMIC_SET_UNION_H
#define DYNAMIC_SET_UNION_H

#include "dynamic_set.h"

#include <boost/enable_shared_from_this.hpp>
#include <boost/make_shared.hpp>
#include <boost/unordered_map.hpp>

#include <sigc++/signal.h>

namespace aptitude
{
  namespace util
  {
    /** \brief A read-only dynamic set representing the union of one
     *  or more dynamic sets.
     */
    template<typename T>
    class dynamic_set_union
      : public dynamic_set<T>,
        public boost::enable_shared_from_this<dynamic_set_union<T> >
    {
      typedef boost::unordered_map<T, int> value_counts_t;

      // Counts how many times each element occurs.  By assumption,
      // individual sets can contain an element only once, so this
      // effectively counts the number of individual sets containing
      // the element.
      value_counts_t value_counts;

      // Keeps sets contained in this union alive, and also stores the
      // connections binding each set to this object's handlers.
      typedef boost::unordered_multimap<boost::shared_ptr<dynamic_set<T> >,
                                        sigc::connection>
      contained_sets_t;

      // Maintains pointers to the individual sets this contains.
      contained_sets_t contained_sets;

      sigc::signal<void, T> signal_inserted;
      sigc::signal<void, T> signal_removed;

      void handle_inserted(const T &t);
      void handle_removed(const T &t);
      class set_enumerator;

    public:
      dynamic_set_union();

      static boost::shared_ptr<dynamic_set_union> create();

      void insert_set(const boost::shared_ptr<dynamic_set<T> > &set);
      void remove_set(const boost::shared_ptr<dynamic_set<T> > &set);

      std::size_t size();
      boost::shared_ptr<enumerator<T> > enumerate();

      sigc::connection connect_inserted(const sigc::slot<void, T> &slot);
      sigc::connection connect_removed(const sigc::slot<void, T> &slot);
    };

    template<typename T>
    dynamic_set_union<T>::dynamic_set_union()
    {
    }

    template<typename T>
    boost::shared_ptr<dynamic_set_union<T> >
    dynamic_set_union<T>::create()
    {
      return boost::make_shared<dynamic_set_union<T> >();
    }


    template<typename T>
    void dynamic_set_union<T>::insert_set(const boost::shared_ptr<dynamic_set<T> > &set)
    {
      if(contained_sets.find(set) == contained_sets.end())
        {
          for(boost::shared_ptr<enumerator<T> > e = set->enumerate();
              e->advance(); )
            handle_inserted(e->get_current());

          sigc::connection inserted_connection =
            set->connect_inserted(sigc::mem_fun(*this, &dynamic_set_union::handle_inserted));
          sigc::connection removed_connection =
            set->connect_removed(sigc::mem_fun(*this, &dynamic_set_union::handle_removed));

          contained_sets.insert(std::make_pair(set, inserted_connection));
          contained_sets.insert(std::make_pair(set, removed_connection));
        }
    }

    template<typename T>
    void dynamic_set_union<T>::remove_set(const boost::shared_ptr<dynamic_set<T> > &set)
    {
      // Paranoia: defensively take an extra reference in case "set"
      // is somehow being kept alive by our reference (which should be
      // impossible).
      const boost::shared_ptr<dynamic_set<T> > set_copy = set;

      typedef typename contained_sets_t::iterator contained_iterator;
      const std::pair<contained_iterator, contained_iterator> found
        = contained_sets.equal_range(set);

      if(found.first != found.second)
        {
          for(boost::shared_ptr<enumerator<T> > e = set->enumerate();
              e->advance(); )
            handle_removed(e->get_current());

          for(contained_iterator it = found.first; it != found.second; ++it)
            it->second.disconnect();

          contained_sets.erase(found.first, found.second);
        }
    }


    template<typename T>
    std::size_t dynamic_set_union<T>::size()
    {
      return value_counts.size();
    }

    template<typename T>
    boost::shared_ptr<enumerator<T> > dynamic_set_union<T>::enumerate()
    {
      return boost::make_shared<set_enumerator>(this->shared_from_this(),
                                                value_counts.begin(),
                                                value_counts.end());
    }

    template<typename T>
    sigc::connection
    dynamic_set_union<T>::connect_inserted(const sigc::slot<void, T> &slot)
    {
      return signal_inserted.connect(slot);
    }

    template<typename T>
    sigc::connection
    dynamic_set_union<T>::connect_removed(const sigc::slot<void, T> &slot)
    {
      return signal_removed.connect(slot);
    }


    template<typename T>
    void dynamic_set_union<T>::handle_inserted(const T &t)
    {
      typename value_counts_t::iterator found =
        value_counts.find(t);

      if(found != value_counts.end())
        ++found->second;
      else
        {
          value_counts.insert(std::make_pair(t, 1));
          signal_inserted(t);
        }
    }

    template<typename T>
    void dynamic_set_union<T>::handle_removed(const T &t)
    {
      typename value_counts_t::iterator found =
        value_counts.find(t);

      if(found != value_counts.end())
        {
          --found->second;

          if(found->second == 0)
            {
              // Copy the value to send it to the signal.
              const T value = found->first;
              value_counts.erase(found);
              signal_removed(value);
            }
        }
    }


    template<typename T>
    class dynamic_set_union<T>::set_enumerator
      : public enumerator<T>
    {
    public:
      typedef
      typename dynamic_set_union<T>::value_counts_t::const_iterator
      values_iterator;

    private:
      boost::shared_ptr<dynamic_set<T> > parent;
      values_iterator begin, end;
      bool is_first;

    public:
      set_enumerator(const boost::shared_ptr<dynamic_set<T> > &_parent,
                     const values_iterator &_begin,
                     const values_iterator &_end);

      T get_current();
      bool advance();
    };

    template<typename T>
    dynamic_set_union<T>::set_enumerator::set_enumerator(const boost::shared_ptr<dynamic_set<T> > &_parent,
                                                         const values_iterator &_begin,
                                                         const values_iterator &_end)
      : parent(_parent),
        begin(_begin),
        end(_end),
        is_first(true)
    {
    }

    template<typename T>
    T dynamic_set_union<T>::set_enumerator::get_current()
    {
      return begin->first;
    }

    template<typename T>
    bool dynamic_set_union<T>::set_enumerator::advance()
    {
      if(is_first)
        is_first = false;
      else
        ++begin;

      return begin != end;
    }
  }
}

#endif // DYNAMIC_SET_UNION_H
