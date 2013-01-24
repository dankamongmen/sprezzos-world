/** \file test_dynamic_list.cc */   // -*-c++-*-


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


#include <generic/util/dynamic_list.h>
#include <generic/util/dynamic_list_collection.h>

#include <generic/util/dynamic_list_impl.h>

#include <boost/test/unit_test.hpp>
#include <boost/variant.hpp>

using aptitude::util::dynamic_list;
using aptitude::util::dynamic_list_collection;
using aptitude::util::dynamic_list_impl;
using aptitude::util::writable_dynamic_list;

namespace
{
  template<typename T>
  class inserted_call
  {
    T value;
    std::size_t position;

  public:
    inserted_call(const T &_value,
                  const std::size_t _position)
      : value(_value),
        position(_position)
    {
    }

    T get_value() const { return value; }
    std::size_t get_position() const { return position; }
    bool operator==(const inserted_call &other) const
    {
      return
        value == other.value &&
        position == other.position;
    }

    bool operator!=(const inserted_call &other) const
    {
      return !(*this == other);
    }
  };

  template<typename T>
  std::ostream &operator<<(std::ostream &out,
                           const inserted_call<T> &call)
  {
    return out << "inserted(value = "
               << call.get_value()
               << ", position = "
               << call.get_position() << ")";
  }

  template<typename T>
  class removed_call
  {
    T value;
    std::size_t idx;

  public:
    removed_call(const T &_value, std::size_t _idx)
      : value(_value), idx(_idx)
    {
    }

    const T get_value() const { return value; }
    std::size_t get_idx() const { return idx; }
    bool operator==(const removed_call &other) const
    {
      return value == other.value && idx == other.idx;
    }
    bool operator!=(const removed_call &other) const
    {
      return !(*this == other);
    }
  };

  template<typename T>
  std::ostream &operator<<(std::ostream &out,
                           const removed_call<T> &call)
  {
    return out << "removed(value = "
               << call.get_value()
               << ", idx = "
               << call.get_idx() << ")";
  }

  template<typename T>
  class moved_call
  {
    T value;
    std::size_t from, to;

  public:
    moved_call(const T &_value, std::size_t _from, std::size_t _to)
      : value(_value), from(_from), to(_to)
    {
    }

    T get_value() const { return value; }
    std::size_t get_from() const { return from; }
    std::size_t get_to() const { return to; }

    bool operator==(const moved_call &other) const
    {
      return
        value == other.value &&
        from == other.from &&
        to == other.to;
    }

    bool operator!=(const moved_call &other) const
    {
      return !(*this == other);
    }
  };

  template<typename T>
  std::ostream &operator<<(std::ostream &out,
                           const moved_call<T> &call)
  {
    return out << "moved(value = "
               << call.get_value()
               << ", from = "
               << call.get_from()
               << ", to = "
               << call.get_to()
               << ")";
  }

  template<typename T>
  class list_signal
  {
  public:
    typedef boost::variant<inserted_call<T>,
                           removed_call<T>,
                           moved_call<T> > value_type;

  private:
    value_type value;

  public:
    list_signal(const inserted_call<T> &_value)
      : value(_value)
    {
    }

    list_signal(const removed_call<T> &_value)
      : value(_value)
    {
    }

    list_signal(const moved_call<T> &_value)
      : value(_value)
    {
    }

    list_signal(const value_type &_value)
      : value(_value)
    {
    }

    const value_type &get_value() const { return value; }

    bool operator==(const list_signal &other) const
    {
      return value == other.value;
    }

    bool operator!=(const list_signal &other) const
    {
      return !(value == other.value);
    }
  };

  template<typename T>
  std::ostream &operator<<(std::ostream &out, const list_signal<T> &signal)
  {
    return out << signal.get_value();
  }

  template<typename T>
  class dynamic_list_signals
  {
    std::vector<list_signal<T> > calls;

    void inserted(const T &value, std::size_t position)
    {
      calls.push_back(inserted_call<T>(value, position));
    }

    void removed(const T &value, std::size_t position)
    {
      calls.push_back(removed_call<T>(value, position));
    }

    void moved(const T &value, std::size_t from, std::size_t to)
    {
      calls.push_back(moved_call<T>(value, from, to));
    }

    dynamic_list_signals(const dynamic_list_signals &);

  public:
    dynamic_list_signals()
    {
    }

    ~dynamic_list_signals()
    {
    }

    void clear()
    {
      calls.clear();
    }

    void attach(dynamic_list<T> &list)
    {
      list.signal_inserted.connect(sigc::mem_fun(*this, &dynamic_list_signals::inserted));
      list.signal_removed.connect(sigc::mem_fun(*this, &dynamic_list_signals::removed));
      list.signal_moved.connect(sigc::mem_fun(*this, &dynamic_list_signals::moved));

      BOOST_REQUIRE(list.signal_inserted.size() > 0);
    }

    /** \brief To be used only by test code. */
    void push_back(const list_signal<T> &signal)
    {
      calls.push_back(signal);
    }

    typedef typename std::vector<list_signal<T> >::const_iterator const_iterator;
    const_iterator begin() const { return calls.begin(); }
    const_iterator end() const { return calls.end(); }

    bool operator==(const dynamic_list_signals &other) const
    {
      return calls == other.calls;
    }

    bool operator!=(const dynamic_list_signals &other) const
    {
      return calls != other.calls;
    }
  };

  template<typename T>
  std::ostream &operator<<(std::ostream &out,
                           const dynamic_list_signals<T> &signals)
  {
    out << "[";
    for(typename dynamic_list_signals<T>::const_iterator
          it = signals.begin(); it != signals.end(); ++it)
      {
        if(it != signals.begin())
          out << ", ";
        out << *it;
      }

    out << "]";

    return out;
  }

  struct list_test
  {
    boost::shared_ptr<dynamic_list_impl<int> > valuesPtr;
    dynamic_list_impl<int> &values;
    dynamic_list_signals<int> signals;

    std::vector<int> expected;

    list_test()
      : valuesPtr(dynamic_list_impl<int>::create()),
        values(*valuesPtr),
        signals()
    {
      values.insert(1, 0);
      values.insert(2, 1);
      values.insert(3, 2);
      signals.attach(values);

      expected.push_back(1);
      expected.push_back(2);
      expected.push_back(3);
    }

    std::vector<int> as_vector() const
    {
      std::vector<int> rval;
      for(std::size_t i = 0; i < values.size(); ++i)
        rval.push_back(values.get_at(i));

      return rval;
    }
  };
}

BOOST_AUTO_TEST_CASE(listSignals)
{
  dynamic_list_signals<int> signals1, signals2, signals3;

  typedef inserted_call<int> ins;
  typedef removed_call<int> rem;

  signals1.push_back(ins(1, 0));
  signals1.push_back(ins(2, 1));
  signals1.push_back(ins(3, 2));

  signals2.push_back(ins(1, 0));
  signals2.push_back(rem(2, 5));
  signals2.push_back(ins(3, 2));

  signals3.push_back(ins(1, 0));
  signals3.push_back(ins(2, 1));
  signals3.push_back(ins(3, 2));

  BOOST_CHECK_EQUAL(signals1, signals3);
  BOOST_CHECK_EQUAL(signals3, signals1);
  BOOST_CHECK_EQUAL_COLLECTIONS(signals1.begin(), signals1.end(),
                                signals3.begin(), signals3.end());
  BOOST_CHECK_EQUAL_COLLECTIONS(signals3.begin(), signals3.end(),
                                signals1.begin(), signals1.end());

  BOOST_CHECK_NE(signals1, signals2);
  BOOST_CHECK_NE(signals2, signals1);
  BOOST_CHECK_NE(signals2, signals3);
  BOOST_CHECK_NE(signals3, signals2);
}

BOOST_AUTO_TEST_CASE(listSignalsAttach)
{
  dynamic_list_signals<int> signals, expected;
  dynamic_list_impl<int> list;
  signals.attach(list);

  typedef inserted_call<int> ins;
  typedef removed_call<int> rem;
  typedef moved_call<int> mov;

  expected.push_back(ins(5, 0));
  expected.push_back(mov(5, 0, 1));
  expected.push_back(rem(5, 1));

  // Note: call the signals directly so we test the attachment and not
  // the dynamic list.
  list.signal_inserted(5, 0);
  list.signal_moved(5, 0, 1);
  list.signal_removed(5, 1);

  BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(),
                                signals.begin(), signals.end());
}

// Tests that a few operations change the size as expected.
BOOST_AUTO_TEST_CASE(dynamicListSize)
{
  boost::shared_ptr<writable_dynamic_list<int> > list = dynamic_list_impl<int>::create();

  BOOST_CHECK_EQUAL(0, list->size());

  list->insert(5, 0);

  BOOST_CHECK_EQUAL(1, list->size());

  list->insert(13, 1);

  BOOST_CHECK_EQUAL(2, list->size());

  list->move(0, 1);

  BOOST_CHECK_EQUAL(2, list->size());

  list->remove(0);

  BOOST_CHECK_EQUAL(1, list->size());
}

BOOST_FIXTURE_TEST_CASE(dynamicListGetAt, list_test)
{
  BOOST_CHECK_EQUAL(1, values.get_at(0));
  BOOST_CHECK_EQUAL(2, values.get_at(1));
  BOOST_CHECK_EQUAL(3, values.get_at(2));
}

BOOST_FIXTURE_TEST_CASE(dynamicListInsertFirst, list_test)
{
  values.insert(99, 0);
  expected.insert(expected.begin(), 99);

  std::vector<int> values_vector = as_vector();
  dynamic_list_signals<int> expected_calls;
  expected_calls.push_back(inserted_call<int>(99, 0));

  BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(),
                                values_vector.begin(), values_vector.end());

  BOOST_CHECK_EQUAL_COLLECTIONS(expected_calls.begin(), expected_calls.end(),
                                signals.begin(), signals.end());
}

BOOST_FIXTURE_TEST_CASE(dynamicListInsertMiddle, list_test)
{
  values.insert(99, 1);
  expected.insert(expected.begin() + 1, 99);

  std::vector<int> values_vector = as_vector();
  dynamic_list_signals<int> expected_calls;
  expected_calls.push_back(inserted_call<int>(99, 1));

  BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(),
                                values_vector.begin(), values_vector.end());

  BOOST_CHECK_EQUAL_COLLECTIONS(expected_calls.begin(), expected_calls.end(),
                                signals.begin(), signals.end());
}

BOOST_FIXTURE_TEST_CASE(dynamicListInsertBeforeEnd, list_test)
{
  values.insert(99, 2);
  expected.insert(expected.begin() + 2, 99);

  std::vector<int> values_vector = as_vector();
  dynamic_list_signals<int> expected_calls;
  expected_calls.push_back(inserted_call<int>(99, 2));

  BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(),
                                values_vector.begin(), values_vector.end());

  BOOST_CHECK_EQUAL_COLLECTIONS(expected_calls.begin(), expected_calls.end(),
                                signals.begin(), signals.end());
}

BOOST_FIXTURE_TEST_CASE(dynamicListInsertAfterEnd, list_test)
{
  values.insert(99, 3);
  expected.push_back(99);

  std::vector<int> values_vector = as_vector();
  dynamic_list_signals<int> expected_calls;
  expected_calls.push_back(inserted_call<int>(99, 3));

  BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(),
                                values_vector.begin(), values_vector.end());

  BOOST_CHECK_EQUAL_COLLECTIONS(expected_calls.begin(), expected_calls.end(),
                                signals.begin(), signals.end());
}

BOOST_FIXTURE_TEST_CASE(dynamicListRemoveFirst, list_test)
{
  values.remove(0);

  std::vector<int> values_vector = as_vector();
  dynamic_list_signals<int> expected_calls;

  expected.clear();
  expected.push_back(2);
  expected.push_back(3);

  expected_calls.push_back(removed_call<int>(1, 0));


  BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(),
                                values_vector.begin(), values_vector.end());

  BOOST_CHECK_EQUAL_COLLECTIONS(expected_calls.begin(), expected_calls.end(),
                                signals.begin(), signals.end());
}

BOOST_FIXTURE_TEST_CASE(dynamicListRemoveMiddle, list_test)
{
  values.remove(1);

  std::vector<int> values_vector = as_vector();
  dynamic_list_signals<int> expected_calls;

  expected.clear();
  expected.push_back(1);
  expected.push_back(3);

  expected_calls.push_back(removed_call<int>(2, 1));


  BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(),
                                values_vector.begin(), values_vector.end());

  BOOST_CHECK_EQUAL_COLLECTIONS(expected_calls.begin(), expected_calls.end(),
                                signals.begin(), signals.end());
}

BOOST_FIXTURE_TEST_CASE(dynamicListRemoveLast, list_test)
{
  values.remove(2);

  std::vector<int> values_vector = as_vector();
  dynamic_list_signals<int> expected_calls;

  expected.clear();
  expected.push_back(1);
  expected.push_back(2);

  expected_calls.push_back(removed_call<int>(3, 2));


  BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(),
                                values_vector.begin(), values_vector.end());

  BOOST_CHECK_EQUAL_COLLECTIONS(expected_calls.begin(), expected_calls.end(),
                                signals.begin(), signals.end());
}

BOOST_FIXTURE_TEST_CASE(dynamicListMoveLastToFront, list_test)
{
  values.move(2, 0);

  std::vector<int> values_vector = as_vector();
  dynamic_list_signals<int> expected_calls;

  expected.clear();
  expected.push_back(3);
  expected.push_back(1);
  expected.push_back(2);

  expected_calls.push_back(moved_call<int>(3, 2, 0));


  BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(),
                                values_vector.begin(), values_vector.end());

  BOOST_CHECK_EQUAL_COLLECTIONS(expected_calls.begin(), expected_calls.end(),
                                signals.begin(), signals.end());
}

BOOST_FIXTURE_TEST_CASE(dynamicListMoveMiddleToFront, list_test)
{
  values.move(1, 0);

  std::vector<int> values_vector = as_vector();
  dynamic_list_signals<int> expected_calls;

  expected.clear();
  expected.push_back(2);
  expected.push_back(1);
  expected.push_back(3);

  expected_calls.push_back(moved_call<int>(2, 1, 0));


  BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(),
                                values_vector.begin(), values_vector.end());

  BOOST_CHECK_EQUAL_COLLECTIONS(expected_calls.begin(), expected_calls.end(),
                                signals.begin(), signals.end());
}

BOOST_FIXTURE_TEST_CASE(dynamicListMoveFrontToFront, list_test)
{
  values.move(0, 0);

  std::vector<int> values_vector = as_vector();
  dynamic_list_signals<int> expected_calls;

  expected.clear();
  expected.push_back(1);
  expected.push_back(2);
  expected.push_back(3);


  BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(),
                                values_vector.begin(), values_vector.end());

  BOOST_CHECK_EQUAL_COLLECTIONS(expected_calls.begin(), expected_calls.end(),
                                signals.begin(), signals.end());
}

BOOST_FIXTURE_TEST_CASE(dynamicListMoveLastToMiddle, list_test)
{
  values.move(2, 1);

  std::vector<int> values_vector = as_vector();
  dynamic_list_signals<int> expected_calls;

  expected.clear();
  expected.push_back(1);
  expected.push_back(3);
  expected.push_back(2);

  expected_calls.push_back(moved_call<int>(3, 2, 1));


  BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(),
                                values_vector.begin(), values_vector.end());

  BOOST_CHECK_EQUAL_COLLECTIONS(expected_calls.begin(), expected_calls.end(),
                                signals.begin(), signals.end());
}

BOOST_FIXTURE_TEST_CASE(dynamicListMoveMiddleToMiddle, list_test)
{
  values.move(1, 1);

  std::vector<int> values_vector = as_vector();
  dynamic_list_signals<int> expected_calls;

  expected.clear();
  expected.push_back(1);
  expected.push_back(2);
  expected.push_back(3);


  BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(),
                                values_vector.begin(), values_vector.end());

  BOOST_CHECK_EQUAL_COLLECTIONS(expected_calls.begin(), expected_calls.end(),
                                signals.begin(), signals.end());
}

BOOST_FIXTURE_TEST_CASE(dynamicListMoveFrontToMiddle, list_test)
{
  values.move(0, 1);

  std::vector<int> values_vector = as_vector();
  dynamic_list_signals<int> expected_calls;

  expected.clear();
  expected.push_back(2);
  expected.push_back(1);
  expected.push_back(3);

  expected_calls.push_back(moved_call<int>(1, 0, 1));


  BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(),
                                values_vector.begin(), values_vector.end());

  BOOST_CHECK_EQUAL_COLLECTIONS(expected_calls.begin(), expected_calls.end(),
                                signals.begin(), signals.end());
}

BOOST_FIXTURE_TEST_CASE(dynamicListMoveLastToLast, list_test)
{
  values.move(2, 2);

  std::vector<int> values_vector = as_vector();
  dynamic_list_signals<int> expected_calls;

  expected.clear();
  expected.push_back(1);
  expected.push_back(2);
  expected.push_back(3);


  BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(),
                                values_vector.begin(), values_vector.end());

  BOOST_CHECK_EQUAL_COLLECTIONS(expected_calls.begin(), expected_calls.end(),
                                signals.begin(), signals.end());
}

BOOST_FIXTURE_TEST_CASE(dynamicListMoveMiddleToLast, list_test)
{
  values.move(1, 2);

  std::vector<int> values_vector = as_vector();
  dynamic_list_signals<int> expected_calls;

  expected.clear();
  expected.push_back(1);
  expected.push_back(3);
  expected.push_back(2);

  expected_calls.push_back(moved_call<int>(2, 1, 2));

  BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(),
                                values_vector.begin(), values_vector.end());

  BOOST_CHECK_EQUAL_COLLECTIONS(expected_calls.begin(), expected_calls.end(),
                                signals.begin(), signals.end());
}

BOOST_FIXTURE_TEST_CASE(dynamicListMoveFrontToLast, list_test)
{
  values.move(0, 2);

  std::vector<int> values_vector = as_vector();
  dynamic_list_signals<int> expected_calls;

  expected.clear();
  expected.push_back(2);
  expected.push_back(3);
  expected.push_back(1);

  expected_calls.push_back(moved_call<int>(1, 0, 2));


  BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(),
                                values_vector.begin(), values_vector.end());

  BOOST_CHECK_EQUAL_COLLECTIONS(expected_calls.begin(), expected_calls.end(),
                                signals.begin(), signals.end());
}

struct list_collection_test
{
  boost::shared_ptr<writable_dynamic_list<int> > list1, list2, list3;
  boost::shared_ptr<dynamic_list_collection<int> > collection;

  dynamic_list_signals<int> signals, expected;

  typedef inserted_call<int> ins;
  typedef removed_call<int> rem;
  typedef moved_call<int> mov;

  list_collection_test()
    : list1(dynamic_list_impl<int>::create()),
      list2(dynamic_list_impl<int>::create()),
      list3(dynamic_list_impl<int>::create()),
      collection(dynamic_list_collection<int>::create())
  {
    list1->insert(1, 0);
    list1->insert(2, 1);
    list1->insert(3, 2);

    list2->insert(5, 0);

    signals.attach(*collection);
  }

  std::vector<int> as_vector(dynamic_list<int> &list)
  {
    std::vector<int> rval;

    for(std::size_t i = 0; i < collection->size(); ++i)
      rval.push_back(collection->get_at(i));

    return rval;
  }
};




// Note: the list collection tests magically know the order that the
// list collection puts its sub-items in.
BOOST_FIXTURE_TEST_CASE(dynamicListCollectionAddList, list_collection_test)
{
  collection->add_list(list2);
  collection->add_list(list3);
  collection->add_list(list1);

  std::vector<int> expected_values;
  expected_values.push_back(5);
  expected_values.push_back(1);
  expected_values.push_back(2);
  expected_values.push_back(3);

  expected.push_back(ins(5, 0));
  expected.push_back(ins(1, 1));
  expected.push_back(ins(2, 2));
  expected.push_back(ins(3, 3));

  std::vector<int> collection_vector = as_vector(*collection);
  BOOST_CHECK_EQUAL_COLLECTIONS(expected_values.begin(), expected_values.end(),
                                collection_vector.begin(), collection_vector.end());

  BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(),
                                signals.begin(), signals.end());
}

BOOST_FIXTURE_TEST_CASE(dynamicListCollectionRemoveList, list_collection_test)
{
  collection->add_list(list1);
  collection->add_list(list2);
  collection->add_list(list3);

  // Clear the current list of signals, since for the purposes of this
  // test, we don't care about the ones emitted by add_list() above.
  signals.clear();

  collection->remove_list(list3);
  collection->remove_list(list2);
  collection->remove_list(list1);

  expected.push_back(rem(5, 3));
  expected.push_back(rem(1, 0));
  expected.push_back(rem(2, 0));
  expected.push_back(rem(3, 0));

  std::vector<int> expected_values;
  std::vector<int> collection_vector = as_vector(*collection);
  BOOST_CHECK_EQUAL_COLLECTIONS(expected_values.begin(), expected_values.end(),
                                collection_vector.begin(), collection_vector.end());

  BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(),
                                signals.begin(), signals.end());
}

BOOST_FIXTURE_TEST_CASE(dynamicListCollectionInsertIntoSublist, list_collection_test)
{
  collection->add_list(list1);
  collection->add_list(list2);
  collection->add_list(list3);

  signals.clear();

  // Now [1, 2, 3, 5]
  list2->insert(9, 0); // Now [1, 2, 3, 9, 5]
  list1->insert(4, 3); // Now [1, 2, 3, 9, 5, 4]
  list1->insert(0, 0); // Now [0, 1, 2, 3, 9, 5, 4]
  list3->insert(6, 0); // Now [0, 1, 2, 3, 9, 5, 4, 6]
  list2->insert(8, 1); // Now [0, 1, 2, 3, 9, 8, 5, 4, 6]

  expected.push_back(ins(9, 3));
  expected.push_back(ins(4, 5));
  expected.push_back(ins(0, 0));
  expected.push_back(ins(6, 7));
  expected.push_back(ins(8, 5));

  const int expected_values_begin[] = { 0, 1, 2, 3, 9, 8, 5, 4, 6 };
  const int expected_values_size =
    sizeof(expected_values_begin) / sizeof(expected_values_begin[0]);
  const int * const expected_values_end =
    expected_values_begin + expected_values_size;

  std::vector<int> collection_vector = as_vector(*collection);

  BOOST_CHECK_EQUAL_COLLECTIONS(expected_values_begin, expected_values_end,
                                collection_vector.begin(), collection_vector.end());

  BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(),
                                signals.begin(), signals.end());
}

BOOST_FIXTURE_TEST_CASE(dynamicListCollectionRemoveFromSublist, list_collection_test)
{
  list1->insert(4, 3);
  collection->add_list(list1);
  collection->add_list(list2);

  signals.clear();

  // Now [1, 2, 3, 4, 5].
  list1->remove(2); // Now [1, 2, 4, 5]
  list2->remove(0); // Now [1, 2, 4]
  list1->remove(0); // Now [2, 4]
  list1->remove(1); // Now [2]
  list1->remove(0); // Now []

  expected.push_back(rem(3, 2));
  expected.push_back(rem(5, 3));
  expected.push_back(rem(1, 0));
  expected.push_back(rem(4, 1));
  expected.push_back(rem(2, 0));

  std::vector<int> expected_values;
  std::vector<int> collection_vector = as_vector(*collection);

  BOOST_CHECK_EQUAL_COLLECTIONS(expected_values.begin(), expected_values.end(),
                                collection_vector.begin(), collection_vector.end());

  BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(),
                                signals.begin(), signals.end());
}

BOOST_FIXTURE_TEST_CASE(dynamicListCollectionMoveInSublist, list_collection_test)
{
  collection->add_list(list2);
  collection->add_list(list1);
  list2->insert(4, 1);

  signals.clear();

  // Note that it's not possible to move an element to the end of its
  // list.  I wonder if this behavior is ideal?  In the current case
  // where we'll use this, it doesn't matter so much, but eventually
  // it could.
  //
  // Now [5, 1, 2, 3, 4]
  {
    std::vector<int> expected_values;
    expected_values.push_back(5);
    expected_values.push_back(1);
    expected_values.push_back(2);
    expected_values.push_back(3);
    expected_values.push_back(4);

    std::vector<int> collection_vector = as_vector(*collection);

    BOOST_CHECK_EQUAL_COLLECTIONS(expected_values.begin(), expected_values.end(),
                                  collection_vector.begin(), collection_vector.end());
  }

  list1->move(0, 2); // Now [5, 2, 3, 1, 4]
  {
    std::vector<int> expected_values;
    expected_values.push_back(5);
    expected_values.push_back(2);
    expected_values.push_back(3);
    expected_values.push_back(1);
    expected_values.push_back(4);

    std::vector<int> collection_vector = as_vector(*collection);

    BOOST_CHECK_EQUAL_COLLECTIONS(expected_values.begin(), expected_values.end(),
                                  collection_vector.begin(), collection_vector.end());
  }


  list2->move(1, 0); // Now [4, 5, 2, 3, 1]
  {
    std::vector<int> expected_values;
    expected_values.push_back(4);
    expected_values.push_back(5);
    expected_values.push_back(2);
    expected_values.push_back(3);
    expected_values.push_back(1);

    std::vector<int> collection_vector = as_vector(*collection);

    BOOST_CHECK_EQUAL_COLLECTIONS(expected_values.begin(), expected_values.end(),
                                  collection_vector.begin(), collection_vector.end());

  }

  list1->move(1, 2); // Now [4, 5, 2, 1, 3]
  {
    std::vector<int> expected_values;
    expected_values.push_back(4);
    expected_values.push_back(5);
    expected_values.push_back(2);
    expected_values.push_back(1);
    expected_values.push_back(3);

    std::vector<int> collection_vector = as_vector(*collection);

    BOOST_CHECK_EQUAL_COLLECTIONS(expected_values.begin(), expected_values.end(),
                                  collection_vector.begin(), collection_vector.end());
  }

  expected.push_back(mov(1, 1, 3));
  expected.push_back(mov(4, 4, 0));
  expected.push_back(mov(3, 3, 4));

  BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(),
                                signals.begin(), signals.end());
}
