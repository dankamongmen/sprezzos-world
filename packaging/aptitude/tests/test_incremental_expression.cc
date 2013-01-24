// test_incremental_expression.cc
//
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

#include <generic/problemresolver/incremental_expression.h>

#include <boost/variant.hpp>

#include <cppunit/extensions/HelperMacros.h>

namespace cw = cwidget;

namespace
{
  // Make it possible to "show" vectors.
  template<typename T, typename Alloc>
  std::ostream &operator<<(std::ostream &out, const std::vector<T, Alloc> &v)
  {
    out << "[";

    for(typename std::vector<T, Alloc>::const_iterator it =
          v.begin(); it != v.end(); ++it)
      {
        if(it != v.begin())
          out << ", ";

        out << *it;
      }

    out << "]";

    return out;
  }

  // Helper class for the code below that records a single call to
  // child_modified().
  template<typename T>
  class child_modified_call
  {
    cw::util::ref_ptr<expression<T> > child;
    T old_value, new_value;

  public:
    child_modified_call(const cw::util::ref_ptr<expression<T> > &_child,
                        const T &_old_value, const T &_new_value)
      : child(_child), old_value(_old_value), new_value(_new_value)
    {
    }

    const cw::util::ref_ptr<expression<T> > &get_child() const { return child; }
    const T &get_old_value() const { return old_value; }
    const T &get_new_value() const { return new_value; }

    bool operator==(const child_modified_call &other) const
    {
      return
        child == other.child &&
        old_value == other.old_value &&
        new_value == other.new_value;
    }
  };

  template<typename T>
  std::ostream &operator<<(std::ostream &out, const child_modified_call<T> &t)
  {
    return out << "child_modified(child = " << t.get_child()
               << ", old_value = " << t.get_old_value()
               << ", new_value = " << t.get_new_value() << ")";
  }

  template<typename T>
  class get_value_call
  {
    T return_value;

  public:
    get_value_call(const T &_return_value)
      : return_value(_return_value)
    {
    }

    const T &get_return_value() const { return return_value; }

    bool operator==(const get_value_call &other) const
    {
      return return_value == other.return_value;
    }
  };

  template<typename T>
  std::ostream &operator<<(std::ostream &out, const get_value_call<T> &c)
  {
    return out << "get_value() => " << c.get_return_value();
  }

  // A class that implements the expression container interface,
  // passing all interesting calls to a single sub-object and
  // recording all the calls to child_modified.
  template<typename T>
  class fake_container : public expression_container<T>
  {
    cw::util::ref_ptr<expression<T> > real_object;
    std::vector<child_modified_call<T> > calls;

    fake_container(const cw::util::ref_ptr<expression<T> > &_real_object)
      : real_object(_real_object)
    {
      real_object->add_parent(this);
    }

  public:
    ~fake_container()
    {
      real_object->remove_parent(this);
    }

    static cw::util::ref_ptr<fake_container<T> >
    create(const cw::util::ref_ptr<expression<T> > &child)
    {
      return new fake_container<T>(child);
    }

    const std::vector<child_modified_call<T> > &get_calls() const { return calls; }

    void child_modified(const cw::util::ref_ptr<expression<T> > &child,
                        T old_value,
                        T new_value)
    {
      calls.push_back(child_modified_call<T>(child, old_value, new_value));
    }

    T get_value()
    {
      return real_object->get_value();
    }

    void dump(std::ostream &out)
    {
      real_object->dump(out);
    }
  };

  // Records calls to expression_box::changed.
  template<typename T>
  class changed_call
  {
    T new_value;

  public:
    changed_call(const T &_new_value)
      : new_value(_new_value)
    {
    }

    const T &get_new_value() const { return new_value; }
  };

  template<typename T>
  std::ostream &operator<<(std::ostream &out, const changed_call<T> &c)
  {
    return out << "changed(" << c.get_new_value() << ")";
  }

  // A helper class used to test that expression_wrapper behaves as
  // advertised.
  //
  // Wraps its subexpression and records all the calls to
  // child_modified() and changed().
  template<typename T>
  class fake_wrapper : public expression_wrapper<T>
  {
    std::vector<boost::variant<child_modified_call<T>, changed_call<T> > > calls;

    fake_wrapper() : expression_wrapper<T>() { }
    fake_wrapper(const cw::util::ref_ptr<expression<T> > &child)
      : expression_wrapper<T>(child)
    {
    }

  public:
    static cw::util::ref_ptr<fake_wrapper> create()
    {
      return new fake_wrapper;
    }

    static cw::util::ref_ptr<fake_wrapper>
    create(const cw::util::ref_ptr<expression<T> > &child)
    {
      return new fake_wrapper(child);
    }

    void child_modified(const cw::util::ref_ptr<expression<T> > &child,
                        T old_value,
                        T new_value)
    {
      calls.push_back(child_modified_call<T>(child, old_value, new_value));

      expression_wrapper<T>::child_modified(child, old_value, new_value);
    }

    void changed(T new_value)
    {
      calls.push_back(changed_call<T>(new_value));

      expression_wrapper<T>::changed(this->get_child());
    }
  };
}

class TestIncrementalExpression : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TestIncrementalExpression);

  CPPUNIT_TEST(testIncrementalExpressionGetVarValue);
  CPPUNIT_TEST(testIncrementalExpressionSetVarValue);
  CPPUNIT_TEST(testIncrementalExpressionVarSignalChange);
  // TODO: test comparisons?
  // TODO: test conversions to string?

  CPPUNIT_TEST(testIncrementalExpressionWeakRefDeref);
  CPPUNIT_TEST(testIncrementalExpressionWeakRefGetValidLive);
  CPPUNIT_TEST(testIncrementalExpressionWeakRefGetValidDead);

  CPPUNIT_TEST(testNotFalse);
  CPPUNIT_TEST(testNotTrue);
  CPPUNIT_TEST(testNotRaiseInput);
  CPPUNIT_TEST(testNotLowerInput);
  CPPUNIT_TEST(testNotNull);

  CPPUNIT_TEST(testAndEmpty);

  CPPUNIT_TEST(testAndSingletonRaise);
  CPPUNIT_TEST(testAndSingletonLower);
  CPPUNIT_TEST(testAndSingletonRaiseByRemoving);
  CPPUNIT_TEST(testAndSingletonLowerByAppending);
  CPPUNIT_TEST(testAndSingletonNull);

  CPPUNIT_TEST(testAndDoubletonFirstNull);
  CPPUNIT_TEST(testAndDoubletonSecondNull);
  CPPUNIT_TEST(testAndDoubletonBothNull);

  CPPUNIT_TEST(testAndDoubletonRaiseFirst);
  CPPUNIT_TEST(testAndDoubletonRaiseSecond);
  CPPUNIT_TEST(testAndDoubletonRaiseFirstNoEffect);
  CPPUNIT_TEST(testAndDoubletonRaiseSecondNoEffect);

  CPPUNIT_TEST(testAndDoubletonLowerFirst);
  CPPUNIT_TEST(testAndDoubletonLowerSecond);
  CPPUNIT_TEST(testAndDoubletonLowerFirstNoEffect);
  CPPUNIT_TEST(testAndDoubletonLowerSecondNoEffect);

  CPPUNIT_TEST(testOrEmpty);

  CPPUNIT_TEST(testOrSingletonRaise);
  CPPUNIT_TEST(testOrSingletonLower);
  CPPUNIT_TEST(testOrSingletonLowerByRemoving);
  CPPUNIT_TEST(testOrSingletonRaiseByAppending);
  CPPUNIT_TEST(testOrSingletonNull);

  CPPUNIT_TEST(testOrDoubletonFirstNull);
  CPPUNIT_TEST(testOrDoubletonSecondNull);
  CPPUNIT_TEST(testOrDoubletonBothNull);

  CPPUNIT_TEST(testOrDoubletonRaiseFirst);
  CPPUNIT_TEST(testOrDoubletonRaiseSecond);
  CPPUNIT_TEST(testOrDoubletonRaiseFirstNoEffect);
  CPPUNIT_TEST(testOrDoubletonRaiseSecondNoEffect);

  CPPUNIT_TEST(testOrDoubletonLowerFirst);
  CPPUNIT_TEST(testOrDoubletonLowerSecond);
  CPPUNIT_TEST(testOrDoubletonLowerFirstNoEffect);
  CPPUNIT_TEST(testOrDoubletonLowerSecondNoEffect);

  CPPUNIT_TEST_SUITE_END();

public:
  void testIncrementalExpressionGetVarValue()
  {
    cw::util::ref_ptr<var_e<int> >
      v0 = var_e<int>::create(0),
      v5 = var_e<int>::create(5),
      v9 = var_e<int>::create(9);

    CPPUNIT_ASSERT_EQUAL(0, v0->get_value());
    CPPUNIT_ASSERT_EQUAL(5, v5->get_value());
    CPPUNIT_ASSERT_EQUAL(9, v9->get_value());
  }

  void testIncrementalExpressionSetVarValue()
  {
    cw::util::ref_ptr<var_e<int> > v = var_e<int>::create(123456);

    v->set_value(987654);
    CPPUNIT_ASSERT_EQUAL(987654, v->get_value());
  }

  void testIncrementalExpressionVarSignalChange()
  {
    cw::util::ref_ptr<var_e<int> > v = var_e<int>::create(55555);

    cw::util::ref_ptr<fake_container<int> > c = fake_container<int>::create(v);

    std::vector<child_modified_call<int> > expected_calls;
    expected_calls.push_back(child_modified_call<int>(v, 55555, 42));
    expected_calls.push_back(child_modified_call<int>(v, 42, 10));

    v->set_value(42);
    // Test that setting to the same value doesn't emit a signal.
    v->set_value(42);
    v->set_value(10);

    CPPUNIT_ASSERT_EQUAL(expected_calls, c->get_calls());
  }


  void testIncrementalExpressionWeakRefDeref()
  {
    cw::util::ref_ptr<expression<int> > e = var_e<int>::create(5);
    expression_weak_ref<expression<int> > e_ref = e;

    CPPUNIT_ASSERT_EQUAL(e.unsafe_get_ref(), e_ref.get_value());
  }

  void testIncrementalExpressionWeakRefGetValidLive()
  {
    cw::util::ref_ptr<expression<int> > e = var_e<int>::create(5);
    expression_weak_ref<expression<int> > e_ref = e;

    CPPUNIT_ASSERT(e_ref.get_valid());
  }

  void testIncrementalExpressionWeakRefGetValidDead()
  {
    cw::util::ref_ptr<expression<int> > e = var_e<int>::create(5);
    expression_weak_ref<expression<int> > e_ref = e;

    e = cw::util::ref_ptr<expression<int> >();

    CPPUNIT_ASSERT(!e_ref.get_valid());
  }


  void testNotFalse()
  {
    cw::util::ref_ptr<expression<bool> > v = var_e<bool>::create(true);
    cw::util::ref_ptr<expression<bool> > not_v = not_e::create(v);

    CPPUNIT_ASSERT(!not_v->get_value());
  }

  void testNotTrue()
  {
    cw::util::ref_ptr<expression<bool> > v = var_e<bool>::create(false);
    cw::util::ref_ptr<expression<bool> > not_v = not_e::create(v);

    CPPUNIT_ASSERT(not_v->get_value());
  }

  void testNotRaiseInput()
  {
    cw::util::ref_ptr<var_e<bool> > v = var_e<bool>::create(false);
    cw::util::ref_ptr<expression<bool> > not_v = not_e::create(v);

    cw::util::ref_ptr<fake_container<bool> > not_v_wrap =
      fake_container<bool>::create(not_v);

    CPPUNIT_ASSERT(not_v->get_value());
    v->set_value(true);
    CPPUNIT_ASSERT(!not_v->get_value());

    std::vector<child_modified_call<bool> > expected;
    expected.push_back(child_modified_call<bool>(not_v, true, false));

    CPPUNIT_ASSERT_EQUAL(expected, not_v_wrap->get_calls());
  }

  void testNotLowerInput()
  {
    cw::util::ref_ptr<var_e<bool> > v = var_e<bool>::create(true);
    cw::util::ref_ptr<expression<bool> > not_v = not_e::create(v);

    cw::util::ref_ptr<fake_container<bool> > not_v_wrap =
      fake_container<bool>::create(not_v);

    CPPUNIT_ASSERT(!not_v->get_value());
    v->set_value(false);
    CPPUNIT_ASSERT(not_v->get_value());

    std::vector<child_modified_call<bool> > expected;
    expected.push_back(child_modified_call<bool>(not_v, false, true));

    CPPUNIT_ASSERT_EQUAL(expected, not_v_wrap->get_calls());
  }

  void testNotNull()
  {
    cw::util::ref_ptr<expression<bool> > not_null = not_e::create(cw::util::ref_ptr<expression<bool> >());

    CPPUNIT_ASSERT(!not_null->get_value());
  }


  void testAndEmpty()
  {
    cw::util::ref_ptr<expression<bool> > empty[] = { };
    cw::util::ref_ptr<expression<bool> > e = and_e::create(empty, empty);

    CPPUNIT_ASSERT(e->get_value());
  }

private:
  cw::util::ref_ptr<and_e> getAndSingleton(cw::util::ref_ptr<var_e<bool> > v1)
  {
    cw::util::ref_ptr<expression<bool> > subexprs_begin[] = { v1 };
    cw::util::ref_ptr<expression<bool> > *subexprs_end =
      subexprs_begin + sizeof(subexprs_begin) / sizeof(subexprs_begin[0]);

    return and_e::create(subexprs_begin, subexprs_end);
  }

public:
  void testAndSingletonRaise()
  {
    cw::util::ref_ptr<var_e<bool> > v1 = var_e<bool>::create(false);
    cw::util::ref_ptr<and_e> e = getAndSingleton(v1);

    cw::util::ref_ptr<fake_container<bool> > e_wrap =
      fake_container<bool>::create(e);

    CPPUNIT_ASSERT(!e->get_value());
    v1->set_value(true);
    CPPUNIT_ASSERT(e->get_value());

    std::vector<child_modified_call<bool> > expected;
    expected.push_back(child_modified_call<bool>(e, false, true));

    CPPUNIT_ASSERT_EQUAL(expected, e_wrap->get_calls());
  }

  void testAndSingletonLower()
  {
    cw::util::ref_ptr<var_e<bool> > v1 = var_e<bool>::create(true);
    cw::util::ref_ptr<and_e> e = getAndSingleton(v1);

    cw::util::ref_ptr<fake_container<bool> > e_wrap =
      fake_container<bool>::create(e);

    CPPUNIT_ASSERT(e->get_value());
    v1->set_value(false);
    CPPUNIT_ASSERT(!e->get_value());

    std::vector<child_modified_call<bool> > expected;
    expected.push_back(child_modified_call<bool>(e, true, false));

    CPPUNIT_ASSERT_EQUAL(expected, e_wrap->get_calls());
  }

  void testAndSingletonRaiseByRemoving()
  {
    cw::util::ref_ptr<var_e<bool> > v1 = var_e<bool>::create(false);
    cw::util::ref_ptr<and_e> e = getAndSingleton(v1);

    cw::util::ref_ptr<fake_container<bool> > e_wrap =
      fake_container<bool>::create(e);

    CPPUNIT_ASSERT(!e->get_value());
    e->remove_child(v1);
    CPPUNIT_ASSERT(e->get_value());

    std::vector<child_modified_call<bool> > expected;
    expected.push_back(child_modified_call<bool>(e, false, true));

    CPPUNIT_ASSERT_EQUAL(expected, e_wrap->get_calls());
  }

  void testAndSingletonLowerByAppending()
  {
    cw::util::ref_ptr<var_e<bool> > v1 = var_e<bool>::create(true);
    cw::util::ref_ptr<and_e> e = getAndSingleton(v1);

    cw::util::ref_ptr<fake_container<bool> > e_wrap =
      fake_container<bool>::create(e);

    CPPUNIT_ASSERT(e->get_value());
    e->add_child(var_e<bool>::create(false));
    CPPUNIT_ASSERT(!e->get_value());

    std::vector<child_modified_call<bool> > expected;
    expected.push_back(child_modified_call<bool>(e, true, false));

    CPPUNIT_ASSERT_EQUAL(expected, e_wrap->get_calls());
  }

  void testAndSingletonNull()
  {
    cw::util::ref_ptr<and_e> e = getAndSingleton(cw::util::ref_ptr<var_e<bool> >());

    CPPUNIT_ASSERT(e->get_value());
  }

private:
  cw::util::ref_ptr<and_e> getAndDoubleton(const cw::util::ref_ptr<var_e<bool> > &v1,
                                           const cw::util::ref_ptr<var_e<bool> > &v2)
  {
    cw::util::ref_ptr<expression<bool> > subexprs_begin[] = { v1, v2 };
    cw::util::ref_ptr<expression<bool> > *subexprs_end =
      subexprs_begin + sizeof(subexprs_begin) / sizeof(subexprs_begin[0]);

    return and_e::create(subexprs_begin, subexprs_end);
  }

public:
  void testAndDoubletonFirstNull()
  {
    cw::util::ref_ptr<var_e<bool> >
      v1,
      v2 = var_e<bool>::create(true);

    cw::util::ref_ptr<and_e> e = getAndDoubleton(v1, v2);

    CPPUNIT_ASSERT(e->get_value());
  }

  void testAndDoubletonSecondNull()
  {
    cw::util::ref_ptr<var_e<bool> >
      v1 = var_e<bool>::create(true),
      v2;

    cw::util::ref_ptr<and_e> e = getAndDoubleton(v1, v2);

    CPPUNIT_ASSERT(e->get_value());
  }

  void testAndDoubletonBothNull()
  {
    cw::util::ref_ptr<var_e<bool> >
      v1,
      v2;

    cw::util::ref_ptr<and_e> e = getAndDoubleton(v1, v2);

    CPPUNIT_ASSERT(e->get_value());
  }

  void testAndDoubletonRaiseSecond()
  {
    cw::util::ref_ptr<var_e<bool> >
      v1 = var_e<bool>::create(true),
      v2 = var_e<bool>::create(false);
    cw::util::ref_ptr<and_e> e = getAndDoubleton(v1, v2);
    cw::util::ref_ptr<fake_container<bool> > e_wrap =
      fake_container<bool>::create(e);

    CPPUNIT_ASSERT(!e->get_value());
    v2->set_value(true);
    CPPUNIT_ASSERT(e->get_value());

    std::vector<child_modified_call<bool> > expected;
    expected.push_back(child_modified_call<bool>(e, false, true));

    CPPUNIT_ASSERT_EQUAL(expected, e_wrap->get_calls());
  }

  void testAndDoubletonRaiseFirst()
  {
    cw::util::ref_ptr<var_e<bool> >
      v1 = var_e<bool>::create(false),
      v2 = var_e<bool>::create(true);
    cw::util::ref_ptr<and_e> e = getAndDoubleton(v1, v2);
    cw::util::ref_ptr<fake_container<bool> > e_wrap =
      fake_container<bool>::create(e);

    CPPUNIT_ASSERT(!e->get_value());
    v1->set_value(true);
    CPPUNIT_ASSERT(e->get_value());

    std::vector<child_modified_call<bool> > expected;
    expected.push_back(child_modified_call<bool>(e, false, true));

    CPPUNIT_ASSERT_EQUAL(expected, e_wrap->get_calls());
  }

  void testAndDoubletonRaiseFirstNoEffect()
  {
    cw::util::ref_ptr<var_e<bool> >
      v1 = var_e<bool>::create(false),
      v2 = var_e<bool>::create(false);
    cw::util::ref_ptr<and_e> e = getAndDoubleton(v1, v2);
    cw::util::ref_ptr<fake_container<bool> > e_wrap =
      fake_container<bool>::create(e);

    CPPUNIT_ASSERT(!e->get_value());
    v1->set_value(true);
    CPPUNIT_ASSERT(!e->get_value());

    std::vector<child_modified_call<bool> > expected;

    CPPUNIT_ASSERT_EQUAL(expected, e_wrap->get_calls());
  }

  void testAndDoubletonRaiseSecondNoEffect()
  {
    cw::util::ref_ptr<var_e<bool> >
      v1 = var_e<bool>::create(false),
      v2 = var_e<bool>::create(false);
    cw::util::ref_ptr<and_e> e = getAndDoubleton(v1, v2);
    cw::util::ref_ptr<fake_container<bool> > e_wrap =
      fake_container<bool>::create(e);

    CPPUNIT_ASSERT(!e->get_value());
    v2->set_value(true);
    CPPUNIT_ASSERT(!e->get_value());

    std::vector<child_modified_call<bool> > expected;

    CPPUNIT_ASSERT_EQUAL(expected, e_wrap->get_calls());
  }


  void testAndDoubletonLowerFirst()
  {
    cw::util::ref_ptr<var_e<bool> >
      v1 = var_e<bool>::create(true),
      v2 = var_e<bool>::create(true);
    cw::util::ref_ptr<and_e> e = getAndDoubleton(v1, v2);
    cw::util::ref_ptr<fake_container<bool> > e_wrap =
      fake_container<bool>::create(e);

    CPPUNIT_ASSERT(e->get_value());
    v1->set_value(false);
    CPPUNIT_ASSERT(!e->get_value());

    std::vector<child_modified_call<bool> > expected;
    expected.push_back(child_modified_call<bool>(e, true, false));

    CPPUNIT_ASSERT_EQUAL(expected, e_wrap->get_calls());
  }

  void testAndDoubletonLowerSecond()
  {
    cw::util::ref_ptr<var_e<bool> >
      v1 = var_e<bool>::create(true),
      v2 = var_e<bool>::create(true);
    cw::util::ref_ptr<and_e> e = getAndDoubleton(v1, v2);
    cw::util::ref_ptr<fake_container<bool> > e_wrap =
      fake_container<bool>::create(e);

    CPPUNIT_ASSERT(e->get_value());
    v2->set_value(false);
    CPPUNIT_ASSERT(!e->get_value());

    std::vector<child_modified_call<bool> > expected;
    expected.push_back(child_modified_call<bool>(e, true, false));

    CPPUNIT_ASSERT_EQUAL(expected, e_wrap->get_calls());
  }


  void testAndDoubletonLowerFirstNoEffect()
  {
    cw::util::ref_ptr<var_e<bool> >
      v1 = var_e<bool>::create(true),
      v2 = var_e<bool>::create(false);
    cw::util::ref_ptr<and_e> e = getAndDoubleton(v1, v2);
    cw::util::ref_ptr<fake_container<bool> > e_wrap =
      fake_container<bool>::create(e);

    CPPUNIT_ASSERT(!e->get_value());
    v1->set_value(false);
    CPPUNIT_ASSERT(!e->get_value());

    std::vector<child_modified_call<bool> > expected;

    CPPUNIT_ASSERT_EQUAL(expected, e_wrap->get_calls());
  }

  void testAndDoubletonLowerSecondNoEffect()
  {
    cw::util::ref_ptr<var_e<bool> >
      v1 = var_e<bool>::create(false),
      v2 = var_e<bool>::create(true);
    cw::util::ref_ptr<and_e> e = getAndDoubleton(v1, v2);
    cw::util::ref_ptr<fake_container<bool> > e_wrap =
      fake_container<bool>::create(e);

    CPPUNIT_ASSERT(!e->get_value());
    v2->set_value(false);
    CPPUNIT_ASSERT(!e->get_value());

    std::vector<child_modified_call<bool> > expected;

    CPPUNIT_ASSERT_EQUAL(expected, e_wrap->get_calls());
  }




  void testOrEmpty()
  {
    cw::util::ref_ptr<expression<bool> > empty[] = { };
    cw::util::ref_ptr<expression<bool> > e = or_e::create(empty, empty);

    CPPUNIT_ASSERT(!e->get_value());
  }

private:
  cw::util::ref_ptr<or_e> getOrSingleton(cw::util::ref_ptr<var_e<bool> > v1)
  {
    cw::util::ref_ptr<expression<bool> > subexprs_begin[] = { v1 };
    cw::util::ref_ptr<expression<bool> > *subexprs_end =
      subexprs_begin + sizeof(subexprs_begin) / sizeof(subexprs_begin[0]);

    return or_e::create(subexprs_begin, subexprs_end);
  }

public:
  void testOrSingletonRaise()
  {
    cw::util::ref_ptr<var_e<bool> > v1 = var_e<bool>::create(false);
    cw::util::ref_ptr<or_e> e = getOrSingleton(v1);

    cw::util::ref_ptr<fake_container<bool> > e_wrap =
      fake_container<bool>::create(e);

    CPPUNIT_ASSERT(!e->get_value());
    v1->set_value(true);
    CPPUNIT_ASSERT(e->get_value());

    std::vector<child_modified_call<bool> > expected;
    expected.push_back(child_modified_call<bool>(e, false, true));

    CPPUNIT_ASSERT_EQUAL(expected, e_wrap->get_calls());
  }

  void testOrSingletonLower()
  {
    cw::util::ref_ptr<var_e<bool> > v1 = var_e<bool>::create(true);
    cw::util::ref_ptr<or_e> e = getOrSingleton(v1);

    cw::util::ref_ptr<fake_container<bool> > e_wrap =
      fake_container<bool>::create(e);

    CPPUNIT_ASSERT(e->get_value());
    v1->set_value(false);
    CPPUNIT_ASSERT(!e->get_value());

    std::vector<child_modified_call<bool> > expected;
    expected.push_back(child_modified_call<bool>(e, true, false));

    CPPUNIT_ASSERT_EQUAL(expected, e_wrap->get_calls());
  }

  void testOrSingletonLowerByRemoving()
  {
    cw::util::ref_ptr<var_e<bool> > v1 = var_e<bool>::create(true);
    cw::util::ref_ptr<or_e> e = getOrSingleton(v1);

    cw::util::ref_ptr<fake_container<bool> > e_wrap =
      fake_container<bool>::create(e);

    CPPUNIT_ASSERT(e->get_value());
    e->remove_child(v1);
    CPPUNIT_ASSERT(!e->get_value());

    std::vector<child_modified_call<bool> > expected;
    expected.push_back(child_modified_call<bool>(e, true, false));

    CPPUNIT_ASSERT_EQUAL(expected, e_wrap->get_calls());
  }

  void testOrSingletonRaiseByAppending()
  {
    cw::util::ref_ptr<var_e<bool> > v1 = var_e<bool>::create(false);
    cw::util::ref_ptr<or_e> e = getOrSingleton(v1);

    cw::util::ref_ptr<fake_container<bool> > e_wrap =
      fake_container<bool>::create(e);

    CPPUNIT_ASSERT(!e->get_value());
    e->add_child(var_e<bool>::create(true));
    CPPUNIT_ASSERT(e->get_value());

    std::vector<child_modified_call<bool> > expected;
    expected.push_back(child_modified_call<bool>(e, false, true));

    CPPUNIT_ASSERT_EQUAL(expected, e_wrap->get_calls());
  }

  void testOrSingletonNull()
  {
    cw::util::ref_ptr<or_e> e = getOrSingleton(cw::util::ref_ptr<var_e<bool> >());

    CPPUNIT_ASSERT(e->get_value());
  }

private:
  cw::util::ref_ptr<or_e> getOrDoubleton(const cw::util::ref_ptr<var_e<bool> > &v1,
                                         const cw::util::ref_ptr<var_e<bool> > &v2)
  {
    cw::util::ref_ptr<expression<bool> > subexprs_begin[] = { v1, v2 };
    cw::util::ref_ptr<expression<bool> > *subexprs_end =
      subexprs_begin + sizeof(subexprs_begin) / sizeof(subexprs_begin[0]);

    return or_e::create(subexprs_begin, subexprs_end);
  }

public:
  void testOrDoubletonFirstNull()
  {
    cw::util::ref_ptr<var_e<bool> >
      v1,
      v2 = var_e<bool>::create(false);

    cw::util::ref_ptr<or_e> e = getOrDoubleton(v1, v2);

    CPPUNIT_ASSERT(e->get_value());
  }

  void testOrDoubletonSecondNull()
  {
    cw::util::ref_ptr<var_e<bool> >
      v1 = var_e<bool>::create(false),
      v2;

    cw::util::ref_ptr<or_e> e = getOrDoubleton(v1, v2);

    CPPUNIT_ASSERT(e->get_value());
  }

  void testOrDoubletonBothNull()
  {
    cw::util::ref_ptr<var_e<bool> >
      v1,
      v2;

    cw::util::ref_ptr<or_e> e = getOrDoubleton(v1, v2);

    CPPUNIT_ASSERT(e->get_value());
  }

  void testOrDoubletonRaiseFirst()
  {
    cw::util::ref_ptr<var_e<bool> >
      v1 = var_e<bool>::create(false),
      v2 = var_e<bool>::create(false);
    cw::util::ref_ptr<or_e> e = getOrDoubleton(v1, v2);
    cw::util::ref_ptr<fake_container<bool> > e_wrap =
      fake_container<bool>::create(e);

    CPPUNIT_ASSERT(!e->get_value());
    v1->set_value(true);
    CPPUNIT_ASSERT(e->get_value());

    std::vector<child_modified_call<bool> > expected;
    expected.push_back(child_modified_call<bool>(e, false, true));

    CPPUNIT_ASSERT_EQUAL(expected, e_wrap->get_calls());
  }

  void testOrDoubletonRaiseSecond()
  {
    cw::util::ref_ptr<var_e<bool> >
      v1 = var_e<bool>::create(false),
      v2 = var_e<bool>::create(false);
    cw::util::ref_ptr<or_e> e = getOrDoubleton(v1, v2);
    cw::util::ref_ptr<fake_container<bool> > e_wrap =
      fake_container<bool>::create(e);

    CPPUNIT_ASSERT(!e->get_value());
    v2->set_value(true);
    CPPUNIT_ASSERT(e->get_value());

    std::vector<child_modified_call<bool> > expected;
    expected.push_back(child_modified_call<bool>(e, false, true));

    CPPUNIT_ASSERT_EQUAL(expected, e_wrap->get_calls());
  }

  void testOrDoubletonRaiseFirstNoEffect()
  {
    cw::util::ref_ptr<var_e<bool> >
      v1 = var_e<bool>::create(false),
      v2 = var_e<bool>::create(true);
    cw::util::ref_ptr<or_e> e = getOrDoubleton(v1, v2);
    cw::util::ref_ptr<fake_container<bool> > e_wrap =
      fake_container<bool>::create(e);

    CPPUNIT_ASSERT(e->get_value());
    v1->set_value(true);
    CPPUNIT_ASSERT(e->get_value());

    std::vector<child_modified_call<bool> > expected;

    CPPUNIT_ASSERT_EQUAL(expected, e_wrap->get_calls());
  }

  void testOrDoubletonRaiseSecondNoEffect()
  {
    cw::util::ref_ptr<var_e<bool> >
      v1 = var_e<bool>::create(true),
      v2 = var_e<bool>::create(false);
    cw::util::ref_ptr<or_e> e = getOrDoubleton(v1, v2);
    cw::util::ref_ptr<fake_container<bool> > e_wrap =
      fake_container<bool>::create(e);

    CPPUNIT_ASSERT(e->get_value());
    v2->set_value(true);
    CPPUNIT_ASSERT(e->get_value());

    std::vector<child_modified_call<bool> > expected;

    CPPUNIT_ASSERT_EQUAL(expected, e_wrap->get_calls());
  }


  void testOrDoubletonLowerFirst()
  {
    cw::util::ref_ptr<var_e<bool> >
      v1 = var_e<bool>::create(true),
      v2 = var_e<bool>::create(false);
    cw::util::ref_ptr<or_e> e = getOrDoubleton(v1, v2);
    cw::util::ref_ptr<fake_container<bool> > e_wrap =
      fake_container<bool>::create(e);

    CPPUNIT_ASSERT(e->get_value());
    v1->set_value(false);
    CPPUNIT_ASSERT(!e->get_value());

    std::vector<child_modified_call<bool> > expected;
    expected.push_back(child_modified_call<bool>(e, true, false));

    CPPUNIT_ASSERT_EQUAL(expected, e_wrap->get_calls());
  }

  void testOrDoubletonLowerSecond()
  {
    cw::util::ref_ptr<var_e<bool> >
      v1 = var_e<bool>::create(false),
      v2 = var_e<bool>::create(true);
    cw::util::ref_ptr<or_e> e = getOrDoubleton(v1, v2);
    cw::util::ref_ptr<fake_container<bool> > e_wrap =
      fake_container<bool>::create(e);

    CPPUNIT_ASSERT(e->get_value());
    v2->set_value(false);
    CPPUNIT_ASSERT(!e->get_value());

    std::vector<child_modified_call<bool> > expected;
    expected.push_back(child_modified_call<bool>(e, true, false));

    CPPUNIT_ASSERT_EQUAL(expected, e_wrap->get_calls());
  }


  void testOrDoubletonLowerFirstNoEffect()
  {
    cw::util::ref_ptr<var_e<bool> >
      v1 = var_e<bool>::create(true),
      v2 = var_e<bool>::create(true);
    cw::util::ref_ptr<or_e> e = getOrDoubleton(v1, v2);
    cw::util::ref_ptr<fake_container<bool> > e_wrap =
      fake_container<bool>::create(e);

    CPPUNIT_ASSERT(e->get_value());
    v1->set_value(false);
    CPPUNIT_ASSERT(e->get_value());

    std::vector<child_modified_call<bool> > expected;

    CPPUNIT_ASSERT_EQUAL(expected, e_wrap->get_calls());
  }

  void testOrDoubletonLowerSecondNoEffect()
  {
    cw::util::ref_ptr<var_e<bool> >
      v1 = var_e<bool>::create(true),
      v2 = var_e<bool>::create(true);
    cw::util::ref_ptr<or_e> e = getOrDoubleton(v1, v2);
    cw::util::ref_ptr<fake_container<bool> > e_wrap =
      fake_container<bool>::create(e);

    CPPUNIT_ASSERT(e->get_value());
    v2->set_value(false);
    CPPUNIT_ASSERT(e->get_value());

    std::vector<child_modified_call<bool> > expected;

    CPPUNIT_ASSERT_EQUAL(expected, e_wrap->get_calls());
  }
};

CPPUNIT_TEST_SUITE_REGISTRATION(TestIncrementalExpression);
