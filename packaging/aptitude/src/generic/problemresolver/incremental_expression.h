/** \file incremental_expression.h */   // -*-c++-*-

//   Copyright (C) 2009-2010 Daniel Burrows
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


#ifndef BOOL_EXPRESSION_H
#define BOOL_EXPRESSION_H

#include <cwidget/generic/util/eassert.h>
#include <cwidget/generic/util/ref_ptr.h>

#include <generic/util/compare3.h>
#include <generic/util/refcounted_base.h>

#include <algorithm>
#include <set>

#include <ostream>

// A system of incrementally computed expressions stored as a DAG.
// NOT THREADSAFE (the weak-reference system would utterly break in
// the presence of threads without a lot of expensive locking, and
// inside the resolver we don't need it).
//
// \todo Support delayed propagation of updates? (basically using a
// queue -- this would avoid excessive recursion, and also avoid
// having an unresponsive UI if there's a lot of stuff to update)
// Maybe have a propagate() method on a generic (non-templated) base
// class and have the user pass in a deque of pointers to that class
// when signaling updates or modifying variable values.  propagate()
// would also accept a deque, to totally flatten out the computation.

template<typename T>
class expression;

template<typename T>
class expression_weak_ref;

// Generic base class so arbitrary incoming weak references can be
// invalidated by the expression type.
class expression_weak_ref_generic
{
  // This should be a friend of the weak_ref below, but I don't
  // remember how to make friends and templates get along; making its
  // private parts protected instead.
protected:
  bool valid;

  expression_weak_ref_generic(bool _valid)
    : valid(_valid)
  {
  }

public:
  expression_weak_ref_generic()
    : valid(false)
  {
  }

  bool get_valid() const
  {
    return valid;
  }

  // This should be private, but it has to be exposed to a templated
  // class (expression<T>).
  void invalidate()
  {
    valid = false;
  }
};

/** \brief A weak reference to something derived from "expression".
 *
 *  \tparam T   The type that is encapsulated (should inherit from "expression").
 */
template<typename T>
class expression_weak_ref : public expression_weak_ref_generic
{
  T *expr;

public:
  expression_weak_ref()
    : expr(NULL)
  {
  }

  expression_weak_ref(T *_expr);
  expression_weak_ref(const cwidget::util::ref_ptr<T> &_expr);

  expression_weak_ref(const expression_weak_ref &other);

  expression_weak_ref &operator=(const expression_weak_ref &other);

  T *get_value() const
  {
    eassert(valid);

    return expr;
  }

  bool operator==(const expression_weak_ref &other) const
  {
    return expr == other.expr;
  }

  bool operator==(const T *other) const
  {
    return expr == other;
  }

  int compare(const expression_weak_ref &other) const
  {
    return aptitude::util::compare3(expr, other.expr);
  }

  bool operator<(const expression_weak_ref &other) const
  {
    return expr < other.expr;
  }

  ~expression_weak_ref();
};

namespace aptitude
{
  namespace util
  {
    template<typename T>
    class compare3_f<expression_weak_ref<T> >
    {
    public:
      int operator()(const expression_weak_ref<T> &r1,
		     const expression_weak_ref<T> &r2) const
      {
	return r1.compare(r2);
      }
    };
  }
}

template<typename T>
class expression_container;

/** \brief An expression whose value can be computed incrementally
 *  and updated in its parents.
 *
 *  \typeparam T   The type of value computed by this expression;
 *                 should be copy-constructable and equality-comparable.
 */
template<typename T>
class expression : public aptitude::util::refcounted_base_not_threadsafe
{
  // Weak references to parents.
  std::set<expression_weak_ref<expression_container<T> > > parents;

  // Incoming weak references.
  std::set<expression_weak_ref_generic *> weak_refs;

  // These two routines should be private, but they need to be exposed
  // to a templated class (expression_weak_ref<T>).
public:
  void add_weak_ref(expression_weak_ref_generic *ref)
  {
    weak_refs.insert(ref);
  }

  void remove_weak_ref(expression_weak_ref_generic *ref)
  {
    weak_refs.erase(ref);
  }

protected:
  void signal_value_changed(T old_value, T new_value)
  {
    cwidget::util::ref_ptr<expression> self(this);

    // Strip out dead references as we go, to save a bit of space.
    typename std::set<expression_weak_ref<expression_container<T> > >::iterator
      curr = parents.begin();

    while(curr != parents.end())
      {
	typename std::set<expression_weak_ref<expression_container<T> > >::iterator
	  next = curr;
	++next;

	if(curr->get_valid())
	  curr->get_value()->child_modified(self, old_value, new_value);
	else
	  parents.erase(curr);

	curr = next;
      }
  }

public:
  virtual ~expression()
  {
    for(typename std::set<expression_weak_ref_generic *>::const_iterator it =
	  weak_refs.begin(); it != weak_refs.end(); ++it)
      (*it)->invalidate();

    weak_refs.clear(); // Not strictly necessary, but avoids potential
                       // surprises.
  }

  // Add a weak reference to the given expression; its
  // child_modified() routine will be invoked when this child's value
  // changes.
  void add_parent(expression_container<T> *parent)
  {
    if(parent != NULL)
      parents.insert(parent);
  }

private:
  class parent_equals_weak_ref
  {
    const expression_container<T> *parent;

  public:
    parent_equals_weak_ref(const expression_container<T> *_parent)
      : parent(_parent)
    {
    }

    bool operator()(const expression_weak_ref<expression_container<T> > &other) const
    {
      return other == parent;
    }
  };

public:
  void remove_parent(expression_container<T> *parent)
  {
    parents.erase(expression_weak_ref<expression_container<T> >(parent));
  }

  virtual T get_value() = 0;
  virtual void dump(std::ostream &out) = 0;
};

/** \brief Base class for expressions that can contain other
 *  expressions as children.
 */
template<typename T>
class expression_container : public expression<T>
{
public:
  /** \brief Invoked when the value of a child has changed from
   * old_value to new_value.
   *
   *  When this method is called, the value really has changed (i.e.,
   *  old_value does not equal new_value).
   */
  virtual void child_modified(const cwidget::util::ref_ptr<expression<T> > &child,
			      T old_value,
			      T new_value) = 0;
};

/** \brief Base class for objects that have a single sub-expression. */
template<typename T>
class expression_box : public expression_container<T>
{
  cwidget::util::ref_ptr<expression<T> > child;

public:
  expression_box() : child() { }

  expression_box(const cwidget::util::ref_ptr<expression<T> > &_child)
    : child(_child)
  {
    if(child.valid())
      child->add_parent(this);
  }

  expression_box(const expression_box &other)
    : child(other.child)
  {
    if(child.valid())
      child->add_parent(this);
  }

  ~expression_box()
  {
    if(child.valid())
      child->remove_parent(this);
  }

  void set_child(const cwidget::util::ref_ptr<expression<T> > &new_child)
  {
    if(child.valid())
      child->remove_parent(this);
    child = new_child;
    if(new_child.valid())
      new_child->add_parent(this);
  }

  /** \brief Set the child of this box to the child of the other box.
   */
  expression_box &operator=(const expression_box &other)
  {
    set_child(other.child);
    return *this;
  }

  const cwidget::util::ref_ptr<expression<T> > &get_child() const
  {
    return child;
  }

  /** \brief Returns the child's value, or a default-constructed T if
   *  there is no child.
   */
  T get_value()
  {
    if(child.valid())
      return child->get_value();
    else
      return T();
  }

  /** \brief Write this expression to the given stream.
   *
   *  The default implementation writes nothing if the child is
   *  invalid, and writes the child if the child is valid.
   */
  void dump(std::ostream &out)
  {
    if(get_child().valid())
      get_child()->dump(out);
  }
};

/** \brief Base class for expressions that trivially wrap their
 *  subexpression.
 */
template<typename T>
class expression_wrapper : public expression_box<T>
{
public:
  expression_wrapper() : expression_box<T>() { }
  expression_wrapper(const cwidget::util::ref_ptr<expression<T> > &child)
    : expression_box<T>(child)
  {
  }
  expression_wrapper(const expression_wrapper &other)
    : expression_box<T>(other)
  {
  }

  expression_wrapper &operator=(const expression_wrapper &other)
  {
    set_child(other.get_child());
    return *this;
  }

  /** \brief Invoked by the default implementation of child_modified.
   *
   *  This method's default implementation does nothing.
   */
  virtual void changed(T new_value)
  {
  }

  void child_modified(const cwidget::util::ref_ptr<expression<T> > &child,
		      T old_value,
		      T new_value)
  {
    changed(new_value);
  }		      
};

/** \brief Base class for N-ary containers that support adding and
 *  removing children.
 */
template<typename T>
class expression_container_base : public expression_container<T>
{
  // Strong references to children.
  std::vector<cwidget::util::ref_ptr<expression<T> > > children;

public:
  template<typename Iter>
  expression_container_base(Iter begin, Iter end)
    : children(begin, end)
  {
    for(typename std::vector<cwidget::util::ref_ptr<expression<T> > >::const_iterator
	  it = get_children().begin(); it != get_children().end(); ++it)
      if(it->valid())
        (*it)->add_parent(this);
  }

  const std::vector<cwidget::util::ref_ptr<expression<T> > > &get_children() const
  {
    return children;
  }

  /** \brief Add a new child to this container. */
  virtual void add_child(const cwidget::util::ref_ptr<expression<T> > &new_child)
  {
    children.push_back(new_child);
  }

  /** \brief Remove one copy of a child from this container. */
  virtual void remove_child(const cwidget::util::ref_ptr<expression<T> > &child)
  {
    if(child.valid())
      child->remove_parent(this);

    typename std::vector<cwidget::util::ref_ptr<expression<T> > >::iterator
      found = std::find(children.begin(), children.end(), child);

    if(found != children.end())
      children.erase(found);
  }

  virtual std::string get_name() = 0;

  void dump(std::ostream &out)
  {
    out << get_name() << "(";
    for(typename std::vector<cwidget::util::ref_ptr<expression<T> > >::const_iterator
	  it = get_children().begin(); it != get_children().end(); ++it)
      {
	if(it != get_children().begin())
	  out << ", ";

        if(it->valid())
          (*it)->dump(out);
        else
          out << "(null)";
      }
    out << ")";
  }
};

template<typename T>
expression_weak_ref<T>::expression_weak_ref(T *_expr)
  : expression_weak_ref_generic(true), expr(_expr)
{
  expr->add_weak_ref(this);
}

template<typename T>
expression_weak_ref<T>::expression_weak_ref(const cwidget::util::ref_ptr<T> &_expr)
  : expression_weak_ref_generic(true), expr(_expr.unsafe_get_ref())
{
  expr->add_weak_ref(this);
}

template<typename T>
expression_weak_ref<T>::expression_weak_ref(const expression_weak_ref<T> &other)
  : expression_weak_ref_generic(other.get_valid()), expr(other.expr)
{
  if(valid)
    expr->add_weak_ref(this);
}

template<typename T>
expression_weak_ref<T> &expression_weak_ref<T>::operator=(const expression_weak_ref<T> &other)
{
  if(valid)
    expr->remove_weak_ref(this);

  valid = other.valid;
  expr = other.expr;

  if(valid)
    expr->add_weak_ref(this);

  return *this;
}

template<typename T>
expression_weak_ref<T>::~expression_weak_ref()
{
  if(valid)
    expr->remove_weak_ref(this);
}

/** \brief Represents a variable in the expression language.
 *
 *  Variables can be modified arbitrarily; changes are immediately
 *  propagated to parent expressions.
 *
 *  It would be nice if the user could attach names for better
 *  printing of expressions, but that would take a lot of memory.
 */
template<typename T>
class var_e : public expression<T>
{
  T value;

protected:
  var_e(T _value) : value(_value)
  {
  }

public:
  static cwidget::util::ref_ptr<var_e> create(T value)
  {
    return new var_e(value);
  }

  T get_value()
  {
    return value;
  }

  /** \brief Set the value of this expression to new_value,
   *  and propagate it to our parent if necessary.
   */
  void set_value(T new_value)
  {
    if(value != new_value)
      {
	T old_value = value;
	value = new_value;
	this->signal_value_changed(old_value, new_value);
      }
  }

  void dump(std::ostream &out)
  {
    out << "v" << this;
  }
};

/** \brief Boolean-specific expressions. */

// @{

// Base class for Boolean expressions that use the number of their
// sub-parts that are "true" to determine their own value.
class counting_bool_e : public expression_container_base<bool>
{
  std::vector<cwidget::util::ref_ptr<expression<bool> > >::size_type num_true;

  // Invoked from the constructor; counts how many of the children are
  // true.
  void init_num_true();
public:
  template<typename Iter>
  counting_bool_e(Iter begin, Iter end)
    : expression_container_base<bool>(begin, end), num_true(0)
  {
    init_num_true();
  }

  std::vector<cwidget::util::ref_ptr<expression<bool> > >::size_type
  get_num_true() const
  {
    return num_true;
  }

  void child_modified(const cwidget::util::ref_ptr<expression<bool> > &child,
		      bool old_value,
		      bool new_value);

  void add_child(const cwidget::util::ref_ptr<expression<bool> > &new_child);
  void remove_child(const cwidget::util::ref_ptr<expression<bool> > &child);
};

class and_e : public counting_bool_e
{
  template<typename Iter>
  and_e(Iter begin, Iter end)
    : counting_bool_e(begin, end)
  {
  }

public:
  template<typename Iter>
  static cwidget::util::ref_ptr<and_e>
  create(Iter begin, Iter end)
  {
    return new and_e(begin, end);
  }

  static cwidget::util::ref_ptr<and_e>
  create(const cwidget::util::ref_ptr<expression<bool> > &e1,
	 const cwidget::util::ref_ptr<expression<bool> > &e2)
  {
    cwidget::util::ref_ptr<expression<bool> > tmp[2];
    tmp[0] = e1;
    tmp[1] = e2;

    return create(tmp, tmp + 2);
  }

  bool get_value();
  std::string get_name();
};

class or_e : public counting_bool_e
{
  template<typename Iter>
  or_e(Iter begin, Iter end)
    : counting_bool_e(begin, end)
  {
  }

public:
  template<typename Iter>
  static cwidget::util::ref_ptr<or_e>
  create(Iter begin, Iter end)
  {
    return new or_e(begin, end);
  }

  static cwidget::util::ref_ptr<or_e>
  create(const cwidget::util::ref_ptr<expression<bool> > &e1,
	 const cwidget::util::ref_ptr<expression<bool> > &e2)
  {
    cwidget::util::ref_ptr<expression<bool> > tmp[2];
    tmp[0] = e1;
    tmp[1] = e2;

    return create(tmp, tmp + 2);
  }

  bool get_value();
  std::string get_name();
};

class not_e : public expression_box<bool>
{
  not_e(const cwidget::util::ref_ptr<expression<bool> > &_child)
    : expression_box<bool>(_child)
  {
  }

public:
  static cwidget::util::ref_ptr<not_e>
  create(const cwidget::util::ref_ptr<expression<bool> > &child)
  {
    return new not_e(child);
  }

  void child_modified(const cwidget::util::ref_ptr<expression<bool> > &child,
		      bool old_value,
		      bool new_value);
  bool get_value();
  void dump(std::ostream &out);
};

template<typename T>
std::ostream &operator<<(std::ostream &out,
			 const cwidget::util::ref_ptr<expression<T> > &o)
{
  if(o.valid())
    o->dump(out);
  else
    out << "(null)";

  return out;
}

// @}

#endif
