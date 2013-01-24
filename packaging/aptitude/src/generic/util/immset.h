// immset.h                                     -*-c++-*-
//
//   Copyright (C) 2005-2006, 2008-2010 Daniel Burrows
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
//

#ifndef IMMSET_H
#define IMMSET_H

#include <cwidget/generic/util/eassert.h>
#include <iostream>

#include <vector>

#include <boost/compressed_pair.hpp>

#include "compare3.h"

/** \brief A class to represent immutable sets
 *
 * 
 *  This file defines a class to represent immutable sets.  These sets
 *  behave like std::set, except that their contents cannot be changed;
 *  they have no erase() or insert() operators, and only
 *  const_iterators [0].  This restriction allows immutable sets to be
 *  implemented in a way that makes both copying and creating a new set
 *  by adding an element very efficient (O(1) and O(lg n) respectively;
 *  with std::set these are both O(n)).
 *  
 *  [0] this refers to the wtree_node class; imm::set is a wrapper
 *  around a wtree_node pointer that emulates a mutative interface by
 *  creating a new tree and modifying its encapsulated pointer to point
 *  there instead.
 * 
 *  \file immset.h
 */

namespace imm
{
  /** \brief A class used in metaprogramming for imm::set.
   */
  class nil_t {};

  template<typename Val, typename AccumVal, typename AccumOps, int w>
  class wtree_node;

  /** \brief Helper function used below to accumulate information up
   *  the tree.
   *
   *  \tparam AccumOps The class carrying information about how to
   *  accumulate values up the tree; if this is nil_t, then this
   *  function is a NOP.
   *
   *  \todo Isn't there a better way of getting this behavior?  I
   *  tried partially specializing a private member function, but ran
   *  into syntactic trouble.
   */
  template<typename Val, typename AccumVal, typename AccumOps, int w>
  class doAccumulate
  {
  public:
    static AccumVal
    call(const Val &val,
	 const wtree_node<Val, AccumVal, AccumOps, w> &left,
	 const wtree_node<Val, AccumVal, AccumOps, w> &right,
	 const AccumOps &accumOps);
  };

  /** A generic node in a weighted tree as described in "Implementing
   *  Sets Efficiently In A Functional Language".  The tree invariant
   *  is that the tree is not "too unbalanced"; in this case, that no
   *  subtree is more than 4 times larger than its sibling.  (note
   *  that w must be at least 3.75; see the paper for details)
   *
   *  A brief note on choice of algorithm: while a rbtree is more
   *  common and may be slightly more efficient, it is a much trickier
   *  data structure to implement.
   *
   *  The weighted-tree data structure is reasonably efficient and
   *  much more straightforward to implement correctly.
   *
   *
   *  In addition to the above notes, this class can be used to
   *  iteratively maintain the result of accumulating an associative
   *  and commutative operation across the set.
   *
   *  \tparam Val  The type of value stored in this set.
   *
   *  \tparam AccumVal The type of value being accumulated at each
   *  node, or nil_t for no accumulation.
   *
   *  \tparam AccumOps If AccumVal is non-nil, this is the type used to
   *  compute accumulated values.  Must be a class type supporting
   *  three operations: empty(), project(Val) and merge(AccumVal,
   *  AccumVal), where empty() produces an AccumVal corresponding to
   *  an empty set, project() computes the AccumVal associated with a
   *  single value, and merge() combines two AccumVals.  merge() must
   *  be associative and commutative.
   *
   *  \tparam the weighting value of the tree; must be at least 3.75.
   */
  template<typename Val, typename AccumVal = nil_t, typename AccumOps = nil_t, const int w = 4>
  class wtree_node
  {
    // Type hack to let us dump trees containing pairs without
    // operator<< (needed for map bindings).
    template<typename T>
    struct dumper
    {
      static void dump(std::ostream &out, const T &t)
      {
	out << t;
      }
    };

    template<typename A, typename B>
    struct dumper<std::pair<A, B> >
    {
      static void dump(std::ostream &out, const std::pair<A, B> &pair)
      {
	out << "(" << pair.first << ", " << pair.second << ")";
      }
    };

    class impl
    {
      typedef unsigned int size_type;

      /** Left and right children (may be \b null). */
      wtree_node left, right;

      /** The size of the subtree rooted at this node, paired with any
       *  accumulated information specified by the user.
       *
       *  Using compressed_pair allows us to avoid allocating space if
       *  the accumulated value is empty, without having to contain an
       *  implementation of empty member elision.
       */
      boost::compressed_pair<size_type, AccumVal> sizeAndAccumVal;

      /** The reference-count of this node. */
      mutable int refcount;

      /** The enclosed value. */
      Val val;

    public:
      impl(const Val &_val,
	   const wtree_node &_left, const wtree_node &_right,
	   const AccumOps &ops)
	:left(_left), right(_right),
	 sizeAndAccumVal(_left.size() + _right.size() + 1,
			 doAccumulate<Val, AccumVal, AccumOps, w>::call(_val, _left, _right, ops)),
	 refcount(1), val(_val)
      {
      }

      impl *clone(const AccumOps &ops) const
      {
	return new impl(val, left.clone(ops), right.clone(ops), ops);
      }

      /** \return the left child. */
      const wtree_node &getLeftChild() const
      {
	return left;
      }

      /** \return the right child. */
      const wtree_node &getRightChild() const
      {
	return right;
      }

      size_type getSize() const
      {
	return sizeAndAccumVal.first();
      }

      const Val &getVal() const
      {
	return val;
      }

      void incref() const
      {
	//eassert(refcount>0);

	++refcount;
      }

      void decref() const
      {
	//eassert(refcount>0);
	--refcount;

	if(refcount == 0)
	  delete this;
      }


      const AccumVal &getAccumVal() const { return sizeAndAccumVal.second(); }
    };

    const impl *realNode;

  public:
    typedef unsigned int size_type;

    wtree_node(const Val &val,
	       const wtree_node &left, const wtree_node &right,
	       const AccumOps &accumOps)
      :realNode(new impl(val, left, right, accumOps))
    {
    }

    wtree_node(const Val &val,
	       const AccumOps &accumOps)
      :realNode(new impl(val, wtree_node(), wtree_node(),
			 accumOps))
    {
    }

    /** Takes possession of the given reference (the caller should
     *  incref() if necessary).
     */
    wtree_node(const impl *_realNode)
      :realNode(_realNode)
    {
    }

    wtree_node()
      :realNode(NULL)
    {
    }

    wtree_node(const wtree_node &other)
      :realNode(other.realNode)
    {
      if(realNode != NULL)
	realNode->incref();
    }

    ~wtree_node()
    {
      if(realNode != NULL)
	realNode->decref();
    }

    wtree_node getLeft() const
    {
      return realNode->getLeftChild();
    }

    wtree_node getRight() const
    {
      return realNode->getRightChild();
    }

    size_type size() const
    {
      if(realNode == NULL)
	return 0;
      else
	return realNode->getSize();
    }

    wtree_node &operator=(const wtree_node &other)
    {
      if(other.realNode != NULL)
	other.realNode->incref();
      if(realNode != NULL)
	realNode->decref();

      realNode = other.realNode;

      return *this;
    }

    /** Pointer comparison. */
    bool operator==(const wtree_node &other) const
    {
      return realNode == other.realNode;
    }

    /** Pointer comparison. */
    bool operator!=(const wtree_node &other) const
    {
      return realNode != other.realNode;
    }

    bool empty() const
    {
      return realNode == NULL;
    }

    bool isValid() const
    {
      return realNode != NULL;
    }

    /** \return the value of this node. */
    const Val &getVal() const
    {
      return realNode->getVal();
    }

    /** \return the accumulated value of this node.
     *
     *  Only allowed if AccumVal is non-nil and isValid() is truea.
     */
    const AccumVal &getAccumVal() const
    {
      return realNode->getAccumVal();
    }

    // Tree management routines:

    /** Perform a 'left rotate' operation on this node.  Requires
     *  that the right child is not \b null.
     */
    wtree_node left_rotate_single(const AccumOps &accumOps) const
    {
      wtree_node right = getRight(), left = getLeft();
      wtree_node right_left = right.getLeft(), right_right = right.getRight();

      return wtree_node(right.getVal(),
			wtree_node(getVal(), left, right_left, accumOps),
			right_right,
			accumOps);
    }

    /** Perform a 'right rotate' operation on this node.  Requires
     *  that the left child is not \b null.
     */
    wtree_node right_rotate_single(const AccumOps &accumOps) const
    {
      wtree_node right = getRight(), left = getLeft();
      wtree_node left_left = left.getLeft(), left_right = left.getRight();

      return wtree_node(left.getVal(),
			left_left,
			wtree_node(getVal(), left_right, right, accumOps),
			accumOps);
    }

    /** Perform a 'double left rotate' operation on this node.
     *  Requires that the right child not be \b null and that
     *  its left child is also not \b null.
     */
    wtree_node left_rotate_double(const AccumOps &accumOps) const
    {
      wtree_node right = getRight(), left = getLeft();
      wtree_node right_right = right.getRight(), right_left = right.getLeft();
      wtree_node right_left_left = right_left.getLeft();
      wtree_node right_left_right = right_left.getRight();

      return wtree_node(right_left.getVal(),
			wtree_node(getVal(), left, right_left_left, accumOps),
			wtree_node(right.getVal(), right_left_right,
				   right_right, accumOps),
			accumOps);
    }

    /** Perform a 'double right rotate' operation on this node.
     *  Requires that the left child not be \b null and that
     *  its right child is also not \b null.
     */
    wtree_node right_rotate_double(const AccumOps &accumOps) const
    {
      wtree_node right = getRight(), left = getLeft();
      wtree_node left_right = left.getRight(), left_left = left.getLeft();
      wtree_node left_right_left = left_right.getLeft();
      wtree_node left_right_right = left_right.getRight();

      return wtree_node(left_right.getVal(),
			wtree_node(left.getVal(), left_left, left_right_left,
				   accumOps),
			wtree_node(getVal(), left_right_right, right,
				   accumOps),
			accumOps);
    }




    /** Rebalance the given subtree, returning a new subtree
     *  reference.  The subtree should be unbalanced by at most one
     *  element (think inserting or deleting a single element).
     *  Equivalent to T' in the paper.
     */ 
    wtree_node rebalance(const AccumOps &accumOps) const
    {
      wtree_node left = getLeft(), right = getRight();
      size_type left_size = left.size();
      size_type right_size = right.size();

      // If one subtree is empty and the other contains at most one
      // element, there is nothing to do.
      if(left_size + right_size < 2)
	return *this;
      else if(left_size * w < right_size)
	{
	  // The right tree is too heavy.  As explained in the paper,
	  // a single rotation is guaranteed sufficient if its outer
	  // (right) child is larger than its inner child; otherwise a
	  // double rotation is guaranteed sufficient.
	  wtree_node right_left = right.getLeft(), right_right = right.getRight();
	  if(right_left.size() < right_right.size())
	    return left_rotate_single(accumOps);
	  else
	    return left_rotate_double(accumOps);
	}
      else if(right_size * w < left_size)
	{
	  // Dual of above.
	  wtree_node left_left = left.getLeft(), left_right = left.getRight();
	  if(left_right.size() < left_left.size())
	    return right_rotate_single(accumOps);
	  else
	    return right_rotate_double(accumOps);
	}
      else
	// Nothing to do; the tree is already balanced.
	return *this;
    }

    /** Apply the given operator to each value in the tree in
     *  order.
     *
     *  If the operator returns false, the traversal is aborted.
     */
    template<typename Op>
    bool for_each(const Op &o) const
    {
      if(isValid())
	{
	  if(!realNode->getLeftChild().for_each(o))
	    return false;

	  if(!o(getVal()))
	    return false;

	  if(!realNode->getRightChild().for_each(o))
	    return false;
	}

      return true;
    }

    void dump(std::ostream &out,
	      int indent = 2,
	      int level = 0,
	      int mode = -1) const
    {
      if(empty())
	return;

      for(int i = 0; i < indent; ++i)
	for(int j = 0; j < level; ++j)
	  out << " ";

      if(mode == 0)
	out << "L-> ";
      else if(mode == 1)
	out << "R-> ";

      dumper<Val>::dump(out, getVal());
      out << std::endl;

      getLeft().dump(out, indent, level+1, 0);
      getRight().dump(out, indent, level+1, 1);
    }

    /** Return a new tree that does not share its structure with any
     *  other tree.
     */
    wtree_node clone(const AccumOps &ops) const
    {
      if(empty())
	return wtree_node();
      else
	return wtree_node(realNode->clone(ops));
    }
  };

  template<typename Val, typename AccumVal, typename AccumOps, int w>
  inline AccumVal
  doAccumulate<Val, AccumVal, AccumOps, w>::call(const Val &val,
						 const wtree_node<Val, AccumVal, AccumOps, w> &left,
						 const wtree_node<Val, AccumVal, AccumOps, w> &right,
						 const AccumOps &accumOps)
  {
    if(left.isValid() && right.isValid())
      {
	const AccumVal valAndRight =
	  accumOps.merge(accumOps.project(val),
			 right.getAccumVal());
	return accumOps.merge(left.getAccumVal(),
			      valAndRight);
      }
    else if(left.isValid())
      return accumOps.merge(left.getAccumVal(),
			    accumOps.project(val));
    else if(right.isValid())
      return accumOps.merge(accumOps.project(val),
			    right.getAccumVal());
    else
      return accumOps.project(val);
  }

  template<typename Val, typename AccumVal, int w>
  class doAccumulate<Val, AccumVal, nil_t, w>
  {
  public:
    static inline AccumVal
    call(const Val &val,
	 const wtree_node<Val, AccumVal, nil_t, w> &left,
	 const wtree_node<Val, AccumVal, nil_t, w> &right,
	 const nil_t &accumOps)
    {
      return AccumVal();
    }
  };


  /** An entire weighted tree.
   *
   *  \tparam AccumVal if non-nil, a value of this type will be
   *  computed that represents the combination of all the values in
   *  the tree under AccumOps.
   *
   *  \tparam AccumOps A class type that allows values to be combined;
   *  documented under wtree_node.
   */
  template<typename Val, typename Compare = aptitude::util::compare3_f<Val>,
	   typename AccumVal = nil_t, typename AccumOps = nil_t, int w = 4 >
  class set
  {
  public:
    typedef Val value_type;
    typedef wtree_node<Val, AccumVal, AccumOps, w> node;
    typedef typename node::size_type size_type;

    /** An iterator over a wtree.  Note that the lack of parent
     *  pointers (necessary to allow full memory sharing) forces the
     *  iterator class to allocate!  I don't recommend using iterators
     *  except for the purpose of spitting the tree out for debugging.
     */
    class const_iterator
    {
      /** If the first entry of the pair is \b true, then we have not
       *  yet descended into the right subtree of this entry.  For
       *  nodes other than the las tone, if the first entry is \b
       *  false, then when the node becomes the last node again its
       *  right child will be entered; otherwise the node itself will
       *  be visited.
       */
      typedef std::pair<bool, node > path_entry;

      std::vector<path_entry> path;
    public:
      typedef std::input_iterator_tag iterator_category;
      typedef Val value_type;
      typedef int difference_type;
      typedef const Val* pointer;
      typedef const Val& reference;

      const_iterator()
      {
      }

      const_iterator(const node &root)
      {
	if(root.isValid())
	  {
	    path.push_back(path_entry(false, root));
	    while(path.back().second.getLeft().isValid())
	      path.push_back(path_entry(false, path.back().second.getLeft()));
	  }
      }

      const Val &operator*() const
      {
	return path.back().second.getVal();
      }

      const Val *operator->() const
      {
	return &path.back().second.getVal();
      }

      const_iterator &operator=(const const_iterator &other)
      {
	path = other.path;
	return *this;
      }

      bool operator==(const const_iterator &other) const
      {
	return path == other.path;
      }

      bool operator!=(const const_iterator &other) const
      {
	return path != other.path;
      }

      const_iterator &operator++()
      {
	eassert(!path.empty());

	if(!path.back().first)
	  {
	    path.back().first = true;
	    path.push_back(path_entry(false, path.back().second.getRight()));

	    while(!path.empty() && path.back().second.isValid())
	      path.push_back(path_entry(false, path.back().second.getLeft()));
	  }

	// Clear out any invalid nodes or nodes that already fired.
	while(!path.empty() && (!path.back().second.isValid() || path.back().first))
	  path.pop_back();

	// Now either the path is empty, or we're at a node that's
	// valid and hasn't fired yet (meaning we just finished
	// descending into its left subtree, so we should stop and
	// visit it).

	return *this;
      }
    };
  private:
    // Save space by throwing out empty comparison operators and
    // accumulation operators.
    class contents
    {
      boost::compressed_pair<Compare, boost::compressed_pair<AccumOps, node> >
      rootAndParameters;

    public:
      contents(const node &root,
	       const Compare &value_compare,
	       const AccumOps &accumOps)
	: rootAndParameters(value_compare,
			    boost::compressed_pair<AccumOps, node>(accumOps, root))
      {
      }

      const Compare &get_value_compare() const { return rootAndParameters.first(); }
      const AccumOps &get_accumOps() const { return rootAndParameters.second().first(); }
      const node &get_root() const { return rootAndParameters.second().second(); }
      void set_root(const node &n) { rootAndParameters.second().second() = n; }
    };

    contents impl;

    /** Returns a balanced tree constructed by adding x to n.
     *
     *  \param added_anything Set to \b false if x already
     *  exists in this set.
     */
    node add(const node &n, const Val &x, bool &added_anything) const
    {
      if(n.empty())
	return node(x, node(), node(), impl.get_accumOps());
      else
	{
	  int cmp = impl.get_value_compare()(x, n.getVal());

	  if(cmp < 0)
	    return node(n.getVal(),
			add(n.getLeft(), x, added_anything),
			n.getRight(),
			impl.get_accumOps()).rebalance(impl.get_accumOps());
	  else if(cmp > 0)
	    return node(n.getVal(),
			n.getLeft(),
			add(n.getRight(), x, added_anything),
			impl.get_accumOps()).rebalance(impl.get_accumOps());
	  else
	    {
	      added_anything = false;
	      return n;
	    }
	}
    }

    /** Returns a balanced tree constructed by adding x to n.  Will
     *  replace existing nodes equivalent to x.
     *
     *  \param added_new_entry   Set to \b false if the key already existed
     *  in this set.
     */
    node addUpdate(const node &n, const Val &x, bool &added_new_entry) const
    {
      if(n.empty())
        return node(x, node(), node(), impl.get_accumOps());
      else
	{
	  int cmp = impl.get_value_compare()(x, n.getVal());

	  if(cmp < 0)
	    return node(n.getVal(),
			addUpdate(n.getLeft(), x, added_new_entry),
			n.getRight(),
			impl.get_accumOps()).rebalance(impl.get_accumOps());
	  else if(cmp > 0)
	    return node(n.getVal(),
			n.getLeft(),
			addUpdate(n.getRight(), x, added_new_entry),
			impl.get_accumOps()).rebalance(impl.get_accumOps());
	  else
	    {
	      added_new_entry = false;
	      return node(x, n.getLeft(), n.getRight(), impl.get_accumOps());
	    }
	}
    }

    /** Given a node, find and return its minimum element while
     *  simultaneously constructing a new (balanced) node that doesn't
     *  contain the minimum.
     */
    static std::pair<node, Val> find_and_remove_min(const node &n,
						    const AccumOps &accumOps)
    {
      if(n.getLeft().isValid())
      {
	std::pair<node, Val> tmp = find_and_remove_min(n.getLeft(), accumOps);
	  return std::pair<node, Val>(node(n.getVal(),
					   tmp.first, n.getRight(),
					   accumOps).rebalance(accumOps),
				      tmp.second);
      }
      else
      {
	return std::pair<node, Val>(n.getRight(), n.getVal());
      }
    }

    /** Join together two trees; every element of l must be less than
     *  every element of r.
     */
    static node splice_trees(const node &l, const node &r,
			     const AccumOps &accumOps)
    {
      if(r.empty())
        return l;
      else if(l.empty())
        return r;

      std::pair<node, Val> tmp = find_and_remove_min(r, accumOps);
      return node(tmp.second, l, tmp.first, accumOps);
    }

    /** \return \b true if there exist elements x1 and x2 in n1 and n2
     *  respectively such that x1 and x2 are equivalent and f(x1, x2)
     *  holds.  If f is \b lambda x1 x2 . \b true, then this routine
     *  simply tests whether the sets represented by n1 and n2
     *  intersect.
     */
    template<typename F>
    bool nodes_intersect(const node &n1, const node &n2, const F &f) const
    {
      // This algorithm is simple and obviously right, but not
      // terribly efficient; it is a candidate for optimization if it
      // becomes a bottleneck.
      if(n1.empty())
        return false;
      else if(n2.empty())
        return false;
      else
	{
	  int cmp = impl.get_value_compare()(n1.getVal(), n2.getVal());

	  if(cmp < 0)
	    return
	      nodes_intersect(n1.getRight(), n2, f) ||
	      nodes_intersect(n1, n2.getLeft(), f);
	  else if(cmp > 0)
	    return
	      nodes_intersect(n1.getLeft(), n2, f) ||
	      nodes_intersect(n1, n2.getRight(), f);
	  else if(f(n1.getVal(), n2.getVal()))
	    return true;
	  else
	    return
	      nodes_intersect(n1.getLeft(), n2.getLeft(), f) ||
	      nodes_intersect(n1.getRight(), n2.getRight(), f);
	}
    }

    /** \return \b true if n1 contains n2 under f; i.e., if for each
     *  element x2 of n2 there exists an element x1 of n1 such that x1
     *  is equivalent to x2 and f(x1, x2) holds.  If f is
     *  \b lambda x1 x2 . \b true, then this is simply set containment.
     */
    template<typename F>
    bool node_contains(const node &n1, const node &n2, const F &f) const
    {
      if(n2.empty())
        return true;
      else if(n1.empty())
        return false;
      else
	{
	  int cmp = impl.get_value_compare()(n1.getVal(), n2.getVal());

	  if(cmp < 0)
	    {
	      // Strip the left subtree of n2.
	      node n2repl = n2.getLeft().empty()
		? n2 : node(n2.getVal(), node(), n2.getRight(),
			    impl.get_accumOps());

	      return node_contains(n1, n2.getLeft(), f) &&
		node_contains(n1.getRight(), n2repl, f);
	    }
	  else if(cmp > 0)
	    {
	      // Strip the right subtree of n2.
	      node n2repl = n2.getRight().empty()
		? n2 : node(n2.getVal(), n2.getLeft(), node(),
			    impl.get_accumOps());

	      return node_contains(n1, n2.getRight(), f) &&
		node_contains(n1.getLeft(), n2repl, f);
	    }
	  else
	    {
	      return f(n2.getVal(), n1.getVal()) &&
		node_contains(n1.getLeft(), n2.getLeft(), f) &&
		node_contains(n1.getRight(), n2.getRight(), f);
	    }
	}
    }

    /** Remove the given value from the given tree. */
    node remove(const node &n, const Val &x, bool &removed_anything) const
    {
      if(n.empty())
        return n;
      else
	{
	  const int cmp = impl.get_value_compare()(x, n.getVal());

	  if(cmp < 0)
	    return node(n.getVal(),
			remove(n.getLeft(), x, removed_anything),
			n.getRight(),
			impl.get_accumOps()).rebalance(impl.get_accumOps());
	  else if(cmp > 0)
	    return node(n.getVal(),
			n.getLeft(),
			remove(n.getRight(), x, removed_anything),
			impl.get_accumOps()).rebalance(impl.get_accumOps());
	  else // found an equivalent node:
	    {
	      removed_anything = true;
	      return splice_trees(n.getLeft(), n.getRight(), impl.get_accumOps());
	    }
	}
    }

    set(const node &n, const Compare &value_compare, const AccumOps &accumOps)
      : impl(n, value_compare, accumOps)
    {
    }

    /** The binary predicate \b lambda x1 x2 . \b true */
    template<typename T>
    struct universal_relation
    {
      bool operator()(const T &a, const T &b) const
      {
	return true;
      }
    };
  public:
    /** Construct an empty tree. */
    set(const Compare &value_compare = Compare(),
	const AccumOps &accumOps = AccumOps())
      : impl(node(), value_compare, accumOps)
    {
    }

    /** Apply the given operator to each member of this set. */
    template<typename Op>
    bool for_each(const Op &o) const
    {
      return impl.get_root().for_each(o);
    }

    /** Insert an element into a tree, returning a new tree.  This is
     *  a static function to stress that it does NOT modify the old
     *  tree; instead, it returns a new tree containing the element in
     *  addition to the elements of the old tree.
     */
    static set add(const set &old, const Val &x, bool &added_anything)
    {
      return set(old.add(old.impl.get_root(), x, added_anything),
		 old.impl.get_value_compare(),
		 old.impl.get_accumOps());
    }

    static set add(const set &old, const Val &x)
    {
      bool dummy;
      return add(old, x, dummy);
    }

    /** Like add, but updates existing equivalent elements. */
    static set addUpdate(const set &old, const Val &x, bool &added_new_entry)
    {
      return set(old.addUpdate(old.impl.get_root(), x, added_new_entry),
		 old.impl.get_value_compare(),
		 old.impl.get_accumOps());
    }

    static set addUpdate(const set &old, const Val &x)
    {
      bool dummy;
      return addUpdate(old, x, dummy);
    }

    /** Remove x from the tree. */
    static set remove(const set &old, const Val &x, bool &removed_anything)
    {
      removed_anything = false;
      return set(old.remove(old.impl.get_root(), x, removed_anything),
		 old.impl.get_value_compare(),
		 old.impl.get_accumOps());
    }

    static set remove(const set &old, const Val &x)
    {
      bool dummy;
      return remove(old, x, dummy);
    }

    /** \return \b true if other contains an element equivalent to
     *                  an element in this and related by f.
     *                  f is invoked as (thiselt, otherelt).
     */
    template<typename F>
    bool intersects(const set &other, const F &f) const
    {
      return nodes_intersect(impl.get_root(), other.impl.get_root(), f);
    }

    /** \return \b true if this set intersects the given set. */
    bool intersects(const set &other) const
    {
      return nodes_intersect(impl.get_root(), other.impl.get_root(), universal_relation<Val>());
    }

    /** \return \b true if each element of other is related by f to an
     *                  element in this.  f is invoked as
     *                  (otherelt, thiselt).
     */
    template<typename F>
    bool contains(const set &other, const F &f) const
    {
      return node_contains(impl.get_root(), other.impl.get_root(), f);
    }

    /** \return \b true if each element in other has an equivalent
     *                  element in this set.
     */
    bool contains(const set &other) const
    {
      return node_contains(impl.get_root(), other.impl.get_root(), universal_relation<Val>());
    }

    /** Do an "in-place" update of this set, by replacing the root
     *  with a new root.
     *
     *  \return \b true if anything was added to the set.
     */
    bool insert(const Val &x)
    {
      bool rval = true;
      impl.set_root(add(impl.get_root(), x, rval));
      return rval;
    }

    /** Similar. */
    bool insertUpdate(const Val &x)
    {
      bool rval = true;
      impl.set_root(addUpdate(impl.get_root(), x, rval));
      return rval;
    }

    /** Similar.
     *
     *  \return \b true if any values were removed from the set.
     */
    bool erase(const Val &x)
    {
      bool rval = false;
      impl.set_root(remove(impl.get_root(), x, rval));
      return rval;
    }

    /** Find a tree node by value.  \return the node, or an invalid
     *	node if none exists.
     */
    node find_node(const Val &x) const
    {
      node rval = impl.get_root();

      while(rval.isValid())
      {
	int cmp = impl.get_value_compare()(x, rval.getVal());

	if(cmp < 0)
	  rval = rval.getLeft();
	else if(cmp > 0)
	  rval = rval.getRight();
	else
	  return rval;
      }

      return rval;
    }

    /** \return the accumulated value for the entire set. */
    AccumVal getAccumVal() const
    {
      if(impl.get_root().isValid())
	return impl.get_root().getAccumVal();
      else
	return impl.get_accumOps().empty();
    }

    /** \return \b true if this set contains the given value. */
    bool contains(const Val &x) const
    {
      return find_node(x).isValid();
    }

    const_iterator begin() const
    {
      return const_iterator(impl.get_root());
    }

    const_iterator end() const
    {
      return const_iterator();
    }

    node get_root() const
    {
      return impl.get_root();
    }

    node get_minimum() const
    {
      if(!impl.get_root().isValid())
	return impl.get_root();

      node rval = impl.get_root();
      while(rval.getLeft().isValid())
	rval = rval.getLeft();

      return rval;
    }

    size_type size() const
    {
      return impl.get_root().size();
    }

    int empty() const
    {
      return impl.get_root().empty();
    }

    void dump(std::ostream &out) const
    {
      impl.get_root().dump(out);
    }

    /** Return a new set that does not share memory with the original
     *  set.  It is safe for the old and new sets to be simultaneously
     *  accessed by separate threads.
     */
    set clone() const
    {
      return set(impl.get_root().clone(impl.get_accumOps()),
		 impl.get_value_compare(),
		 impl.get_accumOps());
    }
  };

  template<typename Val>
  struct set_write_action
  {
    std::ostream &out;
    mutable bool first;

    set_write_action(std::ostream &_out)
      : out(_out), first(true)
    {
    }

    bool operator()(const Val &v) const
    {
      if(first)
	first = false;
      else
	out << ", ";

      out << v;

      return true;
    }
  };

  /** \brief Write a set to a stream as a set (values are written with
   *  operator<<).
   */
  template<typename Val, typename Compare, typename AccumVal, typename AccumOps, int w>
  std::ostream &operator<<(std::ostream &out, const set<Val, Compare, AccumVal, AccumOps, w> &s)
  {
    out.put('{');
    set_write_action<Val> act(out);
    s.for_each(act);
    out.put('}');

    return out;
  }
}

namespace aptitude
{
  namespace util
  {
    /** Compare two sets.  Will produce strange results unless the two
     *  sets have the same comparison *object*; you are responsible for
     *  ensuring that this is the case. (i.e., if the comparator has
     *  associated data, it should be identical in the two sets)
     */
    template<typename Val, typename Compare, typename AccumVal, typename AccumOps, int w>
    class compare3_f<imm::set<Val, Compare, AccumVal, AccumOps, w> >
    {
    public:
      int operator()(const imm::set<Val, Compare, AccumVal, AccumOps, w> &s1,
		     const imm::set<Val, Compare, AccumVal, AccumOps, w> &s2) const
      {
	return aptitude::util::lexicographical_compare3(s1.begin(), s1.end(),
							s2.begin(), s2.end());
      }
    };
  }
}

namespace imm
{
  template<typename Val, typename Compare, typename AccumVal, typename AccumOps, int w>
  inline bool operator<(const set<Val, Compare, AccumVal, AccumOps, w> &s1,
			const set<Val, Compare, AccumVal, AccumOps, w> &s2)
  {
    return aptitude::util::compare3(s1, s2) < 0;
  }

  /** Compare two sets for equality, with the same caveat as operator<. */
  template<typename Val, typename Compare, typename AccumVal, typename AccumOps, int w>
  inline bool operator==(const set<Val, Compare, AccumVal, AccumOps, w> &s1,
			 const set<Val, Compare, AccumVal, AccumOps, w> &s2)
  {
    typename set<Val, Compare, AccumVal, AccumOps, w>::const_iterator
      i1 = s1.begin(), i2 = s2.begin();

    while(i1 != s1.end() && i2 != s2.end())
      {
	if(!(*i1 == *i2))
	  return false;
	else
	  {
	    ++i1;
	    ++i2;
	  }
      }

    return i1 == s1.end() && i2 == s2.end();
  }

  /** Auxillary class for map; used to order the contents of the map.
   */
  template<typename Key, typename Val, typename Compare>
  struct key_compare
  {
    Compare real_cmp;
  public:
    key_compare(const Compare &_real_cmp)
      :real_cmp(_real_cmp)
    {
    }

    int operator()(const std::pair<Key, Val> &p1,
		   const std::pair<Key, Val> &p2) const
    {
      return real_cmp(p1.first, p2.first);
    }
  };

  template<typename Key, typename Val, typename Compare = aptitude::util::compare3_f<Key>,
	   typename AccumVal = nil_t,
	   typename AccumOps = nil_t>
  class map
  {
  public:
    typedef std::pair<Key, Val> binding_type;
    typedef set<binding_type, key_compare<Key, Val, Compare>, AccumVal, AccumOps> mapping_type;
    typedef typename mapping_type::const_iterator const_iterator;
    typedef typename mapping_type::size_type size_type;
    typedef typename mapping_type::node node;

  private:
    mapping_type contents;

  public:
    /** Construct a map directly from a set of bindings. */
    map(const mapping_type &_contents)
      :contents(_contents)
    {
    }

    /** Construct an empty map */
    map(const Compare &value_compare = Compare(),
	const AccumOps &accumOps = AccumOps())
      :contents(mapping_type(key_compare<Key, Val, Compare>(value_compare),
			     accumOps))
    {
    }

    /** Apply the given operator to each binding in this map.
     *
     *  \param o a function object that takes a pair (key, val).
     */
    template<typename Op>
    bool for_each(const Op &o) const
    {
      return contents.for_each(o);
    }

    mapping_type get_bindings() const
    {
      return contents;
    }

    const_iterator begin() const
    {
      return contents.begin();
    }

    const_iterator end() const
    {
      return contents.end();
    }

    bool empty() const
    {
      return contents.empty();
    }

    size_type size() const
    {
      return contents.size();
    }

    /** \return either the node corresponding to the given key,
     *  or an empty tree.
     */
    node lookup(const Key &k) const
    {
      return contents.find_node(binding_type(k, Val()));
    }

    /** \return the accumulated value of the whole map. */
    AccumVal getAccumVal() const
    {
      return contents.getAccumVal();
    }

    /** \return either the value of the mapping at k, or dflt if k is
     *  unbound.
     */
    Val get(const Key &k, const Val &dflt) const
    {
      node found = contents.find_node(binding_type(k, Val()));

      if(found.isValid())
	return found.getVal().second;
      else
	return dflt;
    }

    /** \return a new map that binds k to v, overwriting any existing
     *  binding.
     */
    static map bind(const map &m, const Key &k, const Val &v, bool &inserted_new_binding)
    {
      return map(mapping_type::add(m.contents, binding_type(k, v), inserted_new_binding));
    }

    static map bind(const map &m, const Key &k, const Val &v)
    {
      bool dummy;
      return bind(m, k, v, dummy);
    }

    /** \return a new map based on m in which k is unbound. */
    static map unbind(const map &m, const Key &k, bool &removed_binding)
    {
      return map(mapping_type::erase(m.contents, binding_type(k, Val()), removed_binding));
    } 

    static map unbind(const map &m, const Key &k)
    {
      bool dummy;
      return unbind(m, k, dummy);
    }

    /** Add a binding to this map. */
    bool put(const Key &k, const Val &v)
    {
      return contents.insertUpdate(binding_type(k, v));
    }

    /** Delete a binding from this map by key. */
    bool erase(const Key &k)
    {
      return contents.erase(binding_type(k, Val()));
    }

    /** \return \b true if k is in the domain of this mapping. */
    bool domain_contains(const Key &k) const
    {
      return contents.contains(binding_type(k, Val()));
    }

    bool operator<(const map &other) const
    {
      return contents < other.contents;
    }

    bool operator==(const map &other) const
    {
      return contents == other.contents;
    }

    void dump(std::ostream &out) const
    {
      contents.dump(out);
    }

    /** \brief Return \b true if each binding is related under
     *  compare to a binding in other of an equivalent key.
     */
    template<typename BindingCmp>
    bool is_supermap_of_under(const map &other,
			      const BindingCmp &compare = BindingCmp()) const
    {
      return contents.contains(other.contents, compare);
    }

    bool is_supermap_of(const map &other)
    {
      return contents.contains(other.contents, std::equal_to<std::pair<Key, Val> >());
    }

    bool domain_intersects(const map &other) const
    {
      return contents.intersects(other.contents);
    }

    bool shares_value(const map &other) const
    {
      return contents.intersects(other.contents, std::equal_to<std::pair<Key, Val> >());
    }

    /** \return \b true if the two maps have an element with an equivalent
     *                  key such that f(thiselt, otherelt) is \b true.
     */
    template<typename F>
    bool has_related_mapping(const map &other, const F &f) const
    {
      return contents.intersects(other.contents, f);
    }

    /** Return a new identical map that does not share memory with
     *  this map.
     */
    map clone() const
    {
      return map(contents.clone());
    }
  };

  template<typename Key, typename Val>
  struct map_write_action
  {
    std::ostream &out;
    mutable bool first;

    map_write_action(std::ostream &_out)
      : out(_out), first(true)
    {
    }

    bool operator()(const std::pair<Key, Val> &entry) const
    {
      if(first)
	first = false;
      else
	out << ", ";

      out << entry.first << " := " << entry.second;

      return true;
    }
  };


  /** \brief Write a map to a stream as a map (values are written with
   *  operator<<).
   */
  template<typename Key, typename Val, typename Compare, typename AccumVal, typename AccumOps>
  std::ostream &operator<<(std::ostream &out, const map<Key, Val, Compare, AccumVal, AccumOps> &m)
  {

    out.put('{');
    map_write_action<Key, Val> act(out);
    m.for_each(act);
    out.put('}');

    return out;
  }
};


namespace aptitude
{
  namespace util
  {
    /** Compare two sets.  Will produce strange results unless the two
     *  sets have the same comparison *object*; you are responsible for
     *  ensuring that this is the case. (i.e., if the comparator has
     *  associated data, it should be identical in the two sets)
     */
    template<typename Key, typename Val, typename Compare, typename AccumVal, typename AccumOps>
    class compare3_f<imm::map<Key, Val, Compare, AccumVal, AccumOps> >
    {
    public:
      int operator()(const imm::map<Key, Val, Compare, AccumVal, AccumOps> &m1,
		     const imm::map<Key, Val, Compare, AccumVal, AccumOps> &m2) const
      {
	return compare3(m1.get_bindings(), m2.get_bindings());
      }
    };
  }
}

#endif
