/** \file compare3.h */   // -*-c++-*-


//   Copyright (C) 2009-2010 Daniel Burrows

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

#ifndef COMPARE3_H
#define COMPARE3_H

// Forward-declare STL templates to avoid pulling the whole STL in.
namespace std
{
  template<typename Iterator>
  class iterator_traits;

  template<typename T1, typename T2>
  class pair;

  template<typename T, typename Alloc>
  class list;

  template<typename T, typename Alloc>
  class vector;

  template<typename T, typename Alloc>
  class deque;

  template<typename T, typename Compare, typename Alloc>
  class set;

  template<typename Key, typename Val, typename Compare, typename Alloc>
  class map;

  template<typename T, typename Compare, typename Alloc>
  class multiset;

  template<typename Key, typename Val, typename Compare, typename Alloc>
  class multimap;

  template<typename charT, typename traits, typename Alloc>
  class basic_string;
}

// Forward-declare ref_ptrs.
namespace cwidget
{
  namespace util
  {
    template<typename T>
    class ref_ptr;
  }
}

namespace aptitude
{
  namespace util
  {
    // \todo If Boost is added as a dependency, this should use
    // call_traits instead.

    /** \brief Helper metafunction for building parameter types
     *	automatically.
     */
    template<typename T>
    struct make_param
    {
      typedef const T &type;
    };

    // Stuff to ensure that integers are passed as-is.
    template<>
    struct make_param<unsigned int>
    {
      typedef unsigned int type;
    };

    template<>
    struct make_param<int>
    {
      typedef int type;
    };

    template<>
    struct make_param<unsigned long>
    {
      typedef unsigned long type;
    };

    template<>
    struct make_param<long>
    {
      typedef long type;
    };

    template<>
    struct make_param<unsigned short>
    {
      typedef unsigned short type;
    };

    template<>
    struct make_param<short>
    {
      typedef short type;
    };

    template<>
    struct make_param<unsigned char>
    {
      typedef unsigned short type;
    };

    template<>
    struct make_param<char>
    {
      typedef short type;
    };

    // Pass pointers and references as-is.
    template<typename T>
    struct make_param<const T *>
    {
      typedef const T *type;
    };

    template<typename T>
    struct make_param<T *>
    {
      typedef const T *type;
    };

    template<typename T>
    struct make_param<const T &>
    {
      typedef const T &type;
    };

    template<typename T>
    struct make_param<T &>
    {
      typedef const T &type;
    };

    /** \brief An overloaded structure that provides a default 3-way
     *  compare for various types.
     *
     *  The default uses operator<, but some common types have more
     *  efficient implementations.
     */
    template<typename T>
    class compare3_f
    {
      typedef typename make_param<T>::type T_param;
    public:
      /** \brief Return a value less than zero if t1<t2, a value greater
       *  than zero if t1>t2, and zero if t1=t2.
       */
      int operator()(T_param t1, T_param t2) const
      {
	if(t1 < t2)
	  return -1;
	else if(t2 < t1)
	  return 1;
	else
	  return 0;
      }
    };

    template<typename T>
    inline int compare3(const T &t1, const T &t2)
    {
      return compare3_f<T>()(t1, t2);
    }

    /** \brief 3-way lexicographic comparison. */
    template<typename Iter, typename Compare>
    inline int lexicographical_compare3(const Iter &begin1, const Iter &end1,
					const Iter &begin2, const Iter &end2,
					Compare compare)
    {
      Iter it1(begin1), it2(begin2);

      while(it1 != end1 && it2 != end2)
	{
	  const int cmp(compare(*it1, *it2));
	  if(cmp != 0)
	    return cmp;

	  ++it1;
	  ++it2;
	}

      if(it2 != end2)
	return -1;
      else if(it1 != end1)
	return 1;
      else
	return 0;
    }

    /** \brief 3-way lexicographic comparison. */
    template<typename Iter>
    inline int lexicographical_compare3(const Iter &begin1, const Iter &end1,
					const Iter &begin2, const Iter &end2)
    {
      return lexicographical_compare3(begin1, end1, begin2, end2,
				      compare3_f<typename std::iterator_traits<Iter>::value_type>());
    }




    // Comparison for integers.
    template<typename T>
    class compare3_f_int
    {
    public:
      int operator()(T n1, T n2) const
      {
	if(n1 < n2)
	   return -1;
	else if(n2 < n1)
	  return 1;
	else
	  return 0;
      }
    };

    template<> class compare3_f<bool>
    {
      int operator()(bool b1, bool b2) const
      {
	if(b1 == b2)
	  return 0;
	else if(b1 < b2)
	  return -1;
	else if(b1 > b2)
	  return 1;
      }
    };

    template<> class compare3_f<int> : public compare3_f_int<int> { };
    template<> class compare3_f<unsigned int> : public compare3_f_int<unsigned int> { };

    template<> class compare3_f<long> : public compare3_f_int<long> { };
    template<> class compare3_f<unsigned long> : public compare3_f_int<unsigned long> { };

    template<> class compare3_f<short> : public compare3_f_int<short> { };
    template<> class compare3_f<unsigned short> : public compare3_f_int<unsigned short> { };

    template<> class compare3_f<char> : public compare3_f_int<char> { };
    template<> class compare3_f<unsigned char> : public compare3_f_int<unsigned char> { };


    // Overloaded comparison on pointers.
    template<typename T> class compare3_f<T *>
    {
    public:
      int operator()(T *t1, T *t2) const { return t1 - t2; }
    };

    template<typename T> class compare3_f<const T *>
    {
    public:
      int operator()(const T *t1, const T *t2) const { return t1 - t2; }
    };

    template<typename T> class compare3_f<cwidget::util::ref_ptr<T> >
    {
    public:
      int operator()(const cwidget::util::ref_ptr<T> &t1,
		     const cwidget::util::ref_ptr<T> &t2) const
      {
	return t1.unsafe_get_ref() - t2.unsafe_get_ref();
      }
    };


    template<typename charT, typename traits, typename Alloc>
    class compare3_f<std::basic_string<charT, traits, Alloc> >
    {
    public:
      int operator()(const std::basic_string<charT, traits, Alloc> &s1,
                     const std::basic_string<charT, traits, Alloc> &s2) const
      {
	return s1.compare(s2);
      }
    };

    template<typename T>
    class compare3_f_container
    {
    public:
      int operator()(const T &c1, const T &c2) const
      {
	return lexicographical_compare3(c1.begin(), c1.end(),
					c2.begin(), c2.end());
      }
    };

    // Comparison operators for common STL types.
    template<typename T1, typename T2>
    class compare3_f<std::pair<T1, T2> >
    {
    public:
      int operator()(const std::pair<T1, T2> &p1,
		     const std::pair<T1, T2> &p2) const
      {
	const int cmp1 = compare3(p1.first, p2.first);
	if(cmp1 != 0)
	  return cmp1;
	else
	  return compare3(p1.second, p2.second);
      }
    };

    template<typename T, typename Alloc>
    class compare3_f<std::list<T, Alloc> >
      : public compare3_f_container<std::list<T, Alloc> >
    {
    };

    template<typename T, typename Alloc>
    class compare3_f<std::vector<T, Alloc> >
      : public compare3_f_container<std::vector<T, Alloc> >
    {
    };

    template<typename T, typename Alloc>
    class compare3_f<std::deque<T, Alloc> >
      : public compare3_f_container<std::deque<T, Alloc> >
    {
    };

    template<typename T, typename Compare, typename Alloc>
    class compare3_f<std::set<T, Compare, Alloc> >
      : public compare3_f_container<std::set<T, Compare, Alloc> >
    {
    };

    template<typename Key, typename Val, typename Compare, typename Alloc>
    class compare3_f<std::map<Key, Val, Compare, Alloc> >
      : public compare3_f_container<std::map<Key, Val, Compare, Alloc> >
    {
    };

    template<typename T, typename Compare, typename Alloc>
    class compare3_f<std::multiset<T, Compare, Alloc> >
      : public compare3_f_container<std::multiset<T, Compare, Alloc> >
    {
    };

    template<typename Key, typename Val, typename Compare, typename Alloc>
    class compare3_f<std::multimap<Key, Val, Compare, Alloc> >
      : public compare3_f_container<std::multimap<Key, Val, Compare, Alloc> >
    {
    };
  }
}

#endif // COMPARE3_H
