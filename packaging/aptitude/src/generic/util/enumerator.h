/** \brief enumerator.h */    // -*-c++-*-

#ifndef ENUMERATOR_H
#define ENUMERATOR_H

#include <boost/shared_ptr.hpp>

namespace aptitude
{
  namespace util
  {
    /** \brief Utility class to allow generic iteration over a list.
     *
     *  Unlike an iterator, instantiations of this class can be used
     *  without the definition of the concrete object being visible to
     *  the user.  Like Java and .NET iterators, these iterators start
     *  "before" the list being iterated over and must be advanced to
     *  the first entry.
     */
    template<typename T>
    class enumerator
    {
    public:
      virtual ~enumerator() {}

      typedef T value_type;

      /** \brief Get the value at the current position. */
      virtual T get_current() = 0;

      /** \brief Advance to the next entry in the list.
       *
       *  \return \b true if there was another entry, or \b false if we
       *  reached the end of the list.
       */
      virtual bool advance() = 0;
    };

    /** \brief An enumerator over a range of STL iterators. */
    template<typename ForwardIter>
    class iterator_enumerator : public enumerator<typename ForwardIter::value_type>
    {
      ForwardIter begin, end;
      bool first;

    public:
      iterator_enumerator(ForwardIter _begin, ForwardIter _end);
      typename ForwardIter::value_type get_current();
      bool advance();
    };

    template<typename ForwardIter>
    iterator_enumerator<ForwardIter>::iterator_enumerator(ForwardIter _begin,
                                                          ForwardIter _end)
      : begin(_begin), end(_end), first(true)
    {
    }

    template<typename ForwardIter>
    typename ForwardIter::value_type
    iterator_enumerator<ForwardIter>::get_current()
    {
      return *begin;
    }

    template<typename ForwardIter>
    bool iterator_enumerator<ForwardIter>::advance()
    {
      if(begin == end)
        return false;

      if(first)
        first = false;
      else
        ++begin;

      return begin != end;
    }

    /** \brief An enumerator over an iterator range that holds a strong
     *  shared reference to an object (presumably the one containing the
     *  iterators).
     */
    template<typename ForwardIter, typename T>
    class iterator_enumerator_with_keepalive : public iterator_enumerator<ForwardIter>
    {
      boost::shared_ptr<T> keptalive;

    public:
      iterator_enumerator_with_keepalive(ForwardIter begin, ForwardIter end,
                                         boost::shared_ptr<T> _keptalive);
    };

    template<typename ForwardIter, typename T>
    iterator_enumerator_with_keepalive<ForwardIter, T>
    ::iterator_enumerator_with_keepalive(ForwardIter begin, ForwardIter end,
                                         boost::shared_ptr<T> _keptalive)
      : iterator_enumerator<ForwardIter>(begin, end),
        keptalive(_keptalive)
    {
    }
  }
}

#endif // ENUMERATOR_H
