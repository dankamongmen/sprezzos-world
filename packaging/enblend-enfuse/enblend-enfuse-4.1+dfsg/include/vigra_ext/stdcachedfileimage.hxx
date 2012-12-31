/*
 *  Copyright (C) 2004 Andrew Mihal
 *
 *  This software is an extension of the VIGRA computer vision library.
 *  ( Version 1.2.0, Aug 07 2003 )
 *  You may use, modify, and distribute this software according
 *  to the terms stated in the LICENSE file included in
 *  the VIGRA distribution.
 *
 *  VIGRA is Copyright 1998-2002 by Ullrich Koethe
 *  Cognitive Systems Group, University of Hamburg, Germany
 *
 *  The VIGRA Website is
 *      http://kogs-www.informatik.uni-hamburg.de/~koethe/vigra/
 *  Please direct questions, bug reports, and contributions to
 *      koethe@informatik.uni-hamburg.de
 *
 *  THIS SOFTWARE IS PROVIDED AS IS AND WITHOUT ANY EXPRESS OR
 *  IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 *  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */
#ifndef VIGRA_EXT_STDCACHEDFILEIMAGE_HXX
#define VIGRA_EXT_STDCACHEDFILEIMAGE_HXX

#include <vigra/tuple.hxx>
#include <vigra/iteratortraits.hxx>
#include <vigra/accessor.hxx>
#include <vigra/rgbvalue.hxx>
#include <vigra/sized_int.hxx>

#include "vigra_ext/cachedfileimage.hxx"


namespace vigra_ext {

#define CFI_DEFINE_ITERATORTRAITS(VALUETYPE, ACCESSOR, CONSTACCESSOR) \
    template<> \
    struct IteratorTraits< \
        CachedFileImageIterator<VALUETYPE > > \
    { \
        typedef CachedFileImageIterator<VALUETYPE >  Iterator; \
        typedef Iterator                             iterator; \
        typedef iterator::iterator_category          iterator_category; \
        typedef iterator::value_type                 value_type; \
        typedef iterator::reference                  reference; \
        typedef iterator::index_reference            index_reference; \
        typedef iterator::pointer                    pointer; \
        typedef iterator::difference_type            difference_type; \
        typedef iterator::row_iterator               row_iterator; \
        typedef iterator::column_iterator            column_iterator; \
        typedef ACCESSOR<VALUETYPE >                 default_accessor; \
        typedef ACCESSOR<VALUETYPE >                 DefaultAccessor; \
    }; \
    template<> \
    struct IteratorTraits< \
        ConstCachedFileImageIterator<VALUETYPE > > \
    { \
        typedef \
          ConstCachedFileImageIterator<VALUETYPE >   Iterator; \
        typedef Iterator                             iterator; \
        typedef iterator::iterator_category          iterator_category; \
        typedef iterator::value_type                 value_type; \
        typedef iterator::reference                  reference; \
        typedef iterator::index_reference            index_reference; \
        typedef iterator::pointer                    pointer; \
        typedef iterator::difference_type            difference_type; \
        typedef iterator::row_iterator               row_iterator; \
        typedef iterator::column_iterator            column_iterator; \
        typedef CONSTACCESSOR<VALUETYPE >            default_accessor; \
        typedef CONSTACCESSOR<VALUETYPE >            DefaultAccessor; \
    }; \
    template<> \
    struct IteratorTraits< \
        StridedCachedFileImageIterator<VALUETYPE > > \
    { \
        typedef StridedCachedFileImageIterator<VALUETYPE > Iterator; \
        typedef Iterator                                   iterator; \
        typedef iterator::iterator_category                iterator_category; \
        typedef iterator::value_type                       value_type; \
        typedef iterator::reference                        reference; \
        typedef iterator::index_reference                  index_reference; \
        typedef iterator::pointer                          pointer; \
        typedef iterator::difference_type                  difference_type; \
        typedef iterator::row_iterator                     row_iterator; \
        typedef iterator::column_iterator                  column_iterator; \
        typedef ACCESSOR<VALUETYPE >                       default_accessor; \
        typedef ACCESSOR<VALUETYPE >                       DefaultAccessor; \
    }; \
    template<> \
    struct IteratorTraits< \
        ConstStridedCachedFileImageIterator<VALUETYPE > > \
    { \
        typedef ConstStridedCachedFileImageIterator<VALUETYPE > Iterator; \
        typedef Iterator                                        iterator; \
        typedef iterator::iterator_category                     iterator_category; \
        typedef iterator::value_type                            value_type; \
        typedef iterator::reference                             reference; \
        typedef iterator::index_reference                       index_reference; \
        typedef iterator::pointer                               pointer; \
        typedef iterator::difference_type                       difference_type; \
        typedef iterator::row_iterator                          row_iterator; \
        typedef iterator::column_iterator                       column_iterator; \
        typedef CONSTACCESSOR<VALUETYPE >                       default_accessor; \
        typedef CONSTACCESSOR<VALUETYPE >                       DefaultAccessor; \
    };

CFI_DEFINE_ITERATORTRAITS(vigra::UInt8, vigra::StandardValueAccessor, vigra::StandardConstValueAccessor)
typedef CachedFileImage<vigra::UInt8> UInt8CFImage;

CFI_DEFINE_ITERATORTRAITS(vigra::Int8, vigra::StandardValueAccessor, vigra::StandardConstValueAccessor)
typedef CachedFileImage<vigra::Int8> Int8CFImage;

CFI_DEFINE_ITERATORTRAITS(vigra::UInt16, vigra::StandardValueAccessor, vigra::StandardConstValueAccessor)
typedef CachedFileImage<vigra::UInt16> UInt16CFImage;

CFI_DEFINE_ITERATORTRAITS(vigra::Int16, vigra::StandardValueAccessor, vigra::StandardConstValueAccessor)
typedef CachedFileImage<vigra::Int16> Int16CFImage;

CFI_DEFINE_ITERATORTRAITS(vigra::UInt32, vigra::StandardValueAccessor, vigra::StandardConstValueAccessor)
typedef CachedFileImage<vigra::UInt32> UInt32CFImage;

CFI_DEFINE_ITERATORTRAITS(vigra::Int32, vigra::StandardValueAccessor, vigra::StandardConstValueAccessor)
typedef CachedFileImage<vigra::Int32> Int32CFImage;

CFI_DEFINE_ITERATORTRAITS(float, vigra::StandardValueAccessor, vigra::StandardConstValueAccessor)
typedef CachedFileImage<float> FCFImage;

CFI_DEFINE_ITERATORTRAITS(double, vigra::StandardValueAccessor, vigra::StandardConstValueAccessor)
typedef CachedFileImage<double> DCFImage;

CFI_DEFINE_ITERATORTRAITS(vigra::RGBValue<vigra::UInt8>, vigra::RGBAccessor, vigra::RGBAccessor)
typedef CachedFileImage<vigra::RGBValue<vigra::UInt8> > UInt8RGBCFImage;

CFI_DEFINE_ITERATORTRAITS(vigra::RGBValue<vigra::Int8>, vigra::RGBAccessor, vigra::RGBAccessor)
typedef CachedFileImage<vigra::RGBValue<vigra::Int8> > Int8RGBCFImage;

CFI_DEFINE_ITERATORTRAITS(vigra::RGBValue<vigra::UInt16>, vigra::RGBAccessor, vigra::RGBAccessor)
typedef CachedFileImage<vigra::RGBValue<vigra::UInt16> > UInt16RGBCFImage;

CFI_DEFINE_ITERATORTRAITS(vigra::RGBValue<vigra::Int16>, vigra::RGBAccessor, vigra::RGBAccessor)
typedef CachedFileImage<vigra::RGBValue<vigra::Int16> > Int16RGBCFImage;

CFI_DEFINE_ITERATORTRAITS(vigra::RGBValue<vigra::UInt32>, vigra::RGBAccessor, vigra::RGBAccessor)
typedef CachedFileImage<vigra::RGBValue<vigra::UInt32> > UInt32RGBCFImage;

CFI_DEFINE_ITERATORTRAITS(vigra::RGBValue<vigra::Int32>, vigra::RGBAccessor, vigra::RGBAccessor)
typedef CachedFileImage<vigra::RGBValue<vigra::Int32> > Int32RGBCFImage;

CFI_DEFINE_ITERATORTRAITS(vigra::RGBValue<float>, vigra::RGBAccessor, vigra::RGBAccessor)
typedef CachedFileImage<vigra::RGBValue<float> > FRGBCFImage;

CFI_DEFINE_ITERATORTRAITS(vigra::RGBValue<double>, vigra::RGBAccessor, vigra::RGBAccessor)
typedef CachedFileImage<vigra::RGBValue<double> > DRGBCFImage;

#ifndef NO_PARTIAL_TEMPLATE_SPECIALIZATION

// define traits for BasicImageIterator instanciations that
// were not explicitly defined above
template <class T>
struct IteratorTraits<CachedFileImageIterator<T> >
{
    typedef CachedFileImageIterator<T>           Iterator;
    typedef Iterator                             iterator;
    typedef typename iterator::iterator_category iterator_category;
    typedef typename iterator::value_type        value_type;
    typedef typename iterator::reference         reference;
    typedef typename iterator::index_reference   index_reference;
    typedef typename iterator::pointer           pointer;
    typedef typename iterator::difference_type   difference_type;
    typedef typename iterator::row_iterator      row_iterator;
    typedef typename iterator::column_iterator   column_iterator;
    typedef vigra::StandardAccessor<T>           DefaultAccessor;
    typedef vigra::StandardAccessor<T>           default_accessor;
};

template <class T>
struct IteratorTraits<ConstCachedFileImageIterator<T> >
{
    typedef ConstCachedFileImageIterator<T>        Iterator;
    typedef Iterator                               iterator;
    typedef typename iterator::iterator_category   iterator_category;
    typedef typename iterator::value_type          value_type;
    typedef typename iterator::reference           reference;
    typedef typename iterator::index_reference     index_reference;
    typedef typename iterator::pointer             pointer;
    typedef typename iterator::difference_type     difference_type;
    typedef typename iterator::row_iterator        row_iterator;
    typedef typename iterator::column_iterator     column_iterator;
    typedef vigra::StandardConstAccessor<T>        DefaultAccessor;
    typedef vigra::StandardConstAccessor<T>        default_accessor;
};

#endif

} // namespace vigra_ext

#endif /* VIGRA_EXT_STDCACHEDFILEIMAGE_HXX */
