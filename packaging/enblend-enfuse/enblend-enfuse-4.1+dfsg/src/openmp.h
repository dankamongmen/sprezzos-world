/*
 * Copyright (C) 2009-2012 Christoph L. Spiel
 *
 * This file is part of Enblend.
 *
 * Enblend is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Enblend is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Enblend; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
#ifndef __OPENMP_H__
#define __OPENMP_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <limits>

#include <vigra/diff2d.hxx>
#include <vigra/initimage.hxx>
#include <vigra/inspectimage.hxx>
#include <vigra/transformimage.hxx>
#include <vigra/combineimages.hxx>
#include <vigra/convolution.hxx>
#include <vigra/distancetransform.hxx>


#if _OPENMP >= 200203 // at least OpenMP version 2.0

#include <omp.h>


#define OPENMP
#define OPENMP_YEAR (_OPENMP / 100)
#define OPENMP_MONTH (_OPENMP % 100)


// These are the image sizes (measured in pixels) where we switch from
// serial (single thread) to multi processing.  The crossover points
// can be different for scalar, i.e. black-and-white images and
// non-scalar, i.e. RGB images.

#define CROSSOVER_COMBINETWOIMAGES_SCALAR 65536
#define CROSSOVER_COMBINETWOIMAGES_NON_SCALAR 16384

#define CROSSOVER_COMBINETHREEIMAGES_SCALAR 46656
#define CROSSOVER_COMBINETHREEIMAGES_NON_SCALAR 12544

#define CROSSOVER_TRANSFORMIMAGE_SCALAR 57600
#define CROSSOVER_TRANSFORMIMAGE_NON_SCALAR 32768

#define CROSSOVER_DISTANCE_TRANSFORM 1936


template <class SrcImageIterator1, class SrcAccessor1,
          class SrcImageIterator2, class SrcAccessor2,
          class DestImageIterator, class DestAccessor,
          class Functor>
inline void
combineTwoImagesMP(SrcImageIterator1 src1_upperleft, SrcImageIterator1 src1_lowerright, SrcAccessor1 src1_acc,
                   SrcImageIterator2 src2_upperleft, SrcAccessor2 src2_acc,
                   DestImageIterator dest_upperleft, DestAccessor dest_acc,
                   const Functor& func)
{
    typedef typename DestAccessor::value_type value_type;
    typedef typename vigra::NumericTraits<value_type>::isScalar isScalar;

    const vigra::Diff2D size(src1_lowerright - src1_upperleft);

    if (size.x * size.y >=
        (isScalar().asBool ? CROSSOVER_COMBINETWOIMAGES_SCALAR : CROSSOVER_COMBINETWOIMAGES_NON_SCALAR))
    {
#pragma omp parallel
        {
            const int n = omp_get_num_threads();
            const int i = omp_get_thread_num();
            const vigra::Diff2D begin(0, (i * size.y) / n);
            const vigra::Diff2D end(size.x, ((i + 1) * size.y) / n);

            vigra::combineTwoImages(src1_upperleft + begin, src1_upperleft + end, src1_acc,
                                    src2_upperleft + begin, src2_acc,
                                    dest_upperleft + begin, dest_acc,
                                    func);
        } // omp parallel
    }
    else
    {
        vigra::combineTwoImages(src1_upperleft, src1_lowerright, src1_acc,
                                src2_upperleft, src2_acc,
                                dest_upperleft, dest_acc,
                                func);
    }
}


template <class SrcImageIterator1, class SrcAccessor1,
          class SrcImageIterator2, class SrcAccessor2,
          class MaskImageIterator, class MaskAccessor,
          class DestImageIterator, class DestAccessor,
          class Functor>
inline void
combineTwoImagesIfMP(SrcImageIterator1 src1_upperleft, SrcImageIterator1 src1_lowerright, SrcAccessor1 src1_acc,
                     SrcImageIterator2 src2_upperleft, SrcAccessor2 src2_acc,
                     MaskImageIterator mask_upperleft, MaskAccessor mask_acc,
                     DestImageIterator dest_upperleft, DestAccessor dest_acc,
                     const Functor& func)
{
    typedef typename DestAccessor::value_type value_type;
    typedef typename vigra::NumericTraits<value_type>::isScalar isScalar;

    const vigra::Diff2D size(src1_lowerright - src1_upperleft);

    if (size.x * size.y >=
        (isScalar().asBool ? CROSSOVER_COMBINETWOIMAGES_SCALAR : CROSSOVER_COMBINETWOIMAGES_NON_SCALAR))
    {
#pragma omp parallel
        {
            const int n = omp_get_num_threads();
            const int i = omp_get_thread_num();
            const vigra::Diff2D begin(0, (i * size.y) / n);
            const vigra::Diff2D end(size.x, ((i + 1) * size.y) / n);

            vigra::combineTwoImagesIf(src1_upperleft + begin, src1_upperleft + end, src1_acc,
                                      src2_upperleft + begin, src2_acc,
                                      mask_upperleft + begin, mask_acc,
                                      dest_upperleft + begin, dest_acc,
                                      func);
        } // omp parallel
    }
    else
    {
        vigra::combineTwoImagesIf(src1_upperleft, src1_lowerright, src1_acc,
                                  src2_upperleft, src2_acc,
                                  mask_upperleft, mask_acc,
                                  dest_upperleft, dest_acc,
                                  func);
    }
}


template <class SrcImageIterator1, class SrcAccessor1,
          class SrcImageIterator2, class SrcAccessor2,
          class SrcImageIterator3, class SrcAccessor3,
          class DestImageIterator, class DestAccessor,
          class Functor>
inline void
combineThreeImagesMP(SrcImageIterator1 src1_upperleft, SrcImageIterator1 src1_lowerright, SrcAccessor1 src1_acc,
                     SrcImageIterator2 src2_upperleft, SrcAccessor2 src2_acc,
                     SrcImageIterator3 src3_upperleft, SrcAccessor3 src3_acc,
                     DestImageIterator dest_upperleft, DestAccessor dest_acc,
                     const Functor& func)
{
    typedef typename DestAccessor::value_type value_type;
    typedef typename vigra::NumericTraits<value_type>::isScalar isScalar;

    const vigra::Diff2D size(src1_lowerright - src1_upperleft);

    if (size.x * size.y >=
        (isScalar().asBool ? CROSSOVER_COMBINETHREEIMAGES_SCALAR : CROSSOVER_COMBINETHREEIMAGES_NON_SCALAR))
    {
#pragma omp parallel
        {
            const int n = omp_get_num_threads();
            const int i = omp_get_thread_num();
            const vigra::Diff2D begin(0, (i * size.y) / n);
            const vigra::Diff2D end(size.x, ((i + 1) * size.y) / n);

            vigra::combineThreeImages(src1_upperleft + begin, src1_upperleft + end, src1_acc,
                                      src2_upperleft + begin, src2_acc,
                                      src3_upperleft + begin, src3_acc,
                                      dest_upperleft + begin, dest_acc,
                                      func);
        } // omp parallel
    }
    else
    {
        vigra::combineThreeImages(src1_upperleft, src1_lowerright, src1_acc,
                                  src2_upperleft, src2_acc,
                                  src3_upperleft, src3_acc,
                                  dest_upperleft, dest_acc,
                                  func);
    }
}


template <class SrcImageIterator, class SrcAccessor,
          class DestImageIterator, class DestAccessor,
          class Functor>
inline void
transformImageMP(SrcImageIterator src_upperleft, SrcImageIterator src_lowerright, SrcAccessor src_acc,
                 DestImageIterator dest_upperleft, DestAccessor dest_acc,
                 const Functor& func)
{
    typedef typename DestAccessor::value_type value_type;
    typedef typename vigra::NumericTraits<value_type>::isScalar isScalar;

    const vigra::Diff2D size(src_lowerright - src_upperleft);

    if (size.x * size.y >=
        (isScalar().asBool ? CROSSOVER_TRANSFORMIMAGE_SCALAR : CROSSOVER_TRANSFORMIMAGE_NON_SCALAR))
    {
#pragma omp parallel
        {
            const int n = omp_get_num_threads();
            const int i = omp_get_thread_num();
            const vigra::Diff2D begin(0, (i * size.y) / n);
            const vigra::Diff2D end(size.x, ((i + 1) * size.y) / n);

            vigra::transformImage(src_upperleft + begin, src_upperleft + end, src_acc,
                                  dest_upperleft + begin, dest_acc,
                                  func);
        } // omp parallel
    }
    else
    {
        vigra::transformImage(src_upperleft, src_lowerright, src_acc,
                              dest_upperleft, dest_acc,
                              func);
    }
}


template <class SrcImageIterator, class SrcAccessor,
          class MaskImageIterator, class MaskAccessor,
          class DestImageIterator, class DestAccessor,
          class Functor>
inline void
transformImageIfMP(SrcImageIterator src_upperleft, SrcImageIterator src_lowerright, SrcAccessor src_acc,
                   MaskImageIterator mask_upperleft, MaskAccessor mask_acc,
                   DestImageIterator dest_upperleft, DestAccessor dest_acc,
                   const Functor& func)
{
    typedef typename DestAccessor::value_type value_type;
    typedef typename vigra::NumericTraits<value_type>::isScalar isScalar;

    const vigra::Diff2D size(src_lowerright - src_upperleft);

    if (size.x * size.y >=
        (isScalar().asBool ? CROSSOVER_TRANSFORMIMAGE_SCALAR : CROSSOVER_TRANSFORMIMAGE_NON_SCALAR))
    {
#pragma omp parallel
        {
            const int n = omp_get_num_threads();
            const int i = omp_get_thread_num();
            const vigra::Diff2D begin(0, (i * size.y) / n);
            const vigra::Diff2D end(size.x, ((i + 1) * size.y) / n);

            vigra::transformImageIf(src_upperleft + begin, src_upperleft + end, src_acc,
                                    mask_upperleft + begin, mask_acc,
                                    dest_upperleft + begin, dest_acc,
                                    func);
        } // omp parallel
    }
    else
    {
        vigra::transformImageIf(src_upperleft, src_lowerright, src_acc,
                                mask_upperleft, mask_acc,
                                dest_upperleft, dest_acc,
                                func);
    }
}


namespace fh
{
namespace detail
{
    template <class ValueType>
    inline static ValueType
    square(ValueType x)
    {
        return x * x;
    }


    // Pedro F. Felzenszwalb, Daniel P. Huttenlocher
    // "Distance Transforms of Sampled Functions"


    template <class ValueType>
    struct ChessboardTransform1D
    {
        typedef ValueType value_type;

        int id() const {return 0;}

        void operator()(ValueType*, const ValueType*, int) const
        {
            vigra_fail("fh::detail::ChessboardTransform1D: not implemented");
        }
    };


    template <class ValueType>
    struct ManhattanTransform1D
    {
        typedef ValueType value_type;

        int id() const {return 1;}

        void operator()(ValueType* d, const ValueType* f, int n) const
        {
            const ValueType one = static_cast<ValueType>(1);

            d[0] = f[0];
            for (int q = 1; q < n; ++q)
            {
                d[q] = std::min<ValueType>(f[q], d[q - 1] + one);
            }
            for (int q = n - 2; q >= 0; --q)
            {
                d[q] = std::min<ValueType>(d[q], d[q + 1] + one);
            }
        }
    };


    template <class ValueType>
    struct EuclideanTransform1D
    {
        typedef ValueType value_type;

        int id() const {return 2;}

        void operator()(ValueType* d, const ValueType* f, int n) const
        {
            // IMPLEMENTATION NOTE: Defining math_t as float instead
            // of double would speed up the computation especially on
            // 64bit systems, but in rare occasions causes multiple
            // seam lines when using OpenMP.
            typedef double math_t;
            const math_t max = static_cast<math_t>(std::numeric_limits<ValueType>::max());

            int* v = new int[n];
            math_t* z = new math_t[n + 1];
            int k = 0;

            v[0] = 0;
            z[0] = -max;
            z[1] = max;

            for (int q = 1; q < n; ++q)
            {
                const math_t sum_q = static_cast<math_t>(f[q]) + square(static_cast<math_t>(q));
                math_t s = (sum_q - (f[v[k]] + square(v[k]))) / (2 * (q - v[k]));

                while (s <= z[k])
                {
                    --k;
                    s = (sum_q - (f[v[k]] + square(v[k]))) / (2 * (q - v[k]));
                }
                ++k;

                v[k] = q;
                z[k] = s;
                z[k + 1] = max;
            }

            k = 0;
            for (int q = 0; q < n; ++q)
            {
                while (z[k + 1] < static_cast<math_t>(q))
                {
                    ++k;
                }
                d[q] = square(q - v[k]) + f[v[k]];
            }

            delete [] v;
            delete [] z;
        }
    };


    template <class SrcImageIterator, class SrcAccessor,
              class DestImageIterator, class DestAccessor,
              class ValueType, class Transform1dFunctor>
    void
    fhDistanceTransform(SrcImageIterator src_upperleft, SrcImageIterator src_lowerright, SrcAccessor sa,
                        DestImageIterator dest_upperleft, DestAccessor da,
                        ValueType background, Transform1dFunctor transform1d)
    {
        typedef typename Transform1dFunctor::value_type DistanceType;
        typedef typename vigra::NumericTraits<DistanceType> DistanceTraits;
        typedef vigra::BasicImage<DistanceType> DistanceImageType;

        const vigra::Diff2D size(src_lowerright - src_upperleft);
        const int greatest_length = std::max(size.x, size.y);
        DistanceImageType intermediate(size);

#pragma omp parallel
        {
            DistanceType* f = new DistanceType[greatest_length];
            DistanceType* d = new DistanceType[greatest_length];

// IMPLEMENTATION NOTE
//     We need "guided" schedule to reduce the waiting time at the
//     (implicit) barriers.  This holds true for the next OpenMP
//     parallelized "for" loop, too.
#pragma omp for schedule(guided)
            for (int x = 0; x < size.x; ++x)
            {
                SrcImageIterator si(src_upperleft + vigra::Diff2D(x, 0));
                for (DistanceType* pf = f; pf != f + size.y; ++pf, ++si.y)
                {
                    *pf = sa(si) == background ? DistanceTraits::max() : DistanceTraits::zero();
                }

                transform1d(d, f, size.y);

                typename DistanceImageType::column_iterator ci(intermediate.columnBegin(x));
                for (DistanceType* pd = d; pd != d + size.y; ++pd, ++ci)
                {
                    *ci = *pd;

                    // IMPLEMENTATION NOTE
                    //     PREFETCH() about halves the number of stalls
                    //     per instruction of this loop!
                    PREFETCH((ci + 1).operator->(), PREPARE_FOR_WRITE, HIGH_TEMPORAL_LOCALITY);
                }
            }

#pragma omp for nowait schedule(guided)
            for (int y = 0; y < size.y; ++y)
            {
                transform1d(d, &intermediate(0, y), size.x);
                DestImageIterator i(dest_upperleft + vigra::Diff2D(0, y));

                if (transform1d.id() == 2)
                {
                    for (DistanceType* pd = d; pd != d + size.x; ++pd, ++i.x)
                    {
                        da.set(sqrt(*pd), i);
                    }
                }
                else
                {
                    for (DistanceType* pd = d; pd != d + size.x; ++pd, ++i.x)
                    {
                        da.set(*pd, i);
                    }
                }
            }

            delete [] d;
            delete [] f;
        } // omp parallel
    }
} // namespace detail
} // namespace fh


template <class SrcImageIterator, class SrcAccessor,
          class DestImageIterator, class DestAccessor,
          class ValueType>
void
distanceTransformMP(SrcImageIterator src_upperleft, SrcImageIterator src_lowerright, SrcAccessor sa,
                    DestImageIterator dest_upperleft, DestAccessor da,
                    ValueType background, int norm)
{
    const vigra::Diff2D size(src_lowerright - src_upperleft);

    if (norm != 0 && // We have no multi-threaded version for chessboard metric yet. - cls
        size.x * size.y >= CROSSOVER_DISTANCE_TRANSFORM)
    {
        if (norm == 1)
        {
            fh::detail::fhDistanceTransform(src_upperleft, src_lowerright, sa,
                                            dest_upperleft, da,
                                            background,
                                            fh::detail::ManhattanTransform1D<float>());
        }
        else
        {
            fh::detail::fhDistanceTransform(src_upperleft, src_lowerright, sa,
                                            dest_upperleft, da,
                                            background,
                                            fh::detail::EuclideanTransform1D<float>());
        }
    }
    else
    {
        distanceTransform(src_upperleft, src_lowerright, sa,
                          dest_upperleft, da,
                          background, norm);
    }
}


#else


#undef OPENMP
#define OPENMP_YEAR 0
#define OPENMP_MONTH 0

inline void omp_set_num_threads(int) {}
inline int omp_get_num_threads() {return 1;}
inline int omp_get_max_threads() {return 1;}
inline int omp_get_thread_num() {return 0;}
inline int omp_get_num_procs() {return 1;}
inline void omp_set_dynamic(int) {}
inline int omp_get_dynamic() {return 0;}
inline int omp_in_parallel() {return 0;}
inline void omp_set_nested(int) {}
inline int omp_get_nested() {return 0;}


template <class SrcImageIterator1, class SrcAccessor1,
          class SrcImageIterator2, class SrcAccessor2,
          class DestImageIterator, class DestAccessor,
          class Functor>
inline void
combineTwoImagesMP(SrcImageIterator1 src1_upperleft,
                   SrcImageIterator1 src1_lowerright, SrcAccessor1 src1_acc,
                   SrcImageIterator2 src2_upperleft, SrcAccessor2 src2_acc,
                   DestImageIterator dest_upperleft, DestAccessor dest_acc,
                   const Functor& func)
{
    vigra::combineTwoImages(src1_upperleft, src1_lowerright, src1_acc,
                            src2_upperleft, src2_acc,
                            dest_upperleft, dest_acc,
                            func);
}


template <class SrcImageIterator1, class SrcAccessor1,
          class SrcImageIterator2, class SrcAccessor2,
          class MaskImageIterator, class MaskAccessor,
          class DestImageIterator, class DestAccessor,
          class Functor>
inline void
combineTwoImagesIfMP(SrcImageIterator1 src1_upperleft, SrcImageIterator1 src1_lowerright, SrcAccessor1 src1_acc,
                     SrcImageIterator2 src2_upperleft, SrcAccessor2 src2_acc,
                     MaskImageIterator mask_upperleft, MaskAccessor mask_acc,
                     DestImageIterator dest_upperleft, DestAccessor dest_acc,
                     const Functor& func)
{
    vigra::combineTwoImagesIf(src1_upperleft, src1_lowerright, src1_acc,
                              src2_upperleft, src2_acc,
                              mask_upperleft, mask_acc,
                              dest_upperleft, dest_acc,
                              func);
}


template <class SrcImageIterator1, class SrcAccessor1,
          class SrcImageIterator2, class SrcAccessor2,
          class SrcImageIterator3, class SrcAccessor3,
          class DestImageIterator, class DestAccessor,
          class Functor>
inline void
combineThreeImagesMP(SrcImageIterator1 src1_upperleft, SrcImageIterator1 src1_lowerright, SrcAccessor1 src1_acc,
                     SrcImageIterator2 src2_upperleft, SrcAccessor2 src2_acc,
                     SrcImageIterator3 src3_upperleft, SrcAccessor3 src3_acc,
                     DestImageIterator dest_upperleft, DestAccessor dest_acc,
                     const Functor& func)
{
    vigra::combineThreeImages(src1_upperleft, src1_lowerright, src1_acc,
                              src2_upperleft, src2_acc,
                              src3_upperleft, src3_acc,
                              dest_upperleft, dest_acc,
                              func);
}


template <class SrcImageIterator, class SrcAccessor,
          class DestImageIterator, class DestAccessor,
          class Functor>
inline void
transformImageMP(SrcImageIterator src_upperleft, SrcImageIterator src_lowerright, SrcAccessor src_acc,
                 DestImageIterator dest_upperleft, DestAccessor dest_acc,
                 const Functor& func)
{
    vigra::transformImage(src_upperleft, src_lowerright, src_acc,
                          dest_upperleft, dest_acc,
                          func);
}


template <class SrcImageIterator, class SrcAccessor,
          class MaskImageIterator, class MaskAccessor,
          class DestImageIterator, class DestAccessor,
          class Functor>
inline void
transformImageIfMP(SrcImageIterator src_upperleft, SrcImageIterator src_lowerright, SrcAccessor src_acc,
                   MaskImageIterator mask_upperleft, MaskAccessor mask_acc,
                   DestImageIterator dest_upperleft, DestAccessor dest_acc,
                   const Functor& func)
{
    vigra::transformImageIf(src_upperleft, src_lowerright, src_acc,
                            mask_upperleft, mask_acc,
                            dest_upperleft, dest_acc,
                            func);
}


template <class SrcImageIterator, class SrcAccessor,
          class DestImageIterator, class DestAccessor,
          class ValueType>
void
distanceTransformMP(SrcImageIterator src_upperleft, SrcImageIterator src_lowerright, SrcAccessor src_acc,
                    DestImageIterator dest_upperleft, DestAccessor dest_acc,
                    ValueType background, int norm)
{
    vigra::distanceTransform(src_upperleft, src_lowerright, src_acc,
                             dest_upperleft, dest_acc,
                             background, norm);
}

#endif // _OPENMP >= 200505


// Answer whether the underlying OpenMP implementation really (thinks
// that it) supports nested parallelism.
inline static bool
have_openmp_nested()
{
    const bool openmp_nested = omp_get_nested();
    omp_set_nested(true);
    const bool result = omp_get_nested() != 0;
    omp_set_nested(openmp_nested);
    return result;
}


// Answer whether the underlying OpenMP implementation really (thinks
// that it) supports dynamic adjustment of the number of threads.
inline static bool
have_openmp_dynamic()
{
    const bool openmp_dynamic = omp_get_dynamic();
    omp_set_dynamic(true);
    const bool result = omp_get_dynamic() != 0;
    omp_set_dynamic(openmp_dynamic);
    return result;
}


//
// Argument Object Factory versions
//

template <class SrcImageIterator1, class SrcAccessor1,
          class SrcImageIterator2, class SrcAccessor2,
          class DestImageIterator, class DestAccessor,
          class Functor>
inline void
combineTwoImagesMP(vigra::triple<SrcImageIterator1, SrcImageIterator1, SrcAccessor1> src1,
                   vigra::pair<SrcImageIterator2, SrcAccessor2> src2,
                   vigra::pair<DestImageIterator, DestAccessor> dest,
                   const Functor& func)
{
    combineTwoImagesMP(src1.first, src1.second, src1.third,
                       src2.first, src2.second,
                       dest.first, dest.second,
                       func);
}


template <class SrcImageIterator1, class SrcAccessor1,
          class SrcImageIterator2, class SrcAccessor2,
          class MaskImageIterator, class MaskAccessor,
          class DestImageIterator, class DestAccessor,
          class Functor>
inline void
combineTwoImagesIfMP(vigra::triple<SrcImageIterator1, SrcImageIterator1, SrcAccessor1> src1,
                     vigra::pair<SrcImageIterator2, SrcAccessor2> src2,
                     vigra::pair<MaskImageIterator, MaskAccessor> mask,
                     vigra::pair<DestImageIterator, DestAccessor> dest,
                     const Functor& func)
{
    combineTwoImagesIfMP(src1.first, src1.second, src1.third,
                         src2.first, src2.second,
                         mask.first, mask.second,
                         dest.first, dest.second,
                         func);
}


template <class SrcImageIterator1, class SrcAccessor1,
          class SrcImageIterator2, class SrcAccessor2,
          class SrcImageIterator3, class SrcAccessor3,
          class DestImageIterator, class DestAccessor,
          class Functor>
inline void
combineThreeImagesMP(vigra::triple<SrcImageIterator1, SrcImageIterator1, SrcAccessor1> src1,
                     vigra::pair<SrcImageIterator2, SrcAccessor2> src2,
                     vigra::pair<SrcImageIterator3, SrcAccessor3> src3,
                     vigra::pair<DestImageIterator, DestAccessor> dest,
                     const Functor& func)
{
    combineThreeImagesMP(src1.first, src1.second, src1.third,
                         src2.first, src2.second,
                         src3.first, src3.second,
                         dest.first, dest.second,
                         func);
}


template <class SrcImageIterator, class SrcAccessor,
          class DestImageIterator, class DestAccessor,
          class Functor>
inline void
transformImageMP(vigra::triple<SrcImageIterator, SrcImageIterator, SrcAccessor> src,
                 vigra::pair<DestImageIterator, DestAccessor> dest,
                 const Functor& func)
{
    transformImageMP(src.first, src.second, src.third,
                     dest.first, dest.second,
                     func);
}


template <class SrcImageIterator, class SrcAccessor,
          class MaskImageIterator, class MaskAccessor,
          class DestImageIterator, class DestAccessor,
          class Functor>
inline void
transformImageIfMP(vigra::triple<SrcImageIterator, SrcImageIterator, SrcAccessor> src,
                   vigra::pair<MaskImageIterator, MaskAccessor> mask,
                   vigra::pair<DestImageIterator, DestAccessor> dest,
                   const Functor& func)
{
    transformImageIfMP(src.first, src.second, src.third,
                       mask.first, mask.second,
                       dest.first, dest.second,
                       func);
}


template <class SrcImageIterator, class SrcAccessor,
          class DestImageIterator, class DestAccessor,
          class ValueType>
inline void
distanceTransformMP(vigra::triple<SrcImageIterator, SrcImageIterator, SrcAccessor> src,
                    vigra::pair<DestImageIterator, DestAccessor> dest,
                    ValueType background, int norm)
{
    distanceTransformMP(src.first, src.second, src.third,
                        dest.first, dest.second,
                        background, norm);
}


#endif // __OPENMP_H__

// Local Variables:
// mode: c++
// End:
