/*
 * Copyright (C) 2004-2012 Andrew Mihal
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
#ifndef __ENFUSE_H__
#define __ENFUSE_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>
#include <iomanip>
#include <list>

#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>
#include <boost/lambda/construct.hpp>
#include <boost/static_assert.hpp>

#include <vigra/flatmorphology.hxx>
#include <vigra/functorexpression.hxx>
#include <vigra/imageiterator.hxx>
#include <vigra/impex.hxx>
#include <vigra/initimage.hxx>
#include <vigra/inspectimage.hxx>
#include <vigra/stdimage.hxx>
#include <vigra/transformimage.hxx>

#include "common.h"
#include "filespec.h"
#include "openmp.h"
#include "numerictraits.h"
#include "fixmath.h"
#include "assemble.h"
#include "blend.h"
#include "bounds.h"
#include "pyramid.h"
#include "mga.h"


using boost::lambda::_1;
using boost::lambda::_2;
using boost::lambda::bind;
using boost::lambda::const_parameters;

using vigra::functor::Arg1;
using vigra::functor::Arg2;
using vigra::functor::Param;


namespace enblend {

static inline double gaussDistribution(double x, double mu, double sigma)
{
    return exp(-0.5 * square((x - mu) / sigma));
}


// Keep sum and sum-of-squares together for improved CPU-cache locality.
template <typename T>
struct ScratchPad {
    ScratchPad() : sum(T()), sumSqr(T()), n(size_t()) {}

    T sum;
    T sumSqr;
    size_t n;
};


template <class SrcIterator, class SrcAccessor,
          class MaskIterator, class MaskAccessor,
          class DestIterator, class DestAccessor>
void localStdDevIf(SrcIterator src_ul, SrcIterator src_lr, SrcAccessor src_acc,
                   MaskIterator mask_ul, MaskAccessor mask_acc,
                   DestIterator dest_ul, DestAccessor dest_acc,
                   vigra::Size2D size)
{
    typedef typename vigra::NumericTraits<typename SrcAccessor::value_type>::RealPromote SrcSumType;
    typedef vigra::NumericTraits<typename DestAccessor::value_type> DestTraits;
    typedef typename SrcIterator::PixelType SrcPixelType;
    typedef typename SrcIterator::row_iterator SrcRowIterator;
    typedef typename MaskIterator::row_iterator MaskRowIterator;
    typedef typename DestIterator::PixelType DestPixelType;
    typedef ScratchPad<SrcSumType> ScratchPadType;
    typedef std::vector<ScratchPadType> ScratchPadArray;
    typedef typename ScratchPadArray::iterator ScratchPadArrayIterator;

    vigra_precondition(size.x > 1 && size.y > 1,
                       "localStdDevIf(): window for local variance must be at least 2x2");
    vigra_precondition(src_lr.x - src_ul.x >= size.x &&
                       src_lr.y - src_ul.y >= size.y,
                       "localStdDevIf(): window larger than image");

    const typename SrcIterator::difference_type imageSize = src_lr - src_ul;
    ScratchPadArray scratchPad(imageSize.x + 1);

    const vigra::Diff2D border(size.x / 2, size.y / 2);
    const vigra::Diff2D nextUpperRight(size.x / 2 + 1, -size.y / 2);

    SrcIterator const srcEnd(src_lr - border);
    SrcIterator const srcEndXm1(srcEnd - vigra::Diff2D(1, 0));

    // For each row in the source image...
#ifdef OPENMP
#pragma omp parallel for firstprivate (scratchPad)
#endif
    for (int row = 0; row < imageSize.y - 2 * border.y; ++row)
    {
        SrcIterator srcRow(src_ul + border + vigra::Diff2D(0, row));
        MaskIterator maskRow(mask_ul + border + vigra::Diff2D(0, row));
        DestIterator destRow(dest_ul + border + vigra::Diff2D(0, row));

        // Row's running values
        SrcSumType sum = vigra::NumericTraits<SrcSumType>::zero();
        SrcSumType sumSqr = vigra::NumericTraits<SrcSumType>::zero();
        size_t n = 0;

        SrcIterator const windowSrcUpperLeft(srcRow - border);
        SrcIterator const windowSrcLowerRight(srcRow + border);
        SrcIterator windowSrc;
        MaskIterator const windowMaskUpperLeft(maskRow - border);
        MaskIterator windowMask;
        ScratchPadArrayIterator spCol;

        // Initialize running-sums of this row
        for (windowSrc = windowSrcUpperLeft, windowMask = windowMaskUpperLeft,
                 spCol = scratchPad.begin();
             windowSrc.x <= windowSrcLowerRight.x;
             ++windowSrc.x, ++windowMask.x, ++spCol)
        {
            SrcSumType sumInit = vigra::NumericTraits<SrcSumType>::zero();
            SrcSumType sumSqrInit = vigra::NumericTraits<SrcSumType>::zero();
            size_t nInit = 0;

            for (windowSrc.y = windowSrcUpperLeft.y, windowMask.y = windowMaskUpperLeft.y;
                 windowSrc.y <= windowSrcLowerRight.y;
                 ++windowSrc.y, ++windowMask.y)
            {
                if (mask_acc(windowMask))
                {
                    const SrcSumType value = src_acc(windowSrc);
                    sumInit += value;
                    sumSqrInit += square(value);
                    ++nInit;
                }
            }

            // Set scratch pad's column-wise values
            spCol->sum = sumInit;
            spCol->sumSqr = sumSqrInit;
            spCol->n = nInit;

            // Update totals
            sum += sumInit;
            sumSqr += sumSqrInit;
            n += nInit;
        }

        // Write one row of results
        SrcIterator srcCol(srcRow);
        MaskIterator maskCol(maskRow);
        DestIterator destCol(destRow);
        ScratchPadArrayIterator old(scratchPad.begin());
        ScratchPadArrayIterator next(scratchPad.begin() + size.x);

        while (true)
        {
            // Compute standard deviation
            if (mask_acc(maskCol))
            {
                const SrcSumType result =
                    n <= 1 ?
                    vigra::NumericTraits<SrcSumType>::zero() :
                    sqrt((sumSqr - square(sum) / n) / (n - 1));
                dest_acc.set(DestTraits::fromRealPromote(result), destCol);
            }
            if (srcCol.x == srcEndXm1.x)
            {
                break;
            }

            // Compute auxilliary values of next column
            SrcSumType sumInit = vigra::NumericTraits<SrcSumType>::zero();
            SrcSumType sumSqrInit = vigra::NumericTraits<SrcSumType>::zero();
            size_t nInit = 0;

            for (windowSrc = srcCol + nextUpperRight, windowMask = maskCol + nextUpperRight;
                 windowSrc.y <= windowSrcLowerRight.y;
                 ++windowSrc.y, ++windowMask.y)
            {
                if (mask_acc(windowMask))
                {
                    const SrcSumType value = src_acc(windowSrc);
                    sumInit += value;
                    sumSqrInit += square(value);
                    ++nInit;
                }
            }

            // Set sums of next column
            next->sum = sumInit;
            next->sumSqr = sumSqrInit;
            next->n = nInit;

            // Update totals
            sum += sumInit - old->sum;
            sumSqr += sumSqrInit - old->sumSqr;
            n += nInit - old->n;

            // Advance to next column
            ++srcCol.x;
            ++maskCol.x;
            ++destCol.x;
            ++old;
            ++next;
        }
    }
}


template <typename InputPixelType, typename ResultPixelType>
class Histogram
{
    enum {GRAY = 0, CHANNELS = 3};

public:
    typedef vigra::NumericTraits<InputPixelType> InputPixelTraits;
    typedef typename InputPixelTraits::ValueType KeyType; // scalar values are our keys
    typedef typename InputPixelTraits::isScalar pixelIsScalar;
    typedef unsigned DataType;  // pixel counts are our data
    typedef vigra::NumericTraits<ResultPixelType> ResultPixelTraits;
    typedef typename ResultPixelTraits::ValueType ResultType;
    typedef map<KeyType, DataType> MapType;
    typedef std::pair<KeyType, DataType> PairType;
    typedef typename MapType::const_iterator MapConstIterator;
    typedef typename MapType::iterator MapIterator;
    typedef typename MapType::size_type MapSizeType;

    Histogram() {clear();}

    void clear() {
        for (int channel = 0; channel < CHANNELS; ++channel) {
            totalCount[channel] = DataType();
            histogram[channel].clear();
        }
    }

    static void setPrecomputedEntropySize(size_t size) {
        // PERFORMANCE: setPrecomputedEntropySize is a pure
        // performance enhancer otherwise the function is completely
        // redundant.  It derives its existence from the facts that
        // computing the entropy "p * log(p)" given the probability
        // "p" is an expensive operation _and_ most of the time the
        // moving window is filled completely, i.e., no pixel is
        // masked.
        precomputedSize = size;
        delete [] precomputedLog;
        delete [] precomputedEntropy;
        if (size == 0)
        {
            precomputedLog = NULL;
            precomputedEntropy = NULL;
        }
        else
        {
            precomputedLog = new double[size + 1];
            vigra_precondition(precomputedLog != NULL,
                               "Histogram::setPrecomputedSize: failed to allocate log-preevaluate memory");
            precomputedEntropy = new double[size + 1];
            vigra_precondition(precomputedEntropy != NULL,
                               "Histogram::setPrecomputedSize: failed to allocate entropy-preevaluate memory");
            precomputedLog[0] = 0.0; // just to have a reliable value
            precomputedEntropy[0] = 0.0;
            for (size_t i = 1; i <= size; ++i)
            {
                const double p = static_cast<double>(i) / static_cast<double>(size);
                precomputedLog[i] = log(static_cast<double>(i));
                precomputedEntropy[i] = p * log(p);
            }
        }
    }

    Histogram& operator=(const Histogram& other) {
        if (this != &other)
        {
            for (int channel = 0; channel < CHANNELS; ++channel)
            {
                totalCount[channel] = other.totalCount[channel];
                histogram[channel] = other.histogram[channel];
            }
        }
        return *this;
    }

    void insert(const InputPixelType& x) {insertFun(x, pixelIsScalar());}
    void insert(const Histogram* other) {insertFun(other, pixelIsScalar());}

    void erase(const InputPixelType& x) {eraseFun(x, pixelIsScalar());}
    void erase(const Histogram* other) {eraseFun(other, pixelIsScalar());}

    ResultPixelType entropy() const {return entropyFun(pixelIsScalar());}

protected:
    void insertInChannel(int channel, const PairType& keyval) {
        // PERFORMANCE: The actual insertion code below code is a much
        // faster version of
        //     MapIterator const i = histogram[channel].find(keyval.first);
        //     if (i == histogram[channel].end())
        //         histogram[channel].insert(keyval);
        //     else i->second += keyval.second;
        MapIterator const lowerBound = histogram[channel].lower_bound(keyval.first);
        const DataType count = keyval.second;
        if (lowerBound != histogram[channel].end() &&
            !(histogram[channel].key_comp()(keyval.first, lowerBound->first)))
        {
            lowerBound->second += count;
        }
        else
        {
            histogram[channel].insert(lowerBound, keyval);
        }
        totalCount[channel] += count;
    }

    void eraseInChannel(int channel, const PairType& keyval) {
        MapIterator const i = histogram[channel].find(keyval.first);
        assert(i != histogram[channel].end());
        DataType& c = i->second;
        const DataType count = keyval.second;
        if (c > count)
        {
            c -= count;
        }
        else
        {
            // PERFORMANCE: It is _much_ faster to erase unneeded bins
            // right away than it is e.g. to periodically (think of
            // every column) cleaning up the whole map while wasting
            // time in lots of comparisons until then.
            histogram[channel].erase(i);
        }
        totalCount[channel] -= count;
    }

    double entropyOfChannel(int channel) const {
        const DataType total = totalCount[channel];
        const MapSizeType actualBins = histogram[channel].size();
        if (total == 0 || actualBins <= 1)
        {
            return 0.0;
        }
        else
        {
            double e = 0.0;
            MapConstIterator const end = histogram[channel].end();
            if (total == precomputedSize)
            {
                for (MapConstIterator i = histogram[channel].begin(); i != end; ++i)
                {
                    e += precomputedEntropy[i->second];
                }
                return -e / precomputedLog[actualBins];
            }
            else
            {
                for (MapConstIterator i = histogram[channel].begin(); i != end; ++i)
                {
                    const double p = i->second / static_cast<double>(total);
                    e += p * log(p);
                }
                return -e / log(static_cast<double>(actualBins));
            }
        }
    }

    // Grayscale
    void insertFun(const InputPixelType& x, vigra::VigraTrueType) {
        insertInChannel(GRAY, PairType(x, 1U));
    }

    void insertFun(const Histogram* other, vigra::VigraTrueType) {
        MapConstIterator const end = other->histogram[GRAY].end();
        for (MapConstIterator i = other->histogram[GRAY].begin(); i != end; ++i)
        {
            insertInChannel(GRAY, *i);
        }
    }

    void eraseFun(const InputPixelType& x, vigra::VigraTrueType) {
        eraseInChannel(GRAY, PairType(x, 1U));
    }

    void eraseFun(const Histogram* other, vigra::VigraTrueType) {
        MapConstIterator const end = other->histogram[GRAY].end();
        for (MapConstIterator i = other->histogram[GRAY].begin(); i != end; ++i)
        {
            eraseInChannel(GRAY, *i);
        }
    }

    ResultPixelType entropyFun(vigra::VigraTrueType) const {
        const double max = static_cast<double>(vigra::NumericTraits<KeyType>::max());
        return ResultPixelType(ResultPixelTraits::fromRealPromote(entropyOfChannel(GRAY) * max));
    }

    // RGB
    void insertFun(const InputPixelType& x, vigra::VigraFalseType) {
        for (int channel = 0; channel < CHANNELS; ++channel) {
            insertInChannel(channel, PairType(x[channel], 1U));
        }
    }

    void insertFun(const Histogram* other, vigra::VigraFalseType) {
        for (int channel = 0; channel < CHANNELS; ++channel)
        {
            MapConstIterator const end = other->histogram[channel].end();
            for (MapConstIterator i = other->histogram[channel].begin(); i != end; ++i)
            {
                insertInChannel(channel, *i);
            }
        }
    }

    void eraseFun(const InputPixelType& x, vigra::VigraFalseType) {
        for (int channel = 0; channel < CHANNELS; ++channel) {
            eraseInChannel(channel, PairType(x[channel], 1U));
        }
    }

    void eraseFun(const Histogram* other, vigra::VigraFalseType) {
        for (int channel = 0; channel < CHANNELS; ++channel)
        {
            MapConstIterator const end = other->histogram[channel].end();
            for (MapConstIterator i = other->histogram[channel].begin(); i != end; ++i)
            {
                eraseInChannel(channel, *i);
            }
        }
    }

    ResultPixelType entropyFun(vigra::VigraFalseType) const {
        const double max = static_cast<double>(vigra::NumericTraits<KeyType>::max());
        return ResultPixelType(vigra::NumericTraits<ResultType>::fromRealPromote(entropyOfChannel(0) * max),
                               vigra::NumericTraits<ResultType>::fromRealPromote(entropyOfChannel(1) * max),
                               vigra::NumericTraits<ResultType>::fromRealPromote(entropyOfChannel(2) * max));
    }

private:
    static size_t precomputedSize;
    static double* precomputedLog;
    static double* precomputedEntropy;
    MapType histogram[CHANNELS];
    DataType totalCount[CHANNELS];
};


template <class SrcIterator, class SrcAccessor,
          class MaskIterator, class MaskAccessor,
          class DestIterator, class DestAccessor>
void localEntropyIf(SrcIterator src_ul, SrcIterator src_lr, SrcAccessor src_acc,
                    MaskIterator mask_ul, MaskAccessor mask_acc,
                    DestIterator dest_ul, DestAccessor dest_acc,
                    vigra::Size2D size)
{
    typedef vigra::NumericTraits<typename DestAccessor::value_type> DestTraits;
    typedef typename SrcIterator::PixelType SrcPixelType;
    typedef typename SrcIterator::row_iterator SrcRowIterator;
    typedef typename MaskIterator::row_iterator MaskRowIterator;
    typedef typename DestIterator::PixelType DestPixelType;
    typedef Histogram<SrcPixelType, DestPixelType> ScratchPadType;

    vigra_precondition(src_lr.x - src_ul.x >= size.x &&
                       src_lr.y - src_ul.y >= size.y,
                       "localEntropyIf(): window larger than image");

    const typename SrcIterator::difference_type imageSize = src_lr - src_ul;
    ScratchPadType* const scratchPad = new ScratchPadType[imageSize.y + 1];

    ScratchPadType::setPrecomputedEntropySize(size.x * size.y);

    const vigra::Diff2D border(size.x / 2, size.y / 2);
    const vigra::Diff2D deltaX(size.x / 2, 0);
    const vigra::Diff2D deltaXp1(size.x / 2 + 1, 0);
    const vigra::Diff2D deltaY(0, size.y / 2);

    // Fill scratch pad for the first time.
    {
        SrcIterator srcRow(src_ul + deltaX);
        SrcIterator const srcEnd(src_lr - deltaX);
        MaskIterator maskRow(mask_ul + deltaX);
        ScratchPadType* spRow(scratchPad);

        for (; srcRow.y < srcEnd.y; ++srcRow.y, ++maskRow.y, ++spRow)
        {
            SrcIterator srcCol(srcRow - deltaX);
            SrcIterator srcColEnd(srcRow + deltaX);
            MaskIterator maskCol(maskRow - deltaX);

            for (; srcCol.x <= srcColEnd.x; ++srcCol.x, ++maskCol.x)
            {
                if (mask_acc(maskCol))
                {
                    spRow->insert(src_acc(srcCol));
                }
            }
        }
    }

    // Iterate through the image
    {
        SrcIterator srcCol(src_ul + border);
        SrcIterator const srcEnd(src_lr - border);
        MaskIterator maskCol(mask_ul + border);
        DestIterator destCol(dest_ul + border);

        ScratchPadType hist;

        // For each column in the source image...
        for (; srcCol.x < srcEnd.x; ++srcCol.x, ++maskCol.x, ++destCol.x)
        {
            SrcIterator srcRow(srcCol);
            MaskIterator maskRow(maskCol);
            DestIterator destRow(destCol);
            ScratchPadType* spRow(scratchPad + border.y);

            // Initialize running histogram of this column
            hist.clear();
            for (ScratchPadType* s = spRow - border.y; s <= spRow + border.y; ++s)
            {
                hist.insert(s);
            }

            // Write one column of results
            for (; srcRow.y < srcEnd.y; ++srcRow.y, ++maskRow.y, ++destRow.y, ++spRow)
            {
                // Compute entropy
                if (mask_acc(maskRow))
                {
                    dest_acc.set(hist.entropy(), destRow);
                }

                // Update running histogram to next row
                hist.erase(spRow - border.y); // remove oldest row
                hist.insert(spRow + border.y + 1); // add next row
            }

            // Update scratch pad to next column
            for (srcRow = srcCol - deltaY, maskRow = maskCol - deltaY, spRow = scratchPad;
                 srcRow.y < src_lr.y;
                 ++srcRow.y, ++maskRow.y, ++spRow)
            {
                if (mask_acc(maskRow - deltaX))
                {
                    // remove oldest column
                    spRow->erase(src_acc(srcRow - deltaX));
                }
                if (mask_acc(maskRow + deltaXp1))
                {
                    // add next column
                    spRow->insert(src_acc(srcRow + deltaXp1));
                }
            }
        }
    }

    ScratchPadType::setPrecomputedEntropySize(0);
    delete [] scratchPad;
}


template <typename SrcIterator, typename SrcAccessor,
          typename MaskIterator, typename MaskAccessor,
          typename DestIterator, typename DestAccessor>
inline void
localEntropyIf(vigra::triple<SrcIterator, SrcIterator, SrcAccessor> src,
               vigra::pair<MaskIterator, MaskAccessor> mask,
               vigra::pair<DestIterator, DestAccessor> dest,
               vigra::Size2D size)
{
    localEntropyIf(src.first, src.second, src.third,
                   mask.first, mask.second,
                   dest.first, dest.second,
                   size);
}


template <typename SrcIterator, typename SrcAccessor,
          typename MaskIterator, typename MaskAccessor,
          typename DestIterator, typename DestAccessor>
inline void
localStdDevIf(vigra::triple<SrcIterator, SrcIterator, SrcAccessor> src,
              vigra::pair<MaskIterator, MaskAccessor> mask,
              vigra::pair<DestIterator, DestAccessor> dest,
              vigra::Size2D size)
{
    localStdDevIf(src.first, src.second, src.third,
                  mask.first, mask.second,
                  dest.first, dest.second,
                  size);
}


template <typename MaskPixelType>
class ImageMaskMultiplyFunctor {
public:
    ImageMaskMultiplyFunctor(MaskPixelType d) :
        divisor(vigra::NumericTraits<MaskPixelType>::toRealPromote(d)) {}

    template <typename ImagePixelType>
    ImagePixelType operator()(const ImagePixelType& iP, const MaskPixelType& maskP) const {
        typedef typename vigra::NumericTraits<ImagePixelType>::RealPromote RealImagePixelType;

        // Convert mask pixel to blend coefficient in range [0.0, 1.0].
        const double maskCoeff = vigra::NumericTraits<MaskPixelType>::toRealPromote(maskP) / divisor;
        const RealImagePixelType riP = vigra::NumericTraits<ImagePixelType>::toRealPromote(iP);
        const RealImagePixelType blendP = riP * maskCoeff;
        return vigra::NumericTraits<ImagePixelType>::fromRealPromote(blendP);
    }

protected:
    const double divisor;
};


template <typename InputType, typename InputAccessor, typename ResultType>
class ExposureFunctor {
public:
    typedef ResultType result_type;

    ExposureFunctor(double w, double m, double s, InputAccessor a) :
        weight(w), mu(m), sigma(s), acc(a) {}

    inline ResultType operator()(const InputType& a) const {
        typedef typename vigra::NumericTraits<InputType>::isScalar srcIsScalar;
        return f(a, srcIsScalar());
    }

protected:
    // grayscale
    template <typename T>
    inline ResultType f(const T& a, vigra::VigraTrueType) const {
        typedef typename vigra::NumericTraits<T>::RealPromote RealType;
        const RealType ra = vigra::NumericTraits<T>::toRealPromote(a);
        const double b = vigra::NumericTraits<T>::max() * mu;
        const double c = vigra::NumericTraits<T>::max() * sigma;
        return vigra::NumericTraits<ResultType>::fromRealPromote(weight * gaussDistribution(ra, b, c));
    }

    // RGB
    template <typename T>
    inline ResultType f(const T& a, vigra::VigraFalseType) const {
        return f(acc.operator()(a), vigra::VigraTrueType());
    }

    const double weight;
    const double mu;
    const double sigma;
    InputAccessor acc;
};


template <typename InputType, typename InputAccessor, typename ResultType>
class CutoffExposureFunctor {
public:
    typedef ResultType result_type;

    CutoffExposureFunctor(double w, double m, double s, InputAccessor a,
                          const AlternativePercentage& lc, const AlternativePercentage& uc,
                          InputAccessor lca, InputAccessor uca) :
        weight(w), mu(m), sigma(s), acc(a),
        lower_cutoff(lc.instantiate<typename InputAccessor::value_type>()),
        upper_cutoff(uc.instantiate<typename InputAccessor::value_type>()),
        lower_acc(lca), upper_acc(uca)
    {
        typedef typename InputAccessor::value_type value_type;

        const value_type max = vigra::NumericTraits<value_type>::max();

        if (lower_cutoff > upper_cutoff) {
            std::cerr << command <<
                ": lower exposure cutoff (" << lower_cutoff << "/" << max <<
                " = " << 100.0 * lower_cutoff / max <<
                "%) exceeds upper cutoff (" << upper_cutoff << "/" << max <<
                " = " << 100.0 * upper_cutoff / max <<
                "%)" << endl;
            exit(1);
        }
    }

    inline ResultType operator()(const InputType& a) const {
        typedef typename vigra::NumericTraits<InputType>::isScalar srcIsScalar;
        return f(a, srcIsScalar());
    }

protected:
    // grayscale
    template <typename T>
    inline ResultType f(const T& a, vigra::VigraTrueType) const {
        typedef typename vigra::NumericTraits<T>::RealPromote RealType;
        const RealType ra = vigra::NumericTraits<T>::toRealPromote(a);
        if (ra >= lower_cutoff && ra <= upper_cutoff) {
            const double b = vigra::NumericTraits<T>::max() * mu;
            const double c = vigra::NumericTraits<T>::max() * sigma;
            return vigra::NumericTraits<ResultType>::fromRealPromote(weight * gaussDistribution(ra, b, c));
        } else {
            return ResultType();
        }
    }

    // RGB
    template <typename T>
    inline ResultType f(const T& a, vigra::VigraFalseType) const {
        typedef typename T::value_type ValueType;
        typedef typename vigra::NumericTraits<ValueType>::RealPromote RealType;
        const RealType ra = vigra::NumericTraits<ValueType>::toRealPromote(acc.operator()(a));
        const RealType lower_ra = vigra::NumericTraits<ValueType>::toRealPromote(lower_acc.operator()(a));
        const RealType upper_ra = vigra::NumericTraits<ValueType>::toRealPromote(upper_acc.operator()(a));
        if (lower_ra >= lower_cutoff && upper_ra <= upper_cutoff) {
            const double b = vigra::NumericTraits<ValueType>::max() * mu;
            const double c = vigra::NumericTraits<ValueType>::max() * sigma;
            return vigra::NumericTraits<ResultType>::fromRealPromote(weight * gaussDistribution(ra, b, c));
        } else {
            return ResultType();
        }
    }

    const double weight;
    const double mu;
    const double sigma;
    InputAccessor acc;
    const double lower_cutoff;
    const double upper_cutoff;
    InputAccessor lower_acc;
    InputAccessor upper_acc;
};


template <typename InputType, typename ResultType>
class SaturationFunctor {
public:
    typedef ResultType result_type;

    SaturationFunctor(double w) : weight(w) {}

    inline ResultType operator()(const InputType& a) const {
        typedef typename vigra::NumericTraits<InputType>::isScalar srcIsScalar;
        return f(a, srcIsScalar());
    }

protected:
    // grayscale
    template <typename T>
    inline ResultType f(const T& a, vigra::VigraTrueType) const {
        return vigra::NumericTraits<ResultType>::zero();
    }

    // RGB
    template <typename T>
    inline ResultType f(const T& a, vigra::VigraFalseType) const {
        typedef typename T::value_type value_type;
        typedef vigra::NumericTraits<value_type> value_traits;
        typedef vigra::NumericTraits<ResultType> result_traits;

        const value_type max = std::max(a.red(), std::max(a.green(), a.blue()));
        const value_type min = std::min(a.red(), std::min(a.green(), a.blue()));
        if (max == min)
        {
            return result_traits::zero();
        }
        else
        {
            const double max_value =
                value_traits::isIntegral::asBool ?
                static_cast<double>(value_traits::max()) :
                1.0;
            const double sum = static_cast<double>(max) + static_cast<double>(min);
            const double difference = static_cast<double>(max) - static_cast<double>(min);
            const double saturation =
                sum <= max_value ?
                difference / sum :
                difference / (2.0 * max_value - sum);
            return result_traits::fromRealPromote(weight * saturation);
        }
    }

    const double weight;
};


template <typename InputType, typename ScaleType, typename ResultType>
class ContrastFunctor {
public:
    typedef ResultType result_type;

    ContrastFunctor(double w) : weight(w) {}

    inline ResultType operator()(const InputType& a) const {
        typedef typename vigra::NumericTraits<InputType>::isScalar srcIsScalar;
        typedef typename vigra::NumericTraits<ScaleType>::isIntegral scaleIsIntegral;
        return f(a, srcIsScalar(), scaleIsIntegral());
    }

protected:
    // grayscale, integral
    template <typename T>
    inline ResultType f(const T& a, vigra::VigraTrueType, vigra::VigraTrueType) const {
        const typename vigra::NumericTraits<T>::RealPromote ra = vigra::NumericTraits<T>::toRealPromote(a);
        return vigra::NumericTraits<ResultType>::fromRealPromote(weight * ra / vigra::NumericTraits<ScaleType>::max());
    }

    // grayscale, floating-point
    template <typename T>
    inline ResultType f(const T& a, vigra::VigraTrueType, vigra::VigraFalseType) const {
        const typename vigra::NumericTraits<T>::RealPromote ra = vigra::NumericTraits<T>::toRealPromote(a);
        return vigra::NumericTraits<ResultType>::fromRealPromote(weight * ra);
    }

    // RGB, integral
    template <typename T>
    inline ResultType f(const T& a, vigra::VigraFalseType, vigra::VigraTrueType) const {
        typedef typename T::value_type TComponentType;
        typedef typename vigra::NumericTraits<TComponentType>::RealPromote RealTComponentType;
        typedef typename ScaleType::value_type ScaleComponentType;
        const RealTComponentType ra = static_cast<RealTComponentType>(a.lightness());
        return vigra::NumericTraits<ResultType>::fromRealPromote(weight * ra / vigra::NumericTraits<ScaleComponentType>::max());
    }

    // RGB, floating-point
    template <typename T>
    inline ResultType f(const T& a, vigra::VigraFalseType, vigra::VigraFalseType) const {
        typedef typename T::value_type TComponentType;
        typedef typename vigra::NumericTraits<TComponentType>::RealPromote RealTComponentType;
        const RealTComponentType ra = static_cast<RealTComponentType>(a.lightness());
        return vigra::NumericTraits<ResultType>::fromRealPromote(weight * ra);
    }

    const double weight;
};


template <typename InputType, typename ResultType>
class EntropyFunctor {
public:
    typedef vigra::NumericTraits<InputType> InputTraits;
    typedef vigra::NumericTraits<ResultType> ResultTraits;
    typedef ResultType result_type;

    EntropyFunctor(double w) : weight(w) {}

    ResultType operator()(const InputType& x) const {
        typedef typename InputTraits::isScalar srcIsScalar;
        return entropy(x, srcIsScalar());
    }

protected:
    // Grayscale
    ResultType entropy(const InputType& x, vigra::VigraTrueType) const {
        return ResultTraits::fromRealPromote(weight * ResultTraits::toRealPromote(x));
    }

    // RGB
    ResultType entropy(const InputType& x, vigra::VigraFalseType) const {
        const typename ResultTraits::RealPromote minimum =
            ResultTraits::toRealPromote(std::min(std::min(x.red(), x.green()), x.blue()));
        return ResultTraits::fromRealPromote(weight * minimum);
    }

private:
    const double weight;
};


template <typename ValueType>
struct MagnitudeAccessor
{
    typedef ValueType value_type;

    template <class Iterator>
    ValueType operator()(const Iterator& i) const {return std::abs(*i);}

    ValueType operator()(const ValueType* i) const {return std::abs(*i);}

    template <class Iterator, class Difference>
    ValueType operator()(const Iterator& i, Difference d) const {return std::abs(i[d]);}

    template <class Value, class Iterator>
    void set(const Value& v, const Iterator& i) const {
        *i = vigra::detail::RequiresExplicitCast<ValueType>::cast(std::abs(v));
    }

    template <class Value, class Iterator>
    void set(const Value& v, Iterator& i) const {
        *i = vigra::detail::RequiresExplicitCast<ValueType>::cast(std::abs(v));
    }

    template <class Value, class Iterator, class Difference>
    void set(const Value& v, const Iterator& i, const Difference& d) const {
        i[d] = vigra::detail::RequiresExplicitCast<ValueType>::cast(std::abs(v));
    }
};


template <typename InputType, typename ResultType>
class ClampingFunctor
{
public:
    typedef vigra::NumericTraits<InputType> InputTraits;

    ClampingFunctor(InputType lower, ResultType lowerValue,
                    InputType upper, ResultType upperValue) :
        lo(lower), up(upper), loval(lowerValue), upval(upperValue)
    {}

    ResultType operator()(const InputType& x) const {
        typedef typename InputTraits::isScalar srcIsScalar;
        return clamp(x, srcIsScalar());
    }

protected:
    ResultType clamp(const InputType& x, vigra::VigraTrueType) const {
        return x <= lo ? loval : (x >= up ? upval : x);
    }

    // RGB
    ResultType clamp(const InputType& x, vigra::VigraFalseType) const {
        return ResultType(x.red() <= lo.red() ?
                          loval.red() :
                          (x.red() >= up.red() ? upval.red() : x.red()),
                          x.green() <= lo.green() ?
                          loval.green() :
                          (x.green() >= up.green() ? upval.green() : x.green()),
                          x.red() <= lo.red() ?
                          loval.blue() :
                          (x.blue() >= up.blue() ? upval.blue() : x.blue()));
    }

private:
    const InputType lo, up;
    const ResultType loval, upval;
};


// If the first argument is lower than THRESHOLD return the second
// argument, i.e. the "fill-in" value multiplied with SCALE2,
// otherwise return the first argument multiplied with SCALE1.
template <typename InputType, typename ResultType>
class FillInFunctor
{
public:
    FillInFunctor(InputType thr, double s1, double s2) :
        threshold(thr), scale1(s1), scale2(s2)
    {}

    ResultType operator()(const InputType& x, const InputType& y) const {
        if (x >= threshold)
        {
            return vigra::NumericTraits<ResultType>::fromRealPromote(scale1 * x);
        }
        else
        {
            return vigra::NumericTraits<ResultType>::fromRealPromote(scale2 * y);
        }
    }

private:
    const InputType threshold;
    const double scale1, scale2;
};


template <typename ImageType, typename AlphaType, typename MaskType>
void enfuseMask(vigra::triple<typename ImageType::const_traverser, typename ImageType::const_traverser, typename ImageType::ConstAccessor> src,
                vigra::pair<typename AlphaType::const_traverser, typename AlphaType::ConstAccessor> mask,
                vigra::pair<typename MaskType::traverser, typename MaskType::Accessor> result) {
    typedef typename ImageType::value_type ImageValueType;
    typedef typename ImageType::PixelType PixelType;
    typedef typename vigra::NumericTraits<PixelType>::ValueType ScalarType;
    typedef typename MaskType::value_type MaskValueType;

    const typename ImageType::difference_type imageSize = src.second - src.first;

    // Exposure
    if (WExposure > 0.0) {
        typedef MultiGrayscaleAccessor<ImageValueType, ScalarType> MultiGrayAcc;
        MultiGrayAcc ga(GrayscaleProjector);

        if (ExposureLowerCutoff.is_effective<ScalarType>() ||
            ExposureUpperCutoff.is_effective<ScalarType>()) {
            MultiGrayAcc lca(ExposureLowerCutoffGrayscaleProjector.empty() ?
                             GrayscaleProjector :
                             ExposureLowerCutoffGrayscaleProjector);
            MultiGrayAcc uca(ExposureUpperCutoffGrayscaleProjector.empty() ?
                             ExposureLowerCutoffGrayscaleProjector :
                             ExposureUpperCutoffGrayscaleProjector);
            CutoffExposureFunctor<ImageValueType, MultiGrayAcc, MaskValueType>
                cef(WExposure, WMu, WSigma, ga,
                    ExposureLowerCutoff, ExposureUpperCutoff, lca, uca);
#ifdef DEBUG_EXPOSURE
            std::cout << "+ enfuseMask: cutoff - GrayscaleProjector = <" <<
                GrayscaleProjector << ">\n" <<
                "+ enfuseMask:          ExposureLowerCutoffGrayscaleProjector = <" <<
                ExposureLowerCutoffGrayscaleProjector << ">\n" <<
                "+ enfuseMask:          ExposureUpperCutoffGrayscaleProjector = <" <<
                ExposureUpperCutoffGrayscaleProjector << ">\n";
#endif
            transformImageIfMP(src, mask, result, cef);
        } else {
            ExposureFunctor<ImageValueType, MultiGrayAcc, MaskValueType>
                ef(WExposure, WMu, WSigma, ga);
#ifdef DEBUG_EXPOSURE
            std::cout << "+ enfuseMask: plain - GrayscaleProjector = <" <<
                GrayscaleProjector << ">\n";
#endif
            transformImageIfMP(src, mask, result, ef);
        }
    }

    // Contrast
    if (WContrast > 0.0) {
        typedef typename vigra::NumericTraits<ScalarType>::Promote LongScalarType;
        typedef IMAGETYPE<LongScalarType> GradImage;
        typedef typename GradImage::iterator GradIterator;

        GradImage grad(imageSize);
        MultiGrayscaleAccessor<PixelType, LongScalarType> ga(GrayscaleProjector);

        if (FilterConfig.edgeScale > 0.0)
        {
#ifdef DEBUG_LOG
            std::cout << "+ Laplacian Edge Detection, scale = "
                      << FilterConfig.edgeScale << " pixels" << endl;
#endif
            GradImage laplacian(imageSize);

            if (FilterConfig.lceScale > 0.0)
            {
#ifdef DEBUG_LOG
                std::cout << "+ Local Contrast Enhancement, (scale, amount) = "
                          << FilterConfig.lceScale << " pixels, "
                          << (100.0 * FilterConfig.lceFactor) << "%" << endl;
#endif
                GradImage lce(imageSize);
                vigra::gaussianSharpening(src.first, src.second, ga,
                                          lce.upperLeft(), lce.accessor(),
                                          FilterConfig.lceFactor, FilterConfig.lceScale);
                vigra::laplacianOfGaussian(lce.upperLeft(), lce.lowerRight(), lce.accessor(),
                                           laplacian.upperLeft(), MagnitudeAccessor<LongScalarType>(),
                                           FilterConfig.edgeScale);
            }
            else
            {
                vigra::laplacianOfGaussian(src.first, src.second, ga,
                                           laplacian.upperLeft(), MagnitudeAccessor<LongScalarType>(),
                                           FilterConfig.edgeScale);
            }

#ifdef DEBUG_LOG
            {
                vigra::FindMinMax<LongScalarType> minmax;
                vigra::inspectImage(srcImageRange(laplacian), minmax);
                std::cout << "+ after Laplacian and Magnitude: min = " <<
                    minmax.min << ", max = " << minmax.max << endl;
            }
#endif

            const double minCurve = static_cast<double>(MinCurvature.instantiate<ScalarType>());
            if (minCurve <= 0.0)
            {
#ifdef DEBUG_LOG
                std::cout << "+ truncate values below " << -minCurve << endl;;
#endif
                transformImageIfMP(laplacian.upperLeft(), laplacian.lowerRight(), laplacian.accessor(),
                                   mask.first, mask.second,
                                   grad.upperLeft(), grad.accessor(),
                                   ClampingFunctor<LongScalarType, LongScalarType>
                                   (static_cast<LongScalarType>(-minCurve), LongScalarType(),
                                    vigra::NumericTraits<LongScalarType>::max(), vigra::NumericTraits<LongScalarType>::max()));
            }
            else
            {
#ifdef DEBUG_LOG
                std::cout << "+ merge local contrast and edges - switch at " << minCurve << endl;
#endif
                GradImage localContrast(imageSize);
                // TODO: use localStdDev
                localStdDevIf(src.first, src.second, ga,
                              mask.first, mask.second,
                              localContrast.upperLeft(), localContrast.accessor(),
                              vigra::Size2D(ContrastWindowSize, ContrastWindowSize));

                combineTwoImagesIfMP(laplacian.upperLeft(), laplacian.lowerRight(), laplacian.accessor(),
                                     localContrast.upperLeft(), localContrast.accessor(),
                                     mask.first, mask.second,
                                     grad.upperLeft(), grad.accessor(),
                                     FillInFunctor<LongScalarType, LongScalarType>
                                     (static_cast<LongScalarType>(minCurve), // threshold
                                      1.0, // scale factor for "laplacian"
                                      minCurve / vigra::NumericTraits<ScalarType>::max())); // scale factor for "localContrast"
            }
        }
        else
        {
#ifdef DEBUG_LOG
            std::cout << "+ Variance of Local Contrast" << endl;
#endif
            localStdDevIf(src.first, src.second, ga,
                          mask.first, mask.second,
                          grad.upperLeft(), grad.accessor(),
                          vigra::Size2D(ContrastWindowSize, ContrastWindowSize));
        }

#ifdef DEBUG_LOG
        {
            vigra::FindMinMax<LongScalarType> minmax;
            vigra::inspectImage(srcImageRange(grad), minmax);
            std::cout << "+ final grad: min = " << minmax.min << ", max = " << minmax.max << endl;
        }
#endif
        ContrastFunctor<LongScalarType, ScalarType, MaskValueType> cf(WContrast);
        combineTwoImagesIfMP(srcImageRange(grad), result, mask, result,
                             const_parameters(bind(cf, _1) + _2));
    }

    // Saturation
    if (WSaturation > 0.0) {
        SaturationFunctor<ImageValueType, MaskValueType> sf(WSaturation);
        combineTwoImagesIfMP(src, result, mask, result,
                             const_parameters(bind(sf, _1) + _2));
    }

    // Entropy
    if (WEntropy > 0.0) {
        typedef typename ImageType::PixelType PixelType;
        typedef typename vigra::NumericTraits<PixelType>::ValueType ScalarType;
        typedef IMAGETYPE<PixelType> Image;
        Image entropy(imageSize);

        if (EntropyLowerCutoff.is_effective<ScalarType>())
        {
            const ScalarType lowerCutoff = EntropyLowerCutoff.instantiate<ScalarType>();
            const ScalarType upperCutoff = EntropyUpperCutoff.instantiate<ScalarType>();
#ifdef DEBUG_ENTROPY
            std::cout <<
                "+ EntropyLowerCutoff.value = " << EntropyLowerCutoff.value() << ", " <<
                "lowerCutoff = " << static_cast<double>(lowerCutoff) << "\n" <<
                "+ EntropyUpperCutoff.value = " << EntropyUpperCutoff.value() << ", " <<
                "upperCutoff = " << static_cast<double>(upperCutoff) << endl;
#endif
            if (lowerCutoff > upperCutoff)
            {
                const double max = static_cast<double>(vigra::NumericTraits<ScalarType>::max());
                std::cerr << command <<
                    ": lower entropy cutoff (" << static_cast<double>(lowerCutoff) << "/" << max <<
                    " = " << 100.0 * lowerCutoff / max <<
                    "%) exceeds upper cutoff (" << static_cast<double>(upperCutoff) << "/" << max <<
                    " = " << 100.0 * upperCutoff / max <<
                    "%)" << endl;
                exit(1);
            }

            Image trunc(imageSize);
            ClampingFunctor<PixelType, PixelType>
                cf((PixelType(lowerCutoff)),  // IMPLEMENTATION NOTE:
                   (PixelType(ScalarType())), //     The extra parenthesis avoid a bug in the VC9 compiler.
                   (PixelType(upperCutoff)),
                   (PixelType(vigra::NumericTraits<ScalarType>::max())));
            transformImageMP(src.first, src.second, src.third,
                             trunc.upperLeft(), trunc.accessor(),
                             cf);
            localEntropyIf(trunc.upperLeft(), trunc.lowerRight(), trunc.accessor(),
                           mask.first, mask.second,
                           entropy.upperLeft(), entropy.accessor(),
                           vigra::Size2D(EntropyWindowSize, EntropyWindowSize));
        }
        else
        {
            localEntropyIf(src.first, src.second, src.third,
                           mask.first, mask.second,
                           entropy.upperLeft(), entropy.accessor(),
                           vigra::Size2D(EntropyWindowSize, EntropyWindowSize));
        }

        EntropyFunctor<PixelType, MaskValueType> ef(WEntropy);
        combineTwoImagesIfMP(srcImageRange(entropy), result, mask, result,
                             const_parameters(bind(ef, _1) + _2));
    }
};


/** Enfuse's main blending loop. Templatized to handle different image types.
 */
template <typename ImagePixelType>
void enfuseMain(const FileNameList& anInputFileNameList,
                const std::list<vigra::ImageImportInfo*>& anImageInfoList,
                vigra::ImageExportInfo& anOutputImageInfo,
                vigra::Rect2D& anInputUnion)
{
    typedef typename EnblendNumericTraits<ImagePixelType>::ImagePixelComponentType ImagePixelComponentType;
    typedef typename EnblendNumericTraits<ImagePixelType>::ImageType ImageType;
    typedef typename EnblendNumericTraits<ImagePixelType>::AlphaPixelType AlphaPixelType;
    typedef typename EnblendNumericTraits<ImagePixelType>::AlphaType AlphaType;
    typedef IMAGETYPE<float> MaskType;
    typedef typename MaskType::value_type MaskPixelType;
    typedef typename EnblendNumericTraits<ImagePixelType>::ImagePyramidPixelType ImagePyramidPixelType;
    typedef typename EnblendNumericTraits<ImagePixelType>::ImagePyramidType ImagePyramidType;
    typedef typename EnblendNumericTraits<ImagePixelType>::MaskPyramidPixelType MaskPyramidPixelType;
    typedef typename EnblendNumericTraits<ImagePixelType>::MaskPyramidType MaskPyramidType;

    enum {ImagePyramidIntegerBits = EnblendNumericTraits<ImagePixelType>::ImagePyramidIntegerBits};
    enum {ImagePyramidFractionBits = EnblendNumericTraits<ImagePixelType>::ImagePyramidFractionBits};
    enum {MaskPyramidIntegerBits = EnblendNumericTraits<ImagePixelType>::MaskPyramidIntegerBits};
    enum {MaskPyramidFractionBits = EnblendNumericTraits<ImagePixelType>::MaskPyramidFractionBits};
    typedef typename EnblendNumericTraits<ImagePixelType>::SKIPSMImagePixelType SKIPSMImagePixelType;
    typedef typename EnblendNumericTraits<ImagePixelType>::SKIPSMAlphaPixelType SKIPSMAlphaPixelType;
    typedef typename EnblendNumericTraits<ImagePixelType>::SKIPSMMaskPixelType SKIPSMMaskPixelType;

    // List of input image / input alpha / mask triples
    typedef std::list< vigra::triple<ImageType*, AlphaType*, MaskType*> > imageListType;
    typedef typename imageListType::iterator imageListIteratorType;
    imageListType imageList;

    // Sum of all masks
    MaskType *normImage = new MaskType(anInputUnion.size());

    // Result image. Alpha will be union of all input alphas.
    std::pair<ImageType*, AlphaType*> outputPair(static_cast<ImageType*>(NULL),
                                                 new AlphaType(anInputUnion.size()));
    std::list<vigra::ImageImportInfo*> imageInfoList(anImageInfoList);
    const unsigned numberOfImages = imageInfoList.size();

    unsigned m = 0;
    FileNameList::const_iterator inputFileNameIterator(anInputFileNameList.begin());
    while (!imageInfoList.empty()) {
        vigra::Rect2D imageBB;
        std::pair<ImageType*, AlphaType*> imagePair =
            assemble<ImageType, AlphaType>(imageInfoList, anInputUnion, imageBB);

        MaskType* mask = new MaskType(anInputUnion.size());

        if (LoadMasks) {
            // IMPLEMENTATION NOTE: For simplicity of the code, here
            // we also load in hard masks.  Computing the set of hard
            // masks from a set of soft masks is done by maximum
            // selection, which is an idempotent function.
            const std::string maskFilename =
                enblend::expandFilenameTemplate(UseHardMask ? HardMaskTemplate : SoftMaskTemplate,
                                                numberOfImages,
                                                *inputFileNameIterator,
                                                OutputFileName,
                                                m);
            if (can_open_file(maskFilename)) {
                vigra::ImageImportInfo maskInfo(maskFilename.c_str());
                if (Verbose >= VERBOSE_MASK_MESSAGES) {
                    std::cerr << command
                              << ": info: loading " << (UseHardMask ? "hard" : "soft")
                              << "mask \"" << maskFilename << "\"" << endl;
                }
                if (maskInfo.width() != anInputUnion.width() || maskInfo.height() != anInputUnion.height()) {
                    std::cerr << command
                              << ": warning: mask in \"" << maskFilename << "\" has size "
                              << "(" << maskInfo.width() << "x" << maskInfo.height() << "),\n"
                              << command
                              << ": warning:     but image union has size " << anInputUnion.size() << ";\n"
                              << command
                              << ": warning:     make sure this is the right mask for the given images"
                              << endl;
                }
                importImage(maskInfo, destImage(*mask));
            } else {
                exit(1);
            }
        } else {
            enfuseMask<ImageType, AlphaType, MaskType>(srcImageRange(*(imagePair.first)),
                                                       srcImage(*(imagePair.second)),
                                                       destImage(*mask));
        }

        if (SaveMasks) {
            const std::string maskFilename =
                enblend::expandFilenameTemplate(SoftMaskTemplate,
                                                numberOfImages,
                                                *inputFileNameIterator,
                                                OutputFileName,
                                                m);
            if (maskFilename == *inputFileNameIterator) {
                std::cerr << command
                          << ": will not overwrite input image \""
                          << *inputFileNameIterator
                          << "\" with soft mask file"
                          << endl;
                exit(1);
            } else if (maskFilename == OutputFileName) {
                std::cerr << command
                          << ": will not overwrite output image \""
                          << OutputFileName
                          << "\" with soft mask file"
                          << endl;
                exit(1);
            } else {
                if (Verbose >= VERBOSE_MASK_MESSAGES) {
                    std::cerr << command
                              << ": info: saving soft mask \"" << maskFilename << "\"" << endl;
                }
                vigra::ImageExportInfo maskInfo(maskFilename.c_str());
                maskInfo.setXResolution(ImageResolution.x);
                maskInfo.setYResolution(ImageResolution.y);
                maskInfo.setCompression(MASK_COMPRESSION);
                exportImage(srcImageRange(*mask), maskInfo);
            }
        }

        // Make output alpha the union of all input alphas.
        copyImageIf(srcImageRange(*(imagePair.second)),
                    maskImage(*(imagePair.second)),
                    destImage(*(outputPair.second)));

        // Add the mask to the norm image.
        combineTwoImagesMP(srcImageRange(*mask),
                           srcImage(*normImage),
                           destImage(*normImage),
                           Arg1() + Arg2());

        imageList.push_back(vigra::make_triple(imagePair.first, imagePair.second, mask));

#ifdef CACHE_IMAGES
        if (Verbose >= VERBOSE_CFI_MESSAGES) {
            vigra_ext::CachedFileImageDirector& v = vigra_ext::CachedFileImageDirector::v();
            std::cerr << command
                      << ": info: image cache statistics after loading image "
                      << m << "\n";
            v.printStats(std::cerr, command + ": info:     image", imagePair.first);
            v.printStats(std::cerr, command + ": info:     alpha", imagePair.second);
            v.printStats(std::cerr, command + ": info:     weight", mask);
            v.printStats(std::cerr, command + ": info:     normImage", normImage);
            v.printStats(std::cerr, command + ": info: ");
            v.resetCacheMisses();
        }
#endif

        ++m;
        ++inputFileNameIterator;
    }

    if (StopAfterMaskGeneration && !UseHardMask) {
        exit(0);
    }

    const int totalImages = imageList.size();

    typename EnblendNumericTraits<ImagePixelType>::MaskPixelType maxMaskPixelType =
        vigra::NumericTraits<typename EnblendNumericTraits<ImagePixelType>::MaskPixelType>::max();

    if (UseHardMask) {
        if (Verbose >= VERBOSE_MASK_MESSAGES) {
            std::cerr << command
                      << ": info: creating hard blend mask" << endl;
        }
        const vigra::Size2D sz = normImage->size();
        imageListIteratorType imageIter;
#ifdef OPENMP
#pragma omp parallel for private (imageIter)
#endif
        for (int y = 0; y < sz.y; ++y) {
            for (int x = 0; x < sz.x; ++x) {
                float max = 0.0f;
                int maxi = 0;
                int i = 0;
                for (imageIter = imageList.begin();
                     imageIter != imageList.end();
                     ++imageIter) {
                    const float w = static_cast<float>((*imageIter->third)(x, y));
                    if (w > max) {
                        max = w;
                        maxi = i;
                    }
                    i++;
                }
                i = 0;
                for (imageIter = imageList.begin();
                     imageIter != imageList.end();
                     ++imageIter) {
                    if (max == 0.0f) {
                        (*imageIter->third)(x, y) =
                            static_cast<MaskPixelType>(maxMaskPixelType) / totalImages;
                    } else if (i == maxi) {
                        (*imageIter->third)(x, y) = maxMaskPixelType;
                    } else {
                        (*imageIter->third)(x, y) = 0.0f;
                    }
                    i++;
                }
            }
        }
        unsigned i = 0;
        if (SaveMasks) {
            for (imageIter = imageList.begin(), inputFileNameIterator = anInputFileNameList.begin();
                 imageIter != imageList.end();
                 ++imageIter, ++inputFileNameIterator) {
                const std::string maskFilename =
                    enblend::expandFilenameTemplate(HardMaskTemplate,
                                                    imageList.size(),
                                                    *inputFileNameIterator,
                                                    OutputFileName,
                                                    i);
                if (maskFilename == *inputFileNameIterator) {
                    std::cerr << command
                              << ": will not overwrite input image \""
                              << *inputFileNameIterator
                              << "\" with hard mask"
                              << endl;
                    exit(1);
                } else if (maskFilename == OutputFileName) {
                    std::cerr << command
                              << ": will not overwrite output image \""
                              << OutputFileName
                              << "\" with hard mask"
                              << endl;
                    exit(1);
                } else {
                    if (Verbose >= VERBOSE_MASK_MESSAGES) {
                        std::cerr << command
                                  << ": info: saving hard mask \"" << maskFilename << "\"" << endl;
                    }
                    vigra::ImageExportInfo maskInfo(maskFilename.c_str());
                    maskInfo.setXResolution(ImageResolution.x);
                    maskInfo.setYResolution(ImageResolution.y);
                    maskInfo.setCompression(MASK_COMPRESSION);
                    exportImage(srcImageRange(*(imageIter->third)), maskInfo);
                }
                i++;
            }
        }
#ifdef CACHE_IMAGES
        if (Verbose >= VERBOSE_CFI_MESSAGES) {
            vigra_ext::CachedFileImageDirector& v = vigra_ext::CachedFileImageDirector::v();
            std::cerr << command
                      << ": info: image cache statistics after creating hard mask\n";
            v.printStats(std::cerr, command + ": info: ");
            v.resetCacheMisses();
        }
#endif
    }

    if (StopAfterMaskGeneration) {
        exit(0);
    }

    vigra::Rect2D junkBB;
    const unsigned int numLevels =
        roiBounds<ImagePixelComponentType>(anInputUnion, anInputUnion, anInputUnion, anInputUnion,
                                           junkBB,
                                           WrapAround != OpenBoundaries);

    std::vector<ImagePyramidType*> *resultLP = NULL;

    m = 0;
    while (!imageList.empty()) {
        vigra::triple<ImageType*, AlphaType*, MaskType*> imageTriple = imageList.front();
        imageList.erase(imageList.begin());

        std::ostringstream oss0;
        oss0 << "imageGP" << m << "_";

        // imageLP is constructed using the image's own alpha channel
        // as the boundary for extrapolation.
        std::vector<ImagePyramidType*> *imageLP =
            laplacianPyramid<ImageType, AlphaType, ImagePyramidType,
                             ImagePyramidIntegerBits, ImagePyramidFractionBits,
                             SKIPSMImagePixelType, SKIPSMAlphaPixelType>(
                                                                         oss0.str().c_str(),
                                                                         numLevels, WrapAround != OpenBoundaries,
                                                                         srcImageRange(*(imageTriple.first)),
                                                                         maskImage(*(imageTriple.second)));

        delete imageTriple.first;
        delete imageTriple.second;

        //std::ostringstream oss1;
        //oss1 << "imageLP" << m << "_";
        //exportPyramid<ImagePyramidType>(imageLP, oss1.str().c_str());

        if (!UseHardMask) {
            // Normalize the mask coefficients.
            // Scale to the range expected by the MaskPyramidPixelType.
            combineTwoImagesMP(srcImageRange(*(imageTriple.third)),
                               srcImage(*normImage),
                               destImage(*(imageTriple.third)),
                               ifThenElse(Arg2() > Param(0.0),
                                          Param(maxMaskPixelType) * Arg1() / Arg2(),
                                          Param(maxMaskPixelType / totalImages)));
        }

        // maskGP is constructed using the union of the input alpha channels
        // as the boundary for extrapolation.
        std::vector<MaskPyramidType*> *maskGP =
            gaussianPyramid<MaskType, AlphaType, MaskPyramidType,
            MaskPyramidIntegerBits, MaskPyramidFractionBits,
            SKIPSMMaskPixelType, SKIPSMAlphaPixelType>
            (numLevels,
             WrapAround != OpenBoundaries,
             srcImageRange(*(imageTriple.third)),
             maskImage(*(outputPair.second)));

        delete imageTriple.third;

        //std::ostringstream oss2;
        //oss2 << "maskGP" << m << "_";
        //exportPyramid<MaskPyramidType>(maskGP, oss2.str().c_str());

        ConvertScalarToPyramidFunctor<typename EnblendNumericTraits<ImagePixelType>::MaskPixelType,
            MaskPyramidPixelType,
            MaskPyramidIntegerBits,
            MaskPyramidFractionBits> maskConvertFunctor;
        MaskPyramidPixelType maxMaskPyramidPixelValue = maskConvertFunctor(maxMaskPixelType);

        for (unsigned int i = 0; i < maskGP->size(); ++i) {
            // Multiply image lp with the mask gp.
            combineTwoImagesMP(srcImageRange(*((*imageLP)[i])),
                               srcImage(*((*maskGP)[i])),
                               destImage(*((*imageLP)[i])),
                               ImageMaskMultiplyFunctor<MaskPyramidPixelType>(maxMaskPyramidPixelValue));

            // Done with maskGP.
            delete (*maskGP)[i];
        }
        delete maskGP;

        //std::ostringstream oss3;
        //oss3 << "multLP" << m << "_";
        //exportPyramid<ImagePyramidType>(imageLP, oss3.str().c_str());

        if (resultLP != NULL) {
            // Add imageLP to resultLP.
            for (unsigned int i = 0; i < imageLP->size(); ++i) {
                combineTwoImagesMP(srcImageRange(*((*imageLP)[i])),
                                   srcImage(*((*resultLP)[i])),
                                   destImage(*((*resultLP)[i])),
                                   Arg1() + Arg2());
                delete (*imageLP)[i];
            }
            delete imageLP;
        } else {
            resultLP = imageLP;
        }

        //std::ostringstream oss4;
        //oss4 << "resultLP" << m << "_";
        //exportPyramid<ImagePyramidType>(resultLP, oss4.str().c_str());

        ++m;
    }

    delete normImage;

    //exportPyramid<ImagePyramidType>(resultLP, "resultLP");

    collapsePyramid<SKIPSMImagePixelType>(WrapAround != OpenBoundaries, resultLP);

    outputPair.first = new ImageType(anInputUnion.size());

    copyFromPyramidImageIf<ImagePyramidType, AlphaType, ImageType,
                           ImagePyramidIntegerBits, ImagePyramidFractionBits>
        (srcImageRange(*((*resultLP)[0])),
         maskImage(*(outputPair.second)),
         destImage(*(outputPair.first)));

    // Delete result pyramid.
    for (unsigned int i = 0; i < resultLP->size(); ++i) {
        delete (*resultLP)[i];
    }
    delete resultLP;

    checkpoint(outputPair, anOutputImageInfo);

    delete outputPair.first;
    delete outputPair.second;
}

} // namespace enblend

#endif /* __ENFUSE_H__ */

// Local Variables:
// mode: c++
// End:
