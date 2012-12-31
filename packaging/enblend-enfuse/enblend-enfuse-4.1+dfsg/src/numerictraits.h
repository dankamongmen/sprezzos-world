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
#ifndef __NUMERICTRAITS_H__
#define __NUMERICTRAITS_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <vigra/basicimage.hxx>
#include <vigra/rgbvalue.hxx>
#include <vigra/numerictraits.hxx>
#include <vigra/utilities.hxx>

#include "vigra_ext/cachedfileimage.hxx"

#include "common.h"


namespace enblend {

struct Error_EnblendNumericTraits_not_specialized_for_this_case {};

template<class A>
struct EnblendNumericTraits {
    // Types related to input images
    typedef Error_EnblendNumericTraits_not_specialized_for_this_case ImagePixelComponentType;
    typedef Error_EnblendNumericTraits_not_specialized_for_this_case ImagePixelType;
    typedef Error_EnblendNumericTraits_not_specialized_for_this_case ImageType;
    typedef Error_EnblendNumericTraits_not_specialized_for_this_case ImageIsScalar;
    typedef Error_EnblendNumericTraits_not_specialized_for_this_case AlphaPixelType;
    typedef Error_EnblendNumericTraits_not_specialized_for_this_case AlphaType;

    // Types related to the mask
    typedef Error_EnblendNumericTraits_not_specialized_for_this_case MaskPixelType;
    typedef Error_EnblendNumericTraits_not_specialized_for_this_case MaskType;

    // Types related to image pyramids
    typedef Error_EnblendNumericTraits_not_specialized_for_this_case ImagePyramidPixelComponentType;
    typedef Error_EnblendNumericTraits_not_specialized_for_this_case ImagePyramidPixelType;
    typedef Error_EnblendNumericTraits_not_specialized_for_this_case ImagePyramidType;
    enum { ImagePyramidIntegerBits = 0 };
    enum { ImagePyramidFractionBits = 0 };

    // Pixel type used by SKIPSM algorithm for intermediate image pixel calculations
    typedef Error_EnblendNumericTraits_not_specialized_for_this_case SKIPSMImagePixelType;

    // Pixel type used by SKIPSM algorithm for intermediate alpha pixel calculations
    typedef Error_EnblendNumericTraits_not_specialized_for_this_case SKIPSMAlphaPixelType;

    // Types related to mask pyramid
    typedef Error_EnblendNumericTraits_not_specialized_for_this_case MaskPyramidPixelType;
    typedef Error_EnblendNumericTraits_not_specialized_for_this_case MaskPyramidType;
    enum { MaskPyramidIntegerBits = 0 };
    enum { MaskPyramidFractionBits = 0 };

    typedef Error_EnblendNumericTraits_not_specialized_for_this_case SKIPSMMaskPixelType;
};

#define DEFINE_ENBLENDNUMERICTRAITS(IMAGE, IMAGECOMPONENT, ALPHA, MASK, PYRAMIDCOMPONENT, PYRAMIDINTEGER, PYRAMIDFRACTION, SKIPSMIMAGE, SKIPSMALPHA, MASKPYRAMID, MASKPYRAMIDINTEGER, MASKPYRAMIDFRACTION, SKIPSMMASK) \
template<> \
struct EnblendNumericTraits<IMAGECOMPONENT> { \
    typedef IMAGECOMPONENT ImagePixelComponentType; \
    typedef IMAGECOMPONENT ImagePixelType; \
    typedef IMAGE<IMAGECOMPONENT> ImageType; \
    typedef vigra::VigraTrueType ImageIsScalar; \
    typedef ALPHA AlphaPixelType; \
    typedef IMAGE<ALPHA> AlphaType; \
    typedef MASK MaskPixelType; \
    typedef IMAGE<MASK> MaskType; \
    typedef PYRAMIDCOMPONENT ImagePyramidPixelComponentType; \
    typedef PYRAMIDCOMPONENT ImagePyramidPixelType; \
    typedef IMAGE<PYRAMIDCOMPONENT> ImagePyramidType; \
    enum {ImagePyramidIntegerBits = PYRAMIDINTEGER}; \
    enum {ImagePyramidFractionBits = PYRAMIDFRACTION}; \
    typedef SKIPSMIMAGE SKIPSMImagePixelComponentType; \
    typedef SKIPSMIMAGE SKIPSMImagePixelType; \
    typedef SKIPSMALPHA SKIPSMAlphaPixelType; \
    typedef MASKPYRAMID MaskPyramidPixelType; \
    typedef IMAGE<MASKPYRAMID> MaskPyramidType; \
    enum {MaskPyramidIntegerBits = MASKPYRAMIDINTEGER}; \
    enum {MaskPyramidFractionBits = MASKPYRAMIDFRACTION}; \
    typedef SKIPSMMASK SKIPSMMaskPixelType; \
};\
template<> \
struct EnblendNumericTraits<vigra::RGBValue<IMAGECOMPONENT,0,1,2> > { \
    typedef IMAGECOMPONENT ImagePixelComponentType; \
    typedef vigra::RGBValue<IMAGECOMPONENT,0,1,2> ImagePixelType; \
    typedef IMAGE<vigra::RGBValue<IMAGECOMPONENT,0,1,2> > ImageType; \
    typedef vigra::VigraFalseType ImageIsScalar;                     \
    typedef ALPHA AlphaPixelType; \
    typedef IMAGE<ALPHA> AlphaType; \
    typedef MASK MaskPixelType; \
    typedef IMAGE<MASK> MaskType; \
    typedef PYRAMIDCOMPONENT ImagePyramidPixelComponentType; \
    typedef vigra::RGBValue<PYRAMIDCOMPONENT,0,1,2> ImagePyramidPixelType; \
    typedef IMAGE<vigra::RGBValue<PYRAMIDCOMPONENT,0,1,2> > ImagePyramidType; \
    enum {ImagePyramidIntegerBits = PYRAMIDINTEGER}; \
    enum {ImagePyramidFractionBits = PYRAMIDFRACTION}; \
    typedef SKIPSMIMAGE SKIPSMImagePixelComponentType; \
    typedef vigra::RGBValue<SKIPSMIMAGE,0,1,2> SKIPSMImagePixelType; \
    typedef SKIPSMALPHA SKIPSMAlphaPixelType; \
    typedef MASKPYRAMID MaskPyramidPixelType; \
    typedef IMAGE<MASKPYRAMID> MaskPyramidType; \
    enum {MaskPyramidIntegerBits = MASKPYRAMIDINTEGER}; \
    enum {MaskPyramidFractionBits = MASKPYRAMIDFRACTION}; \
    typedef SKIPSMMASK SKIPSMMaskPixelType; \
}

// Traits for converting between image pixel types and pyramid pixel types
// Pyramids require one more bit of precision than the regular image type.
// SKIPSM math requires 6 bits on top of the pyramid type.
//                                      IMCOMP          ALPHA          MASK           IMPYR           I    F   SKIPI          SKIPA          MASKPYR        I    F   SKIPM
DEFINE_ENBLENDNUMERICTRAITS(IMAGETYPE,  vigra::Int8,    vigra::UInt8,  vigra::UInt8,  vigra::Int16,   9,   7,  vigra::Int32,  vigra::Int16,  vigra::Int16,  9,   7,  vigra::Int32);
DEFINE_ENBLENDNUMERICTRAITS(IMAGETYPE,  vigra::UInt8,   vigra::UInt8,  vigra::UInt8,  vigra::Int16,   9,   7,  vigra::Int32,  vigra::Int16,  vigra::Int16,  9,   7,  vigra::Int32);
DEFINE_ENBLENDNUMERICTRAITS(IMAGETYPE,  vigra::Int16,   vigra::UInt8,  vigra::UInt8,  vigra::Int32,  17,   7,  vigra::Int32,  vigra::Int16,  vigra::Int32,  9,  15,  vigra::Int32);
DEFINE_ENBLENDNUMERICTRAITS(IMAGETYPE,  vigra::UInt16,  vigra::UInt8,  vigra::UInt8,  vigra::Int32,  17,   7,  vigra::Int32,  vigra::Int16,  vigra::Int32,  9,  15,  vigra::Int32);
//DEFINE_ENBLENDNUMERICTRAITS(IMAGETYPE,  vigra::Int32,   vigra::UInt8,  vigra::UInt8,  vigra::Int64,  33,  25,  vigra::Int64,  vigra::Int16,  vigra::Int32,  9,  17,  vigra::Int32);
//DEFINE_ENBLENDNUMERICTRAITS(IMAGETYPE,  vigra::UInt32,  vigra::UInt8,  vigra::UInt8,  vigra::Int64,  33,  25,  vigra::Int64,  vigra::Int16,  vigra::Int32,  9,  17,  vigra::Int32);
DEFINE_ENBLENDNUMERICTRAITS(IMAGETYPE,  vigra::Int32,   vigra::UInt8,  vigra::UInt8,  double,         8,   0,  double,        vigra::Int16,  vigra::Int32,  9,  15,  vigra::Int32);
DEFINE_ENBLENDNUMERICTRAITS(IMAGETYPE,  vigra::UInt32,  vigra::UInt8,  vigra::UInt8,  double,         8,   0,  double,        vigra::Int16,  vigra::Int32,  9,  15,  vigra::Int32);
//DEFINE_ENBLENDNUMERICTRAITS(IMAGETYPE,  vigra::Int64,   vigra::UInt8,  vigra::UInt8,  double,         8,   0,  double,        vigra::Int16,  vigra::Int32,  9,  17,  vigra::Int32);
//DEFINE_ENBLENDNUMERICTRAITS(IMAGETYPE,  vigra::UInt64,  vigra::UInt8,  vigra::UInt8,  double,         8,   0,  double,        vigra::Int16,  vigra::Int32,  9,  17,  vigra::Int32);
DEFINE_ENBLENDNUMERICTRAITS(IMAGETYPE,  float,          vigra::UInt8,  vigra::UInt8,  double,         8,   0,  double,        vigra::Int16,  vigra::Int32,  9,  15,  vigra::Int32);
DEFINE_ENBLENDNUMERICTRAITS(IMAGETYPE,  double,         vigra::UInt8,  vigra::UInt8,  double,         8,   0,  double,        vigra::Int16,  vigra::Int32,  9,  15,  vigra::Int32);



// Traits for correctly handling alpha/mask values in floating point files
// This differs from NumericTraits for floating point values
#define ALPHA_TRAITS(T1,S) \
template<> \
struct AlphaTraits<T1> \
{ \
    static T1 max() \
{ \
    return S; \
} \
    static T1 zero() \
{ \
    return 0; \
} \
}; \
template<> \
struct AlphaTraits<vigra::RGBValue<T1> > \
{ \
    static T1 max() \
{ \
    return S; \
} \
    static T1 zero() \
{ \
    return 0; \
} \
}

template <class T1>
struct AlphaTraits;

ALPHA_TRAITS(unsigned char, UCHAR_MAX);
ALPHA_TRAITS(signed char, SCHAR_MAX);
ALPHA_TRAITS(unsigned short, USHRT_MAX);
ALPHA_TRAITS(signed short, SHRT_MAX);
ALPHA_TRAITS(unsigned int, UINT_MAX);
ALPHA_TRAITS(signed int, INT_MAX);
ALPHA_TRAITS(float, 1.0);
ALPHA_TRAITS(double, 1.0);

#undef ALPHA_TRAITS

} // namespace enblend

#endif /* __NUMERICTRAITS_H__ */

// Local Variables:
// mode: c++
// End:
