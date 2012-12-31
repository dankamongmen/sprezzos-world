/*
 * Copyright (C) 2004-2012 Andrew Mihal, Mikolaj Leszczynski
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
#ifndef MASKCOMMON_H
#define MASKCOMMON_H

#include <vigra/colorconversions.hxx>

namespace enblend {

    template <typename PixelType, typename ResultType>
    class DifferenceFunctor
    {
    public:
        typedef typename EnblendNumericTraits<PixelType>::ImagePixelComponentType PixelComponentType;
        typedef typename EnblendNumericTraits<ResultType>::ImagePixelComponentType ResultPixelComponentType;
        typedef vigra::LinearIntensityTransform<ResultType> RangeMapper;

        DifferenceFunctor() :
            scale_(vigra::linearRangeMapping(vigra::NumericTraits<PixelComponentType>::min(),
                                             vigra::NumericTraits<PixelComponentType>::max(),
                                             ResultType(vigra::NumericTraits<ResultPixelComponentType>::min()),
                                             ResultType(vigra::NumericTraits<ResultPixelComponentType>::max()))) {}

        virtual ~DifferenceFunctor() {}

        ResultType operator()(const PixelType& a, const PixelType& b) const {
            typedef typename vigra::NumericTraits<PixelType>::isScalar src_is_scalar;
            return difference(a, b, src_is_scalar());
        }

    protected:
        virtual ResultType difference(const vigra::RGBValue<PixelComponentType>& a,
                                      const vigra::RGBValue<PixelComponentType>& b,
                                      vigra::VigraFalseType) const = 0;

        ResultType difference(PixelType a, PixelType b, vigra::VigraTrueType) const {
            typedef typename vigra::NumericTraits<PixelType>::isSigned src_is_signed;
            return scalar_difference(a, b, src_is_signed());
        }

        ResultType scalar_difference(PixelType a, PixelType b, vigra::VigraTrueType) const {
            return scale_(std::abs(a - b));
        }

        // This appears necessary because NumericTraits<unsigned int>::Promote
        // is an unsigned int instead of an int.
        ResultType scalar_difference(PixelType a, PixelType b, vigra::VigraFalseType) const {
            return scale_(std::abs(static_cast<int>(a) - static_cast<int>(b)));
        }

        RangeMapper scale_;
    };


    template <typename value_type>
    value_type
    hue(const vigra::RGBValue<value_type>& pixel)
    {
        typedef typename vigra::NumericTraits<value_type>::RealPromote real_value_type;

        const value_type red = pixel.red();
        const value_type green = pixel.green();
        const value_type blue = pixel.blue();

        const value_type max = std::max(red, std::max(green, blue));
        const value_type min = std::min(red, std::min(green, blue));

        if (min == max) {
            return vigra::NumericTraits<value_type>::max();
        } else {
            const real_value_type delta = vigra::NumericTraits<value_type>::toRealPromote(6 * (max - min));
            real_value_type h = 0.0;
            if (red == max) {
                h = (green - blue) / delta;
            } else if (green == max) {
                h = (1.0 / 3.0) + (blue - red) / delta;
            } else {
                h = (2.0 / 3.0) + (red - green) / delta;
            }
            if (h < 0.0) {
                h += 1.0;
            }

            return vigra::NumericTraits<value_type>::fromRealPromote(h * vigra::NumericTraits<value_type>::max());
        }
    }


    template <typename PixelType, typename ResultType>
    class MaxHueLuminanceDifferenceFunctor : public DifferenceFunctor<PixelType, ResultType>
    {
        typedef DifferenceFunctor<PixelType, ResultType> super;

    public:
        typedef typename super::PixelComponentType PixelComponentType;

        MaxHueLuminanceDifferenceFunctor(double aLuminanceWeight, double aChrominanceWeight) {
            const double total = aLuminanceWeight + aChrominanceWeight;
            assert(total != 0.0);
            luma_ = aLuminanceWeight / total;
            chroma_ = aChrominanceWeight / total;
        }

    protected:
        ResultType difference(const vigra::RGBValue<PixelComponentType>& a,
                              const vigra::RGBValue<PixelComponentType>& b,
                              vigra::VigraFalseType) const {
            const PixelComponentType aLum = a.luminance();
            const PixelComponentType bLum = b.luminance();
            const PixelComponentType aHue = hue(a);
            const PixelComponentType bHue = hue(b);
            const PixelComponentType lumDiff = aLum > bLum ? aLum - bLum : bLum - aLum;
            PixelComponentType hueDiff = aHue > bHue ? aHue - bHue : bHue - aHue;

            if (hueDiff > (vigra::NumericTraits<PixelComponentType>::max() / 2)) {
                hueDiff = vigra::NumericTraits<PixelComponentType>::max() - hueDiff;
            }

            return super::scale_(std::max(luma_ * lumDiff, chroma_ * hueDiff));
        }

    private:
        MaxHueLuminanceDifferenceFunctor(); // NOT IMPLEMENTED

        double luma_;
        double chroma_;
    };


    template <typename PixelType, typename ResultType>
    class DeltaEPixelDifferenceFunctor : public DifferenceFunctor<PixelType, ResultType>
    {
        typedef DifferenceFunctor<PixelType, ResultType> super;

    public:
        typedef typename super::PixelComponentType PixelComponentType;

        DeltaEPixelDifferenceFunctor(double aLuminanceWeight, double aChrominanceWeight) :
            rgb_to_lab_(vigra::RGB2LabFunctor<double>(vigra::NumericTraits<PixelComponentType>::max())) {
            const double total = aLuminanceWeight + 2.0 * aChrominanceWeight;
            assert(total != 0.0);
            luma_ = aLuminanceWeight / total;
            chroma_ = aChrominanceWeight / total;
        }

    protected:
        ResultType difference(const vigra::RGBValue<PixelComponentType>& a,
                              const vigra::RGBValue<PixelComponentType>& b,
                              vigra::VigraFalseType) const {
            typedef typename vigra::RGB2LabFunctor<double>::result_type LABResultType;

            const LABResultType lab_a = rgb_to_lab_(a);
            const LABResultType lab_b = rgb_to_lab_(b);
            // See, e.g. http://en.wikipedia.org/wiki/Color_difference
            // or http://www.colorwiki.com/wiki/Delta_E:_The_Color_Difference
            const double delta_e = sqrt(luma_ * square(lab_a[0] - lab_b[0]) +
                                        chroma_ * square(lab_a[1] - lab_b[1]) +
                                        chroma_ * square(lab_a[2] - lab_b[2]));

            // Vigra documentation: 0 <= L* <= 100.0, -86.1813 <= a* <= 98.2352, -107.862 <= b* <= 94.4758
            // => Maximum delta_e = 291.4619.  Real differences are much smaller and fromRealPromote()
            // clips out-of-destination-range values anyhow.  We use 128.0, which yields values comparable
            // to MaxHueLuminanceDifferenceFunctor.
            return super::scale_(vigra::NumericTraits<PixelComponentType>::
                                 fromRealPromote(delta_e * vigra::NumericTraits<PixelComponentType>::max() / 128.0));
        }

    private:
        DeltaEPixelDifferenceFunctor(); // NOT IMPLEMENTED

        double luma_;
        double chroma_;
        vigra::RGB2LabFunctor<double> rgb_to_lab_;
    };


    template <typename PixelType, typename ResultType>
    class PixelSumFunctor
    {
        typedef typename EnblendNumericTraits<PixelType>::ImagePixelComponentType PixelComponentType;
        typedef typename EnblendNumericTraits<ResultType>::ImagePixelComponentType ResultPixelComponentType;
        typedef vigra::LinearIntensityTransform<ResultType> RangeMapper;

    public:
        PixelSumFunctor() :
            rm(linearRangeMapping(vigra::NumericTraits<PixelComponentType>::min(),
                                  vigra::NumericTraits<PixelComponentType>::max(),
                                  ResultType(vigra::NumericTraits<ResultPixelComponentType>::min()),
                                  ResultType(vigra::NumericTraits<ResultPixelComponentType>::max()))) {}

        ResultType operator()(const PixelType& a, const PixelType& b) const {
            typedef typename vigra::NumericTraits<PixelType>::isScalar src_is_scalar;
            return sum(a, b, src_is_scalar());
        }

    protected:
        ResultType sum(const PixelType& a, const PixelType& b, vigra::VigraFalseType) const {
            PixelComponentType aLum = a.luminance();
            PixelComponentType bLum = b.luminance();
            PixelComponentType lumDiff = (aLum + bLum) / 2;
            return rm(lumDiff);
        }

        ResultType sum(const PixelType& a, const PixelType& b, vigra::VigraTrueType) const {
            typedef typename vigra::NumericTraits<PixelType>::isSigned src_is_signed;
            return scalar_sum(a, b, src_is_signed());
        }

        ResultType scalar_sum(const PixelType& a, const PixelType& b, vigra::VigraTrueType) const {
            return rm(a + b);
        }

        // This appears necessary because NumericTraits<unsigned int>::Promote
        // is an unsigned int instead of an int.
        ResultType scalar_sum(const PixelType& a, const PixelType& b, vigra::VigraFalseType) const {
            return rm(std::abs(static_cast<int>(a) + static_cast<int>(b)));
        }

        RangeMapper rm;
    };


    template <typename PixelType, typename ResultType>
    class MapFunctor
    {
        typedef typename EnblendNumericTraits<PixelType>::ImagePixelComponentType PixelComponentType;
        typedef typename EnblendNumericTraits<ResultType>::ImagePixelComponentType ResultPixelComponentType;
        typedef vigra::LinearIntensityTransform<ResultType> RangeMapper;

    public:
        MapFunctor() :
            rm(vigra::linearRangeMapping(vigra::NumericTraits<PixelComponentType>::min(),
                                         vigra::NumericTraits<PixelComponentType>::max(),
                                         ResultType(vigra::NumericTraits<ResultPixelComponentType>::min()),
                                         ResultType(vigra::NumericTraits<ResultPixelComponentType>::max()))) {}

        ResultType operator()(const PixelType& a) const {
            typedef typename vigra::NumericTraits<PixelType>::isScalar src_is_scalar;
            return map(a, src_is_scalar());
        }

    protected:
        ResultType map(const PixelType& a, vigra::VigraFalseType) const {
            PixelComponentType aLum = a.luminance();
            return rm(aLum);
        }

        ResultType map(const PixelType& a, vigra::VigraTrueType) const {
            typedef typename vigra::NumericTraits<PixelType>::isSigned src_is_signed;
            return scalar_map(a, src_is_signed());
        }

        ResultType scalar_map(const PixelType& a, vigra::VigraTrueType) const {
            return rm(a);
        }

        // This appears necessary because NumericTraits<unsigned int>::Promote
        // is an unsigned int instead of an int.
        ResultType scalar_map(const PixelType& a, vigra::VigraFalseType) const {
            return rm(std::abs(static_cast<int>(a)));
        }

        RangeMapper rm;
    };

} // namespace enblend

#endif  /* MASKCOMMON_H */

// Local Variables:
// mode: c++
// End:
