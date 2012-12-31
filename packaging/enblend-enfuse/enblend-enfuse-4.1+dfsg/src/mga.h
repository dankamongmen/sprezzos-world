/*
 * Copyright (C) 2008-2012 Christoph L. Spiel
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
#ifndef __MGA_H__
#define __MGA_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>
#include <iomanip>

#include <vigra/colorconversions.hxx>

#include "common.h"


namespace enblend {

template <typename InputType, typename ResultType>
class MultiGrayscaleAccessor
{
public:
    typedef ResultType value_type;

    MultiGrayscaleAccessor(const std::string& accessorName) {
        typedef typename vigra::NumericTraits<InputType>::isScalar srcIsScalar;
        initializeTypeSpecific(srcIsScalar());
        initialize(accessorName);
    }

    ResultType operator()(const InputType& x) const {
        typedef typename vigra::NumericTraits<InputType>::isScalar srcIsScalar;
        return f(x, srcIsScalar());
    }

    template <class Iterator>
    ResultType operator()(const Iterator& i) const {
        typedef typename vigra::NumericTraits<InputType>::isScalar srcIsScalar;
        return f(i, srcIsScalar());
    }

    template <class Iterator, class Difference>
    ResultType operator()(const Iterator& i, Difference d) const {
        typedef typename vigra::NumericTraits<InputType>::isScalar srcIsScalar;
        return f(i, d, srcIsScalar());
    }

    static const std::string defaultGrayscaleAccessorName() {return "average";}

private:
    typedef enum AccessorKind {
        AVERAGE, LSTAR, PRIMED_LSTAR, LIGHTNESS, VALUE, ANTI_VALUE, LUMINANCE, MIXER
    } AccKindType;
    typedef std::map<std::string, AccKindType> NameMapType;
    typedef typename NameMapType::const_iterator NameMapConstIterType;

#define CHANNEL_MIXER "channel-mixer"

    void initializeAccessorNameMap() {
        nameMap["average"] = AVERAGE;
        nameMap["l-star"] = LSTAR;
        nameMap["pl-star"] = PRIMED_LSTAR;
        nameMap["lightness"] = LIGHTNESS;
        nameMap["value"] = VALUE;
        nameMap["anti-value"] = ANTI_VALUE;
        nameMap["luminance"] = LUMINANCE;
        nameMap[CHANNEL_MIXER] = MIXER;
    }

    void initialize(const std::string& accessorName) {
        initializeAccessorNameMap();
        if (accessorName.empty())
        {
            kind = nameMap[defaultGrayscaleAccessorName()];
        }
        else
        {
            std::string name(accessorName);
            std::transform(name.begin(), name.end(), name.begin(), tolower);
            NameMapConstIterType const k = nameMap.find(name);
            if (k == nameMap.end())
            {
                char dummy;
                double red, green, blue;
                if (sscanf(name.c_str(),
                           CHANNEL_MIXER "%["
                           NUMERIC_OPTION_DELIMITERS "]%lf%["
                           NUMERIC_OPTION_DELIMITERS "]%lf%["
                           NUMERIC_OPTION_DELIMITERS "]%lf",
                           &dummy, &red, &dummy, &green, &dummy, &blue) == 6)
                {
                    check_weights(red, green, blue);
                    const double sum = red + green + blue;
                    redWeight = red / sum;
                    greenWeight = green / sum;
                    blueWeight = blue / sum;
                    kind = MIXER;
                }
                else
                {
                    std::cerr << command
                         << ": unknown grayscale projector \"" << accessorName << "\""
                         << std::endl;
                    exit(1);
                }

            }
            else
            {
                kind = k->second;
                if (kind == MIXER)
                {
                    std::cerr << command
                         << ": \"" CHANNEL_MIXER "\" is a grayscale projector requiring\n"
                         << command
                         << ":      arguments like e.g. \"channel-mixer:0.30:0.59:0.11\""
                         << std::endl;
                    exit(1);
                }
            }
        }
    }

    void check_weights(double red, double green, double blue) const {
        // TODO: check for isnormal(WEIGHT) before comparison
        if (red < 0.0)
        {
            std::cerr << command
                 << ": nonsensical weight of red channel (" << red << ")"
                 << std::endl;
            exit(1);
        }
        if (green < 0.0)
        {
            std::cerr << command
                 << ": nonsensical weight of green channel (" << green << ")"
                 << std::endl;
            exit(1);
        }
        if (blue < 0.0)
        {
            std::cerr << command
                 << ": nonsensical weight of blue channel (" << blue << ")"
                 << std::endl;
            exit(1);
        }
        if (red + green + blue == 0.0)
        {
            std::cerr << command
                 << ": sum of channel weights is zero"
                 << std::endl;
            exit(1);
        }
    }

    void initializeTypeSpecific(vigra::VigraTrueType) {}

    void initializeTypeSpecific(vigra::VigraFalseType) {
        typedef typename InputType::value_type ValueType;
        rgb_to_lab_fun = vigra::RGB2LabFunctor<double>(vigra::NumericTraits<ValueType>::max());
        rgb_prime_to_lab_fun = vigra::RGBPrime2LabFunctor<double>(vigra::NumericTraits<ValueType>::max());
    }

    ResultType project(const InputType& x) const {
        typedef typename InputType::value_type ValueType;
        switch (kind)
        {
        case AVERAGE:
            return vigra::NumericTraits<ResultType>::fromRealPromote
                ((vigra::NumericTraits<ValueType>::toRealPromote(x.red()) +
                  vigra::NumericTraits<ValueType>::toRealPromote(x.green()) +
                  vigra::NumericTraits<ValueType>::toRealPromote(x.blue())) /
                 3.0);
        case LSTAR:
        {
            typedef typename vigra::RGB2LabFunctor<double>::result_type LABResultType;
            const LABResultType y = rgb_to_lab_fun.operator()(x) / 100.0;
            return vigra::NumericTraits<ResultType>::fromRealPromote(vigra::NumericTraits<ValueType>::max() * y[0]);
        }
        case PRIMED_LSTAR:
        {
            typedef typename vigra::RGBPrime2LabFunctor<double>::result_type LABResultType;
            const LABResultType y = rgb_prime_to_lab_fun.operator()(x) / 100.0;
            return vigra::NumericTraits<ResultType>::fromRealPromote(vigra::NumericTraits<ValueType>::max() * y[0]);
        }
        case LIGHTNESS:
            return vigra::NumericTraits<ResultType>::fromRealPromote
                ((std::min(x.red(), std::min(x.green(), x.blue())) +
                  std::max(x.red(), std::max(x.green(), x.blue()))) /
                 2.0);
        case VALUE:
            return std::max(x.red(), std::max(x.green(), x.blue()));
        case ANTI_VALUE:
            return std::min(x.red(), std::min(x.green(), x.blue()));
        case LUMINANCE:
            return vigra::NumericTraits<ResultType>::fromRealPromote(x.luminance());
        case MIXER:
            return vigra::NumericTraits<ResultType>::fromRealPromote
                (redWeight * vigra::NumericTraits<ValueType>::toRealPromote(x.red()) +
                 greenWeight * vigra::NumericTraits<ValueType>::toRealPromote(x.green()) +
                 blueWeight * vigra::NumericTraits<ValueType>::toRealPromote(x.blue()));
        }

        // never reached
        return ResultType();
    }

    // RGB
    ResultType f(const InputType& x, vigra::VigraFalseType) const {
        return project(x);
    }

    template <class Iterator>
    ResultType f(const Iterator& i, vigra::VigraFalseType) const {
        return project(*i);
    }

    template <class Iterator, class Difference>
    ResultType f(const Iterator& i, Difference d, vigra::VigraFalseType) const {
        return project(i[d]);
    }

    // grayscale
    ResultType f(const InputType& x, vigra::VigraTrueType) const {return x;}

    template <class Iterator>
    ResultType f(const Iterator& i, vigra::VigraTrueType) const {return *i;}

    template <class Iterator, class Difference>
    ResultType f(const Iterator& i, Difference d, vigra::VigraTrueType) const {return i[d];}

    NameMapType nameMap;
    AccKindType kind;
    double redWeight, greenWeight, blueWeight;
    vigra::RGB2LabFunctor<double> rgb_to_lab_fun;
    vigra::RGBPrime2LabFunctor<double> rgb_prime_to_lab_fun;
};

} // namespace enblend

#endif /* __MGA_H__ */

// Local Variables:
// mode: c++
// End:
