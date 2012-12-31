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

#ifndef __FIXMATH_H__
#define __FIXMATH_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cmath>

#include <boost/assign/list_of.hpp>

#ifdef _WIN32
#include <boost/math/special_functions.hpp>
using namespace boost::math;
#endif

#include <vigra/basicimage.hxx>
#include <vigra/mathutil.hxx>
#include <vigra/numerictraits.hxx>
#include <vigra/utilities.hxx>

#include "vigra_ext/cachedfileimage.hxx"

#include "minimizer.h"


#define MAXIMUM_LIGHTNESS 100.0 // J
#define MAXIMUM_CHROMA 120.0    // C
#define MAXIMUM_HUE 360.0       // h

#define XYZ_SCALE 100.0


namespace enblend {

static inline double
radian_of_degree(double x)
{
    return x * M_PI / 180.0;
}


static inline double
degree_of_radian(double x)
{
    return x * 180.0 / M_PI;
}


static inline double
wrap_cyclically(double x, double modulus)
{
    assert(modulus > 0.0);

    while (x < 0.0) {
        x += modulus;
    }

    return fmod(x, modulus);
}


static inline double
limit(double x, double lower_limit, double upper_limit)
{
    assert(lower_limit <= upper_limit);
    if (x != x) {
        throw std::range_error("limit: not a number");
    }

    return std::min(std::max(lower_limit, x), upper_limit);
}


static inline void
rgb_to_jch(const double* rgb, cmsJCh* jch)
{
    double xyz[3];
    cmsDoTransform(InputToXYZTransform, rgb, xyz, 1U);

    const cmsCIEXYZ scaled_xyz = {XYZ_SCALE * xyz[0], XYZ_SCALE * xyz[1], XYZ_SCALE * xyz[2]};
    cmsCIECAM02Forward(CIECAMTransform, &scaled_xyz, jch);
    // J in range [0, 100], C in range [0, 120], h in range [0, 360]
}


static inline void
jch_to_rgb(const cmsJCh* jch, double* rgb)
{
    cmsCIEXYZ scaled_xyz;
    cmsCIECAM02Reverse(CIECAMTransform, jch, &scaled_xyz);
    // xyz values in range [0, 100]

    // scale xyz values to range [0, 1]
    const double xyz[] = {
        scaled_xyz.X / XYZ_SCALE,
        scaled_xyz.Y / XYZ_SCALE,
        scaled_xyz.Z / XYZ_SCALE
    };

    cmsDoTransform(XYZToInputTransform, xyz, rgb, 1U);
    // rgb values in range [0, 1]
}


static inline void
jch_to_lab(const cmsJCh* jch, cmsCIELab* lab)
{
    double rgb[3];

    jch_to_rgb(jch, rgb);
    cmsDoTransform(InputToLabTransform, rgb, lab, 1);
}


struct extra_minimizer_parameter
{
    extra_minimizer_parameter(const cmsJCh& out_of_box_jch) : jch(out_of_box_jch)
    {jch_to_lab(&jch, &bad_lab);}

    cmsJCh jch;
    cmsCIELab bad_lab;
};


inline double
delta_e_of_lab_and_rgb(const cmsCIELab* lab, const double* rgb)
{
    cmsCIELab lab_of_rgb;

    cmsDoTransform(InputToLabTransform, rgb, &lab_of_rgb, 1);

    return cmsCMCdeltaE(lab, &lab_of_rgb, 2.0, 1.0);
}


inline double
out_of_box_penalty(const double* rgb)
{
    const double infinite_badness = 100.0;
    double result = 0.0;

    for (const double* x = rgb; x != rgb + 3U; ++x) {
        if (*x > 1.0) {
            result += *x * infinite_badness;
        } else if (*x < 0.0) {
            result += (1.0 - *x) * infinite_badness;
        }
    }

    return result;
}


inline double
delta_e_cost(const cmsJCh* jch, const extra_minimizer_parameter* parameter)
{
    double rgb[3];
    jch_to_rgb(jch, rgb);

    return delta_e_of_lab_and_rgb(&parameter->bad_lab, rgb) + out_of_box_penalty(rgb);
}


double
delta_e_min_cost(double luminance, void* data)
{
    const extra_minimizer_parameter* parameter = static_cast<const extra_minimizer_parameter*>(data);
    const cmsJCh jch = {luminance, parameter->jch.C, parameter->jch.h};

    return delta_e_cost(&jch, parameter);
}


double
delta_e_multimin_cost(const gsl_vector* x, void* data)
{
    const extra_minimizer_parameter* parameter = static_cast<const extra_minimizer_parameter*>(data);
    const cmsJCh jch = {gsl_vector_get(x, 0), gsl_vector_get(x, 1), parameter->jch.h};

    return delta_e_cost(&jch, parameter);
}


/** A functor for converting scalar pixel values to the number representation used
 *  for pyramids.  These are either fixed-point integers or floating-point numbers.
 */
template <typename SrcPixelType, typename PyramidPixelType, int PyramidIntegerBits, int PyramidFractionBits>
class ConvertScalarToPyramidFunctor {
public:
    ConvertScalarToPyramidFunctor() {}

    inline PyramidPixelType operator()(const SrcPixelType& v) const {
        return doConvert(v, SrcIsIntegral(), PyramidIsIntegral());
    }

protected:
    typedef typename vigra::NumericTraits<SrcPixelType>::isIntegral SrcIsIntegral;
    typedef typename vigra::NumericTraits<PyramidPixelType>::isIntegral PyramidIsIntegral;

    // Convert an integral pixel type to an integral pyramid value type.
    inline PyramidPixelType doConvert(const SrcPixelType& v, vigra::VigraTrueType, vigra::VigraTrueType) const {
        return convertIntegerToFixedPoint(v);
    }

    // Convert an integral pixel type to a real pyramid value type.
    inline PyramidPixelType doConvert(const SrcPixelType& v, vigra::VigraTrueType, vigra::VigraFalseType) const {
        return vigra::NumericTraits<SrcPixelType>::toRealPromote(v);
    }

    // Convert a real pixel type to an integral pyramid value type.
    inline PyramidPixelType doConvert(const SrcPixelType& v, vigra::VigraFalseType, vigra::VigraTrueType) const {
        return convertDoubleToFixedPoint(v);
    }

    // Convert a real pixel type to a real pyramid value type.
    inline PyramidPixelType doConvert(const SrcPixelType& v, vigra::VigraFalseType, vigra::VigraFalseType) const {
        // Convert real data using a log transform.  These achieves
        // two purposes:
        //  1. During blending, even completely non-negative images
        //     can result in negative pixels.  A log transform
        //     followed by the exp inverse guarantees all-positive
        //     output.
        //  2. For HDR data, the log transform put the samples closer
        //     to a perceptual space making the blending a little more
        //     pleasing.  Ideally, all blending should be done in a
        //     strictly perceptually-linear space, such as Luv or Lab.
        //
        // See ConvertPyramidToScalarFunctor::doConvert for the
        // inverse transform.
        //
        // Check for non-positive values -- they should not be in the
        // input, but if they are we need to handle them or log will
        // return a NaN.
        //
        // v >= 0.0 ? 1.0 + log(v + 1.0) : 1.0 / (1.0 - v)
        return v >= 0.0 ? 1.0 + log1p(v) : 1.0 / (1.0 - v);
    }

    inline PyramidPixelType convertDoubleToFixedPoint(const double& v) const {
        // Shift v to get the appropriate number of fraction bits into the integer part,
        // then fromRealPromote this value into the fixed-point type.
        return vigra::NumericTraits<PyramidPixelType>::fromRealPromote(v * static_cast<double>(1U << PyramidFractionBits));
    }

    inline PyramidPixelType convertIntegerToFixedPoint(const SrcPixelType& v) const {
        // Shift v left to move the decimal point and set the fraction bits to zero.
        return static_cast<PyramidPixelType>(v) << PyramidFractionBits;
    }

};


/** A functor for converting numbers stored in the pyramid number representation back
 *  into normal pixel values.
 */
template <typename DestPixelType, typename PyramidPixelType, int PyramidIntegerBits, int PyramidFractionBits>
class ConvertPyramidToScalarFunctor {

public:
    ConvertPyramidToScalarFunctor() {}

    inline DestPixelType operator()(const PyramidPixelType& v) const {
        return doConvert(v, DestIsIntegral(), PyramidIsIntegral());
    }

protected:
    typedef typename vigra::NumericTraits<DestPixelType>::isIntegral DestIsIntegral;
    typedef typename vigra::NumericTraits<PyramidPixelType>::isIntegral PyramidIsIntegral;

    // test time with floating-point dithering: 100.01 sec
    // test time with integer dithering: 94.89 sec
    // Convert an integral pyramid pixel to an integral image pixel.
    inline DestPixelType doConvert(const PyramidPixelType& v, vigra::VigraTrueType, vigra::VigraTrueType) const {
        // Integer Dithering
        PyramidPixelType half = 1U << (PyramidFractionBits - 1);
        PyramidPixelType quarter = 1U << (PyramidFractionBits - 2);
        PyramidPixelType threeQuarter = 3U << (PyramidFractionBits - 2);

        PyramidPixelType vFraction = v & ((1U << PyramidFractionBits) - 1);

        if ((vFraction >= quarter) && (vFraction < threeQuarter)) {
            PyramidPixelType random = (PyramidPixelType(::Twister()) & (half - 1)) + quarter;
            if (random <= vFraction) {
                return DestPixelType(vigra::NumericTraits<DestPixelType>::fromPromote((v >> PyramidFractionBits) + 1));
            } else {
                return DestPixelType(vigra::NumericTraits<DestPixelType>::fromPromote(v >> PyramidFractionBits));
            }
        }
        else if (vFraction >= quarter) {
            return DestPixelType(vigra::NumericTraits<DestPixelType>::fromPromote((v >> PyramidFractionBits) + 1));
        }
        else {
            return DestPixelType(vigra::NumericTraits<DestPixelType>::fromPromote(v >> PyramidFractionBits));
        }

    }

    // Convert a real pyramid pixel to an integral image pixel.
    inline DestPixelType doConvert(const PyramidPixelType& v, vigra::VigraTrueType, vigra::VigraFalseType) const {
        const double d = dither(v);
        return vigra::NumericTraits<DestPixelType>::fromRealPromote(d);
    }

    // Convert an integral pyramid pixel to a real image pixel.
    inline DestPixelType doConvert(const PyramidPixelType& v, vigra::VigraFalseType, vigra::VigraTrueType) const {
        return convertFixedPointToDouble(v);
    }

    // Convert a real pyramid pixel to a real image pixel.
    inline DestPixelType doConvert(const PyramidPixelType& v, vigra::VigraFalseType, vigra::VigraFalseType) const {
        // Undo logarithmic/rational mapping that was done in building
        // the pyramid.  See ConvertScalarToPyramidFunctor::doConvert
        // for the forward transformation.
        return v >= 1.0 ? expm1(v - 1.0) : 1.0 - 1.0 / v;
    }

    // Dithering is used to fool the eye into seeing gradients that are finer
    // than the precision of the pixel type.
    // This prevents the occurence of cleanly-bordered regions in the output where
    // the pixel values suddenly change from N to N+1.
    // Such regions are especially objectionable in the green channel of 8-bit images.
    inline double dither(const double& v) const {
        const double vFraction = v - floor(v);
        // Only dither values within a certain range of the rounding cutoff point.
        if (vFraction > 0.25 && vFraction <= 0.75) {
            // Generate a random number between 0 and 0.5.
            const double random = 0.5 * static_cast<double>(::Twister()) / static_cast<double>(UINT_MAX);
            if ((vFraction - 0.25) >= random) {
                return ceil(v);
            } else {
                return floor(v);
            }
        }
        return v;
    }

    inline double convertFixedPointToDouble(const PyramidPixelType& v) const {
        return vigra::NumericTraits<PyramidPixelType>::toRealPromote(v) /
            static_cast<double>(1U << PyramidFractionBits);
    }
};


/** Wrapper for vector pixel types. */
template <typename SrcVectorType, typename PyramidVectorType, int PyramidIntegerBits, int PyramidFractionBits>
class ConvertVectorToPyramidFunctor {
    typedef typename SrcVectorType::value_type SrcComponentType;
    typedef typename PyramidVectorType::value_type PyramidComponentType;
    typedef ConvertScalarToPyramidFunctor<SrcComponentType, PyramidComponentType,
                                          PyramidIntegerBits, PyramidFractionBits> ConvertFunctorType;

public:
    ConvertVectorToPyramidFunctor() : cf() {}

    inline PyramidVectorType operator()(const SrcVectorType& v) const {
        return PyramidVectorType(cf(v.red()), cf(v.green()), cf(v.blue()));
    }

protected:
    ConvertFunctorType cf;
};


/** Wrapper for vector pixel types. */
template <typename DestVectorType, typename PyramidVectorType, int PyramidIntegerBits, int PyramidFractionBits>
class ConvertPyramidToVectorFunctor {
    typedef typename DestVectorType::value_type DestComponentType;
    typedef typename PyramidVectorType::value_type PyramidComponentType;
    typedef ConvertPyramidToScalarFunctor<DestComponentType, PyramidComponentType,
                                          PyramidIntegerBits, PyramidFractionBits> ConvertFunctorType;

public:
    ConvertPyramidToVectorFunctor() : cf() {}

    inline DestVectorType operator()(const PyramidVectorType& v) const {
        return DestVectorType(cf(v.red()), cf(v.green()), cf(v.blue()));
    }

protected:
    ConvertFunctorType cf;
};


/** Fixed point converter that uses ICC profile transformation */
template <typename SrcVectorType, typename PyramidVectorType, int PyramidIntegerBits, int PyramidFractionBits>
class ConvertVectorToJCHPyramidFunctor {
    typedef typename SrcVectorType::value_type SrcComponentType;
    typedef typename PyramidVectorType::value_type PyramidComponentType;
    typedef ConvertScalarToPyramidFunctor<double, PyramidComponentType,
                                          PyramidIntegerBits, PyramidFractionBits> ConvertFunctorType;

public:
    ConvertVectorToJCHPyramidFunctor() :
        cf(),
        scale(1.0 / vigra::NumericTraits<SrcComponentType>::toRealPromote(vigra::NumericTraits<SrcComponentType>::max())),
        shift(double(1U << (PyramidIntegerBits - 1 - 7)))
    {}

    inline PyramidVectorType operator()(const SrcVectorType& v) const {
        // rgb values must be in range [0, 1]
        const double rgb[] = {
            scale * vigra::NumericTraits<SrcComponentType>::toRealPromote(v.red()),
            scale * vigra::NumericTraits<SrcComponentType>::toRealPromote(v.green()),
            scale * vigra::NumericTraits<SrcComponentType>::toRealPromote(v.blue())
        };
        cmsJCh jch;

        rgb_to_jch(rgb, &jch);

        // convert cylindrical 'JCh' to cartesian, but reuse (yikes!) the cylindrical structure
        const double theta = radian_of_degree(jch.h);
        jch.h = jch.C * cos(theta);
        jch.C = jch.C * sin(theta);

        // scale to maximize usage of fixed-point type
        jch.J *= shift;
        jch.C *= shift;
        jch.h *= shift;

        return PyramidVectorType(cf(jch.J), cf(jch.C), cf(jch.h));
    }

protected:
    ConvertFunctorType cf;
    const double scale;
    const double shift;
};


template <typename forward_iterator>
static inline void
limit_sequence(forward_iterator first, forward_iterator last, double lower_limit, double upper_limit)
{
    while (first != last) {
        *first = limit(*first, lower_limit, upper_limit);
        ++first;
    }
}


static inline double
uniform_random(unsigned* seed)
{
    return static_cast<double>(enblend::rand_r(seed)) / static_cast<double>(RAND_MAX);
}


static inline void
bracket_minimum(const gsl_function& cost, double& x_initial, double x_lower, double x_upper)
{
    const double y_minimum_bound =
        std::min(cost.function(x_lower, cost.params), cost.function(x_upper, cost.params));
    double y_initial = cost.function(x_initial, cost.params);

    if (y_initial < y_minimum_bound) {
        return;
    }

    const unsigned maximum_tries = 100U;
    unsigned i = 0U;
    const double lower = std::max(0.001, 1.001 * x_lower);
    const double upper = 0.999 * x_upper;
    unsigned seed = 1000003U; // fixed seed for reproducibility

    while (y_initial >= y_minimum_bound && i < maximum_tries) {
        x_initial = uniform_random(&seed) * (upper - lower) + lower;
        y_initial = cost.function(x_initial, cost.params);
        ++i;
#ifdef DEBUG_SHADOW_HIGHLIGHT_STATISTICS
#ifdef OPENMP
#pragma omp critical
#endif
        std::cout <<
            "+ highlight recovery -- bracket minimum: x = " << x_initial << ", y = " << y_initial <<
            std::endl;
#endif
    }
}


/** Fixed point converter that uses ICC profile transformation */
template <typename DestVectorType, typename PyramidVectorType, int PyramidIntegerBits, int PyramidFractionBits>
class ConvertJCHPyramidToVectorFunctor {
    typedef typename DestVectorType::value_type DestComponentType;
    typedef typename PyramidVectorType::value_type PyramidComponentType;
    typedef ConvertPyramidToScalarFunctor<double, PyramidComponentType,
                                          PyramidIntegerBits, PyramidFractionBits> ConvertFunctorType;

public:
    ConvertJCHPyramidToVectorFunctor() :
        cf(),
        scale(vigra::NumericTraits<DestComponentType>::toRealPromote(vigra::NumericTraits<DestComponentType>::max())),
        shift(double(1U << (PyramidIntegerBits - 1 - 7))),

        // Parameters for highlight optimizer only
        highlight_lightness_guess_factor(limit(enblend::parameter::as_double("highlight-recovery-lightness-guess-factor", 0.975),
                                               0.25, 4.0)),
        highlight_lightness_guess_offset(enblend::parameter::as_double("highlight-recovery-lightness-guess-offset", 0.0)),

        maximum_highlight_iterations(limit(enblend::parameter::as_unsigned("highlight-recovery-maximum-iterations", 100U),
                                           10U, 1000U)),

        // Parameters for shadow optimizer only
        shadow_lightness_lightness_guess_factor(enblend::parameter::as_double("shadow-recovery-lightness-lightness-guess-factor", 1.24)),
        shadow_lightness_chroma_guess_factor(enblend::parameter::as_double("shadow-recovery-lightness-chroma-guess-factor", -0.136)),
        shadow_lightness_guess_offset(enblend::parameter::as_double("shadow-recovery-lightness-guess-offset", 0.0)),
        shadow_chroma_lightness_guess_factor(enblend::parameter::as_double("shadow-recovery-chroma-lightness-guess-factor", -0.604)),
        shadow_chroma_chroma_guess_factor(enblend::parameter::as_double("shadow-recovery-chroma-chroma-guess-factor", 1.33)),
        shadow_chroma_guess_offset(enblend::parameter::as_double("shadow-recovery-chroma-guess-offset", 0.0)),

        simplex_lightness_step_length(limit(enblend::parameter::as_double("shadow-recovery-lightness-step-length", 0.625),
                                            1.0 / 65536.0, 100.0)),
        simplex_chroma_step_length(limit(enblend::parameter::as_double("shadow-recovery-chroma-step-length", 1.25),
                                         1.0 / 65536.0, 120.0)),
        iterations_per_leg(limit(enblend::parameter::as_unsigned("shadow-recovery-iterations-per-leg", 40U),
                                 4U, 400U)),
        maximum_shadow_leg(limit(enblend::parameter::as_unsigned("shadow-recovery-maximum-legs", 5U),
                                 1U, 50U)),

        // Parameters for both optimizers
        // Desired error limits: LoFi: 0.5/2^8, HiFi: 0.5/2^16, Super-HiFi: 0.5/2^24
        optimizer_error(limit(enblend::parameter::as_double("ciecam-optimizer-error", 0.5 / 65536.0),
                              0.5 / 16777216.0, 1.0)),
        // Delta-E goals: LoFi: 1.0, HiFi: 0.5, Super-HiFi: 0.0
        optimizer_goal(limit(enblend::parameter::as_double("ciecam-optimizer-deltae-goal", 0.5),
                             0.0, 10.0))
    {}

    inline double highlight_lightness_guess(const cmsJCh& jch) const {
        return std::min( // heuristic function with fitted parameter
                        highlight_lightness_guess_factor * jch.J + highlight_lightness_guess_offset,
                        0.995 * MAXIMUM_LIGHTNESS); // backstop such that our guess is less than the maximum
    }

    inline double shadow_lightness_guess(const cmsJCh& jch) const {
        return std::max(shadow_lightness_lightness_guess_factor * jch.J +
                        shadow_lightness_chroma_guess_factor * jch.C +
                        shadow_lightness_guess_offset,
                        0.0);
    }

    inline double shadow_chroma_guess(const cmsJCh& jch) const {
        return std::max(shadow_chroma_lightness_guess_factor * jch.J +
                        shadow_chroma_chroma_guess_factor * jch.C +
                        shadow_chroma_guess_offset,
                        0.0);
    }

    inline DestVectorType operator()(const PyramidVectorType& v) const {
        cmsJCh jch = {cf(v.red()), cf(v.green()), cf(v.blue())};
        if (jch.J <= 0.0) {
#ifdef DEBUG_SHADOW_HIGHLIGHT_STATISTICS
#ifdef OPENMP
#pragma omp critical
#endif
            std::cout << "+ unrecoverable dark shadow: J = " << jch.J << "\n" << std::endl;
#endif
            // Lasciate ogne speranza, voi ch'intrate.
            return DestVectorType(0, 0, 0);
        }

        // scale back to range J: [0, 100], C: [0, 120], h: [0, 120]
        jch.J /= shift;
        jch.C /= shift;
        jch.h /= shift;

        // convert cartesian to cylindrical
        const double chroma = hypot(jch.C, jch.h);
        jch.h = wrap_cyclically(degree_of_radian(atan2(jch.C, jch.h)), MAXIMUM_HUE);
        jch.C = chroma;

        double rgb[3];
        jch_to_rgb(&jch, rgb);

        if (rgb[0] < 0.0 || rgb[1] < 0.0 || rgb[2] < 0.0) {
            extra_minimizer_parameter extra(jch);
            gsl_multimin_function cost = {delta_e_multimin_cost, 2U, &extra};
            const MinimizerMultiDimensionSimplex::array_type initial =
                boost::assign::list_of(shadow_lightness_guess(jch))(shadow_chroma_guess(jch));
            MinimizerMultiDimensionSimplex::array_type step =
                boost::assign::list_of(simplex_lightness_step_length)(simplex_chroma_step_length);
            MinimizerMultiDimensionSimplex2Randomized optimizer(cost, initial, step);

#ifdef DEBUG_SHADOW_HIGHLIGHT_STATISTICS
            double initial_rgb[3]; // for debug print only
            memcpy(rgb, initial_rgb, 3U * sizeof(double));
#endif

            optimizer.set_absolute_error(optimizer_error)->set_goal(optimizer_goal);
            for (unsigned leg = 1U; leg <= maximum_shadow_leg; ++leg) {
                optimizer.set_maximum_number_of_iterations(leg * iterations_per_leg);
                optimizer.run();
                if (optimizer.has_reached_goal()) {
                    break;
                }

                step[0] = optimizer.characteristic_size();
                step[1] = optimizer.characteristic_size();
                optimizer.set_step_sizes(step);
            }

            MinimizerMultiDimensionSimplex::array_type minimum_parameter(2U);
            optimizer.x_minimum(minimum_parameter.begin());

            jch.J = minimum_parameter[0];
            jch.C = minimum_parameter[1];
            jch_to_rgb(&jch, &rgb[0]);
#ifdef DEBUG_SHADOW_HIGHLIGHT_STATISTICS
#ifdef OPENMP
#pragma omp critical
#endif
            std::cout <<
                "+ shadow recovery: ini J = " << initial[0] << ", C = " << initial[1] <<  ", ini RGB = (" <<
                initial_rgb[0] << ", " << initial_rgb[1] << ", " << initial_rgb[2] << ")\n" <<
                "+ shadow recovery: opt J = " << minimum_parameter[0] << ", C = " << minimum_parameter[1] <<
                " after " << optimizer.number_of_iterations() <<
                " iterations, simplex size = " << optimizer.characteristic_size() << "\n" <<
                "+ shadow recovery: opt delta-E = " << optimizer.f_minimum() <<
                ", opt RGB = (" << rgb[0] << ", " << rgb[1] << ", " << rgb[2] << ")\n" <<
                std::endl;
#endif
        } else if (rgb[0] > 1.0 || rgb[1] > 1.0 || rgb[2] > 1.0) {
            extra_minimizer_parameter extra(jch);
            gsl_function cost = {delta_e_min_cost, &extra};
            const double j_max = std::max(MAXIMUM_LIGHTNESS, jch.J);
            double j_initial = highlight_lightness_guess(jch);

            bracket_minimum(cost, j_initial, 0.0, j_max);
            GoldenSectionMinimizer1D optimizer(cost, j_initial, 0.0, j_max);

#ifdef DEBUG_SHADOW_HIGHLIGHT_STATISTICS
            const double initial_j = jch.J; // for debug print only
            double initial_rgb[3]; // for debug print only
            memcpy(rgb, initial_rgb, 3U * sizeof(double));
#endif

            optimizer.set_absolute_error(optimizer_error)->
                set_goal(optimizer_goal)->
                set_maximum_number_of_iterations(maximum_highlight_iterations);
            optimizer.run();

            jch.J = optimizer.x_minimum();
            jch_to_rgb(&jch, &rgb[0]);
#ifdef DEBUG_SHADOW_HIGHLIGHT_STATISTICS
#ifdef OPENMP
#pragma omp critical
#endif
            std::cout <<
                "+ highlight recovery: ini J = " << initial_j << ", {C = " << jch.C << "}, ini RGB = (" <<
                initial_rgb[0] << ", " << initial_rgb[1] << ", " << initial_rgb[2] << ")\n" <<
                "+ highlight recovery: opt J = " << optimizer.x_minimum() <<
                " after " << optimizer.number_of_iterations() << " iterations\n" <<
                "+ highlight recovery: opt delta-E = " << optimizer.f_minimum() <<
                ", opt RGB = (" << rgb[0] << ", " << rgb[1] << ", " << rgb[2] << ")\n" <<
                std::endl;
#endif
        }

        limit_sequence(rgb, rgb + 3U, 0.0, 1.0);

        return DestVectorType(vigra::NumericTraits<DestComponentType>::fromRealPromote(scale * rgb[0]),
                              vigra::NumericTraits<DestComponentType>::fromRealPromote(scale * rgb[1]),
                              vigra::NumericTraits<DestComponentType>::fromRealPromote(scale * rgb[2]));
    }

protected:
    ConvertFunctorType cf;
    const double scale;
    const double shift;

    const double highlight_lightness_guess_factor;
    const double highlight_lightness_guess_offset;
    const unsigned maximum_highlight_iterations;

    const double shadow_lightness_lightness_guess_factor;
    const double shadow_lightness_chroma_guess_factor;
    const double shadow_lightness_guess_offset;
    const double shadow_chroma_lightness_guess_factor;
    const double shadow_chroma_chroma_guess_factor;
    const double shadow_chroma_guess_offset;

    const double simplex_lightness_step_length;
    const double simplex_chroma_step_length;
    const unsigned iterations_per_leg;
    const unsigned maximum_shadow_leg;

    const double optimizer_error;
    const double optimizer_goal;
};


/** Copy a scalar image into a scalar pyramid image. */
template <typename SrcImageType, typename PyramidImageType, int PyramidIntegerBits, int PyramidFractionBits>
void
copyToPyramidImage(typename SrcImageType::const_traverser src_upperleft,
        typename SrcImageType::const_traverser src_lowerright,
        typename SrcImageType::ConstAccessor sa,
        typename PyramidImageType::traverser dest_upperleft,
        typename PyramidImageType::Accessor da,
        vigra::VigraTrueType)
{
    typedef typename SrcImageType::value_type SrcPixelType;
    typedef typename PyramidImageType::value_type PyramidPixelType;

    transformImageMP(src_upperleft, src_lowerright, sa,
                     dest_upperleft, da,
                     ConvertScalarToPyramidFunctor<SrcPixelType, PyramidPixelType, PyramidIntegerBits, PyramidFractionBits>());
}


/** Copy a vector image into a vector pyramid image.
 *  Uses an optional color space conversion.
 */
template <typename SrcImageType, typename PyramidImageType, int PyramidIntegerBits, int PyramidFractionBits>
void
copyToPyramidImage(typename SrcImageType::const_traverser src_upperleft,
                   typename SrcImageType::const_traverser src_lowerright,
                   typename SrcImageType::ConstAccessor sa,
                   typename PyramidImageType::traverser dest_upperleft,
                   typename PyramidImageType::Accessor da,
                   vigra::VigraFalseType)
{
    typedef typename SrcImageType::value_type SrcVectorType;
    typedef typename PyramidImageType::value_type PyramidVectorType;

    if (UseCIECAM) {
        if (Verbose >= VERBOSE_COLOR_CONVERSION_MESSAGES) {
            cerr << command << ": info: CIECAM02 color conversion";
            if (!enblend::profileName(InputProfile).empty()) {
                cerr << " from/to \"" << enblend::profileName(InputProfile) << "\" profile";
            }
            cerr << "\n";
        }
        transformImageMP(src_upperleft, src_lowerright, sa,
                         dest_upperleft, da,
                         ConvertVectorToJCHPyramidFunctor<SrcVectorType, PyramidVectorType, PyramidIntegerBits, PyramidFractionBits>());
    } else {
        transformImageMP(src_upperleft, src_lowerright, sa,
                         dest_upperleft, da,
                         ConvertVectorToPyramidFunctor<SrcVectorType, PyramidVectorType, PyramidIntegerBits, PyramidFractionBits>());
    }
}


// Compile-time switch based on scalar or vector image type.
template <typename SrcImageType, typename PyramidImageType, int PyramidIntegerBits, int PyramidFractionBits>
inline void
copyToPyramidImage(typename SrcImageType::const_traverser src_upperleft,
                   typename SrcImageType::const_traverser src_lowerright,
                   typename SrcImageType::ConstAccessor sa,
                   typename PyramidImageType::traverser dest_upperleft,
                   typename PyramidImageType::Accessor da)
{
    typedef typename vigra::NumericTraits<typename SrcImageType::value_type>::isScalar src_is_scalar;

    copyToPyramidImage<SrcImageType, PyramidImageType, PyramidIntegerBits, PyramidFractionBits>
        (src_upperleft,
         src_lowerright,
         sa,
         dest_upperleft,
         da,
         src_is_scalar());
}


// Version using argument object factories.
template <typename SrcImageType, typename PyramidImageType, int PyramidIntegerBits, int PyramidFractionBits>
inline void
copyToPyramidImage(vigra::triple<typename SrcImageType::const_traverser, typename SrcImageType::const_traverser, typename SrcImageType::ConstAccessor> src,
                   vigra::pair<typename PyramidImageType::traverser, typename PyramidImageType::Accessor> dest)
{
    copyToPyramidImage<SrcImageType, PyramidImageType, PyramidIntegerBits, PyramidFractionBits>
        (src.first,
         src.second,
         src.third,
         dest.first,
         dest.second);
}


/** Copy a scalar pyramid image into a scalar image. */
template <typename PyramidImageType, typename MaskImageType, typename DestImageType, int PyramidIntegerBits, int PyramidFractionBits>
inline void
copyFromPyramidImageIf(typename PyramidImageType::const_traverser src_upperleft,
                       typename PyramidImageType::const_traverser src_lowerright,
                       typename PyramidImageType::ConstAccessor sa,
                       typename MaskImageType::const_traverser mask_upperleft,
                       typename MaskImageType::ConstAccessor ma,
                       typename DestImageType::traverser dest_upperleft,
                       typename DestImageType::Accessor da,
                       vigra::VigraTrueType)
{
    typedef typename DestImageType::value_type DestPixelType;
    typedef typename PyramidImageType::value_type PyramidPixelType;

    transformImageIfMP(src_upperleft, src_lowerright, sa,
                       mask_upperleft, ma,
                       dest_upperleft, da,
                       ConvertPyramidToScalarFunctor<DestPixelType, PyramidPixelType, PyramidIntegerBits, PyramidFractionBits>());
}


/** Copy a vector pyramid image into a vector image.
 *  Uses an optional color space conversion.
 */
template <typename PyramidImageType, typename MaskImageType, typename DestImageType, int PyramidIntegerBits, int PyramidFractionBits>
inline void
copyFromPyramidImageIf(typename PyramidImageType::const_traverser src_upperleft,
                       typename PyramidImageType::const_traverser src_lowerright,
                       typename PyramidImageType::ConstAccessor sa,
                       typename MaskImageType::const_traverser mask_upperleft,
                       typename MaskImageType::ConstAccessor ma,
                       typename DestImageType::traverser dest_upperleft,
                       typename DestImageType::Accessor da,
                       vigra::VigraFalseType)
{
    typedef typename DestImageType::value_type DestVectorType;
    typedef typename PyramidImageType::value_type PyramidVectorType;

    if (UseCIECAM) {
        if (Verbose >= VERBOSE_COLOR_CONVERSION_MESSAGES) {
            cerr << command << ": info: CIECAM02 color conversion" << endl;
        }
        transformImageIfMP(src_upperleft, src_lowerright, sa,
                           mask_upperleft, ma,
                           dest_upperleft, da,
                           ConvertJCHPyramidToVectorFunctor<DestVectorType, PyramidVectorType, PyramidIntegerBits, PyramidFractionBits>());
    } else {
        // OpenMP changes the result here!  The maximum absolute
        // difference is 1 of 255 for 8-bit images.  -- cls
        transformImageIfMP(src_upperleft, src_lowerright, sa,
                           mask_upperleft, ma,
                           dest_upperleft, da,
                           ConvertPyramidToVectorFunctor<DestVectorType, PyramidVectorType, PyramidIntegerBits, PyramidFractionBits>());
    }
}


// Compile-time switch based on scalar or vector image type.
template <typename PyramidImageType, typename MaskImageType, typename DestImageType, int PyramidIntegerBits, int PyramidFractionBits>
inline void
copyFromPyramidImageIf(typename PyramidImageType::const_traverser src_upperleft,
                       typename PyramidImageType::const_traverser src_lowerright,
                       typename PyramidImageType::ConstAccessor sa,
                       typename MaskImageType::const_traverser mask_upperleft,
                       typename MaskImageType::ConstAccessor ma,
                       typename DestImageType::traverser dest_upperleft,
                       typename DestImageType::Accessor da)
{
    typedef typename vigra::NumericTraits<typename PyramidImageType::value_type>::isScalar src_is_scalar;

    copyFromPyramidImageIf<PyramidImageType, MaskImageType, DestImageType, PyramidIntegerBits, PyramidFractionBits>
        (src_upperleft,
         src_lowerright,
         sa,
         mask_upperleft,
         ma,
         dest_upperleft,
         da,
         src_is_scalar());
}


// Version using argument object factories.
template <typename PyramidImageType, typename MaskImageType, typename DestImageType, int PyramidIntegerBits, int PyramidFractionBits>
inline void
copyFromPyramidImageIf(vigra::triple<typename PyramidImageType::const_traverser, typename PyramidImageType::const_traverser, typename PyramidImageType::ConstAccessor> src,
                       vigra::pair<typename MaskImageType::const_traverser, typename MaskImageType::ConstAccessor> mask,
                       vigra::pair<typename DestImageType::traverser, typename DestImageType::Accessor> dest)
{
    copyFromPyramidImageIf<PyramidImageType, MaskImageType, DestImageType, PyramidIntegerBits, PyramidFractionBits>
        (src.first,
         src.second,
         src.third,
         mask.first,
         mask.second,
         dest.first,
         dest.second);
}

} // namespace enblend

#endif /* __FIXMATH_H__ */

// Local Variables:
// mode: c++
// End:
