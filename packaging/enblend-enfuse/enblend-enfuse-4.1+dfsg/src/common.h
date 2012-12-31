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
#ifndef __COMMON_H__
#define __COMMON_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <algorithm>
#include <fstream>
#include <iomanip>
#include <limits>
#include <map>
#include <stdexcept>
#include <string>
#include <vector>

#include <boost/algorithm/string/trim.hpp>
#include <boost/assign/list_inserter.hpp>
#include <boost/assign/list_of.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/scoped_ptr.hpp>

#include <vigra/numerictraits.hxx>

#include "error_message.h"
#include "filenameparse.h"

#define NUMERIC_OPTION_DELIMITERS ";:/"
#define PATH_OPTION_DELIMITERS ",;:"
#define ASSIGNMENT_CHARACTERS "="

#define MASK_COMPRESSION "DEFLATE"

// IMPLEMENTATION NOTE: For 30 or more pyramid levels, the full width
// will just barely fit in a 32-bit integer.  When this range is added
// to a bounding box it will certainly overflow the vigra::Diff2D.
#define MAX_PYRAMID_LEVELS 29   //< src::maximum-pyramid-levels 29

// Colors used in the optimizer visualization
#define VISUALIZE_RGB_COLOR_BLUE1    vigra::RGBValue<vigra::UInt8>(  0,   0, 255)
#define VISUALIZE_RGB_COLOR_BLUE2    vigra::RGBValue<vigra::UInt8>(  0,   0, 238)
#define VISUALIZE_RGB_COLOR_BLUE3    vigra::RGBValue<vigra::UInt8>(  0,   0, 205)
#define VISUALIZE_RGB_COLOR_BLUE4    vigra::RGBValue<vigra::UInt8>(  0,   0, 139)

#define VISUALIZE_RGB_COLOR_CYAN1    vigra::RGBValue<vigra::UInt8>(  0, 255, 255)
#define VISUALIZE_RGB_COLOR_CYAN2    vigra::RGBValue<vigra::UInt8>(  0, 238, 238)
#define VISUALIZE_RGB_COLOR_CYAN3    vigra::RGBValue<vigra::UInt8>(  0, 205, 205)
#define VISUALIZE_RGB_COLOR_CYAN4    vigra::RGBValue<vigra::UInt8>(  0, 139, 139)

#define VISUALIZE_RGB_COLOR_GRAY0    vigra::RGBValue<vigra::UInt8>(  0,   0,   0)
#define VISUALIZE_RGB_COLOR_GRAY63   vigra::RGBValue<vigra::UInt8>( 63,  63,  63)
#define VISUALIZE_RGB_COLOR_GRAY127  vigra::RGBValue<vigra::UInt8>(127, 127, 127)
#define VISUALIZE_RGB_COLOR_GRAY139  vigra::RGBValue<vigra::UInt8>(139, 139, 139)
#define VISUALIZE_RGB_COLOR_GRAY191  vigra::RGBValue<vigra::UInt8>(191, 191, 191)
#define VISUALIZE_RGB_COLOR_GRAY205  vigra::RGBValue<vigra::UInt8>(205, 205, 205)
#define VISUALIZE_RGB_COLOR_GRAY238  vigra::RGBValue<vigra::UInt8>(238, 238, 238)
#define VISUALIZE_RGB_COLOR_GRAY255  vigra::RGBValue<vigra::UInt8>(255, 255, 255)

#define VISUALIZE_RGB_COLOR_GREEN1   vigra::RGBValue<vigra::UInt8>(  0, 255,   0)
#define VISUALIZE_RGB_COLOR_GREEN2   vigra::RGBValue<vigra::UInt8>(  0, 238,   0)
#define VISUALIZE_RGB_COLOR_GREEN3   vigra::RGBValue<vigra::UInt8>(  0, 205,   0)
#define VISUALIZE_RGB_COLOR_GREEN4   vigra::RGBValue<vigra::UInt8>(  0, 139,   0)

#define VISUALIZE_RGB_COLOR_MAGENTA1 vigra::RGBValue<vigra::UInt8>(255,   0, 255)
#define VISUALIZE_RGB_COLOR_MAGENTA2 vigra::RGBValue<vigra::UInt8>(238,   0, 238)
#define VISUALIZE_RGB_COLOR_MAGENTA3 vigra::RGBValue<vigra::UInt8>(205,   0, 205)
#define VISUALIZE_RGB_COLOR_MAGENTA4 vigra::RGBValue<vigra::UInt8>(139,   0, 139)

#define VISUALIZE_RGB_COLOR_ORANGE1  vigra::RGBValue<vigra::UInt8>(255, 165,   0)
#define VISUALIZE_RGB_COLOR_ORANGE2  vigra::RGBValue<vigra::UInt8>(238, 154,   0)
#define VISUALIZE_RGB_COLOR_ORANGE3  vigra::RGBValue<vigra::UInt8>(205, 133,   0)
#define VISUALIZE_RGB_COLOR_ORANGE4  vigra::RGBValue<vigra::UInt8>(139,  90,   0)

#define VISUALIZE_RGB_COLOR_RED1     vigra::RGBValue<vigra::UInt8>(255,   0,   0)
#define VISUALIZE_RGB_COLOR_RED2     vigra::RGBValue<vigra::UInt8>(238,   0,   0)
#define VISUALIZE_RGB_COLOR_RED3     vigra::RGBValue<vigra::UInt8>(205,   0,   0)
#define VISUALIZE_RGB_COLOR_RED4     vigra::RGBValue<vigra::UInt8>(139,   0,   0)

#define VISUALIZE_RGB_COLOR_YELLOW1  vigra::RGBValue<vigra::UInt8>(255, 255,   0)
#define VISUALIZE_RGB_COLOR_YELLOW2  vigra::RGBValue<vigra::UInt8>(238, 238,   0)
#define VISUALIZE_RGB_COLOR_YELLOW3  vigra::RGBValue<vigra::UInt8>(205, 205,   0)
#define VISUALIZE_RGB_COLOR_YELLOW4  vigra::RGBValue<vigra::UInt8>(139, 139,   0)


// Different marker types offered by visualizePoint()
typedef enum
{
    NO_MARKER,
    DOT_MARKER,
    PLUS_MARKER,
    CROSS_MARKER,
    HOLLOW_SQUARE_MARKER,
    HOLLOW_DIAMOND_MARKER,
} marker_t;


//< src::visualize-movable-point light orange
#define VISUALIZE_MOVABLE_POINT VISUALIZE_RGB_COLOR_ORANGE1
//< src::mark-movable-point diamond
#define MARK_MOVABLE_POINT HOLLOW_DIAMOND_MARKER
//< src::visualize-frozen-point bright white
#define VISUALIZE_FROZEN_POINT VISUALIZE_RGB_COLOR_GRAY255
//< src::mark-frozen-point cross
#define MARK_FROZEN_POINT CROSS_MARKER
//< src::visualize-initial-path-color dark yellow
#define VISUALIZE_INITIAL_PATH       VISUALIZE_RGB_COLOR_YELLOW4
//< src::visualize-short-path-value-color bright yellow
#define VISUALIZE_SHORT_PATH_VALUE   VISUALIZE_RGB_COLOR_YELLOW1
//< src::visualize-first-vertex-value-color medium green
#define VISUALIZE_FIRST_VERTEX_VALUE VISUALIZE_RGB_COLOR_GREEN3
//< src::visualize-next-vertex-value-color light green
#define VISUALIZE_NEXT_VERTEX_VALUE  VISUALIZE_RGB_COLOR_GREEN2
//< src::visualize-no-overlap-value-color dark red
#define VISUALIZE_NO_OVERLAP_VALUE   VISUALIZE_RGB_COLOR_RED4
//< src::visualize-state-space-color dark blue
#define VISUALIZE_STATE_SPACE        VISUALIZE_RGB_COLOR_BLUE3
//< src::visualize-state-space-inside-color bright cyan
#define VISUALIZE_STATE_SPACE_INSIDE VISUALIZE_RGB_COLOR_CYAN1
//< src::visualize-state-space-unconverged-color bright magenta
#define VISUALIZE_STATE_SPACE_UNCONVERGED VISUALIZE_RGB_COLOR_MAGENTA1


// For CIECAM blending, Enblend and Enfuse transform the input images
// from their respective RGB color spaces to XYZ space (and then on to
// JCh).  The following two #defines control the color transformation
// from and to XYZ space.
#define RENDERING_INTENT_FOR_BLENDING INTENT_PERCEPTUAL
#define TRANSFORMATION_FLAGS_FOR_BLENDING cmsFLAGS_NOCACHE


// Macros for optimizing at source level

#ifdef __GNUC__
#define PREFETCH(m_addr, m_rw_hint, m_temporal_locality_hint) \
    __builtin_prefetch((m_addr), (m_rw_hint), (m_temporal_locality_hint))
#define EXPECT_RESULT(m_condition, m_expected_result) \
    __builtin_expect((m_condition), static_cast<int>(m_expected_result))
#else
#define PREFETCH(m_addr, m_rw_hint, m_temporal_locality_hint)
#define EXPECT_RESULT(m_condition, m_expected_result) (m_condition)
#endif


typedef enum {
    PREPARE_FOR_READ,
    PREPARE_FOR_WRITE
} rw_hint;


// If data is only touched once, or if the dataset is smaller than the
// cache, prefer the non-temporal version; otherwise use one of the
// temporal versions.
typedef enum {
    // Fetch data into the first way of the L1/L2 cache, minimizing cache pollution.
    NO_TEMPORAL_LOCALITY,

    // Fetch data into the least-recently-used way of the ...
    LOW_TEMPORAL_LOCALITY,      // ... L3 cache?
    MEDIUM_TEMPORAL_LOCALITY,   // ... L2/L3 cache?
    HIGH_TEMPORAL_LOCALITY      // ... L1/L2/L3 cache just as a normal load would do.
} temporal_locality_hint;


// Select our preferred type of image depending on what ./configure
// tells us.
#ifdef CACHE_IMAGES
#define IMAGETYPE vigra_ext::CachedFileImage
#else
#define IMAGETYPE vigra::BasicImage
#endif


#ifdef WIN32
#define sleep(m_duration) Sleep(m_duration)
#endif


#define PENALIZE_DEPRECATED_OPTION(m_old_name, m_new_name) \
    do { \
        cerr << command << \
            ": info: option \"" m_old_name "\" is deprecated; use \"" m_new_name "\" instead" << \
            endl; \
        sleep(1); \
    } while (false)


#define lengthof(m_array) (sizeof(m_array) / sizeof(m_array[0]))


// Replacement for `assert(0)' and `assert(false)'.
class never_reached : public std::runtime_error
{
public:
    never_reached(const std::string& a_message) : std::runtime_error(a_message) {}
    virtual ~never_reached() throw() {}
};


namespace enblend {

/** The different image overlap classifications. */
enum Overlap {NoOverlap, PartialOverlap, CompleteOverlap};


/** Symbolic expressions for the three different metrics that
 *  vigra::distanceTransform understands. */
typedef enum
{
    ChessboardDistance,         // 0
    ManhattanDistance,          // 1, L1 norm
    EuclideanDistance           // 2, L2 norm
} nearest_neighbor_metric_t;


/** Define our own reentrant, uniform pseudo-random number generator. */
inline int
rand_r(unsigned int* seed)
{
    *seed = *seed * 1103515245U + 12345U;
    return static_cast<int>(*seed % (static_cast<unsigned int>(RAND_MAX) + 1U));
}


/** Answer the square of the argument x. */
template <typename t>
inline t
square(t x)
{
    return x * x;
}


/** Answer the previous iterator of i. */
template <class iterator>
inline iterator
prev(iterator i)
{
    return --i;
}


/** Answer the following iterator of i. */
template <class iterator>
inline iterator
next(iterator i)
{
    return ++i;
}


/** Test whether s starts with p. */
inline bool
starts_with(const std::string& s, const std::string& p)
{
    return s.substr(0, p.length()) == p;
}


/** String tokenizer similar to strtok_r().
 *  In contrast to strtok_r this function returns an empty string for
 *  each pair of successive delimiters.  Function strtok_r skips them.
 */
char*
strtoken_r(char* str, const char* delim, char** save_ptr)
{
    char *s = str ? str : *save_ptr;

    if (s)
    {
        char *token = s;

        while (*s != 0 && !strchr(delim, (int) *s))
        {
            s++;
        }
        *save_ptr = *s == 0 ? NULL : s + 1;
        *s = 0;

        return token;
    }
    else
    {
        return NULL;
    }
}


/** Answer the string representation of the boolean b. */
std::string
stringOfBool(bool b)
{
    return b ? "true" : "false";
}


/** Answer whether we can open aFilename. */
bool
can_open_file(const std::string& aFilename)
{
    errno = 0;
    std::ifstream file(aFilename.c_str());
    if (!file)
    {
        std::cerr << command <<
            ": failed to open \"" << aFilename << "\": " <<
            errorMessage(errno) << "\n";
        return false;
    }
    else
    {
        errno = 0;
        file.close();
        if (file.fail())
        {
            std::cerr << command <<
                ": info: problems when closing \"" << aFilename << "\": " <<
                errorMessage(errno) << "\n";
        }
        return true;
    }
}


/** Answer the VIGRA file type as determined by the extension of
 *  aFileName. */
std::string
getFileType(const std::string& aFileName)
{
    std::string ext = aFileName.substr(aFileName.rfind(".") + 1);

    boost::algorithm::to_upper(ext);

    if (ext == "JPG") return "JPEG";
    else if (ext == "TIF") return "TIFF";
    else if (ext == "VIF") return "VIFF";
    else if (ext == "PBM" || ext == "PGM" || ext == "PPM") return "PNM";
    else return ext;
}


/** Convert aWraparoundMode given as string to the internal
 *  representation as enum. */
boundary_t
wraparoundOfString(const char* aWraparoundMode)
{
    std::string mode = std::string(aWraparoundMode);

    boost::algorithm::to_upper(mode);

    if (mode == "NONE" || mode == "OPEN") return OpenBoundaries;
    else if (mode == "HORIZONTAL") return HorizontalStrip;
    else if (mode == "VERTICAL") return VerticalStrip;
    else if (mode == "BOTH" ||
             mode == "HORIZONTAL+VERTICAL" ||
             mode == "VERTICAL+HORIZONTAL") return DoubleStrip;
    else return UnknownWrapAround;
}


/** Convert aBoundaryMode to its string representation. */
std::string
stringOfWraparound(boundary_t aBoundaryMode)
{
    switch (aBoundaryMode)
    {
    case OpenBoundaries:
        return "none";
    case HorizontalStrip:
        return "horizontal";
    case VerticalStrip:
        return "vertical";
    case DoubleStrip:
        return "both";
    default:
        throw never_reached("switch control expression \"aBoundaryMode\" out of range");
    }
}


/** Convert a_string into a number.
 *
 * Perform two validating tests in the numerical result.  These are,
 * for example, tests for lower and upper boundaries. */
template <class NumericType, class Validator1, class Validator2>
NumericType
numberOfString(const char* a_string,                // string we want to convert into a number
               Validator1 is_valid1,                // 1st validator function
               const std::string& invalid_message1, // error message for failed 1st validation
               NumericType replacement_value1,      // replacement return value on 1st failure
               Validator2 is_valid2,                // 2nd validator function
               const std::string& invalid_message2, // error message for failed 2nd validation
               NumericType replacement_value2)      // replacement return value on 2nd failure
{
    typedef std::numeric_limits<NumericType> traits;

    char* tail;
    long int long_int_value;
    double double_value;
    NumericType value;

    errno = 0;
    if (traits::is_exact)
    {
        long_int_value = strtol(a_string, &tail, 10);
    }
    else
    {
        double_value = strtod(a_string, &tail);
    }

    if (errno != 0)
    {
        std::cerr << command << ": "
                  << "illegal numeric format of \""
                  << a_string
                  << "\": "
                  << errorMessage(errno)
                  << std::endl;
        exit(1);
    }

    if (*tail != 0)
    {
        if (strcmp(a_string, tail) == 0)
        {
            std::cerr << command << ": "
                      << "number is garbage; maybe the option before \"" << a_string
                      << "\" needs an argument"
                      << std::endl;
        }
        else
        {
            std::cerr << command << ": "
                      << "trailing garbage \"" << tail << "\" in \"" << a_string << "\""
                      << std::endl;
        }
        exit(1);
    }

    if (traits::is_exact)
    {
        if (traits::is_signed)
        {
            if (long_int_value < traits::min() || long_int_value > traits::max())
            {
                std::cerr << command << ": "
                          << "signed number " << long_int_value
                          << " out of range [" << traits::min() << " .. " << traits::max() << "]"
                          << std::endl;
                exit(1);
            }
            else
            {
                value = static_cast<NumericType>(long_int_value);
            }
        }
        else
        {
            if (long_int_value < 0L || long_int_value > traits::max())
            {
                std::cerr << command << ": "
                          << "unsigned number " << long_int_value
                          << " out of range [0 .. " << traits::max() << "]"
                          << std::endl;
                exit(1);
            }
            else
            {
                value = static_cast<NumericType>(long_int_value);
            }
        }
    }
    else
    {
        value = static_cast<NumericType>(double_value);
    }

    if (is_valid1(value))
    {
        if (is_valid2(value))
        {
            return value;
        }
        else
        {
            std::cerr << command << ": warning: " << invalid_message2 << std::endl;
            return replacement_value2;
        }
    }
    else
    {
        std::cerr << command << ": warning: " << invalid_message1 << std::endl;
        return replacement_value1;
    }
}


/**  Convert a_string into a number.
 */
template <class NumericType, class Validator>
NumericType
numberOfString(const char* a_string,               // string we want to convert into a number
               Validator is_valid,                 // validator function
               const std::string& invalid_message, // error message for failed validation
               NumericType replacement_value)      // replacement return value on failure
{
    return numberOfString(a_string,
                          is_valid, invalid_message, replacement_value,
                          boost::lambda::constant(true), "<never reached>", NumericType());
}


/** Convert an anOutputDepth to a "pixel type" string understood by
 * VIGRA. */
std::string
outputPixelTypeOfString(const char* anOutputDepth)
{
    typedef std::map<std::string, std::string> Str2StrMapType;
    Str2StrMapType depthMap;

    boost::assign::insert(depthMap)
        ("INT16", "INT16")
        ("INT32", "INT32")

        ("8", "UINT8")
        ("16", "UINT16")
        ("32", "UINT32")
        ("UINT8", "UINT8")
        ("UINT16", "UINT16")
        ("UINT32", "UINT32")

        ("DOUBLE", "DOUBLE")
        ("FLOAT", "FLOAT")
        ("R32", "FLOAT")
        ("R64", "DOUBLE")
        ("REAL32", "FLOAT")
        ("REAL64", "DOUBLE");

    std::string output_depth(anOutputDepth);
    boost::algorithm::to_upper(output_depth);
    Str2StrMapType::const_iterator p = depthMap.find(output_depth);
    if (p == depthMap.end())
    {
        throw std::invalid_argument(std::string("unknown output depth \"") +
                                    anOutputDepth + "\"");
    }
    else
    {
        return p->second;
    }
}


/** Answer the best pixel type of an image given aFileType with
 * respect to aPixelType.  This is the type with the largest range. */
std::string
bestPixelType(const std::string& aFileType, const std::string& aPixelType)
{
    if (aFileType == "BMP" || aFileType == "JPEG" || aFileType == "RAS")
        return "UINT8";
    else if (aFileType == "PNG" &&
             (aPixelType == "INT32" || aPixelType == "UINT32" ||
              aPixelType == "FLOAT" || aPixelType == "DOUBLE"))
        return "UINT16";
    else if (aFileType == "EXR")
        return "FLOAT";
    else
        return aPixelType;
}


typedef std::pair<double, double> range_t;


/** Answer the maximum range of values aPixelType can represent. */
range_t
rangeOfPixelType(const std::string& aPixelType)
{
    typedef std::map<std::string, range_t> Str2PairMapType;
    Str2PairMapType rangeMap;

    boost::assign::insert(rangeMap)
        ("INT8", std::make_pair(vigra::NumericTraits<vigra::Int8>::min(),
                                vigra::NumericTraits<vigra::Int8>::max()))
        ("INT16", std::make_pair(vigra::NumericTraits<vigra::Int16>::min(),
                                 vigra::NumericTraits<vigra::Int16>::max()))
        ("INT32", std::make_pair(vigra::NumericTraits<vigra::Int32>::min(),
                                 vigra::NumericTraits<vigra::Int32>::max()))

        ("UINT8", std::make_pair(0.0, vigra::NumericTraits<vigra::UInt8>::max()))
        ("UINT16", std::make_pair(0.0, vigra::NumericTraits<vigra::UInt16>::max()))
        ("UINT32", std::make_pair(0.0, vigra::NumericTraits<vigra::UInt32>::max()))

        ("FLOAT", std::make_pair(0.0, 1.0))
        ("DOUBLE", std::make_pair(0.0, 1.0));

    assert(!aPixelType.empty());
    Str2PairMapType::const_iterator r = rangeMap.find(aPixelType);
    if (r == rangeMap.end())
    {
        throw std::invalid_argument(std::string("unknown pixel type \"") + aPixelType + "\"");
    }
    else
    {
        return r->second;
    }
}


/** Answer whether aPixelType defines a range that is so larges that
 *  it includes both aRange and anotherRange. */
bool
includesBothRanges(const std::string& aPixelType,
                   const range_t& aRange,
                   const range_t& anotherRange)
{
    const range_t range = rangeOfPixelType(aPixelType);

    return (aRange.first >= range.first && aRange.second <= range.second &&
            anotherRange.first >= range.first && anotherRange.second <= range.second);
}


/** Answer the smallest pixel type that is larger or equal to both
 *  aPixelType and anotherPixelType. */
std::string
maxPixelType(const std::string& aPixelType, const std::string& anotherPixelType)
{
    const range_t range1 = rangeOfPixelType(aPixelType);
    const range_t range2 = rangeOfPixelType(anotherPixelType);

    if (aPixelType == "DOUBLE" || anotherPixelType == "DOUBLE") {
        return "DOUBLE";
    } else if (aPixelType == "FLOAT" || anotherPixelType == "FLOAT") {
        return "FLOAT";
    } else if (range1.first <= range2.first && range1.second >= range2.second) {
        return aPixelType;      // first includes second
    } else if (range2.first <= range1.first && range2.second >= range1.second) {
        return anotherPixelType; // second includes first
    } else {
        // Types are different: look for the smallest containing type
        using namespace boost::assign;

        typedef std::vector<std::string> string_array;
        typedef string_array::const_iterator string_array_ci;

        if (range1.first < 0 || range2.first < 0) {
            const string_array types = list_of("INT8")("INT16")("INT32");
            for (string_array_ci i = types.begin(); i != types.end(); ++i) {
                if (includesBothRanges(*i, range1, range2)) {
                    return *i;
                }
            }
            return "INT32";
        } else {
            const string_array types = list_of("UINT8")("UINT16")("UINT32");
            for (string_array_ci i = types.begin(); i != types.end(); ++i) {
                if (includesBothRanges(*i, range1, range2)) {
                    return *i;
                }
            }
            return "UINT32";
        }
    }
}


/** Answer the sign of x.
 */

template <typename T>
inline int
sign(T x)
{
    return x > T() ? 1 : (x < T() ? -1 : 0);
}


/** Compute the integral logarithm of n to the base 10.  We do not
 *  need to take special care of the case n == 0 for our purposes. */
inline unsigned
ilog10(unsigned n)
{
    return n <= 9 ? 0 : 1 + ilog10(n / 10);
}


/** Expand aTemplate filling the variable parts with anInputFilename,
 *  anOutputFilename, and aNumber depending on the conversion
 *  specifiers in aTemplate.
 *
 *  Conversion Specifiers - lowercase characters refer to
 *  anInputFilename whereas uppercase ones refer to anOutputFilename:
 *      %%        A single '%'-sign
 *      %i        aNumber unaltered
 *      %n        successor of aNumber
 *      %p        aFilename unaltered
 *      %d        directory part of aFilename
 *      %b        non-directory part (aka basename) of aFilename
 *      %f        basename of aFilename without extension
 *      %e        extension of aFilename (including the leading dot)
 *  All other characters in aTemplate are passed through literally.
 *
 *  The "%i" and "%n" conversions honor a flag which is either
 *      '0'       pad with zeros (default) or
 *      PUNCT     i.e. any punctuation character to pad with
 *  and a width specification.  If no width is requested, the
 *  width is computed based on aNumberOfImages.
 *
 *  For example
 *          expandFilenameTemplate("mask-%04n.tif", 2, "foobar.jpg", 9)
 *  evaluates to
 *          mask-0009.tif
 */
std::string
expandFilenameTemplate(const std::string& aTemplate,
                       unsigned aNumberOfImages,
                       const std::string& anInputFilename,
                       const std::string& anOutputFilename,
                       unsigned aNumber)
{
    std::string result;

    for (std::string::const_iterator c = aTemplate.begin();
         c != aTemplate.end();
         ++c)
    {
        if (*c == '%')
        {
            ++c;
            if (c == aTemplate.end())
            {
                result.push_back(*c);
            }
            else
            {
                char pad = 0;
                while (c != aTemplate.end() && (*c == '0' || ispunct(*c)))
                {
                    pad = *c;
                    ++c;
                }

                std::string width;
                while (c != aTemplate.end() && isdigit(*c))
                {
                    width.push_back(*c);
                    ++c;
                }

                if (c != aTemplate.end())
                {
                    switch (*c)
                    {
                    case '%':
                        result.push_back(*c);
                        break;
                    case 'n':
                        ++aNumber;
                        ++aNumberOfImages;
                        // FALLTHROUGH
                    case 'i':
                    {
                        std::ostringstream oss;
                        oss <<
                            std::setw(width.empty() ? 1 + ilog10(aNumberOfImages - 1) : atoi(width.c_str())) <<
                            std::setfill(pad == 0 ? '0' : pad) <<
                            aNumber;
                        result.append(oss.str());
                        break;
                    }
                    case 'P':
                        result.append(anOutputFilename);
                        break;
                    case 'p':
                        result.append(anInputFilename);
                        break;
                    case 'D':
                        result.append(extractDirname(anOutputFilename));
                        break;
                    case 'd':
                        result.append(extractDirname(anInputFilename));
                        break;
                    case 'B':
                        result.append(extractBasename(anOutputFilename));
                        break;
                    case 'b':
                        result.append(extractBasename(anInputFilename));
                        break;
                    case 'F':
                        result.append(extractFilename(anOutputFilename));
                        break;
                    case 'f':
                        result.append(extractFilename(anInputFilename));
                        break;
                    case 'E':
                        result.append(extractExtension(anOutputFilename));
                        break;
                    case 'e':
                        result.append(extractExtension(anInputFilename));
                        break;
                    default:
                        std::cerr <<
                            command <<
                            ": warning: ignoring unknown variable character ";
                        if (isprint(*c))
                        {
                            std::cerr << "'" << *c << "'";
                        }
                        else
                        {
                            std::cerr << "0x" << std::hex << *c;
                        }
                        std::cerr << " in\n"
                                  << command
                                  << ": warning:     filename template \""
                                  << aTemplate
                                  << "\""
                                  << std::endl;
                    } // switch (*c)
                }
            }
        }
        else
        {
            result.push_back(*c);
        }
    }

    return result;
}


/** Answer a phrase that describes a layer in an image consisting of
 *  multiple layers.  If the image has got only one layer, we avoid to
 *  confuse the user and answer an empty string. */
inline std::string
optional_layer_name(unsigned layer_number, unsigned layer_total)
{
    if (layer_total <= 1U)
    {
        return std::string();
    }
    else
    {
        std::ostringstream oss;
        oss << ", layer " << layer_number << "/" << layer_total;
        return oss.str();
    }
}


inline std::string
profileInfo(cmsHPROFILE profile, cmsInfoType info)
{
    const size_t size = cmsGetProfileInfoASCII(profile, info, cmsNoLanguage, cmsNoCountry, NULL, 0);
    std::string information(size, '\000');
    cmsGetProfileInfoASCII(profile, info, cmsNoLanguage, cmsNoCountry, &information[0], size);
    boost::trim_if(information, std::bind2nd(std::less_equal<char>(), '\040'));

    return information;
}


inline std::string
profileDescription(cmsHPROFILE profile)
{
    return profileInfo(profile, cmsInfoDescription);
}


inline std::string
profileName(cmsHPROFILE profile)
{
    return profileInfo(profile, cmsInfoModel);
}


namespace parameter
{
    // Identifier: [A-Za-z][A-Za-z0-9_-]*
    inline bool
    is_valid_identifier(const std::string& identifier)
    {
        if (identifier.size() == 0)
        {
            return false;
        }
        else if (!isalpha(identifier[0]))
        {
            return false;
        }
        else
        {
            for (std::string::const_iterator x = identifier.begin(); x != identifier.end(); ++x)
            {
                if (!(isalnum(*x) || *x == '_' || *x == '-'))
                {
                    return false;
                }
            }
        }

        return true;
    }


    struct not_found : public std::runtime_error
    {
        not_found(const std::string& a_message) : std::runtime_error(a_message) {}
    };


    // Notes:
    //
    // * The access of parameters through enblend::parameter::as_* is
    //   reasonable fast.  For time-critical parts of the code, the
    //   parameter's value can always be copied into a local variable.
    //
    // * The map from parameter keys to values is meant to be constant
    //   after the command line was parsed, i.e. neither the map
    //   itself, nor one of its values should be modified.  Following
    //   this convention makes all enblend::parameter::as_* functions
    //   thread safe.
    //
    // Some examples how to use parameters:
    //
    // (1) Check whether a parameter has been set.
    //         if (enblend::parameter::exists("foobar")) {...}
    //         else {...}
    //
    // (2) Use a parameter that is known to exist.
    //         std::string s = enblend::parameter::as_string("foobar");
    //         int i = enblend::parameter::as_integer("foobar");
    //         unsigned u = enblend::parameter::as_unsigned("foobar");
    //         double x = enblend::parameter::as_floating_point("foobar");
    //         bool b = enblend::parameter::as_boolean("foobar");
    //
    // (3) Substitute parameter value if it exists; otherwise go with
    //     the default.
    //         std::string s = enblend::parameter::as_string("foobar", "baz");
    //         int i = enblend::parameter::as_integer("foobar", 123);
    //         unsigned u = enblend::parameter::as_unsigned("foobar", 42U);
    //         double x = enblend::parameter::as_floating_point("foobar", 0.577215665);
    //         bool b = enblend::parameter::as_boolean("foobar", true);
    //
    // (4) React on parameter with a non-local change of control flow
    //         int i;
    //         try {i = enblend::parameter::as_integer("foobar");}
    //         catch (enblend::parameter::not_found&) {...}
    //
    // A parameter always can be retrieved as string with function
    // as_string().  All other as_* functions throw the exception
    // conversion_error, if the parameter's value cannot be
    // represented.  See class ParameterValue in file "global.h" for
    // the details on conversion especially for as_boolean().

    inline bool
    exists(const std::string& key)
    {
        return Parameter.find(key) != Parameter.end();
    }


    inline std::string
    as_string(const std::string& key)
    {
        parameter_map::const_iterator x = Parameter.find(key);
        if (x == Parameter.end())
        {
            throw not_found(key);
        }
        else
        {
            return x->second.as_string();
        }
    }


    inline std::string
    as_string(const std::string& key, const std::string& default_value)
    {
        parameter_map::const_iterator x = Parameter.find(key);
        if (x == Parameter.end())
        {
            return default_value;
        }
        else
        {
            return x->second.as_string();
        }
    }


    inline int
    as_integer(const std::string& key)
    {
        parameter_map::iterator x = Parameter.find(key);
        if (x == Parameter.end())
        {
            throw not_found(key);
        }
        else
        {
            return x->second.as_integer();
        }
    }


    inline int
    as_integer(const std::string& key, int default_value)
    {
        parameter_map::iterator x = Parameter.find(key);
        if (x == Parameter.end())
        {
            return default_value;
        }
        else
        {
            return x->second.as_integer();
        }
    }


    inline unsigned
    as_unsigned(const std::string& key)
    {
        parameter_map::iterator x = Parameter.find(key);
        if (x == Parameter.end())
        {
            throw not_found(key);
        }
        else
        {
            return x->second.as_unsigned();
        }
    }


    inline unsigned
    as_unsigned(const std::string& key, unsigned default_value)
    {
        parameter_map::iterator x = Parameter.find(key);
        if (x == Parameter.end())
        {
            return default_value;
        }
        else
        {
            return x->second.as_unsigned();
        }
    }


    inline double
    as_double(const std::string& key)
    {
        parameter_map::iterator x = Parameter.find(key);
        if (x == Parameter.end())
        {
            throw not_found(key);
        }
        else
        {
            return x->second.as_double();
        }
    }


    inline double
    as_double(const std::string& key, double default_value)
    {
        parameter_map::iterator x = Parameter.find(key);
        if (x == Parameter.end())
        {
            return default_value;
        }
        else
        {
            return x->second.as_double();
        }
    }


    inline bool
    as_boolean(const std::string& key)
    {
        parameter_map::iterator x = Parameter.find(key);
        if (x == Parameter.end())
        {
            throw not_found(key);
        }
        else
        {
            return x->second.as_boolean();
        }
    }


    inline bool
    as_boolean(const std::string& key, bool default_value)
    {
        parameter_map::iterator x = Parameter.find(key);
        if (x == Parameter.end())
        {
            return default_value;
        }
        else
        {
            return x->second.as_boolean();
        }
    }
} // namespace parameter

} // namespace enblend


#ifndef HAVE_STRTOK_R
char*
strtok_r(char* str, const char* delim, char** save_ptr)
{
    char *s = str ? str : *save_ptr;

    if (s)
    {
        while (*s != 0 && strchr(delim, (int) *s))
        {
            s++;
        }

        if (*s)
        {
            char *token = s;

            while (*s != 0 && !strchr(delim, (int) *s))
            {
                s++;
            }
            if (*s)
            {
                *s = 0;
                s++;
            }
            *save_ptr = s;

            return token;
        }
        else
        {
            return NULL;
        }
    }
    else
    {
        return NULL;
    }
}
#endif

#endif /* __COMMON_H__ */

// Local Variables:
// mode: c++
// End:
