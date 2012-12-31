/*
 * Copyright (C) 2009-2012 Dr. Christoph L. Spiel
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
#ifndef __GLOBAL_H__
#define __GLOBAL_H__

// Here we define macros and types that we already need in the
// definitions of global variables.

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cerrno>
#include <sstream>
#include <stdexcept>

#ifdef HAVE_UNORDERED_MAP
#include <unordered_map>
#else
#include <map>
#endif

#include <boost/algorithm/string/case_conv.hpp>

#include <vigra/numerictraits.hxx>


// Defines to control how many -v flags are required for each type
// of message to be produced on stdout.
#define VERBOSE_ASSEMBLE_MESSAGES           1
#define VERBOSE_CHECKPOINTING_MESSAGES      1

#define VERBOSE_BLEND_MESSAGES              2
#define VERBOSE_GPU_MESSAGES                2
#define VERBOSE_MASK_MESSAGES               2
#define VERBOSE_NFT_MESSAGES                2
#define VERBOSE_PYRAMID_MESSAGES            2
#define VERBOSE_SIGNATURE_REPORTING         2
#define VERBOSE_VERSION_REPORTING           2


#define VERBOSE_COLOR_CONVERSION_MESSAGES   3
#define VERBOSE_DIFFERENCE_STATISTICS       3
#define VERBOSE_LAYER_SELECTION             3
#define VERBOSE_RESPONSE_FILES              3

#define VERBOSE_ABB_MESSAGES                4
#define VERBOSE_IBB_MESSAGES                4
#define VERBOSE_INPUT_IMAGE_INFO_MESSAGES   4
#define VERBOSE_INPUT_UNION_SIZE_MESSAGES   4
#define VERBOSE_ROIBB_SIZE_MESSAGES         4
#define VERBOSE_UBB_MESSAGES                4

#define VERBOSE_CFI_MESSAGES                5
#define VERBOSE_GDA_MESSAGES                5

#define VERBOSE_MEMORY_ESTIMATION_MESSAGES  6


//< src::default-output-filename a.tif
#define DEFAULT_OUTPUT_FILENAME "a.tif"


// Safely retrieve the string associated with m_name from OpenGL.
#define GLGETSTRING(m_name)                     \
    (glGetString(m_name) == NULL ?              \
     "<cannot retrieve " #m_name ">" :          \
     (const char*) (glGetString(m_name)))


class AlternativePercentage
{
public:
    AlternativePercentage(double value, bool is_percentage) :
        value_(value), is_percentage_(is_percentage) {}

    double value() const {return value_;}
    double is_percentage() const {return is_percentage_;}

    void set_value(double value) {value_ = value;}
    void set_percentage(bool is_percentage) {is_percentage_ = is_percentage;}

    std::string str() const
    {
        std::ostringstream oss;
        oss << value_;
        if (is_percentage_)
        {
            oss << "%";
        }
        return oss.str();
    }

    template <class T>
    bool is_effective() const
    {
        return
            value_ > 0.0 &&
            ((is_percentage_ && value_ < 100.0) ||
             (!is_percentage_ && value_ < vigra::NumericTraits<T>::max()));
    }

    template <class T>
    T instantiate() const
    {
        return
            is_percentage_ ?
            value_ * static_cast<double>(vigra::NumericTraits<T>::max()) / 100.0 :
            value_;
    }

private:
    double value_;
    bool is_percentage_;
};


/** The different kinds of boundary conditions we can impose upon an
 *  image. */
typedef enum BoundaryKind
{
    UnknownWrapAround,          // unknown kind
    OpenBoundaries,             // contractible
    HorizontalStrip,            // contractible along 2nd axis
    VerticalStrip,              // contractible along 1st axis
    DoubleStrip                 // non-contractible
} boundary_t;

enum MainAlgo {
    NFT, GraphCut
};



//< src::default-tiff-resolution 300@dmn{dpi}
#define DEFAULT_TIFF_RESOLUTION 300.0f


struct TiffResolution {
    TiffResolution() : x(0.0f), y(0.0f) {}

    TiffResolution(float anXresolution, float aYresolution) :
        x(anXresolution), y(aYresolution) {}

    bool operator==(const TiffResolution& anOther) const {
        return this->x == anOther.x && this->y == anOther.y;
    }

    bool operator!=(const TiffResolution& anOther) const {
        return !operator==(anOther);
    }

    float x;
    float y;
};


struct conversion_error : public std::runtime_error
{
    conversion_error(const std::string& a_message) : std::runtime_error(a_message) {}
};


class ParameterValue
{
public:
    ParameterValue() :
        value_as_string(std::string()),
        integer(NULL), unsigned_integer(NULL), floating_point(NULL), boolean(NULL)
    {initialize();}

    ParameterValue(const std::string& a_string) :
        value_as_string(a_string),
        integer(NULL), unsigned_integer(NULL), floating_point(NULL), boolean(NULL)
    {initialize();}

    ParameterValue(const ParameterValue& parameter_value) :
        value_as_string(parameter_value.value_as_string),
        integer(NULL), unsigned_integer(NULL), floating_point(NULL), boolean(NULL)
    {copy_cached_values(parameter_value);}

    ParameterValue& operator=(const ParameterValue& parameter_value)
    {
        if (this != &parameter_value)
        {
            value_as_string = parameter_value.value_as_string;
            release_memory();
            copy_cached_values(parameter_value);
        }

        return *this;
    }

    virtual ~ParameterValue() {release_memory();}

    std::string as_string() const {return value_as_string;}
    const char* as_c_string() const {return value_as_string.c_str();}

    int as_integer() const
    {
        if (integer == NULL)
        {
            throw conversion_error("cannot convert \"" + value_as_string + "\" to an integer");
        }
        else
        {
            return *integer;
        }
    }

    unsigned as_unsigned() const
    {
        if (unsigned_integer == NULL)
        {
            throw conversion_error("cannot convert \"" + value_as_string + "\" to an unsigned integer");
        }
        else
        {
            return *unsigned_integer;
        }
    }

    double as_double() const
    {
        if (floating_point == NULL)
        {
            throw conversion_error("cannot convert \"" + value_as_string + "\" to a floating-point number");
        }
        else
        {
            return *floating_point;
        }
    }

    bool as_boolean() const
    {
        if (boolean == NULL)
        {
            throw conversion_error("cannot convert \"" + value_as_string + "\" to a boolean");
        }
        else
        {
            return *boolean;
        }
    }

private:
    void initialize()
    {
        initialize_integer();
        initialize_unsigned_integer();
        initialize_floating_point();
        initialize_boolean();
    }

    void initialize_integer()
    {
        char* end;
        errno = 0;
        const int i = strtol(value_as_string.c_str(), &end, 10);
        if (errno == 0 && *end == 0)
        {
            integer = new int;
            *integer = i;
        }
    }

    void initialize_unsigned_integer()
    {
        char* end;
        errno = 0;
        const unsigned u = strtoul(value_as_string.c_str(), &end, 10);
        if (errno == 0 && *end == 0)
        {
            unsigned_integer = new unsigned;
            *unsigned_integer = u;
        }
    }

    void initialize_floating_point()
    {
        char* end;
        errno = 0;
        const double x = strtod(value_as_string.c_str(), &end);
        if (errno == 0 && *end == 0)
        {
            floating_point = new double;
            *floating_point = x;
        }
    }

    void initialize_boolean()
    {
        std::string s(value_as_string);
        boost::algorithm::to_lower(s);

        bool b;
        if (s.empty() || s == "f" || s == "false")
        {
            b = false;
        }
        else if (s == "t" || s == "true")
        {
            b = true;
        }
        else
        {
            char* end;
            errno = 0;
            b = strtol(value_as_string.c_str(), &end, 10);
            if (errno != 0 || *end != 0)
            {
                return;
            }
        }

        boolean = new bool;
        *boolean = b;
    }

    void copy_cached_values(const ParameterValue& parameter_value)
    {
        if (parameter_value.integer != NULL)
        {
            integer = new int;
            *integer = *parameter_value.integer;
        }

        if (parameter_value.unsigned_integer != NULL)
        {
            unsigned_integer = new unsigned;
            *unsigned_integer = *parameter_value.unsigned_integer;
        }

        if (parameter_value.floating_point != NULL)
        {
            floating_point = new double;
            *floating_point = *parameter_value.floating_point;
        }

        if (parameter_value.boolean != NULL)
        {
            boolean = new bool;
            *boolean = *parameter_value.boolean;
        }
    }

    void release_memory()
    {
        delete integer;
        delete unsigned_integer;
        delete floating_point;
        delete boolean;
    }

    std::string value_as_string;
    int* integer;
    unsigned* unsigned_integer;
    double* floating_point;
    bool* boolean;
};


#ifdef HAVE_UNORDERED_MAP
typedef std::unordered_map<std::string, ParameterValue> parameter_map;
#else
typedef std::map<std::string, ParameterValue> parameter_map;
#endif

#endif /* __GLOBAL_H__ */

// Local Variables:
// mode: c++
// End:
