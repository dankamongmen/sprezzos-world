/*
 * Copyright (C) 2012 Dr. Christoph L. Spiel
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

#ifndef MINIMIZER_H_INCLUDED
#define MINIMIZER_H_INCLUDED


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <vector>

#include <boost/optional.hpp>

#include <gsl/gsl_errno.h>
#include <gsl/gsl_min.h>
#include <gsl/gsl_multimin.h>
#include <gsl/gsl_vector.h>


class Minimizer
{
    enum {ITERATIONS_PER_DIMENSION = 100U};

public:
    Minimizer(size_t dimension) :
        dimension_(dimension),
        maximum_iteration_(ITERATIONS_PER_DIMENSION * dimension), iteration_(0U),
        f_goal_(boost::none), absolute_error_(sqrt(std::numeric_limits<double>::epsilon()))
    {}

    Minimizer(const Minimizer& minimizer) :
        dimension_(minimizer.dimension_),
        maximum_iteration_(minimizer.maximum_iteration_), iteration_(minimizer.iteration_),
        f_goal_(minimizer.f_goal_), absolute_error_(minimizer.absolute_error_)
    {}

    Minimizer& operator=(const Minimizer& minimizer)
    {
        if (this != &minimizer)
        {
            dimension_ = minimizer.dimension_;
            maximum_iteration_ = minimizer.maximum_iteration_;
            iteration_ = minimizer.iteration_;
            f_goal_ = minimizer.f_goal_;
            absolute_error_ = minimizer.absolute_error_;
        }

        return *this;
    }

    virtual ~Minimizer() {}

    virtual std::string proper_name() const = 0;

    size_t dimension() const {return dimension_;}
    void set_dimension(size_t dimension) {dimension_ = dimension;}

    Minimizer* set_maximum_number_of_iterations(unsigned n)
    {
        maximum_iteration_ = n;
        return this;
    }

    Minimizer* unset_maximum_number_of_iterations()
    {
        maximum_iteration_ = boost::none;
        return this;
    }

    unsigned number_of_iterations() const {return iteration_;}

    void next_iteration() {++iteration_;}

    Minimizer* set_goal(double goal)
    {
        f_goal_ = goal;
        return this;
    }

    Minimizer* unset_goal()
    {
        f_goal_ = boost::none;
        return this;
    }

    Minimizer* set_absolute_error(double absolute_error)
    {
        if (absolute_error > 0.0)
        {
            absolute_error_ = std::max(absolute_error, sqrt(std::numeric_limits<double>::epsilon()));
            return this;
        }
        else
        {
            throw std::domain_error("Minimizer1D::set_absolute_error");
        }
    }

    Minimizer* unset_absolute_error()
    {
        absolute_error_ = boost::none;
        return this;
    }

    virtual double f_minimum() const = 0;

    virtual bool has_reached_goal() const {return f_goal_ && f_minimum() <= *f_goal_;}

    virtual bool has_reached_maximum_iteration() const
    {
        return maximum_iteration_ && iteration_ >= *maximum_iteration_;
    }

protected:
    virtual double absolute_error() const
    {
        return absolute_error_ ? *absolute_error_ : sqrt(std::numeric_limits<double>::epsilon());
    }

private:
    Minimizer();                // not implemented

    size_t dimension_;
    boost::optional<unsigned> maximum_iteration_;
    unsigned iteration_;
    boost::optional<double> f_goal_;
    boost::optional<double> absolute_error_;
};


class Minimizer1D : public Minimizer
{
public:
    Minimizer1D(const gsl_function& function, double x_minimum, double x_lower, double x_upper) :
        Minimizer(1U), minimizer_(NULL), function_(function),
        x_minimum_(x_minimum), x_lower_(x_lower), x_upper_(x_upper),
        relative_error_(0.0) {require_ordered_x();}

    Minimizer1D(const Minimizer1D& minimizer) :
        Minimizer(minimizer), minimizer_(NULL), function_(minimizer.function_),
        x_minimum_(minimizer.x_minimum_), x_lower_(minimizer.x_lower_), x_upper_(minimizer.x_upper_),
        relative_error_(minimizer.relative_error_) {}

    Minimizer1D& operator=(const Minimizer1D& minimizer)
    {
        if (this != &minimizer)
        {
            Minimizer::operator=(minimizer);

            function_ = minimizer.function_;
            x_minimum_ = minimizer.x_minimum_;
            x_lower_ = minimizer.x_lower_;
            x_upper_ = minimizer.x_upper_;
            relative_error_ = minimizer.relative_error_;

            gsl_min_fminimizer_free(minimizer_);
            minimizer_ = NULL;
            initialize();
        }

        return *this;
    }

    virtual ~Minimizer1D() {gsl_min_fminimizer_free(minimizer_);}

    virtual std::string proper_name() const {return std::string(gsl_min_fminimizer_name(minimizer_));}

    void set_bracket(const gsl_function& function, double x_minimum, double x_lower, double x_upper)
    {
        require_ordered_x();

        function_ = function;
        x_minimum_ = x_minimum;
        x_lower_ = x_lower;
        x_upper_ = x_upper;

        const int status = gsl_min_fminimizer_set(minimizer_, &function_, x_minimum_, x_lower_, x_upper_);

        if (status == GSL_EINVAL)
        {
            throw std::runtime_error("Minimizer1D::set_bracket: minimum not bracketed");
        }
        else if (status != GSL_SUCCESS)
        {
            throw std::runtime_error("Minimizer1D::set_bracket");
        }
    }

    Minimizer1D* set_relative_error(double relative_error)
    {
        if (relative_error >= 0.0)
        {
            relative_error_ = relative_error;
            return this;
        }
        else
        {
            throw std::domain_error("Minimizer1D::set_relative_error");
        }
    }

    Minimizer1D* unset_relative_error()
    {
        relative_error_ = boost::none;
        return this;
    }

    void run()
    {
        int test_status = GSL_CONTINUE;

        while (test_status == GSL_CONTINUE)
        {
            next_iteration();

            const int minimizer_status = gsl_min_fminimizer_iterate(minimizer_);
            if (minimizer_status == GSL_EBADFUNC || // singular point or
                minimizer_status == GSL_FAILURE)    // could not improve minimum
            {
                break;
            }

            x_lower_ = gsl_min_fminimizer_x_lower(minimizer_);
            x_upper_ = gsl_min_fminimizer_x_upper(minimizer_);
            x_minimum_ = gsl_min_fminimizer_x_minimum(minimizer_);

            if (has_reached_goal() || has_reached_maximum_iteration())
            {
                break;
            }

            test_status = gsl_min_test_interval(x_lower_, x_upper_, absolute_error(), relative_error());
        }
    }

    virtual double x_minimum() const {return x_minimum_;}
    virtual double f_minimum() const {return gsl_min_fminimizer_f_minimum(minimizer_);}

protected:
    virtual const gsl_min_fminimizer_type* type() const = 0;

    void require_ordered_x() const
    {
        if (!(x_lower_ <= x_minimum_ && x_minimum_ <= x_upper_))
        {
            throw std::runtime_error("Minimizer1D::require_ordered_x: x-values not ascending");
        }
    }

    void initialize()
    {
        assert(minimizer_ == NULL);
        minimizer_ = gsl_min_fminimizer_alloc(type());
        if (minimizer_ == NULL)
        {
            throw std::runtime_error("Minimizer1D::initialize: no memory");
        }

        set_bracket(function_, x_minimum_, x_lower_, x_upper_);
    }

    virtual double relative_error() const {return relative_error_ ? *relative_error_ : 0.0;}

    virtual bool has_reached_tolerance() const
    {
        return gsl_min_test_interval(x_lower_, x_upper_, absolute_error(), relative_error()) == GSL_SUCCESS;
    }

private:
    Minimizer1D();              // not implemented

    gsl_min_fminimizer* minimizer_;

    gsl_function function_;

    double x_minimum_;
    double x_lower_;
    double x_upper_;

    boost::optional<double> relative_error_;
};


class GoldenSectionMinimizer1D : public Minimizer1D
{
public:
    GoldenSectionMinimizer1D(const gsl_function& function, double x_minimum, double x_lower, double x_upper) :
        Minimizer1D(function, x_minimum, x_lower, x_upper) {initialize();}

    GoldenSectionMinimizer1D(const GoldenSectionMinimizer1D& minimizer) :
        Minimizer1D(minimizer) {initialize();}

protected:
    virtual const gsl_min_fminimizer_type* type() const {return gsl_min_fminimizer_goldensection;}

private:
    GoldenSectionMinimizer1D(); // not implemented
};


class BrentMinimizer1D : public Minimizer1D
{
public:
    BrentMinimizer1D(const gsl_function& function, double x_minimum, double x_lower, double x_upper) :
        Minimizer1D(function, x_minimum, x_lower, x_upper) {initialize();}

    BrentMinimizer1D(const BrentMinimizer1D& minimizer) :
        Minimizer1D(minimizer) {initialize();}

protected:
    virtual const gsl_min_fminimizer_type* type() const {return gsl_min_fminimizer_brent;}

private:
    BrentMinimizer1D();         // not implemented
};


class GillMurrayMinimizer1D : public Minimizer1D
{
public:
    GillMurrayMinimizer1D(const gsl_function& function, double x_minimum, double x_lower, double x_upper) :
        Minimizer1D(function, x_minimum, x_lower, x_upper) {initialize();}

    GillMurrayMinimizer1D(const GillMurrayMinimizer1D& minimizer) :
        Minimizer1D(minimizer) {initialize();}

protected:
    virtual const gsl_min_fminimizer_type* type() const {return gsl_min_fminimizer_quad_golden;}

private:
    GillMurrayMinimizer1D();    // not implemented
};


template <class input_iterator>
inline void
copy_to_gsl_vector(input_iterator first, input_iterator last, gsl_vector* vector)
{
    unsigned i = 0U;

    while (first != last)
    {
        gsl_vector_set(vector, i, *first);
        ++i;
        ++first;
    }
}


template <class output_iterator>
inline output_iterator
copy_from_gsl_vector(gsl_vector* vector, output_iterator result)
{
    for (unsigned i = 0U; i != vector->size; ++i)
    {
        *result = gsl_vector_get(vector, i);
        ++result;
    }

    return result;
}


class MinimizerMultiDimensionNoDerivative : public Minimizer
{
public:
    typedef std::vector<double> array_type;

    MinimizerMultiDimensionNoDerivative(const gsl_multimin_function& function,
                                        const array_type& start, const array_type& step_sizes) :
        Minimizer(start.size()), minimizer_(NULL), function_(function),
        xs_(gsl_vector_alloc(start.size())), step_sizes_(gsl_vector_alloc(step_sizes.size())),
        characteristic_size_(std::numeric_limits<double>::max())
    {
        if (xs_ == NULL || step_sizes_ == NULL)
        {
            throw std::runtime_error("MinimizerMultiDimensionNoDerivative::constructor: no memory");
        }
        if (start.size() != step_sizes.size())
        {
            throw std::invalid_argument("MinimizerMultiDimensionNoDerivative::constructor: dimension mismatch");
        }

        copy_to_gsl_vector(start.begin(), start.end(), xs_);
        initialize_step_sizes(step_sizes);
    }


    MinimizerMultiDimensionNoDerivative(const gsl_multimin_function& function, const array_type& start) :
        Minimizer(start.size()), minimizer_(NULL), function_(function),
        xs_(gsl_vector_alloc(start.size())), step_sizes_(gsl_vector_alloc(start.size())),
        characteristic_size_(std::numeric_limits<double>::max())
    {
        if (xs_ == NULL || step_sizes_ == NULL)
        {
            throw std::runtime_error("MinimizerMultiDimensionNoDerivative::constructor: no memory");
        }

        copy_to_gsl_vector(start.begin(), start.end(), xs_);
        gsl_vector_set_all(step_sizes_, 1.0);
    }

    MinimizerMultiDimensionNoDerivative(const MinimizerMultiDimensionNoDerivative& minimizer) :
        Minimizer(minimizer), minimizer_(NULL), function_(minimizer.function_),
        xs_(gsl_vector_alloc(dimension())), step_sizes_(gsl_vector_alloc(dimension())),
        characteristic_size_(minimizer.characteristic_size_)
    {
        if (xs_ == NULL || step_sizes_ == NULL)
        {
            throw std::runtime_error("MinimizerMultiDimensionNoDerivative::constructor: no memory");
        }

        gsl_vector_memcpy(xs_, minimizer.xs_);
        gsl_vector_memcpy(step_sizes_, minimizer.step_sizes_);
    }

    MinimizerMultiDimensionNoDerivative& operator=(const MinimizerMultiDimensionNoDerivative& minimizer)
    {
        if (this != &minimizer)
        {
            Minimizer::operator=(minimizer); // or equivalently: this->Minimizer::operator=(minimizer)

            function_ = minimizer.function_;
            characteristic_size_ = minimizer.characteristic_size_;

            if (dimension() != minimizer.dimension())
            {
                set_dimension(minimizer.dimension());

                gsl_vector_free(xs_);
                gsl_vector_free(step_sizes_);

                xs_ = gsl_vector_alloc(dimension());
                step_sizes_ = gsl_vector_alloc(dimension());
                if (xs_ == NULL || step_sizes_ == NULL)
                {
                    throw std::runtime_error("MinimizerMultiDimensionNoDerivative::operator=: no memory");
                }
            }

            gsl_vector_memcpy(xs_, minimizer.xs_);
            gsl_vector_memcpy(step_sizes_, minimizer.step_sizes_);

            gsl_multimin_fminimizer_free(minimizer_);
            minimizer_ = NULL;
            initialize();
        }

        return *this;
    }

    virtual ~MinimizerMultiDimensionNoDerivative()
    {
        gsl_vector_free(step_sizes_);
        gsl_vector_free(xs_);
        gsl_multimin_fminimizer_free(minimizer_);
    }

    void set_start(const array_type& start)
    {
        if (start.size() == dimension())
        {
            copy_to_gsl_vector(start.begin(), start.end(), xs_);
        }
        else
        {
            throw std::invalid_argument("MinimizerMultiDimensionNoDerivative::set_start: dimension mismatch");
        }

        set();
    }

    void set_step_sizes(const array_type& step_sizes)
    {
        initialize_step_sizes(step_sizes);
        set();
    }

    template <class output_iterator>
    output_iterator get_step_sizes(output_iterator result) const
    {
        return copy_from_gsl_vector(step_sizes_, result);
    }

    virtual std::string proper_name() const {return std::string(gsl_multimin_fminimizer_name(minimizer_));}

    void run()
    {
        int test_status = GSL_CONTINUE;

        while (test_status == GSL_CONTINUE)
        {
            next_iteration();

            const int minimizer_status = gsl_multimin_fminimizer_iterate(minimizer_);
            if (minimizer_status == GSL_ENOPROG ||    // could not improve minimum
                has_reached_goal() || has_reached_maximum_iteration())
            {
                break;
            }

            characteristic_size_ = gsl_multimin_fminimizer_size(minimizer_);
            test_status = gsl_multimin_test_size(characteristic_size_, absolute_error());
        }

        characteristic_size_ = gsl_multimin_fminimizer_size(minimizer_);
        gsl_vector_memcpy(xs_, gsl_multimin_fminimizer_x(minimizer_));
    }

    template <class output_iterator>
    output_iterator x_minimum(output_iterator result) const
    {
        return copy_from_gsl_vector(gsl_multimin_fminimizer_x(minimizer_), result);
    }

    virtual double f_minimum() const {return gsl_multimin_fminimizer_minimum(minimizer_);}
    double characteristic_size() const {return characteristic_size_;}

protected:
    virtual const gsl_multimin_fminimizer_type* type() const = 0;

    void initialize_step_sizes(const array_type& step_sizes)
    {
        if (step_sizes.size() == dimension())
        {
            array_type::const_iterator x = step_sizes.begin();
            unsigned i = 0U;

            while (x != step_sizes.end())
            {
                if (*x > 0.0)
                {
                    gsl_vector_set(step_sizes_, i, *x);
                }
                else
                {
                    throw std::runtime_error("MinimizerMultiDimensionNoDerivative::initialize_step_sizes: "
                                             "non-positive step size");
                }

                ++x;
                ++i;
            }
        }
        else
        {
            throw std::invalid_argument("MinimizerMultiDimensionNoDerivative::initialize_step_sizes: "
                                        "dimension mismatch");
        }
    }

    void set()
    {
        assert(minimizer_ != NULL);
        const int status = gsl_multimin_fminimizer_set(minimizer_, &function_, xs_, step_sizes_);

        if (status != GSL_SUCCESS)
        {
            throw std::runtime_error("MinimizerMultiDimensionNoDerivative::set");
        }
    }

    void initialize()
    {
        assert(minimizer_ == NULL);
        minimizer_ = gsl_multimin_fminimizer_alloc(type(), dimension());
        if (minimizer_ == NULL)
        {
            throw std::runtime_error("MinimizerMultiDimensionNoDerivative::initialize: no memory");
        }

        set();
    }

private:
    MinimizerMultiDimensionNoDerivative(); // not implemented

    gsl_multimin_fminimizer* minimizer_;

    gsl_multimin_function function_;
    gsl_vector* xs_;
    gsl_vector* step_sizes_;

    double characteristic_size_;
};


class MinimizerMultiDimensionSimplex : public MinimizerMultiDimensionNoDerivative
{
public:
    MinimizerMultiDimensionSimplex(const gsl_multimin_function& function,
                                   const array_type& start, const array_type& step_sizes) :
        MinimizerMultiDimensionNoDerivative(function, start, step_sizes) {initialize();}

    MinimizerMultiDimensionSimplex(const gsl_multimin_function& function, const array_type& start) :
        MinimizerMultiDimensionNoDerivative(function, start) {initialize();}

    MinimizerMultiDimensionSimplex(const MinimizerMultiDimensionSimplex& minimizer) :
        MinimizerMultiDimensionNoDerivative(minimizer) {initialize();}

protected:
    virtual const gsl_multimin_fminimizer_type* type() const {return gsl_multimin_fminimizer_nmsimplex;}

private:
    MinimizerMultiDimensionSimplex(); // not implemented
};


class MinimizerMultiDimensionSimplex2 : public MinimizerMultiDimensionNoDerivative
{
public:
    MinimizerMultiDimensionSimplex2(const gsl_multimin_function& function,
                                    const array_type& start, const array_type& step_sizes) :
        MinimizerMultiDimensionNoDerivative(function, start, step_sizes) {initialize();}

    MinimizerMultiDimensionSimplex2(const gsl_multimin_function& function, const array_type& start) :
        MinimizerMultiDimensionNoDerivative(function, start) {initialize();}

    MinimizerMultiDimensionSimplex2(const MinimizerMultiDimensionSimplex2& minimizer) :
        MinimizerMultiDimensionNoDerivative(minimizer) {initialize();}

protected:
    virtual const gsl_multimin_fminimizer_type* type() const {return gsl_multimin_fminimizer_nmsimplex2;}

private:
    MinimizerMultiDimensionSimplex2(); // not implemented
};


class MinimizerMultiDimensionSimplex2Randomized : public MinimizerMultiDimensionNoDerivative
{
public:
    MinimizerMultiDimensionSimplex2Randomized(const gsl_multimin_function& function,
                                              const array_type& start, const array_type& step_sizes) :
        MinimizerMultiDimensionNoDerivative(function, start, step_sizes) {initialize();}

    MinimizerMultiDimensionSimplex2Randomized(const gsl_multimin_function& function, const array_type& start) :
        MinimizerMultiDimensionNoDerivative(function, start) {initialize();}

    MinimizerMultiDimensionSimplex2Randomized(const MinimizerMultiDimensionSimplex2Randomized& minimizer) :
        MinimizerMultiDimensionNoDerivative(minimizer) {initialize();}

protected:
    virtual const gsl_multimin_fminimizer_type* type() const {return gsl_multimin_fminimizer_nmsimplex2rand;}

private:
    MinimizerMultiDimensionSimplex2Randomized(); // not implemented
};


#endif /* MINIMIZER_H_INCLUDED */

// Local Variables:
// mode: c++
// End:
