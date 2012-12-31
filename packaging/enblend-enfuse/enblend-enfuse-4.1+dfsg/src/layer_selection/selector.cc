/*
 * Copyright (C) 2010-2011 Dr. Christoph L. Spiel
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


#include <cassert>

#include <boost/assign/list_of.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "selector.h"


namespace selector
{
    std::string
    AllLayers::name() const
    {
        return "all-layers";
    }


    std::string
    AllLayers::description() const
    {
        return "select all layers in all images;";
    }


    std::string
    FirstLayer::name() const
    {
        return "first-layer";
    }


    std::string
    FirstLayer::description() const
    {
        return "select only first layer in each multi-layer image;";
    }


    std::string
    LargestLayer::name() const
    {
        return "largest-layer";
    }


    std::string
    LargestLayer::description() const
    {
        return "select largest layer in each multi-layer image;";
    }


    static unsigned
    find_largest_layer(const ImageListInformation* an_image_info, const std::string& a_filename)
    {
        const ImageInfo* image_info = an_image_info->image_info_on(a_filename);
        const unsigned n = image_info->number_of_layers();
        unsigned max_index = 0;

        int max_area = -1;
        for (unsigned i = 0; i != n; ++i)
        {
            const LayerInfo* layer_info = an_image_info->layer_info_on(a_filename, i);
            const int area = layer_info->width * layer_info->height;
            if (area > max_area)
            {
                max_area = area;
                max_index = i;
            }
        }

        return max_index;
    }


    bool
    LargestLayer::select(const ImageListInformation* an_image_info,
                         const std::string& a_filename, unsigned a_layer_index)
    {
        cache_t::const_iterator index = cache_.find(a_filename);
        unsigned max_index;

        if (index == cache_.end())
        {
            max_index = 1 + find_largest_layer(an_image_info, a_filename);
            cache_.insert(cache_t::value_type(a_filename, max_index));
        }
        else
        {
            max_index = index->second;
        }

        return a_layer_index == max_index;
    }


    std::string
    NoLayer::name() const
    {
        return "no-layer";
    }


    std::string
    NoLayer::description() const
    {
        return "do not select any layer from any image;";
    }


    algorithm_list algorithms =
        boost::assign::list_of
        (static_cast<Abstract*>(new AllLayers))
        (static_cast<Abstract*>(new FirstLayer))
        (static_cast<Abstract*>(new LargestLayer))
        (static_cast<Abstract*>(new NoLayer))
        ;


    algorithm_list::const_iterator
    find_by_id(id_t an_id)
    {
        algorithm_list::const_iterator algorithm =
            std::find_if(algorithms.begin(), algorithms.end(),
                         boost::lambda::bind(&Abstract::id, boost::lambda::_1) == boost::lambda::constant(an_id));
        assert(algorithm != algorithms.end());

        return algorithm;
    }


    algorithm_list::const_iterator
    find_by_name(const std::string& a_name)
    {
        algorithm_list::const_iterator algorithm =
            std::find_if(algorithms.begin(), algorithms.end(),
                         boost::lambda::bind(&Abstract::name, boost::lambda::_1) == boost::lambda::constant(a_name));

        return algorithm;
    }
} // end namespace selector
