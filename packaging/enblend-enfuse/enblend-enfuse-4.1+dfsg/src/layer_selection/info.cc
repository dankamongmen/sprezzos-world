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


#include <utility>              // make_pair

#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>

#include "info.h"


vigra::Size2D
LayerInfo::size() const
{
    return vigra::Size2D(width, height);
}


bool
LayerInfo::is_float() const
{
    return pixel_type == vigra::ImageImportInfo::FLOAT || pixel_type == vigra::ImageImportInfo::DOUBLE;
}


bool
LayerInfo::is_signed() const
{
    return pixel_type == vigra::ImageImportInfo::INT16 || pixel_type == vigra::ImageImportInfo::INT32;
}


std::pair<float, float>
LayerInfo::resolution() const
{
    return std::make_pair(x_resolution, y_resolution);
}


////////////////////////////////////////////////////////////////////////


ImageListInformation::ImageListInformation(const ImageListInformation* an_image_list)
{
    if (an_image_list != NULL)
    {
        copy(an_image_list->images_.begin(), an_image_list->images_.end(), back_inserter(images_));
    }
}


ImageListInformation::image_list::const_iterator
ImageListInformation::find_image_by_name(const std::string& a_filename) const
{
    return find_if(images_.begin(), images_.end(),
                   boost::lambda::bind(&ImageInfo::filename, boost::lambda::_1) == boost::lambda::constant(a_filename));
}


const ImageInfo*
ImageListInformation::image_info_on(const std::string& a_filename) const
{
    image_list::const_iterator image_info = find_image_by_name(a_filename);

    if (image_info == images_.end())
    {
        return NULL;
    }
    else
    {
        return &(*image_info);
    }
}


const LayerInfo*
ImageListInformation::layer_info_on(const std::string& a_filename, unsigned a_layer_index) const
{
    image_list::const_iterator image_info = find_image_by_name(a_filename);

    if (image_info == images_.end())
    {
        return NULL;
    }
    else
    {
        if (a_layer_index >= image_info->number_of_layers())
        {
            return NULL;
        }
        else
        {
            return image_info->layer(a_layer_index);
        }
    }
}
