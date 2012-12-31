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
#ifndef __INFO_H__
#define __INFO_H__


#include <string>

#include "vigra/imageinfo.hxx"


struct LayerInfo
{
    LayerInfo(int a_width, int a_height,
              bool is_color_image, vigra::ImageImportInfo::PixelType a_pixel_type,
              vigra::Diff2D a_position,
              float an_x_resolution, float a_y_resolution) :
        width(a_width), height(a_height),
        is_color(is_color_image), pixel_type(a_pixel_type),
        position(a_position),
        x_resolution(an_x_resolution), y_resolution(a_y_resolution)
    {}

    int width;
    int height;
    bool is_color;
    vigra::ImageImportInfo::PixelType pixel_type;
    vigra::Diff2D position;
    float x_resolution;
    float y_resolution;

    vigra::Size2D size() const;
    bool is_float() const;
    bool is_signed() const;
    std::pair<float, float> resolution() const;
};


class ImageInfo
{
    typedef std::vector<LayerInfo> layer_list;

public:
    ImageInfo(const std::string& a_filename) : filename_(a_filename) {};

    const std::string& filename() const {return filename_;}
    const LayerInfo* layer(unsigned a_layer_index) const {return &layers_[a_layer_index];}
    void append(const LayerInfo& an_info) {layers_.push_back(an_info);}

    unsigned number_of_layers() const {return layers_.size();}

private:
    /* const */ std::string filename_;
    // const time_t filetime_;
    layer_list layers_;
};


class ImageListInformation
{
    typedef std::vector<ImageInfo> image_list;

public:
    ImageListInformation() {}
    ImageListInformation(const ImageListInformation* an_image_list);
    virtual ~ImageListInformation() {}

    void append(const ImageInfo& an_info) {images_.push_back(an_info);}

    unsigned number_of_images() const {return images_.size();}
    const ImageInfo* image_info_on(const std::string& a_filename) const;
    const LayerInfo* layer_info_on(const std::string& a_filename, unsigned a_layer_index) const;

private:
    image_list::const_iterator find_image_by_name(const std::string& a_filename) const;

    image_list images_;
};


#endif // __INFO_H__


// Local Variables:
// mode: c++
// End:
