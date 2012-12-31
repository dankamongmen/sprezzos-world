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


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <algorithm>

#include "layer_selection.h"
#include "selector.h"


LayerSelectionHost::LayerSelectionHost() :
    selector_(NULL),
    info_(NULL),
    tally_(NULL)
{}


LayerSelectionHost::LayerSelectionHost(const LayerSelectionHost& a_selection) :
    selector_(a_selection.selector_),
    info_(new ImageListInformation(a_selection.info_)),
    tally_(new file_tally_t)
{}


LayerSelectionHost&
LayerSelectionHost::operator=(const LayerSelectionHost& a_selection)
{
    if (this != &a_selection)
    {
        selector_ = a_selection.selector_;

        delete info_;
        info_ = new ImageListInformation(a_selection.info_);

        delete tally_;
        tally_ = new file_tally_t;
    }

    return *this;
}


LayerSelectionHost::~LayerSelectionHost()
{
    delete info_;
    delete tally_;
}


std::string
LayerSelectionHost::name() const
{
    return selector_->name();
}


std::string
LayerSelectionHost::description() const
{
    return selector_->description();
}


selector::Abstract*
LayerSelectionHost::get_selector() const
{
    return selector_;
}


void
LayerSelectionHost::set_selector(selector::Abstract* a_selector)
{
    selector_ = a_selector;
}


bool
LayerSelectionHost::accept(const std::string& a_filename, unsigned a_layer_index)
{
    const bool result = selector_->select(const_cast<const ImageListInformation*>(info_),
                                          a_filename, a_layer_index);

    if (result)
    {
        ++(tally_->operator[](a_filename)[a_layer_index]);
    }

    return result;
}
