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
#ifndef __SELECTOR_H__
#define __SELECTOR_H__


#include <list>
#include <map>
#include <string>

#include "info.h"
#include "layer_selection.h"


namespace selector
{
    typedef enum
    {
        AllLayersId,
        FirstLayerId,
        LargestLayerId,
        NoLayerId
    } id_t;

    struct Abstract
    {
        Abstract() {}
        virtual ~Abstract() {}
        virtual id_t id() const = 0;

        virtual std::string name() const = 0;
        virtual std::string description() const = 0;

        virtual bool select(const ImageListInformation* an_image_info,
                            const std::string& a_filename, unsigned a_layer_index) = 0;
    };


    class AllLayers : public Abstract
    {
    public:
        id_t id() const {return AllLayersId;}
        std::string name() const;
        std::string description() const;

        bool select(const ImageListInformation*, const std::string&, unsigned)
        {
            return true;
        }
    };


    class FirstLayer : public Abstract
    {
    public:
        id_t id() const {return FirstLayerId;}
        std::string name() const;
        std::string description() const;

        bool select(const ImageListInformation*, const std::string&, unsigned a_layer_index)
        {
            return a_layer_index == 1;
        }
    };


    class LargestLayer : public Abstract
    {
    public:
        id_t id() const {return LargestLayerId;}
        std::string name() const;
        std::string description() const;

        bool select(const ImageListInformation* an_image_info,
                    const std::string& a_filename, unsigned a_layer_index);

    private:
        typedef std::map<std::string, unsigned> cache_t;
        cache_t cache_;
    };


    class NoLayer : public Abstract
    {
    public:
        id_t id() const {return NoLayerId;}
        std::string name() const;
        std::string description() const;

        bool select(const ImageListInformation*, const std::string&, unsigned)
        {
            return false;
        }
    };


    typedef std::list<Abstract*> algorithm_list;

    extern algorithm_list algorithms;

    algorithm_list::const_iterator find_by_id(id_t an_id);
    algorithm_list::const_iterator find_by_name(const std::string& a_name);
} // end namespace selector


#endif /* __SELECTOR_H__ */

// Local Variables:
// mode: c++
// End:
