/*
 * Copyright (C) 2011-2012 Mikolaj Leszczynski
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
#ifndef __MASKTYPEDEFS_H__
#define __MASKTYPEDEFS_H__

#include <list>
#include <vector>

namespace enblend
{
    typedef std::pair<bool, vigra::Point2D> SegmentPoint;
    typedef std::list<SegmentPoint> Segment;
    typedef std::vector<Segment*> Contour;
    typedef std::vector<Contour*> ContourVector;
}

#endif  /* __MASKTYPEDEFS_H__ */

// Local Variables:
// mode: c++
// End:
