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
#ifndef __BOUNDS_H__
#define __BOUNDS_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "common.h"
#include "pyramid.h"


namespace enblend {

/** Characterize the overlap between src1 and src2.
 *  The images may overlap completely (CompleteOverlap),
 *  partially (PartialOverlap),
 *  or not at all (NoOverlap).
 */
template <typename SrcImageIterator, typename SrcAccessor>
Overlap
inspectOverlap(SrcImageIterator src1_upperleft, SrcImageIterator src1_lowerright, SrcAccessor s1a,
               SrcImageIterator src2_upperleft, SrcAccessor s2a)
{
    SrcImageIterator s1y = src1_upperleft;
    SrcImageIterator s2y = src2_upperleft;
    SrcImageIterator send = src1_lowerright;

    bool foundOverlap = false;
    bool foundDistinctS2 = false;

    for (; s1y.y < send.y; ++s1y.y, ++s2y.y) {

        SrcImageIterator s1x = s1y;
        SrcImageIterator s2x = s2y;

        for (; s1x.x < send.x; ++s1x.x, ++s2x.x) {
            if (s1a(s1x) && s2a(s2x)) {
                foundOverlap = true;
            } else if (s2a(s2x)) {
                foundDistinctS2 = true;
            }
            if (foundOverlap && foundDistinctS2) {
                // If we have found a pixel where there is overlap,
                // and also a pixel where src2 alone contributes,
                // then we know it's PartialOverlap and we can quit.
                return PartialOverlap;
            }
        }
    }

    if (foundOverlap) {
        return CompleteOverlap;
    } else {
        return NoOverlap;
    }

};

// Argument object factory version.
template <typename SrcImageIterator, typename SrcAccessor>
Overlap
inspectOverlap(vigra::triple<SrcImageIterator, SrcImageIterator, SrcAccessor> src1,
               vigra::pair<SrcImageIterator, SrcAccessor> src2)
{
    return inspectOverlap(src1.first, src1.second, src1.third,
                          src2.first, src2.second);
};

/** Determine the region-of-interest and number of blending levels to use,
 *  given the current mask-bounding-box and intersection-bounding-box.
 *  We also need to know if the image is a 360-degree pano so we can check
 *  for the case that the ROI wraps around the left and right edges.
 */
template <typename ImagePixelComponentType>
unsigned int
roiBounds(const vigra::Rect2D& inputUnion,
          const vigra::Rect2D& iBB, const vigra::Rect2D& mBB, const vigra::Rect2D& uBB,
          vigra::Rect2D& roiBB,        // roiBB is an _output_ parameter!
          bool wraparoundForMask)
{
    roiBB = mBB;
    roiBB.addBorder(filterHalfWidth(MAX_PYRAMID_LEVELS));

    if (wraparoundForMask &&
        (roiBB.left() < 0 || roiBB.right() > uBB.right())) {
        // If the ROI goes off either edge of the uBB, and the uBB is
        // the full size of the output image, and the wraparound flag
        // is specified, then make roiBB the full width of uBB.
        roiBB.setUpperLeft(vigra::Point2D(0, roiBB.top()));
        roiBB.setLowerRight(vigra::Point2D(uBB.right(), roiBB.bottom()));
    }

    // ROI must not be bigger than uBB.
    roiBB &= uBB;
    if (Verbose >= VERBOSE_ROIBB_SIZE_MESSAGES) {
        std::cerr << command << ": info: region-of-interest bounding box: " << roiBB << std::endl;
    }

    // Verify the number of levels based on the size of the ROI.
    unsigned int roiShortDimension = std::min(roiBB.width(), roiBB.height());
    const unsigned int minimumPyramidLevels = 1U; //< src::minimum-pyramid-levels 1
    unsigned int allowableLevels = minimumPyramidLevels;
    while (allowableLevels <= MAX_PYRAMID_LEVELS) {
        if (roiShortDimension <= 8U) {
            // ROI dimensions preclude using more levels than allowableLevels.
            break;
        }
        roiShortDimension = (roiShortDimension + 1U) >> 1;
        ++allowableLevels;
    }

    if (allowableLevels <= minimumPyramidLevels) {
        std::cerr << command << ": info: overlap region is too small to make more than "
                  << minimumPyramidLevels << " pyramid level(s)" << std::endl;
    } else {
        if (ExactLevels >= 1) {
            if (ExactLevels > static_cast<int>(allowableLevels)) {
                std::cerr << command << ": warning: cannot blend with " << ExactLevels << " pyramid level(s) as\n"
                          << command << ": warning:     image geometry precludes using more than "
                          << allowableLevels << " pyramid level(s)" << std::endl;
            }
            allowableLevels = std::min(allowableLevels, static_cast<unsigned int>(ExactLevels));
        } else if (ExactLevels < 0) {
            if (static_cast<int>(allowableLevels) + ExactLevels >= static_cast<int>(minimumPyramidLevels)) {
                allowableLevels -= static_cast<unsigned int>(-ExactLevels);
            } else {
                std::cerr << ": warning: cannot sensibly blend with " << allowableLevels << ExactLevels
                          << " levels\n"
                          << command << ": warning:     will not use less than " << minimumPyramidLevels
                          << " pyramid level(s)" << std::endl;
                allowableLevels = minimumPyramidLevels;
            }
        }
    }

    if (Verbose >= VERBOSE_PYRAMID_MESSAGES) {
        std::cerr << command << ": info: using " << allowableLevels << " blending level(s)" << std::endl;
    }

    assert(allowableLevels >= minimumPyramidLevels);
    assert(allowableLevels <= MAX_PYRAMID_LEVELS);

    return allowableLevels;
}

} // namespace enblend

#endif /* __BOUNDS_H__ */

// Local Variables:
// mode: c++
// End:
