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
#ifndef __BLEND_H__
#define __BLEND_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <vector>

#include <vigra/combineimages.hxx>
#include <vigra/numerictraits.hxx>

#include "fixmath.h"


namespace enblend {

/** Functor for blending a black and white pyramid level using a mask
 *  pyramid level.
 */
template <typename MaskPixelType>
class CartesianBlendFunctor {
public:
    CartesianBlendFunctor(MaskPixelType w) : white(vigra::NumericTraits<MaskPixelType>::toRealPromote(w)) {}

    template <typename ImagePixelType>
    ImagePixelType operator()(const MaskPixelType& maskP, const ImagePixelType& wP, const ImagePixelType& bP) const {
        typedef typename vigra::NumericTraits<ImagePixelType>::RealPromote RealImagePixelType;

        // Convert mask pixel to blend coefficient in range [0.0, 1.0].
        double whiteCoeff = vigra::NumericTraits<MaskPixelType>::toRealPromote(maskP) / white;
        // Sometimes masked image data is invalid.  For floating point samples
        // this includes possible NaN's in the data.   In that case, computing
        // the output sample will result in a NaN output if the weight on that
        // pixel is 0 (since 0*NaN = NaN )
        // Handle this by explicitly ignoring fully masked pixels
        if (whiteCoeff >= 1.0) {
            return wP;
        }
        if (whiteCoeff <= 0.0) {
            return bP;
        }

        const double blackCoeff = 1.0 - whiteCoeff;

        RealImagePixelType rwP = vigra::NumericTraits<ImagePixelType>::toRealPromote(wP);
        RealImagePixelType rbP = vigra::NumericTraits<ImagePixelType>::toRealPromote(bP);

        RealImagePixelType blendP = (whiteCoeff * rwP) + (blackCoeff * rbP);

        return vigra::NumericTraits<ImagePixelType>::fromRealPromote(blendP);
    }

protected:
    double white;
};


/** Blend black and white pyramids using mask pyramid.
 */
template <typename MaskPyramidType, typename ImagePyramidType>
void
blend(std::vector<MaskPyramidType*>* maskGP,
      std::vector<ImagePyramidType*>* whiteLP,
      std::vector<ImagePyramidType*>* blackLP,
      typename MaskPyramidType::value_type maskPyramidWhiteValue)
{
    if (Verbose >= VERBOSE_BLEND_MESSAGES) {
        std::cerr << command << ": info: blending layers:             ";
        std::cerr.flush();
    }

    for (unsigned int layer = 0; layer < maskGP->size(); layer++) {
        if (Verbose >= VERBOSE_BLEND_MESSAGES) {
            std::cerr << " l" << layer;
            std::cerr.flush();
        }

        combineThreeImagesMP(srcImageRange(*((*maskGP)[layer])),
                             srcImage(*((*whiteLP)[layer])),
                             srcImage(*((*blackLP)[layer])),
                             destImage(*((*blackLP)[layer])),
                             CartesianBlendFunctor<typename MaskPyramidType::value_type>(maskPyramidWhiteValue));
    }

    if (Verbose >= VERBOSE_BLEND_MESSAGES) {
        std::cerr << endl;
    }
}

} // namespace enblend

#endif /* __BLEND_H__ */

// Local Variables:
// mode: c++
// End:
