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
#ifndef __ENBLEND_H__
#define __ENBLEND_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>
#include <list>

#include <boost/static_assert.hpp>

#include <vigra/impex.hxx>
#include <vigra/initimage.hxx>
#include <vigra/transformimage.hxx>

#include "vigra_ext/rect2d.hxx"

#include "common.h"
#include "openmp.h"
#include "numerictraits.h"
#include "fixmath.h"
#include "assemble.h"
#include "blend.h"
#include "bounds.h"
#include "mask.h"
#include "pyramid.h"


namespace enblend {

/** Enblend's main blending loop. Templatized to handle different image types.
 */
template <typename ImagePixelType>
void enblendMain(const FileNameList& anInputFileNameList,
                 const std::list<vigra::ImageImportInfo*>& anImageInfoList,
                 vigra::ImageExportInfo& anOutputImageInfo,
                 vigra::Rect2D& anInputUnion)
{
    typedef typename EnblendNumericTraits<ImagePixelType>::ImagePixelComponentType ImagePixelComponentType;
    typedef typename EnblendNumericTraits<ImagePixelType>::ImageType ImageType;
    typedef typename EnblendNumericTraits<ImagePixelType>::AlphaPixelType AlphaPixelType;
    typedef typename EnblendNumericTraits<ImagePixelType>::AlphaType AlphaType;
    typedef typename EnblendNumericTraits<ImagePixelType>::MaskPixelType MaskPixelType;
    typedef typename EnblendNumericTraits<ImagePixelType>::MaskType MaskType;
    typedef typename EnblendNumericTraits<ImagePixelType>::ImagePyramidPixelType ImagePyramidPixelType;
    typedef typename EnblendNumericTraits<ImagePixelType>::ImagePyramidType ImagePyramidType;
    typedef typename EnblendNumericTraits<ImagePixelType>::MaskPyramidPixelType MaskPyramidPixelType;
    typedef typename EnblendNumericTraits<ImagePixelType>::MaskPyramidType MaskPyramidType;

    enum {ImagePyramidIntegerBits = EnblendNumericTraits<ImagePixelType>::ImagePyramidIntegerBits};
    enum {ImagePyramidFractionBits = EnblendNumericTraits<ImagePixelType>::ImagePyramidFractionBits};
    enum {MaskPyramidIntegerBits = EnblendNumericTraits<ImagePixelType>::MaskPyramidIntegerBits};
    enum {MaskPyramidFractionBits = EnblendNumericTraits<ImagePixelType>::MaskPyramidFractionBits};
    typedef typename EnblendNumericTraits<ImagePixelType>::SKIPSMImagePixelType SKIPSMImagePixelType;
    typedef typename EnblendNumericTraits<ImagePixelType>::SKIPSMAlphaPixelType SKIPSMAlphaPixelType;
    typedef typename EnblendNumericTraits<ImagePixelType>::SKIPSMMaskPixelType SKIPSMMaskPixelType;

    std::list<vigra::ImageImportInfo*> imageInfoList(anImageInfoList);

    // Create the initial black image.
    vigra::Rect2D blackBB;
    std::pair<ImageType*, AlphaType*> blackPair =
        assemble<ImageType, AlphaType>(imageInfoList, anInputUnion, blackBB);

    if (Checkpoint) {
        checkpoint(blackPair, anOutputImageInfo);
    }

    // mem usage before = 0
    // mem xsection = OneAtATime: anInputUnion*imageValueType + anInputUnion*AlphaValueType
    //                !OneAtATime: 2*anInputUnion*imageValueType + 2*anInputUnion*AlphaValueType
    // mem usage after = anInputUnion*ImageValueType + anInputUnion*AlphaValueType

#ifdef CACHE_IMAGES
    if (Verbose >= VERBOSE_CFI_MESSAGES) {
        vigra_ext::CachedFileImageDirector& v = vigra_ext::CachedFileImageDirector::v();
        std::cerr << command
                  << ": info: image cache statistics after loading black image\n";
        v.printStats(std::cerr, command + ": info:     blackImage", blackPair.first);
        v.printStats(std::cerr, command + ": info:     blackAlpha", blackPair.second);
        v.printStats(std::cerr, command + ": info: ");
        v.resetCacheMisses();
    }
#endif

    const unsigned numberOfImages = imageInfoList.size();

    // Main blending loop.
    unsigned m = 0;
    FileNameList::const_iterator inputFileNameIterator(anInputFileNameList.begin());
    while (!imageInfoList.empty()) {
        // Create the white image.
        vigra::Rect2D whiteBB;
        std::pair<ImageType*, AlphaType*> whitePair =
            assemble<ImageType, AlphaType>(imageInfoList, anInputUnion, whiteBB);

        // mem usage before = anInputUnion*ImageValueType + anInputUnion*AlphaValueType
        // mem xsection = OneAtATime: anInputUnion*imageValueType + anInputUnion*AlphaValueType
        //                !OneAtATime: 2*anInputUnion*imageValueType + 2*anInputUnion*AlphaValueType
        // mem usage after = 2*anInputUnion*ImageValueType + 2*anInputUnion*AlphaValueType

#ifdef CACHE_IMAGES
        if (Verbose >= VERBOSE_CFI_MESSAGES) {
            vigra_ext::CachedFileImageDirector& v = vigra_ext::CachedFileImageDirector::v();
            std::cerr << command
                      <<": info: image cache statistics after loading white image\n";
            v.printStats(std::cerr, command + ": info:     blackImage", blackPair.first);
            v.printStats(std::cerr, command + ": info:     blackAlpha", blackPair.second);
            v.printStats(std::cerr, command + ": info:     whiteImage", whitePair.first);
            v.printStats(std::cerr, command + ": info:     whiteAlpha", whitePair.second);
            v.printStats(std::cerr, command + ": info: ");
            v.resetCacheMisses();
        }
#endif

        // Union bounding box of whiteImage and blackImage.
        vigra::Rect2D uBB = blackBB | whiteBB;

        if (Verbose >= VERBOSE_UBB_MESSAGES) {
            std::cerr << command
                      << ": info: image union bounding box: "
                      << uBB
                      << std::endl;
        }

        // Intersection bounding box of whiteImage and blackImage.
        vigra::Rect2D iBB = blackBB & whiteBB;
        bool iBBValid = !iBB.isEmpty();

        if (Verbose >= VERBOSE_IBB_MESSAGES) {
            std::cerr << command << ": info: image intersection bounding box: ";
            if (iBBValid) {
                std::cerr << iBB;
            } else {
                std::cerr << "(no intersection)";
            }
            std::cerr << std::endl;
        }

        // Determine what kind of overlap we have.
        const Overlap overlap =
            inspectOverlap(vigra_ext::apply(uBB, srcImageRange(*(blackPair.second))),
                           vigra_ext::apply(uBB, srcImage(*(whitePair.second))));

        // If white image is redundant, skip it and go to next images.
        if (overlap == CompleteOverlap) {
            // White image is redundant.
            delete whitePair.first;
            delete whitePair.second;
            std::cerr << command << ": warning: some images are redundant and will not be blended"
                      << std::endl;
            continue;
        } else if (overlap == NoOverlap && ExactLevels == 0) {
            // Images do not actually overlap.
            std::cerr << command << ": images do not overlap - they will be combined without blending\n"
                      << command << ": use the \"-l\" flag to force blending with a certain number of levels"
                      << std::endl;

            // Copy white image into black image verbatim.
            vigra::copyImageIf(srcImageRange(*(whitePair.first)),
                               maskImage(*(whitePair.second)),
                               destImage(*(blackPair.first)));
            vigra::copyImageIf(srcImageRange(*(whitePair.second)),
                               maskImage(*(whitePair.second)),
                               destImage(*(blackPair.second)));

            delete whitePair.first;
            delete whitePair.second;

            // Checkpoint results.
            if (Checkpoint) {
                if (Verbose >= VERBOSE_CHECKPOINTING_MESSAGES) {
                    std::cerr << command << ": info: ";
                    if (imageInfoList.empty()) {
                        std::cerr << "writing final output" << std::endl;
                    } else {
                        std::cerr << "checkpointing" << std::endl;
                    }
                }
                checkpoint(blackPair, anOutputImageInfo);
            }

            blackBB = uBB;
            continue;
        }

        // Estimate memory requirements.
        if (Verbose >= VERBOSE_MEMORY_ESTIMATION_MESSAGES) {
            long long bytes = 0;

            // Input images
            bytes += 2 * uBB.area() * sizeof(ImagePixelType);

            // Input alpha channels
            bytes += 2 * uBB.area() * sizeof(AlphaPixelType);

            // Mem used during mask generation:
            long long nftBytes = 0;
            if (LoadMasks) {
                nftBytes = 0;
            } else if (CoarseMask) {
                nftBytes =
                    2 * 1/8 * uBB.area() * sizeof(MaskPixelType)
                    + 2 * 1/8 * uBB.area() * sizeof(vigra::UInt32);
            } else {
                nftBytes =
                    2 * uBB.area() * sizeof(MaskPixelType)
                    + 2 * uBB.area() * sizeof(vigra::UInt32);
            }

            long long optBytes = 0;
            if (LoadMasks) {
                optBytes = 0;
            } else if (!OptimizeMask) {
                optBytes = 0;
            } else if (CoarseMask) {
                optBytes = 1/2 * iBB.area() * sizeof(vigra::UInt8);
            } else {
                optBytes = iBB.area() * sizeof(vigra::UInt8);
            }
            if (VisualizeSeam) {
                optBytes *= 2;
            }

            const long long bytesDuringMask = bytes + std::max(nftBytes, optBytes);
            const long long bytesAfterMask = bytes + uBB.area() * sizeof(MaskPixelType);

            bytes = std::max(bytesDuringMask, bytesAfterMask);

            std::cerr << command << ": info: estimated space required for mask generation: "
                      << static_cast<int>(ceil(bytes / 1000000.0))
                      << "MB" << std::endl;
        }

        // Create the blend mask.
        const bool wraparoundForMask =
            WrapAround != OpenBoundaries &&
            uBB.width() == anInputUnion.width();

        MaskType* mask =
            createMask<ImageType, AlphaType, MaskType>(whitePair.first, blackPair.first,
                                                       whitePair.second, blackPair.second,
                                                       uBB, iBB, wraparoundForMask,
                                                       numberOfImages,
                                                       inputFileNameIterator, m);

        // Calculate bounding box of seam line.
        vigra::Rect2D mBB;
        maskBounds(mask, uBB, mBB);

        if (SaveMasks) {
            const std::string maskFilename =
                enblend::expandFilenameTemplate(SaveMaskTemplate,
                                                numberOfImages,
                                                *inputFileNameIterator,
                                                OutputFileName,
                                                m);
            if (maskFilename == *inputFileNameIterator) {
                std::cerr << command
                          << ": will not overwrite input image \""
                          << *inputFileNameIterator
                          << "\" with mask file"
                          << std::endl;
                exit(1);
            } else if (maskFilename == OutputFileName) {
                std::cerr << command
                          << ": will not overwrite output image \""
                          << OutputFileName
                          << "\" with mask file"
                          << std::endl;
                exit(1);
            } else {
                if (Verbose >= VERBOSE_MASK_MESSAGES) {
                    std::cerr << command
                              << ": info: saving mask \"" << maskFilename << "\"" << std::endl;
                }
                vigra::ImageExportInfo maskInfo(maskFilename.c_str());
                maskInfo.setXResolution(ImageResolution.x);
                maskInfo.setYResolution(ImageResolution.y);
                maskInfo.setPosition(uBB.upperLeft());
                maskInfo.setCompression(MASK_COMPRESSION);
                exportImage(srcImageRange(*mask), maskInfo);
            }
        }

        // mem usage here = MaskType*ubb +
        //                  2*anInputUnion*ImageValueType +
        //                  2*anInputUnion*AlphaValueType

#ifdef CACHE_IMAGES
        if (Verbose >= VERBOSE_CFI_MESSAGES) {
            vigra_ext::CachedFileImageDirector& v = vigra_ext::CachedFileImageDirector::v();
            std::cerr << command
                      << ": info: image cache statistics after mask generation\n";
            v.printStats(std::cerr, command + ": info:     blackImage", blackPair.first);
            v.printStats(std::cerr, command + ": info:     blackAlpha", blackPair.second);
            v.printStats(std::cerr, command + ": info:     whiteImage", whitePair.first);
            v.printStats(std::cerr, command + ": info:     whiteAlpha", whitePair.second);
            v.printStats(std::cerr, command + ": info:     mask", mask);
            v.printStats(std::cerr, command + ": info: ");
            v.resetCacheMisses();
        }
#endif

        // Calculate ROI bounds and number of levels from mBB.
        // ROI bounds must be at least mBB but not to extend uBB.
        vigra::Rect2D roiBB;
        const unsigned int numLevels =
            roiBounds<ImagePixelComponentType>(anInputUnion,
                                               iBB, mBB, uBB, roiBB,
                                               wraparoundForMask);
        const bool wraparoundForBlend =
            WrapAround != OpenBoundaries &&
            roiBB.width() == anInputUnion.width();

        if (StopAfterMaskGeneration) {
            vigra::copyImageIf(vigra_ext::apply(uBB, srcImageRange(*(whitePair.first))),
                               maskImage(*mask),
                               vigra_ext::apply(uBB, destImage(*(blackPair.first))));
            vigra::initImageIf(vigra_ext::apply(whiteBB, destImageRange(*(blackPair.second))),
                               vigra_ext::apply(whiteBB, maskImage(*(whitePair.second))),
                               vigra::NumericTraits<AlphaPixelType>::max());

            delete whitePair.first;
            delete whitePair.second;

            blackBB = uBB;
            ++m;
            ++inputFileNameIterator;

            continue;
        }

        // Estimate memory requirements for this blend iteration
        if (Verbose >= VERBOSE_MEMORY_ESTIMATION_MESSAGES) {
            // Maximum utilization is when all three pyramids have been built
            // mem xsection = 4 * roiBB.width() * SKIPSMImagePixelType
            //                + 4 * roiBB.width() * SKIPSMAlphaPixelType
            // mem usage after = anInputUnion*ImageValueType + 2*anInputUnion*AlphaValueType
            //      + (4/3)*roiBB*MaskPyramidType
            //      + 2*(4/3)*roiBB*ImagePyramidType
            long long bytes =
                anInputUnion.area() * (sizeof(ImagePixelType) + 2 * sizeof(AlphaPixelType))
                + (4/3) * roiBB.area() * (sizeof(MaskPyramidPixelType)
                                          + 2 * sizeof(ImagePyramidPixelType))
                + (4 * roiBB.width()) * (sizeof(SKIPSMImagePixelType)
                                         + sizeof(SKIPSMAlphaPixelType));

            std::cerr << command << ": info: estimated space required for this blend step: "
                      << static_cast<int>(ceil(bytes / 1000000.0))
                      << "MB" << std::endl;
        }

        // Create a version of roiBB relative to uBB upperleft corner.
        // This is to access roi within images of size uBB.
        // For example, the mask.
        vigra::Rect2D roiBB_uBB = roiBB;
        roiBB_uBB.moveBy(-uBB.upperLeft());

        // Build Gaussian pyramid from mask.
        std::vector<MaskPyramidType*> *maskGP =
            gaussianPyramid<MaskType, MaskPyramidType,
                            MaskPyramidIntegerBits, MaskPyramidFractionBits,
                            SKIPSMMaskPixelType>(numLevels, wraparoundForBlend,
                                                 vigra_ext::apply(roiBB_uBB, srcImageRange(*mask)));
#ifdef DEBUG_EXPORT_PYRAMID
        exportPyramid<SKIPSMMaskPixelType, MaskPyramidType>(maskGP, "mask");
#endif

        // mem usage before = MaskType*ubb + 2*anInputUnion*ImageValueType + 2*anInputUnion*AlphaValueType
        // mem usage xsection = 3 * roiBB.width * MaskPyramidType
        // mem usage after = MaskType*ubb + 2*anInputUnion*ImageValueType + 2*anInputUnion*AlphaValueType
        //                   + (4/3)*roiBB*MaskPyramidType

#ifdef CACHE_IMAGES
        if (Verbose >= VERBOSE_CFI_MESSAGES) {
            vigra_ext::CachedFileImageDirector& v = vigra_ext::CachedFileImageDirector::v();
            std::cerr << command
                      << ": info: image cache statistics after calculating mask pyramid\n";
            v.printStats(std::cerr, command + ": info:     blackImage", blackPair.first);
            v.printStats(std::cerr, command + ": info:     blackAlpha", blackPair.second);
            v.printStats(std::cerr, command + ": info:     whiteImage", whitePair.first);
            v.printStats(std::cerr, command + ": info:     whiteAlpha", whitePair.second);
            v.printStats(std::cerr, command + ": info:     mask", mask);
            for (unsigned int i = 0; i < maskGP->size(); i++) {
                v.printStats(std::cerr, command + ": info:     maskGP", i, (*maskGP)[i]);
            }
            v.printStats(std::cerr, command + ": info: ");
            v.resetCacheMisses();
        }
#endif

        // Now it is safe to make changes to mask image.
        // Black out the ROI in the mask.
        // Make an roiBounds relative to uBB origin.
        vigra::initImage(vigra_ext::apply(roiBB_uBB, destImageRange(*mask)),
                         vigra::NumericTraits<MaskPyramidPixelType>::zero());

        // Copy pixels inside whiteBB and inside white part of mask into black image.
        // These are pixels where the white image contributes outside of the ROI.
        // We cannot modify black image inside the ROI yet because we haven't built the
        // black pyramid.
        vigra::copyImageIf(vigra_ext::apply(uBB, srcImageRange(*(whitePair.first))),
                           maskImage(*mask),
                           vigra_ext::apply(uBB, destImage(*(blackPair.first))));

        // We no longer need the mask.
        delete mask;
        // mem usage after = 2*anInputUnion*ImageValueType +
        //                   2*anInputUnion*AlphaValueType +
        //                   (4/3)*roiBB*MaskPyramidType

        // Build Laplacian pyramid from white image.
        std::vector<ImagePyramidType*>* whiteLP =
            laplacianPyramid<ImageType, AlphaType, ImagePyramidType,
                             ImagePyramidIntegerBits, ImagePyramidFractionBits,
                             SKIPSMImagePixelType, SKIPSMAlphaPixelType>("whiteGP",
                                                                         numLevels, wraparoundForBlend,
                                                                         vigra_ext::apply(roiBB, srcImageRange(*(whitePair.first))),
                                                                         vigra_ext::apply(roiBB, maskImage(*(whitePair.second))));

#ifdef CACHE_IMAGES
        if (Verbose >= VERBOSE_CFI_MESSAGES) {
            vigra_ext::CachedFileImageDirector& v = vigra_ext::CachedFileImageDirector::v();
            std::cerr << command
                      << ": info: image cache statistics after calculating white pyramid\n";
            v.printStats(std::cerr, command + ": info:     blackImage", blackPair.first);
            v.printStats(std::cerr, command + ": info:     blackAlpha", blackPair.second);
            v.printStats(std::cerr, command + ": info:     whiteImage", whitePair.first);
            v.printStats(std::cerr, command + ": info:     whiteAlpha", whitePair.second);
            for (unsigned int i = 0; i < maskGP->size(); i++) {
                v.printStats(std::cerr, command + ": info:     maskGP", i, (*maskGP)[i]);
            }
            for (unsigned int i = 0; i < whiteLP->size(); i++) {
                v.printStats(std::cerr, command + ": info:     whiteLP", i, (*whiteLP)[i]);
            }
            v.printStats(std::cerr, command + ": info: ");
            v.resetCacheMisses();
        }
#endif
        // mem usage after = 2*anInputUnion*ImageValueType + 2*anInputUnion*AlphaValueType
        //                   + (4/3)*roiBB*MaskPyramidType + (4/3)*roiBB*ImagePyramidType
        // mem xsection = 4 * roiBB.width() * SKIPSMImagePixelType
        //                + 4 * roiBB.width() * SKIPSMAlphaPixelType

        // We no longer need the white rgb data.
        delete whitePair.first;
        // mem usage after = anInputUnion*ImageValueType + 2*anInputUnion*AlphaValueType
        //                   + (4/3)*roiBB*MaskPyramidType + (4/3)*roiBB*ImagePyramidType

        // Build Laplacian pyramid from black image.
        std::vector<ImagePyramidType*>* blackLP =
            laplacianPyramid<ImageType, AlphaType, ImagePyramidType,
                             ImagePyramidIntegerBits, ImagePyramidFractionBits,
                             SKIPSMImagePixelType, SKIPSMAlphaPixelType>("blackGP",
                                                                         numLevels, wraparoundForBlend,
                                                                         vigra_ext::apply(roiBB, srcImageRange(*(blackPair.first))),
                                                                         vigra_ext::apply(roiBB, maskImage(*(blackPair.second))));

#ifdef CACHE_IMAGES
        if (Verbose >= VERBOSE_CFI_MESSAGES) {
            vigra_ext::CachedFileImageDirector& v = vigra_ext::CachedFileImageDirector::v();
            std::cerr << command
                      << ": info: image cache statistics after calculating black pyramid\n";
            v.printStats(std::cerr, command + ": info:     blackImage", blackPair.first);
            v.printStats(std::cerr, command + ": info:     blackAlpha", blackPair.second);
            v.printStats(std::cerr, command + ": info:     whiteAlpha", whitePair.second);
            for (unsigned int i = 0; i < maskGP->size(); i++) {
                v.printStats(std::cerr, command + ": info:     maskGP", i, (*maskGP)[i]);
            }
            for (unsigned int i = 0; i < whiteLP->size(); i++) {
                v.printStats(std::cerr, command + ": info:     whiteLP", i, (*whiteLP)[i]);
            }
            for (unsigned int i = 0; i < blackLP->size(); i++) {
                v.printStats(std::cerr, command + ": info:     blackLP", i, (*blackLP)[i]);
            }
            v.printStats(std::cerr, command + ": info: ");
            v.resetCacheMisses();
        }
#endif

#ifdef DEBUG_EXPORT_PYRAMID
        exportPyramid<SKIPSMImagePixelType, ImagePyramidType>(blackLP, "enblend_black_lp");
#endif

        // Peak memory xsection is here!
        // mem xsection = 4 * roiBB.width() * SKIPSMImagePixelType
        //                + 4 * roiBB.width() * SKIPSMAlphaPixelType
        // mem usage after = anInputUnion*ImageValueType + 2*anInputUnion*AlphaValueType
        //      + (4/3)*roiBB*MaskPyramidType
        //      + 2*(4/3)*roiBB*ImagePyramidType

        // Make the black image alpha equal to the union of the
        // white and black alpha channels.
        vigra::initImageIf(vigra_ext::apply(whiteBB, destImageRange(*(blackPair.second))),
                           vigra_ext::apply(whiteBB, maskImage(*(whitePair.second))),
                           vigra::NumericTraits<AlphaPixelType>::max());

        // We no longer need the white alpha data.
        delete whitePair.second;

        // mem usage after = anInputUnion*ImageValueType + anInputUnion*AlphaValueType
        //      + (4/3)*roiBB*MaskPyramidType + 2*(4/3)*roiBB*ImagePyramidType

        // Blend pyramids
        ConvertScalarToPyramidFunctor<MaskPixelType, MaskPyramidPixelType, MaskPyramidIntegerBits, MaskPyramidFractionBits> whiteMask;
        blend(maskGP, whiteLP, blackLP, whiteMask(vigra::NumericTraits<MaskPixelType>::max()));
#ifdef CACHE_IMAGES
        if (Verbose >= VERBOSE_CFI_MESSAGES) {
            vigra_ext::CachedFileImageDirector& v = vigra_ext::CachedFileImageDirector::v();
            std::cerr << command
                      << ": info: image cache statistics after blending pyramids\n";
            v.printStats(std::cerr, command + ": info:     blackImage", blackPair.first);
            v.printStats(std::cerr, command + ": info:     blackAlpha", blackPair.second);
            for (unsigned int i = 0; i < maskGP->size(); i++) {
                v.printStats(std::cerr, command + ": info:     maskGP", i, (*maskGP)[i]);
            }
            for (unsigned int i = 0; i < whiteLP->size(); i++) {
                v.printStats(std::cerr, command + ": info:     whiteLP", i, (*whiteLP)[i]);
            }
            for (unsigned int i = 0; i < blackLP->size(); i++) {
                v.printStats(std::cerr, command + ": info:     blackLP", i, (*blackLP)[i]);
            }
            v.printStats(std::cerr, command + ": info: ");
            v.resetCacheMisses();
        }
#endif

        // delete mask pyramid
#ifdef DEBUG_EXPORT_PYRAMID
        exportPyramid<SKIPSMMaskPixelType, MaskPyramidType>(maskGP, "enblend_mask_gp");
#endif
        for (unsigned int i = 0; i < maskGP->size(); i++) {
            delete (*maskGP)[i];
        }
        delete maskGP;

        // mem usage after = anInputUnion*ImageValueType + anInputUnion*AlphaValueType + 2*(4/3)*roiBB*ImagePyramidType

        // delete white pyramid
#ifdef DEBUG_EXPORT_PYRAMID
        exportPyramid<SKIPSMImagePixelType, ImagePyramidType>(whiteLP, "enblend_white_lp");
#endif
        for (unsigned int i = 0; i < whiteLP->size(); i++) {
            delete (*whiteLP)[i];
        }
        delete whiteLP;

        // mem usage after = anInputUnion*ImageValueType + anInputUnion*AlphaValueType + (4/3)*roiBB*ImagePyramidType

#ifdef DEBUG_EXPORT_PYRAMID
        exportPyramid<SKIPSMImagePixelType, ImagePyramidType>(blackLP, "enblend_blend_lp");
#endif

        // collapse black pyramid
        collapsePyramid<SKIPSMImagePixelType>(wraparoundForBlend, blackLP);
#ifdef CACHE_IMAGES
        if (Verbose >= VERBOSE_CFI_MESSAGES) {
            vigra_ext::CachedFileImageDirector& v = vigra_ext::CachedFileImageDirector::v();
            std::cerr << command
                      << ": info: image cache statistics after collapsing black pyramid\n";
            v.printStats(std::cerr, command + ": info:     blackImage", blackPair.first);
            v.printStats(std::cerr, command + ": info:     blackAlpha", blackPair.second);
            for (unsigned int i = 0; i < blackLP->size(); i++) {
                v.printStats(std::cerr, command + ": info:     blackLP", i, (*blackLP)[i]);
            }
            v.printStats(std::cerr, command + ": info: ");
            v.resetCacheMisses();
        }
#endif

        // copy collapsed black pyramid into black image ROI, using black alpha mask.
        copyFromPyramidImageIf<ImagePyramidType, MaskType, ImageType,
                               ImagePyramidIntegerBits, ImagePyramidFractionBits>(srcImageRange(*((*blackLP)[0])),
                                                                                  vigra_ext::apply(roiBB, maskImage(*(blackPair.second))),
                                                                                  vigra_ext::apply(roiBB, destImage(*(blackPair.first))));

        // delete black pyramid
        for (unsigned int i = 0; i < blackLP->size(); i++) {
            delete (*blackLP)[i];
        }
        delete blackLP;

        // mem usage after = anInputUnion*ImageValueType + anInputUnion*AlphaValueType

        // Checkpoint results.
        if (Checkpoint) {
            if (Verbose >= VERBOSE_CHECKPOINTING_MESSAGES) {
                std::cerr << command << ": info: ";
                if (imageInfoList.empty()) {
                    std::cerr << "writing final output" << std::endl;
                } else {
                    std::cerr << "checkpointing" << std::endl;
                }
            }
            checkpoint(blackPair, anOutputImageInfo);
        }

#ifdef CACHE_IMAGES
        if (Verbose >= VERBOSE_CFI_MESSAGES) {
            vigra_ext::CachedFileImageDirector& v = vigra_ext::CachedFileImageDirector::v();
            std::cerr << command
                      << ": info: image cache statistics after checkpointing\n";
            v.printStats(std::cerr, command + ": info:     blackImage", blackPair.first);
            v.printStats(std::cerr, command + ": info:     blackAlpha", blackPair.second);
            v.printStats(std::cerr, command + ": info: ");
            v.resetCacheMisses();
        }
#endif

        // Now set blackBB to uBB.
        blackBB = uBB;

        ++m;
        ++inputFileNameIterator;
    } // end main blending loop

    if (!StopAfterMaskGeneration && !Checkpoint) {
        if (Verbose >= VERBOSE_CHECKPOINTING_MESSAGES) {
            std::cerr << command << ": info: writing final output" << std::endl;
        }
        checkpoint(blackPair, anOutputImageInfo);
    }

    delete blackPair.first;
    delete blackPair.second;
}

} // namespace enblend

#endif /* __ENBLEND_H__ */

// Local Variables:
// mode: c++
// End:
