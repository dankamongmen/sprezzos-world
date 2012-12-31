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

#ifndef GRAPHCUT_H
#define GRAPHCUT_H

#include <iostream>

#ifdef _WIN32
#include <cmath>
#else
#include <math.h>
#endif

#include <stdlib.h>
#include <utility>
#include <queue>

#include <boost/functional/hash.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/unordered_set.hpp>

#include <vigra/functorexpression.hxx>
#include <vigra/inspectimage.hxx>
#include <vigra/numerictraits.hxx>
#include <vigra/copyimage.hxx>
#include <vigra/imageiterator.hxx>
#include <vigra/stdconvolution.hxx>
#include <vigra/bordertreatment.hxx>
#include <vigra/labelimage.hxx>
#include <vigra/transformimage.hxx>
#include <vigra/contourcirculator.hxx>
#include <vigra/mathutil.hxx>
#include <vigra/combineimages.hxx>

#include "vigra_ext/rect2d.hxx"
#include "vigra_ext/stdcachedfileimage.hxx"

#include "common.h"
#include "maskcommon.h"
#include "masktypedefs.h"
#include "nearest.h"


using namespace vigra::functor;


#define BIT_MASK_DIR 0x03
#define BIT_MASK_OPDIR 0x02
#define BIT_MASK_OPEN 0x04
#define BIT_MASK_DIVIDE 0x0F


namespace enblend {

    struct pointHash
    {
        std::size_t operator()(const vigra::Point2D& p) const
        {
            std::size_t seed = 0;

            boost::hash_combine(seed, p.x);
            boost::hash_combine(seed, p.y);

            return seed;
        }
    };


    class CheckpointPixels
    {
    public:
        CheckpointPixels() {}
        boost::unordered_set<vigra::Point2D, pointHash> top, bottom;

        ~CheckpointPixels()
        {
            this->clear();
        }

        void clear()
        {
            top.clear();
            bottom.clear();
        }
    };


    int distab(const vigra::Point2D& a, const vigra::Point2D& b)
    {
        return std::abs((a.x - b.x)) + std::abs((a.y - b.y));
    }


    template<class MaskImageIterator, class MaskAccessor,
             class MaskPixelType, class DestImageIterator, class DestAccessor>
    std::vector<vigra::Point2D>*
    findIntermediatePoints(MaskImageIterator mask1_upperleft, MaskImageIterator mask1_lowerright,
                           MaskAccessor ma1, MaskImageIterator mask2_upperleft, MaskAccessor ma2,
                           DestImageIterator dest_upperleft, DestAccessor da,
                           nearest_neighbor_metric_t& norm, boundary_t& boundary,
                           const vigra::Rect2D& iBB)
    {
        typedef vigra::NumericTraits<MaskPixelType> MaskPixelTraits;
        typedef typename IMAGETYPE<MaskPixelType>::traverser IteratorType;
        typedef vigra::CrackContourCirculator<IteratorType> Circulator;
        typedef vigra::triple<IteratorType, IteratorType, unsigned int> EntryPointContainer;
        IMAGETYPE<MaskPixelType> nftTempImg(mask1_lowerright - mask1_upperleft + vigra::Diff2D(2, 2),
                                            MaskPixelTraits::max() / 2);
        IMAGETYPE<MaskPixelType> nft(iBB.lowerRight() - iBB.upperLeft() + vigra::Diff2D(2, 2),
                                     MaskPixelTraits::max() / 2);
        IMAGETYPE<MaskPixelType> overlap(iBB.lowerRight() - iBB.upperLeft() + vigra::Diff2D(2, 2));
        IteratorType nftIter = nft.upperLeft();
        IteratorType previous;
        Circulator* circ;
        Circulator* end;
        bool offTheBorder = false;
        bool inOverlap = false;
        bool ready = false;
        std::vector<vigra::Point2D>* interPointList = new std::vector<vigra::Point2D>();
        std::vector<EntryPointContainer> entryPointList;
        EntryPointContainer* max = NULL;
        std::pair<IteratorType, IteratorType> entryPoint;
        vigra::Point2D intermediatePoint;
        unsigned int counter = 0;

        nearestFeatureTransform(vigra::srcIterRange(mask1_upperleft, mask1_lowerright, ma1),
                                vigra::srcIter(mask2_upperleft, ma2),
                                vigra::destIter(nftTempImg.upperLeft() + vigra::Diff2D(1, 1)),
                                norm, boundary);

        copyImage(vigra::srcIterRange(nftTempImg.upperLeft() + vigra::Diff2D(1, 1), nftTempImg.lowerRight() - vigra::Diff2D(1, 1)),
                  vigra::destIter(dest_upperleft, da));

        copyImage(vigra_ext::apply(iBB, vigra::srcIterRange(nftTempImg.upperLeft() + vigra::Diff2D(1, 1),
                                                     nftTempImg.lowerRight() - vigra::Diff2D(1, 1))),
                  vigra::destIter(nft.upperLeft() + vigra::Diff2D(1, 1)));

        combineTwoImagesMP(vigra_ext::apply(iBB, vigra::srcIterRange(mask1_upperleft, mask1_lowerright, ma1)),
                           vigra_ext::apply(iBB, vigra::srcIter(mask2_upperleft, ma2)),
                           vigra::destIter(overlap.upperLeft() + vigra::Diff2D(1, 1)),
                           ifThenElse(Arg1() && Arg2(),
                                      Param(MaskPixelTraits::max()),
                                      Param(MaskPixelTraits::zero())));

#ifdef DEBUG_GRAPHCUT
        exportImage(srcImageRange(nftTempImg), ImageExportInfo("./debug/nft-orig.tif").setPixelType("UINT8"));
        exportImage(srcImageRange(nft), ImageExportInfo("./debug/nfttotal.tif").setPixelType("UINT8"));
        exportImage(srcImageRange(overlap), ImageExportInfo("./debug/overlap.tif").setPixelType("UINT8"));
#endif

        circ = new Circulator(nftIter + vigra::Diff2D(1, 0), vigra::FourNeighborCode::South);
        end = new Circulator(nftIter + vigra::Diff2D(1, 0), vigra::FourNeighborCode::South);

        previous = circ->outerPixel();

        do {
            (*circ)++;
            if (nft.accessor()(previous) != nft.accessor()(circ->outerPixel())) {
                if (ready) {
                    entryPointList.push_back(EntryPointContainer(entryPoint.first, entryPoint.second, counter));
                    entryPoint.first = previous;
                    entryPoint.second = circ->outerPixel();
                } else {
                    ready = true;
                    entryPoint = std::pair<IteratorType, IteratorType>(previous, circ->outerPixel());
                }
                counter = 0;
            }
            ++counter;
            previous = circ->outerPixel();
        } while (*circ != *end);

        if (entryPointList.empty()) {
            return interPointList;
        }

        for (typename std::vector<EntryPointContainer>::iterator i = entryPointList.begin();
             i != entryPointList.end();
             ++i) {
            if (max == NULL) {
                max = &(*i);
                continue;
            }
            if (i->third > max->third) {
                max = &(*i);
            }
        }

        if (max != NULL) {
            delete circ;
            delete end;
            vigra::Diff2D dir = max->second - max->first;
            if (dir.x != 0) {
                if (max->second.x < max->first.x) {
                    max->second = max->first;
                }

                circ = new Circulator(max->second, vigra::FourNeighborCode::West);
                end = new Circulator(max->second, vigra::FourNeighborCode::West);

            } else if (dir.y != 0) {
                if (max->second.y < max->first.y) {
                    max->second = max->first;
                }

                circ = new Circulator(max->second, vigra::FourNeighborCode::North);
                end = new Circulator(max->second, vigra::FourNeighborCode::North);
            }
        } else {
            return interPointList;
        }

        while (circ != end) {
            (*circ)++;
            intermediatePoint = circ->outerPixel() - nft.upperLeft();
            if (!offTheBorder && !(nft.accessor()(circ->outerPixel()) == MaskPixelTraits::max() / 2)) {
                offTheBorder = true;
                const vigra::Point2D dualGraphPoint = vigra::Point2D(intermediatePoint * 2 - vigra::Diff2D(1, 1));
                if (intermediatePoint.x >= 0 &&
                    intermediatePoint.y >= 0 &&
                    intermediatePoint.x < iBB.lowerRight().x &&
                    intermediatePoint.y < iBB.lowerRight().y) {
                    interPointList->push_back(dualGraphPoint);
                }

#ifdef DEBUG_GRAPHCUT
                std::cout << "Start point: " << dualGraphPoint << std::endl;
#endif
            }

            if (offTheBorder) {
                if (!inOverlap && overlap[intermediatePoint] == MaskPixelTraits::max()) {
                    inOverlap = true;
                    const vigra::Point2D dualGraphPoint = vigra::Point2D(intermediatePoint * 2 - vigra::Diff2D(1, 1));
                    if (!interPointList->empty() &&
                        *(interPointList->begin()) != dualGraphPoint &&
                        intermediatePoint.x >= 0 &&
                        intermediatePoint.y >= 0) {
                        interPointList->push_back(dualGraphPoint);
                    }

#ifdef DEBUG_GRAPHCUT
                    std::cout << "Entering overlap: " << dualGraphPoint << std::endl;
#endif
                }

                if (inOverlap &&
                    overlap[intermediatePoint] == MaskPixelTraits::zero() &&
                    nft.accessor()(circ->outerPixel()) != MaskPixelTraits::max() / 2) {
                    inOverlap = false;
                    const vigra::Point2D dualGraphPoint = vigra::Point2D(intermediatePoint * 2 - vigra::Diff2D(1, 1));
                    if (intermediatePoint.x >= 0 && intermediatePoint.y >= 0) {
                        interPointList->push_back(dualGraphPoint);
                    }

#ifdef DEBUG_GRAPHCUT
                    std::cout << "Leaving overlap: " << dualGraphPoint << std::endl;
#endif
                }

                if (nft.accessor()(circ->outerPixel()) == MaskPixelTraits::max() / 2) {
                    const vigra::Point2D dualGraphPoint = vigra::Point2D((previous - nft.upperLeft()) * 2 - vigra::Diff2D(1, 1));
                    if (!interPointList->empty() &&
                        *(interPointList->rbegin()) != dualGraphPoint &&
                        intermediatePoint.x >= 0 &&
                        intermediatePoint.y >= 0) {
                        interPointList->push_back(dualGraphPoint);
                    }

#ifdef DEBUG_GRAPHCUT
                    std::cout << "Endpoint reached: " << dualGraphPoint << std::endl;
#endif
                    break;
                }
            }
            previous = circ->outerPixel();
        }

        delete circ;
        delete end;
        return interPointList;
    }


    template <typename ImageType>
    class CostComparer
    {
    public:
        CostComparer(const ImageType* image) : img(image) {}

        bool operator()(const vigra::Point2D& a, const vigra::Point2D& b) const
        {
            if (a == vigra::Point2D(-20, -20)) {
                return totalScore > (*img)[b];
            } else if (b == vigra::Point2D(-20, -20)) {
                return (*img)[a] > totalScore;
            }

            return (*img)[a] > (*img)[b];
        }

        void setTotalScore(long s)
        {
            totalScore = s;
        }

    protected:
        const ImageType* img;
        long totalScore;
    };


    struct OutputLabelingFunctor
    {
    public:
        OutputLabelingFunctor(boost::unordered_set<vigra::Point2D, pointHash>* a_,
                              boost::unordered_set<vigra::Point2D, pointHash>* b_,
                              vigra::Point2D offset_) :
            left(a_), right(b_), offset(offset_) {}

        bool operator()(vigra::Diff2D a2, vigra::Diff2D b2)
        {
            vigra::Point2D a(a2);
            vigra::Point2D b(b2);
            //add border to detect seams close to border
            a -= vigra::Point2D(1,1);
            b -= vigra::Point2D(1,1);
            //a-= offset; b-= offset;
            return !((left->find(a) != left->end() && right->find(b) != right->end()) ||
                     (right->find(a) != right->end() && left->find(b) != left->end()));
        }

    protected:
        boost::unordered_set<vigra::Point2D, pointHash>* left;
        boost::unordered_set<vigra::Point2D, pointHash>* right;
        vigra::Point2D offset;
    };

    template<class MaskPixelType>
    struct CutPixelsFunctor
    {
    public:
        CutPixelsFunctor(boost::unordered_set<vigra::Point2D, pointHash>* a_,
                              boost::unordered_set<vigra::Point2D, pointHash>* b_) :
            left(a_), right(b_){}

        MaskPixelType operator()(const vigra::Diff2D& pos2, const MaskPixelType& a2) const
        {
            vigra::Point2D pos(pos2);
            if(left->find(pos) != left->end() && right->find(pos) != right->end())
                return 164;
            else if (left->find(*pos) != left->end())
                return 64;
            else if (right->find(*pos) != right->end())
                return 255;
            else return 0;
        }

    protected:
        boost::unordered_set<vigra::Point2D, pointHash>* left;
        boost::unordered_set<vigra::Point2D, pointHash>* right;
    };

    template<class MaskPixelType>
    struct CountFunctor
    {
    public:
        CountFunctor(int* c, int* c2) : color(c), count(c2) {}

        MaskPixelType operator()(const MaskPixelType& arg1, const MaskPixelType& arg2) const
        {
            if (arg1 > 0 && arg1 == arg2) {
                (*color)++;
            }
            if (arg1 == 255) {
                (*count)++;
            }
            return arg1;
        }

        int* color;
        int* count;
    };


    template <class SrcImageIterator, class SrcAccessor, class DestImageIterator,
              class DestAccessor, class MaskImageIterator, class MaskAccessor>
    inline void
    graphCut(vigra::triple<SrcImageIterator, SrcImageIterator, SrcAccessor> src1,
             vigra::pair<SrcImageIterator, SrcAccessor> src2,
             vigra::pair<DestImageIterator, DestAccessor> dest,
             vigra::triple<MaskImageIterator, MaskImageIterator, MaskAccessor> mask1,
             vigra::pair<MaskImageIterator, MaskAccessor> mask2,
             nearest_neighbor_metric_t norm, boundary_t boundary, const vigra::Rect2D& iBB)
    {
        graphCut(src1.first, src1.second, src1.third,
                 src2.first, src2.second,
                 dest.first, dest.second,
                 mask1.first, mask1.second, mask1.third,
                 mask2.first, mask2.second,
                 norm, boundary, iBB);
    }


    void getNeighbourList(vigra::Point2D src, vigra::Point2D* list,
                          vigra::Diff2D bounds, CheckpointPixels* srcDestPoints)
    {
        // return neighbour points from top to left in clockwise order
        bool check = false;
        if (src.y == 1) {
            list[0] = vigra::Point2D(-1, -1);
            check = true;
        } else {
            list[0] = src(0, -2);
        }

        if (src.x == bounds.x - 1) {
            list[1] = vigra::Point2D(-1, -1);
            check = true;
        } else {
            list[1] = src(2, 0);
        }

        if (src.y == bounds.y - 1) {
            list[2] = vigra::Point2D(-1, -1);
            check = true;
        } else {
            list[2] = src(0, 2);
        }

        if (src.x == 1) {
            list[3] = vigra::Point2D(-1, -1);
            check = true;
        } else {
            list[3] = src(-2, 0);
        }

        if (srcDestPoints->bottom.find(src) != srcDestPoints->bottom.end()) {
            list[0] = vigra::Point2D(-20, -20);
            list[1] = vigra::Point2D(-20, -20);
            list[2] = vigra::Point2D(-20, -20);
            list[3] = vigra::Point2D(-20, -20);
        }

        if (check) {
            if (srcDestPoints->bottom.find(src) != srcDestPoints->bottom.end() ||
                srcDestPoints->bottom.find(src(1, 0)) != srcDestPoints->bottom.end() ||
                srcDestPoints->bottom.find(src(1, 1)) != srcDestPoints->bottom.end() ||
                srcDestPoints->bottom.find(src(0, 1)) != srcDestPoints->bottom.end()) {
                if (list[1] == vigra::Point2D(-1, -1)) {
                    list[1] = vigra::Point2D(-20, -20);
                } else if (list[2] == vigra::Point2D(-1, -1)) {
                    list[2] = vigra::Point2D(-20, -20);
                } else if (list[3] == vigra::Point2D(-1, -1)) {
                    list[3] = vigra::Point2D(-20, -20);
                }
            }
        }
    }


    void
    getNeighbourList(CheckpointPixels* srcDestPoints,
                     boost::unordered_set<vigra::Point2D, pointHash>::iterator* auxList1,
                     boost::unordered_set<vigra::Point2D, pointHash>::iterator* auxList2)
    {
        *auxList1 = srcDestPoints->top.begin();
        *auxList2 = srcDestPoints->top.end();
    }


    template <class ImageType>
    std::vector<vigra::Point2D>* tracePath(vigra::Point2D pt, ImageType* img, CheckpointPixels* srcDestPoints)
    {
        std::vector<vigra::Point2D>* vec = new std::vector<vigra::Point2D>;
        vigra::Point2D current = pt;
        vec->push_back(pt);
        do {
            switch ((*img)[current(1, 1)] & BIT_MASK_DIR) {
            case 0:
                vec->push_back(current(0, -2));
                break;
            case 1:
                vec->push_back(current(2, 0));
                break;
            case 2:
                vec->push_back(current(0, 2));
                break;
            case 3:
                vec->push_back(current(-2, 0));
                break;
            default:
#ifdef DEBUG_GRAPHCUT
                std::cout << "path tracing error" << std::endl;
#endif
                break;
            }
            current = vec->back();
        } while (srcDestPoints->top.find(current) == srcDestPoints->top.end());

        return vec;
    }


    template <class ImageType>
    unsigned int getEdgeWeight(int dir, vigra::Point2D pt, ImageType* img, bool endpt, vigra::Diff2D bounds)
    {
        if (!endpt) {
            switch (dir) {
            case 0:
                return (*img)[pt(1, 0)];
                break;
            case 1:
                return (*img)[pt(2, 1)];
                break;
            case 2:
                return (*img)[pt(1, 2)];
                break;
            case 3:
                return (*img)[pt(0, 1)];
                break;
            }
        } else {
            if (pt.y == 1) {
                return (*img)[pt(1, 0)];
            } else if (pt.x == bounds.x - 2) {
                return (*img)[pt(2, 1)];
            } else if (pt.y == bounds.y - 2) {
                return (*img)[pt(1, 2)];
            } else if (pt.x == 1) {
                return (*img)[pt(0, 1)];
            }
        }

        return 0;
    }


    template <class ImageType, class GradientImageType, class MaskPixelType>
    std::vector<vigra::Point2D>*
    A_star(vigra::Point2D srcpt, vigra::Point2D destpt, ImageType* img,
           GradientImageType* gradientX, GradientImageType* gradientY,
           vigra::Diff2D bounds, CheckpointPixels* srcDestPoints, boost::unordered_set<vigra::Point2D, pointHash>* visited)
    {
        MaskPixelType zeroVal = vigra::NumericTraits<MaskPixelType>::zero();
        typedef std::priority_queue<vigra::Point2D, std::vector<vigra::Point2D>, CostComparer<ImageType> > Queue;
        CostComparer<ImageType> costcomp(img);
        Queue* openset = new Queue(costcomp);
        long score = 0;
        long totalScore = 0;
        long iterCount = 0;
        int gradientA;
        int gradientB;
        bool scoreIsBetter;
        bool pushToList;
        bool destOpen = false;
        vigra::Point2D list[4];
        vigra::Point2D current;
        vigra::Point2D neighbour;
        vigra::Point2D destNeighbour;
        boost::unordered_set<vigra::Point2D, pointHash>::iterator auxListBegin;
        boost::unordered_set<vigra::Point2D, pointHash>::iterator auxListEnd;
        openset->push(srcpt);

        while (!openset->empty()) {
            current = openset->top();
            openset->pop();
            iterCount++;
            if (current == destpt) {
#ifdef DEBUG_GRAPHCUT
                std::cout << "Graphcut completed after visiting " << iterCount << " nodes" << std::endl;
#endif
                delete openset;
                return tracePath<ImageType>(destNeighbour, img, srcDestPoints);
            }

            if (current == vigra::Point2D(-10, -10)) {
                getNeighbourList(srcDestPoints, &auxListBegin, &auxListEnd);
            } else {
                getNeighbourList(current, list, bounds, srcDestPoints);
            }

            if (current != vigra::Point2D(-10, -10)) {
                for (int i = 0; i < 4; i++) {
                    score = 0;
                    scoreIsBetter = false;
                    pushToList = false;
                    neighbour = list[i];

                    //visited during an earlier sub-cut, ignore

                    if(visited->find(neighbour) != visited->end()) {
                        continue;
                    }

                    if (neighbour == vigra::Point2D(-20, -20) ||
                        (neighbour != vigra::Point2D(-1, -1) &&
                         neighbour != vigra::Point2D(-10, -10) &&
                         (*img)[neighbour(1, 1)] == zeroVal)) {
                        if (neighbour == vigra::Point2D(-20, -20)) {
                            score = (*img)[current] + getEdgeWeight(i, current, img, true, bounds);
                        } else {
                            if (i % 2 == 0) {
                                gradientA = std::abs((*gradientY)[(current) / 2]);
                                gradientB = std::abs((*gradientY)[(neighbour) / 2]);
                            } else {
                                gradientA = std::abs((*gradientX)[(current) / 2]);
                                gradientB = std::abs((*gradientX)[(neighbour) / 2]);
                            }

                            if (gradientA + gradientB > 0) {
                                score =
                                    (*img)[current] +
                                    getEdgeWeight(i, current, img, false, bounds) * (gradientA + gradientB);
                            } else {
                                score = (*img)[current] + getEdgeWeight(i, current, img, false, bounds);
                            }
                        }

                        if (neighbour == vigra::Point2D(-20, -20) && !destOpen) {
                            pushToList = true;
                            destOpen = true;
                            scoreIsBetter = true;
                        } else if (neighbour != vigra::Point2D(-20, -20) &&
                                   ((*img)[neighbour(1, 1)] & BIT_MASK_OPEN) == 0) {
                            pushToList = true;
                            (*img)[neighbour(1, 1)] += BIT_MASK_OPEN;
                            scoreIsBetter = true;
                        } else if ((neighbour == vigra::Point2D(-20, -20) && score < totalScore) ||
                                   (neighbour != vigra::Point2D(-20, -20) && score < (*img)[neighbour])) {
                            scoreIsBetter = true;
                        }

                        if (scoreIsBetter) {
                            if (neighbour == vigra::Point2D(-20, -20)) {
                                totalScore = score;
                                destNeighbour = current;
                                costcomp.setTotalScore(totalScore);
                            } else {
                                (*img)[neighbour(1, 1)] &= BIT_MASK_OPEN;
                                (*img)[neighbour(1, 1)] += i;
                                (*img)[neighbour(1, 1)] ^= BIT_MASK_OPDIR;
                                (*img)[neighbour] = score;
                            }

                            if (pushToList) {
                                openset->push(neighbour);
                            }
                        }
                    }
                }
            } else {
                for (boost::unordered_set<vigra::Point2D, pointHash>::iterator x = auxListBegin; x != auxListEnd; ++x) {
                    score = 0;
                    scoreIsBetter = false;
                    pushToList = false;
                    neighbour = *x;
                    if (neighbour != vigra::Point2D(-1, -1) && (*img)[neighbour(1, 1)] == zeroVal) {
                        score = 0;
                        if (((*img)[neighbour(1, 1)] & BIT_MASK_OPEN) == 0) {
                            pushToList = true;
                            (*img)[neighbour(1, 1)] += BIT_MASK_OPEN;
                            scoreIsBetter = true;
                        } else if (score < (*img)[neighbour]) {
                            scoreIsBetter = true;
                        }

                        if (scoreIsBetter) {
                            (*img)[neighbour(1, 1)] &= BIT_MASK_OPEN;
                            (*img)[neighbour] = score;
                            if (pushToList) {
                                openset->push(neighbour);
                            }
                        }
                    }
                }
            }
        }
#ifdef DEBUG_GRAPHCUT
        std::cout << "Graphcut failed after visiting " << iterCount << " nodes" << std::endl;
#endif
        delete openset;
        return new std::vector<vigra::Point2D>();
    }


    vigra::Point2D convertFromDual(const vigra::Point2D& dualPixel)
    {
        const int stride = 2;
        return (vigra::Point2D((dualPixel->x - 1) / stride, (dualPixel->y - 1) / stride));
    }


    void dividePath(std::vector<vigra::Point2D>* cut,
                    boost::unordered_set<vigra::Point2D, pointHash>* left,
                    boost::unordered_set<vigra::Point2D, pointHash>* right,
                    const vigra::Rect2D& iBB)
    {
        vigra::Point2D previous;
        vigra::Point2D current;
        vigra::Point2D next;
        vigra::Point2D endIterPoint;
        unsigned short closest = 0;
        vigra::Diff2D dim(iBB.lowerRight() - iBB.upperLeft());
        std::vector<vigra::Point2D>::iterator previousDual;
        std::vector<vigra::Point2D>::iterator nextDual;

        for (std::vector<vigra::Point2D>::iterator currentDual = cut->begin(), nextDual = cut->begin();
             currentDual != cut->end();
             ++currentDual) {

            current = convertFromDual(*currentDual);
            next = convertFromDual(*nextDual);

            if (currentDual == cut->begin()) {
                ++nextDual;
                next = convertFromDual(*nextDual);
                // find closest edge
                int dist;
                const vigra::Diff2D toLowerRight = dim - current;
                closest = 0;
                dist = current.y;
                if (toLowerRight.x < dist) {
                    closest = 1;
                    dist = toLowerRight.x;
                }

                if (toLowerRight.y < dist) {
                    closest = 2;
                    dist = toLowerRight.y;
                }

                if (current.x < dist) {
                    closest = 3;
                }

                switch (closest) {
                case 0:
                    right->insert(current(0, 0));
                    left->insert(current(1, 0));
                    break;
                case 1:
                    left->insert(current(1, 1));
                    right->insert(current(1, 0));
                    break;
                case 2:
                    left->insert(current(0, 1));
                    right->insert(current(1, 1));
                    break;
                case 3:
                    right->insert(current(0, 1));
                    left->insert(current(0, 0));
                    break;
                default:
#ifdef DEBUG_GRAPHCUT
                    std::cout << "path dividing error" << std::endl;
#endif
                    break;
                }

                int previousDir;

                if (next.x == current.x) {
                    if (next.y < current.y) {
                        previousDir = 0;
                    } else {
                        previousDir = 2;
                    }
                } else {
                    if (next.x > current.x) {
                        previousDir = 1;
                    } else {
                        previousDir = 3;
                    }
                }

                switch (closest) {
                case 0:
                    switch (previousDir) {
                    case 2:
                        right->insert(current(0, 1));
                        left->insert(current(1, 1));
                        break;
                    case 1:
                        right->insert(current(0, 1));
                        right->insert(current(1, 1));
                        break;
                    case 3:
                        left->insert(current(0, 1));
                        left->insert(current(1, 1));
                        break;
                    case 0:
                        left->insert(current(0, 0));
                        right->insert(current(1, 0));
                        break;
                    default:
#ifdef DEBUG_GRAPHCUT
                        std::cout << "path dividing error" << std::endl;
#endif
                        break;
                    }
                    break;

                case 1:
                    switch (previousDir) {
                    case 0:
                        left->insert(current(0, 0));
                        left->insert(current(0, 1));
                        break;
                    case 2:
                        right->insert(current(0, 0));
                        right->insert(current(0, 1));
                        break;
                    case 3:
                        right->insert(current(0, 0));
                        left->insert(current(0, 1));
                        break;
                    case 1:
                        right->insert(current(1, 1));
                        left->insert(current(1, 0));
                        break;
                    default:
#ifdef DEBUG_GRAPHCUT
                        std::cout << "path dividing error" << std::endl;
#endif
                        break;
                    }
                    break;

                case 2:
                    switch (previousDir) {
                    case 0:
                        left->insert(current);
                        right->insert(current(1, 0));
                        break;
                    case 1:
                        left->insert(current);
                        left->insert(current(1, 0));
                        break;
                    case 3:
                        right->insert(current);
                        right->insert(current(1, 0));
                        break;
                    case 2:
                        left->insert(current(1, 1));
                        right->insert(current(0, 1));
                        break;
                    default:
#ifdef DEBUG_GRAPHCUT
                        std::cout << "path dividing error" << std::endl;
#endif
                        break;
                    }
                    break;

                case 3:
                    switch (previousDir) {
                    case 2:
                        left->insert(current(1, 0));
                        left->insert(current(1, 1));
                        break;
                    case 0:
                        right->insert(current(1, 0));
                        right->insert(current(1, 1));
                        break;
                    case 1:
                        right->insert(current(1, 1));
                        left->insert(current(1, 0));
                        break;
                    case 3:
                        right->insert(current(0, 0));
                        left->insert(current(0, 1));
                        break;
                    default:
#ifdef DEBUG_GRAPHCUT
                        std::cout << "path dividing error" << std::endl;
#endif
                        break;
                    }
                    break;
                }

                switch (closest) {
                case 0:
                    endIterPoint = current;
                    while (endIterPoint.y + iBB.upperLeft().y > -2) {
                        endIterPoint.y--;
                        left->insert(endIterPoint(1, 0));
                        right->insert(endIterPoint);
                    }
                    break;
                case 1:
                    endIterPoint = current;
                    while (endIterPoint.x < iBB.lowerRight().x + 2) {
                        endIterPoint.x++;
                        left->insert(endIterPoint(0, 1));
                        right->insert(endIterPoint);
                    }
                    break;
                case 2:
                    endIterPoint = current;
                    while (endIterPoint.y < iBB.lowerRight().y + 2) {
                        endIterPoint.y++;
                        left->insert(endIterPoint);
                        right->insert(endIterPoint(1, 0));
                    }
                    break;
                case 3:
                    endIterPoint = current;
                    while (endIterPoint.x > -2) {
                        endIterPoint.x--;
                        left->insert(endIterPoint);
                        right->insert(endIterPoint(0, 1));
                    }
                    break;
                default:
#ifdef DEBUG_GRAPHCUT
                    std::cout << "path dividing error" << std::endl;
#endif
                    break;

                }
            } else {
                int previousDir;
                int currentDir;

                if (previous.x == current.x) {
                    if (previous.y > current.y) {
                        previousDir = 0;
                    } else {
                        previousDir = 2;
                    }
                } else {
                    if (previous.x < current.x) {
                        previousDir = 1;
                    } else {
                        previousDir = 3;
                    }
                }

                previousDir = previousDir << 2;

                if (nextDual == cut->end()) {
                    int dist;
                    const vigra::Diff2D toLowerRight = dim - current;
                    closest = 0;
                    dist = current.y;
                    if (toLowerRight.x < dist) {
                        closest = 1;
                        dist = toLowerRight.x;
                    }

                    if (toLowerRight.y < dist) {
                        closest = 2;
                        dist = toLowerRight.y;
                    }

                    if (current.x < dist) {
                        closest = 3;
                    }

                    currentDir = closest;
                } else if (next.x == current.x) {
                    if (next.y < current.y) {
                        currentDir = 0;
                    } else {
                        currentDir = 2;
                    }
                } else {
                    if (next.x > current.x) {
                        currentDir = 1;
                    } else {
                        currentDir = 3;
                    }
                }

                switch (previousDir + currentDir) {
                case 0:         // top -> top
                    left->insert(current);
                    right->insert(current(1, 0));
                    left->insert(current(0, 1));
                    right->insert(current(1, 1));
                    break;
                case 1:         // top -> right
                    left->insert(current);
                    left->insert(current(1, 0));
                    left->insert(current(0, 1));
                    right->insert(current(1, 1));
                    break;
                case 3:         // top -> left
                    right->insert(current);
                    right->insert(current(1, 0));
                    left->insert(current(0, 1));
                    right->insert(current(1, 1));
                    break;
                case 4:         // right->top
                    left->insert(current);
                    right->insert(current(1, 0));
                    right->insert(current(0, 1));
                    right->insert(current(1, 1));
                    break;
                case 5:         // right->right
                    left->insert(current);
                    left->insert(current(1, 0));
                    right->insert(current(0, 1));
                    right->insert(current(1, 1));
                    break;
                case 6:         // right->bottom
                    left->insert(current);
                    left->insert(current(1, 0));
                    right->insert(current(0, 1));
                    left->insert(current(1, 1));
                    break;
                case 9:         // bottom->right
                    right->insert(current);
                    left->insert(current(1, 0));
                    right->insert(current(0, 1));
                    right->insert(current(1, 1));
                    break;
                case 10:        // bottom->bottom
                    right->insert(current);
                    left->insert(current(1, 0));
                    right->insert(current(0, 1));
                    left->insert(current(1, 1));
                    break;
                case 11:        // bottom->left
                    right->insert(current);
                    left->insert(current(1, 0));
                    left->insert(current(0, 1));
                    left->insert(current(1, 1));
                    break;
                case 12:        // left->top
                    left->insert(current);
                    right->insert(current(1, 0));
                    left->insert(current(0, 1));
                    left->insert(current(1, 1));
                    break;
                case 14:        // left->bottom
                    right->insert(current);
                    right->insert(current(1, 0));
                    right->insert(current(0, 1));
                    left->insert(current(1, 1));
                    break;
                case 15:        // left->left
                    right->insert(current);
                    right->insert(current(1, 0));
                    left->insert(current(0, 1));
                    left->insert(current(1, 1));
                    break;
                case 2:         // top -> bottom
                case 7:         // right->left
                case 8:         // bottom->top
                case 13:        // left->right
                default:
#ifdef DEBUG_GRAPHCUT
                    std::cout << "path dividing error: two-point loop" << std::endl;
                    std::cout << current << std::endl;
#endif
                    break;
                }

                if (*currentDual == cut->back()) {
                    switch (closest) {
                    case 2:
                        endIterPoint = current;
                        switch (currentDir) {
                        case 1:
                            left->insert(current);
                            left->insert(current(1, 0));
                            right->insert(current(0, 1));
                            left->insert(current(1, 1));
                            break;
                        case 3:
                            right->insert(current);
                            right->insert(current(1, 0));
                            right->insert(current(0, 1));
                            left->insert(current(1, 1));
                            break;
                        }
                        while (endIterPoint.y < iBB.lowerRight().y + 2) {
                            endIterPoint.y++;
                            left->insert(endIterPoint(1, 0));
                            right->insert(endIterPoint);
                        }
                        break;
                    case 3:
                        endIterPoint = current;
                        switch (currentDir) {
                        case 0:
                            right->insert(current);
                            right->insert(current(1, 0));
                            left->insert(current(0, 1));
                            right->insert(current(1, 1));
                            break;
                        case 2:
                            right->insert(current);
                            left->insert(current(1, 0));
                            left->insert(current(0, 1));
                            left->insert(current(1, 1));
                            break;
                        }

                        while (endIterPoint.x > -2) {
                            endIterPoint.x--;
                            left->insert(endIterPoint(0, 1));
                            right->insert(endIterPoint);
                        }
                        break;
                    case 0:
                        endIterPoint = current;
                        switch (currentDir) {
                        case 1:
                            left->insert(current);
                            right->insert(current(1, 0));
                            right->insert(current(0, 1));
                            right->insert(current(1, 1));
                            break;
                        case 3:
                            left->insert(current);
                            right->insert(current(1, 0));
                            left->insert(current(0, 1));
                            left->insert(current(1, 1));
                            break;
                        }
                        while (endIterPoint.y + iBB.upperLeft().y > -2) {
                            endIterPoint.y--;
                            left->insert(endIterPoint);
                            right->insert(endIterPoint(1, 0));
                        }
                        break;
                    case 1:
                        endIterPoint = current;
                        switch (currentDir) {
                        case 0:
                            left->insert(current);
                            left->insert(current(1, 0));
                            left->insert(current(0, 1));
                            right->insert(current(1, 1));
                            break;
                        case 2:
                            right->insert(current);
                            left->insert(current(1, 0));
                            right->insert(current(0, 1));
                            right->insert(current(1, 1));
                            break;
                        }
                        while (endIterPoint.x < iBB.lowerRight().x + 2) {
                            endIterPoint.x++;
                            left->insert(endIterPoint);
                            right->insert(endIterPoint(0, 1));
                        }
                        break;
                    default:
#ifdef DEBUG_GRAPHCUT
                        std::cout << "path dividing error" << std::endl;
#endif
                        break;
                    }
                }
            }

            if (nextDual != cut->end()) {
                ++nextDual;
            }

            previous = current;
            previousDual = currentDual;
        }
    }


    template <class DestImageIterator, class DestAccessor,
              class MaskImageIterator, class MaskAccessor, class MaskPixelType>
    void
    processCutResults(MaskImageIterator mask1_upperleft, MaskImageIterator mask1_lowerright, MaskAccessor ma1,
                      MaskImageIterator mask2_upperleft, MaskAccessor ma2,
                      DestImageIterator dest_upperleft, DestAccessor da,
                      std::vector<vigra::Point2D>& totalDualPath, const vigra::Rect2D& iBB)
    {
        const vigra::Diff2D size(iBB.lowerRight().x - iBB.upperLeft().x,
                          iBB.lowerRight().y - iBB.upperLeft().y);

        IMAGETYPE<MaskPixelType> finalmask(size + vigra::Diff2D(2, 2));
        IMAGETYPE<MaskPixelType> tempImg(size);

        typedef vigra::UInt8 BasePixelType;
        typedef vigra::NumericTraits<BasePixelType> BasePixelTraits;
        typedef vigra::NumericTraits<MaskPixelType> MaskPixelTraits;

        boost::unordered_set<vigra::Point2D, pointHash> pixelsLeftOfCut;
        boost::unordered_set<vigra::Point2D, pointHash> pixelsRightOfCut;

        dividePath(&totalDualPath, &pixelsLeftOfCut, &pixelsRightOfCut, iBB);

#ifdef DEBUG_GRAPHCUT
        combineTwoImagesMP(srcIterRange(Diff2D(), size),
                           vigra::srcIter(finalmask.upperLeft() + Diff2D(1, 1)),
                           vigra::destIter(tempImg.upperLeft()),
                           CutPixelsFunctor<MaskPixelType>(&pixelsLeftOfCut, &pixelsRightOfCut));
        exportImage(srcImageRange(tempImg), ImageExportInfo("./debug/cut_pixels.tif").setPixelType("UINT8"));
#endif

        // labels areas that belong to left/right images
        // adds a 1-pixel border to catch any area that is cut off by the seam
        vigra::labelImage(vigra::srcIterRange(vigra::Diff2D(), vigra::Diff2D() + size + vigra::Diff2D(2, 2)), destImage(finalmask),
                          false, OutputLabelingFunctor(&pixelsLeftOfCut, &pixelsRightOfCut, iBB.upperLeft()));

#ifdef DEBUG_GRAPHCUT
        exportImage(srcImageRange(finalmask), ImageExportInfo("./debug/labels.tif").setPixelType("UINT8"));
#endif

        int colorSum = 0;
        int count = 0;
        CountFunctor<MaskPixelType> c(&colorSum, &count);

        transformImage(vigra::srcIterRange(finalmask.upperLeft() + vigra::Diff2D(1, 1),
                                    finalmask.lowerRight() - vigra::Diff2D(1, 1)),
                       vigra::destIter(finalmask.upperLeft() + vigra::Diff2D(1, 1)),
                       ifThenElse(Arg1() == Param(1), Param(BasePixelTraits::max()),
                                  Param(BasePixelTraits::zero())));

        inspectTwoImages(vigra::srcIterRange(finalmask.upperLeft() + vigra::Diff2D(1, 1),
                                      finalmask.lowerRight() - vigra::Diff2D(1, 1)),
                         vigra::srcIter(dest_upperleft + iBB.upperLeft(), da), c);
#ifdef DEBUG_GRAPHCUT
        exportImage(srcImageRange(finalmask), ImageExportInfo("./debug/labels1.tif").setPixelType("UINT8"));
#endif

        // if colorSum < half the pixels labeled as "1" in finalmask should be black
        if (colorSum < count / 2) {
            transformImage(vigra::srcIterRange(finalmask.upperLeft() + vigra::Diff2D(1, 1),
                                        finalmask.lowerRight() - vigra::Diff2D(1, 1)),
                           vigra::destIter(finalmask.upperLeft() + vigra::Diff2D(1, 1)),
                           ifThenElse(Arg1() == Param(0), Param(BasePixelTraits::max()),
                                      Param(BasePixelTraits::zero())));
        } else {
            transformImage(vigra::srcIterRange(finalmask.upperLeft() + vigra::Diff2D(1, 1),
                                        finalmask.lowerRight() - vigra::Diff2D(1, 1)),
                           vigra::destIter(finalmask.upperLeft() + vigra::Diff2D(1, 1)),
                           ifThenElse(Arg1() == Param(0), Param(BasePixelTraits::zero()),
                                      Param(BasePixelTraits::max())));
        }

#ifdef DEBUG_GRAPHCUT
        exportImage(srcImageRange(finalmask), ImageExportInfo("./debug/labels2.tif").setPixelType("UINT8"));
#endif

        combineTwoImagesMP(vigra_ext::apply(iBB, vigra::srcIterRange(mask1_upperleft, mask1_lowerright, ma1)),
                           vigra_ext::apply(iBB, vigra::srcIter(mask2_upperleft, ma2)), destImage(tempImg),
                           ifThenElse(Arg1() && Arg2(),
                                      Param(MaskPixelTraits::max()),
                                      Param(MaskPixelTraits::zero())));

        copyImageIf(vigra::srcIterRange(finalmask.upperLeft() + vigra::Diff2D(1, 1),
                                 finalmask.lowerRight() - vigra::Diff2D(1, 1)), srcImage(tempImg),
                    vigra::destIter(dest_upperleft + iBB.upperLeft(), da));
    }


    /* Graph-cut
     * implementation of an algorithm based on an article by:
     * V.Kwatra, A.Schoedl, I.Essa, G.Turk, A.Bobick
     * "Graphcut Textures: Image and Video Synthesis Using Graph Cuts"
     *
     * The algorithm finds the best seam between two images using a graph-based approach.
     *
     * Min-cost pathfinding is based on using the initial image graph's dual graph
     */

    template <class SrcImageIterator, class SrcAccessor, class DestImageIterator,
              class DestAccessor, class MaskImageIterator, class MaskAccessor>
    void
    graphCut(SrcImageIterator src1_upperleft, SrcImageIterator src1_lowerright, SrcAccessor sa1,
             SrcImageIterator src2_upperleft, SrcAccessor sa2,
             DestImageIterator dest_upperleft, DestAccessor da,
             MaskImageIterator mask1_upperleft, MaskImageIterator mask1_lowerright, MaskAccessor ma1,
             MaskImageIterator mask2_upperleft, MaskAccessor ma2,
             nearest_neighbor_metric_t norm, boundary_t boundary, const vigra::Rect2D& iBB)
    {
        typedef typename SrcAccessor::value_type SrcPixelType;
        typedef typename MaskAccessor::value_type MaskPixelType;

        typedef vigra::UInt8 BasePixelType;
        typedef float GradientPixelType;
        typedef vigra::NumericTraits<BasePixelType> BasePixelTraits;
        typedef vigra::NumericTraits<GradientPixelType> GradientPixelTraits;
        typedef typename BasePixelTraits::Promote BasePromotePixelType;
        typedef vigra::NumericTraits<BasePromotePixelType> BasePromotePixelTraits;
        typedef typename BasePromotePixelTraits::Promote GraphPixelType;

        const vigra::Diff2D size(src1_lowerright.x - src1_upperleft.x,
                          src1_lowerright.y - src1_upperleft.y);
#ifdef DEBUG_GRAPHCUT
        const vigra::Diff2D masksize(mask1_lowerright.x - mask1_upperleft.x,
                              mask1_lowerright.y - mask1_upperleft.y);
#endif

        IMAGETYPE<BasePixelType> intermediateImg(size);
        IMAGETYPE<BasePromotePixelType> intermediateGraphImg(size + size + vigra::Diff2D(1, 1));
        IMAGETYPE<BasePromotePixelType> gradientPreConvolve(size);
        IMAGETYPE<GradientPixelType> gradientX(size);
        IMAGETYPE<GradientPixelType> gradientY(size);
        IMAGETYPE<GraphPixelType> graphImg(size + size + vigra::Diff2D(1, 1));

        std::vector<vigra::Point2D>* dualPath = NULL;
        std::vector<vigra::Point2D> totalDualPath;
        vigra::Point2D intermediatePoint;
        boost::scoped_ptr<CheckpointPixels> srcDestPoints(new CheckpointPixels());

        const vigra::Diff2D graphsize(graphImg.lowerRight().x - graphImg.upperLeft().x,
                               graphImg.lowerRight().y - graphImg.upperLeft().y);
        const vigra::Rect2D gBB(vigra::Point2D(1, 1), vigra::Size2D(graphsize));

        vigra::Kernel2D<BasePromotePixelType> edgeWeightKernel;

        edgeWeightKernel.initExplicitly(vigra::Diff2D(-1, -1), vigra::Diff2D(1, 1)) =  // upper left and lower right
            0, 1, 0,
            1, 1, 1,
            0, 1, 0;
        edgeWeightKernel.setBorderTreatment(vigra::BORDER_TREATMENT_CLIP);
        vigra::Kernel1D<float> gradientKernel;
        gradientKernel.initSymmetricGradient();

        // gradient images calculation
        transformImage(src1_upperleft, src1_lowerright, sa1,
                       gradientPreConvolve.upperLeft(), gradientPreConvolve.accessor(),
                       MapFunctor<SrcPixelType, BasePixelType>());
        transformImage(src2_upperleft, src2_upperleft + size, sa2,
                       intermediateImg.upperLeft(), intermediateImg.accessor(),
                       MapFunctor<SrcPixelType, BasePixelType>());
        combineTwoImagesMP(srcImageRange(gradientPreConvolve),
                           srcImage(intermediateImg),
                           destImage(gradientPreConvolve),
                           Arg1() + Arg2());

        // computing image gradient for a better cost function
        vigra::separableConvolveX(srcImageRange(gradientPreConvolve),
                                  destImage(gradientX),
                                  vigra::kernel1d(gradientKernel));
        vigra::separableConvolveY(srcImageRange(gradientPreConvolve),
                                  destImage(gradientY),
                                  vigra::kernel1d(gradientKernel));

        // difference image calculation
        switch (PixelDifferenceFunctor)
        {
        case HueLuminanceMaxDifference:
            combineTwoImagesMP(src1_upperleft, src1_lowerright, sa1,
                               src2_upperleft, sa2,
                               intermediateImg.upperLeft(), intermediateImg.accessor(),
                               MaxHueLuminanceDifferenceFunctor<SrcPixelType, BasePixelType>
                               (LuminanceDifferenceWeight, ChrominanceDifferenceWeight));
            break;
        case DeltaEDifference:
            combineTwoImagesMP(src1_upperleft, src1_lowerright, sa1,
                               src2_upperleft, sa2,
                               intermediateImg.upperLeft(), intermediateImg.accessor(),
                               DeltaEPixelDifferenceFunctor<SrcPixelType, BasePixelType>
                               (LuminanceDifferenceWeight, ChrominanceDifferenceWeight));
            break;
        default:
            throw never_reached("switch control expression \"PixelDifferenceFunctor\" out of range");
        }

        // masking overlap region borders
        combineThreeImagesMP(vigra_ext::apply(iBB, vigra::srcIterRange(mask1_upperleft, mask1_lowerright, ma1)),
                             vigra_ext::apply(iBB, vigra::srcIter(mask2_upperleft, ma2)),
                             vigra::srcIter(intermediateImg.upperLeft(), intermediateImg.accessor()),
                             vigra::destIter(intermediateImg.upperLeft(), intermediateImg.accessor()),
                             ifThenElse(!(Arg1() & Arg2()), Param(BasePixelTraits::max()), Arg3()));

        // look for possible start and end points
        std::vector<vigra::Point2D>* intermediatePointList =
            findIntermediatePoints<MaskImageIterator, MaskAccessor, BasePixelType>
            (mask1_upperleft, mask1_lowerright, ma1,
             mask2_upperleft, ma2, dest_upperleft, da,
             norm, boundary, iBB);

        // in case something goes wrong with start/end point finding, returns regular nft image
        if (intermediatePointList->empty()) {
            delete intermediatePointList;
            return;
        }

        // copying to a grid
        copyImage(srcImageRange(intermediateImg),
                  vigra_ext::stride(2, 2, vigra_ext::apply(gBB, destImage(intermediateGraphImg))));

        // calculating differences between pixels that are adjacent in the original image
        convolveImage(srcImageRange(intermediateGraphImg), destImage(graphImg), vigra::kernel2d(edgeWeightKernel));
        copyImage(srcImageRange(graphImg), destImage(intermediateGraphImg));

#ifdef DEBUG_GRAPHCUT
        exportImage(srcImageRange(intermediateImg), ImageExportInfo("./debug/diff.tif").setPixelType("UINT8"));
        exportImage(srcImageRange(intermediateGraphImg), ImageExportInfo("./debug/diff2.tif").setPixelType("UINT8"));
        exportImage(mask1_upperleft, mask1_lowerright, ma1, ImageExportInfo("./debug/mask1.tif").setPixelType("UINT8"));
        exportImage(mask2_upperleft, mask2_upperleft + masksize, ma2, ImageExportInfo("./debug/mask2.tif").setPixelType("UINT8"));
        exportImage(src1_upperleft, src1_lowerright, sa1, ImageExportInfo("./debug/src1.tif").setPixelType("UINT8"));
        exportImage(src2_upperleft, src2_upperleft + size, sa2, ImageExportInfo("./debug/src2.tif").setPixelType("UINT8"));
        exportImage(srcImageRange(gradientX), ImageExportInfo("./debug/gradx.tif").setPixelType("UINT8"));
        exportImage(srcImageRange(gradientY), ImageExportInfo("./debug/grady.tif").setPixelType("UINT8"));
        exportImage(srcImageRange(graphImg), ImageExportInfo("./debug/graph.tif").setPixelType("UINT8"));
#endif

        // a set of points to keep visited points for subsequent graph-cut runs
        boost::unordered_set<vigra::Point2D, pointHash> visited;

        // find optimal cuts in dual graph
        for (std::vector<vigra::Point2D>::iterator i = intermediatePointList->begin();
             i != intermediatePointList->end();
             ++i) {
            if (i == intermediatePointList->begin()) {
                intermediatePoint = *i;
                continue;
            }

            srcDestPoints->clear();
            srcDestPoints->top.insert(intermediatePoint);
            srcDestPoints->bottom.insert(*i);

#ifdef DEBUG_GRAPHCUT
            std::cout << "Running graph-cut: " << intermediatePoint << ":" << *i << std::endl;
#endif

            dualPath = A_star<IMAGETYPE<GraphPixelType>, IMAGETYPE<GradientPixelType>, BasePixelType>
                (vigra::Point2D(-10, -10), vigra::Point2D(-20, -20), &intermediateGraphImg, &gradientX,
                 &gradientY, graphsize - vigra::Diff2D(1, 1), srcDestPoints.get(), &visited);

            visited.insert(dualPath->begin(), dualPath->end());

            for (std::vector<vigra::Point2D>::reverse_iterator j = dualPath->rbegin(); j < dualPath->rend(); j++) {
                if ((j == dualPath->rbegin() && totalDualPath.empty()) || j != dualPath->rbegin()) {
                    totalDualPath.push_back(*j);
                }
            }

            copyImage(srcImageRange(graphImg), destImage(intermediateGraphImg));
            intermediatePoint = *i;
        }

        processCutResults<DestImageIterator, DestAccessor, MaskImageIterator, MaskAccessor, MaskPixelType>
            (mask1_upperleft, mask1_lowerright, ma1, mask2_upperleft, ma2,
             dest_upperleft, da, totalDualPath, iBB);

        delete intermediatePointList;
        delete dualPath;
    }

} /* namespace enblend */
#endif  /* GRAPHCUT_H */

// Local Variables:
// mode: c++
// End:
