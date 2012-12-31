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
#ifndef __PATH_H__
#define __PATH_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <queue>
#include <vector>


namespace enblend {

template <typename Point, typename Image>
class PathCompareFunctor
{
public:
    PathCompareFunctor(const Image* i) : image(i) {}

    bool operator()(const Point& a, const Point& b) const {
#ifdef DEBUG_PATH_COMPARE
        cout << "+ PathCompareFunctor::operator(): comparing "
             << "a = (" << a.x << ", " << a.y << ") = " << (*image)[a]
             << ", b = (" << b.x << ", " << b.y << ") = " << (*image)[b] << endl;
#endif
        // want the priority queue sorted in ascending order
        return (*image)[a] > (*image)[b];
    }

protected:
    const Image* image;
};


template <class CostImageIterator, class CostAccessor>
std::vector<vigra::Point2D>* minCostPath(CostImageIterator cost_upperleft,
                                         CostImageIterator cost_lowerright,
                                         CostAccessor ca,
                                         vigra::Point2D startingPoint,
                                         vigra::Point2D endingPoint)
{
    typedef typename CostAccessor::value_type CostPixelType;
    typedef typename vigra::NumericTraits<CostPixelType>::Promote WorkingPixelType;
    typedef vigra::BasicImage<WorkingPixelType> WorkingImageType;
    typedef typename WorkingImageType::traverser WorkingImageIterator;
    typedef std::priority_queue<vigra::Point2D, std::vector<vigra::Point2D>, PathCompareFunctor<vigra::Point2D, WorkingImageType> > PQ;

    const int w = cost_lowerright.x - cost_upperleft.x;
    const int h = cost_lowerright.y - cost_upperleft.y;

    // 4-bit direction encoding {up, down, left, right}
    // A  8  9
    // 2  0  1
    // 6  4  5
    //const unsigned char neighborArray[] = {0xA, 8, 9, 1, 5, 4, 6, 2};
    const vigra::UInt8 neighborArray[] = {0xA, 1, 6, 8, 5, 2, 9, 4};
    //const unsigned char neighborArrayInverse[] = {5, 4, 6, 2, 0xA, 8, 9, 1};
    const vigra::UInt8 neighborArrayInverse[] = {5, 2, 9, 4, 0xA, 1, 6, 8};

    vigra::UInt8Image* pathNextHop = new vigra::UInt8Image(w, h, vigra::UInt8(0));
    WorkingImageType* costSoFar = new WorkingImageType(w, h, vigra::NumericTraits<WorkingPixelType>::max());
    PQ* pq = new PQ(PathCompareFunctor<vigra::Point2D, WorkingImageType>(costSoFar));
    std::vector<vigra::Point2D>* result = new std::vector<vigra::Point2D>;

#ifdef DEBUG_PATH
    cout << "+ minCostPath: w = " << w << ", h = " << h << "\n"
         << "+ minCostPath: startingPoint = " << startingPoint
         << ", endingPoint = " << endingPoint << endl;
#endif

    (*costSoFar)[endingPoint] =
        std::max(vigra::NumericTraits<WorkingPixelType>::one(),
                 vigra::NumericTraits<CostPixelType>::toPromote(ca(cost_upperleft + endingPoint)));
    pq->push(endingPoint);

    while (!pq->empty()) {
        vigra::Point2D top = pq->top();
        pq->pop();
#ifdef DEBUG_PATH
        cout << "+ minCostPath: visiting point = " << top << endl;
#endif

        if (top != startingPoint) {
            WorkingPixelType costToTop = (*costSoFar)[top];
#ifdef DEBUG_PATH
            cout << "+ minCostPath: costToTop = " << costToTop << endl;
#endif

            // For each 8-neighbor of top with costSoFar == 0 do relax
            for (int i = 0; i < 8; i++) {
                // get the negihbor;
                vigra::UInt8 neighborDirection = neighborArray[i];
                vigra::Point2D neighborPoint = top;
                if (neighborDirection & 0x8) {--neighborPoint.y;}
                if (neighborDirection & 0x4) {++neighborPoint.y;}
                if (neighborDirection & 0x2) {--neighborPoint.x;}
                if (neighborDirection & 0x1) {++neighborPoint.x;}

                // Make sure neighbor is in valid region
                if (neighborPoint.y < 0 || neighborPoint.y >= h
                    || neighborPoint.x < 0 || neighborPoint.x >= w) {
                    continue;
                }
#ifdef DEBUG_PATH
                cout << "+ minCostPath: neighbor = " << neighborPoint << endl;
#endif

                // See if the neighbor has already been visited.
                // If neighbor has maximal cost, it has not been visited.
                // If so skip it.
                WorkingPixelType neighborPreviousCost = (*costSoFar)[neighborPoint];
#ifdef DEBUG_PATH
                cout << "+ minCostPath: neighborPreviousCost = " << neighborPreviousCost << endl;
#endif
                if (neighborPreviousCost != vigra::NumericTraits<WorkingPixelType>::max()) {
                    continue;
                }

                WorkingPixelType neighborCost =
                    std::max(vigra::NumericTraits<WorkingPixelType>::one(),
                             vigra::NumericTraits<CostPixelType>::toPromote(ca(cost_upperleft + neighborPoint)));
#ifdef DEBUG_PATH
                cout << "+ minCostPath: neighborCost = " << neighborCost << endl;
#endif
                if (neighborCost == vigra::NumericTraits<CostPixelType>::max()) {
                    neighborCost *= 65536; // Can't use << since neighborCost may be floating-point
                }

                if ((i & 1) == 0) {
                    // neighbor is diagonal
                    neighborCost = WorkingPixelType(static_cast<double>(neighborCost) * 1.4);
                }

                const WorkingPixelType newNeighborCost = neighborCost + costToTop;
                if (newNeighborCost < neighborPreviousCost) {
                    // We have found the shortest path to neighbor.
                    (*costSoFar)[neighborPoint] = newNeighborCost;
                    (*pathNextHop)[neighborPoint] = neighborArrayInverse[i];
                    pq->push(neighborPoint);
                }
            }
        } else {
            // If yes then follow back to beginning using pathNextHop
            // include neither start nor end point in result
            vigra::UInt8 nextHop = (*pathNextHop)[top];
            while (nextHop != 0) {
                if (nextHop & 0x8) {--top.y;}
                if (nextHop & 0x4) {++top.y;}
                if (nextHop & 0x2) {--top.x;}
                if (nextHop & 0x1) {++top.x;}
                nextHop = (*pathNextHop)[top];
                if (nextHop != 0) {
                    result->push_back(top);
                }
            }
            break;
        }
    }

    delete pathNextHop;
    delete costSoFar;
    delete pq;

    return result;
}


template <class CostImageIterator, class CostAccessor>
inline std::vector<vigra::Point2D>*
minCostPath(vigra::triple<CostImageIterator, CostImageIterator, CostAccessor> cost,
            vigra::Point2D startingPoint, vigra::Point2D endingPoint)
{
    return minCostPath(cost.first, cost.second, cost.third, startingPoint, endingPoint);
}

} // namespace enblend

#endif /* __PATH_H__ */

// Local Variables:
// mode: c++
// End:
