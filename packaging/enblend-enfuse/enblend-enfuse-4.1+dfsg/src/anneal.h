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

#ifndef __ANNEAL_H__
#define __ANNEAL_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <algorithm>
#include <vector>

#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>
#include <boost/lambda/construct.hpp>

#ifdef _WIN32
#include <cmath>
#else
#include <math.h>
#endif

#include <vigra/diff2d.hxx>
#include <vigra/iteratoradapter.hxx>

#include "masktypedefs.h"

#ifdef HAVE_LIBGLEW
#include "gpu.h"
#endif

using boost::lambda::bind;
using boost::lambda::_1;
using boost::lambda::delete_ptr;


namespace enblend {

template <typename CostImage, typename VisualizeImage>
class GDAConfiguration
{
public:
    typedef typename CostImage::PixelType CostImagePixelType;
    typedef typename vigra::NumericTraits<CostImagePixelType>::Promote CostImagePromoteType;

    GDAConfiguration(const CostImage* const d, Segment* v, VisualizeImage* const vi) :
        costImage(d), visualizeStateSpaceImage(vi) {
        kMax = 1;
        distanceWeight = 1.0;
        mismatchWeight = 1.0;

        const int costImageShortDimension = std::min(costImage->width(), costImage->height());
        // Determine state space of currentPoint
        const int stateSpaceWidth = costImageShortDimension / 3;

        vigra::Point2D previousPoint = v->back().second;
        for (Segment::iterator current = v->begin(); current != v->end();) {
            bool currentMoveable = current->first;
            vigra::Point2D currentPoint = current->second;
            ++current;
            vigra::Point2D nextPoint = current == v->end() ? v->begin()->second : current->second;

            mfEstimates.push_back(currentPoint);

            std::vector<vigra::Point2D>* stateSpace = new std::vector<vigra::Point2D>();
            pointStateSpaces.push_back(stateSpace);

            std::vector<int>* stateDistances = new std::vector<int>();
            pointStateDistances.push_back(stateDistances);

            if (currentMoveable) {
                // vp = std::vector from previousPoint to currentPoint
                vigra::Diff2D vp(currentPoint.x - previousPoint.x, currentPoint.y - previousPoint.y);
                // vn = std::vector from currentPoint to nextPoint
                vigra::Diff2D vn(nextPoint.x - currentPoint.x, nextPoint.y - currentPoint.y);
                // np = normal to vp
                vigra::Diff2D np(-vp.y, vp.x);
                // nn = normal to vn
                vigra::Diff2D nn(-vn.y, vn.x);

                // normal = normal vector at currentPoint
                // normal points to the left of vp and vn.
                vigra::Diff2D normal = np + nn;
                normal *= stateSpaceWidth / normal.magnitude();

                vigra::Diff2D leftPoint = currentPoint + normal;
                vigra::Diff2D rightPoint = currentPoint - normal;

                // Choose a reasonable number of state points between these extremes
                const int lineLength = std::max(std::abs(rightPoint.x - leftPoint.x),
                                                std::abs(rightPoint.y - leftPoint.y));
                const int spaceBetweenPoints =
                    static_cast<int>(ceil(lineLength / static_cast<double>(AnnealPara.kmax)));

                vigra::LineIterator<vigra::Diff2D> linePoint(currentPoint, leftPoint);
                for (int i = 0; i < (lineLength + 1) / 2; ++i, ++linePoint) {
                    // Stop searching along the line if we leave the
                    // cost image or enter a max-cost region.
                    if (!costImage->isInside(*linePoint)) {
                        break;
                    } else if ((*costImage)[*linePoint] == vigra::NumericTraits<CostImagePixelType>::max()) {
                        break;
                    } else if (i % spaceBetweenPoints == 0) {
                        stateSpace->push_back(vigra::Point2D(*linePoint));
                        stateDistances->push_back(std::max(std::abs(linePoint->x - currentPoint.x),
                                                           std::abs(linePoint->y - currentPoint.y)) / 2);
                        if (visualizeStateSpaceImage) {
                            (*visualizeStateSpaceImage)[*linePoint] = VISUALIZE_STATE_SPACE;
                        }
                    }
                }
                linePoint = vigra::LineIterator<vigra::Diff2D>(currentPoint, rightPoint);
                ++linePoint;
                for (int i = 1; i < 1 + lineLength / 2; ++i, ++linePoint) {
                    // Stop searching along the line if we leave the
                    // cost image or enter a max-cost region.
                    if (!costImage->isInside(*linePoint)) {
                        break;
                    } else if ((*costImage)[*linePoint] == vigra::NumericTraits<CostImagePixelType>::max()) {
                        break;
                    } else if (i % spaceBetweenPoints == 0) {
                        stateSpace->push_back(vigra::Point2D(*linePoint));
                        stateDistances->push_back(std::max(std::abs(linePoint->x - currentPoint.x),
                                                           std::abs(linePoint->y - currentPoint.y)) / 2);
                        if (visualizeStateSpaceImage) {
                            (*visualizeStateSpaceImage)[*linePoint] = VISUALIZE_STATE_SPACE;
                        }
                    }
                }
            }

            if (stateSpace->size() == 0) {
                stateSpace->push_back(currentPoint);
                stateDistances->push_back(0);
                if (visualizeStateSpaceImage && costImage->isInside(currentPoint)) {
                    (*visualizeStateSpaceImage)[currentPoint] = VISUALIZE_STATE_SPACE_INSIDE;
                }
            }

            const unsigned int localK = stateSpace->size();
            if (localK > AnnealPara.kmax) {
                cerr << command
                     << ": local k = " << localK << " > k_max = " << AnnealPara.kmax
                     << endl;
                exit(1);
            }

            kMax = std::max(kMax, localK);

            pointStateProbabilities.push_back(new std::vector<double>(localK, 1.0 / localK));

            convergedPoints.push_back(localK < 2);

            previousPoint = currentPoint;
        }

        tau = AnnealPara.tau;
        deltaEMax = AnnealPara.deltaEMax;
        deltaEMin = AnnealPara.deltaEMin;
        const double kmax = static_cast<double>(kMax);
        tInitial = ceil(deltaEMax / log(kmax / (kmax - 2.0)));
        tFinal = deltaEMin / log(kmax * kmax - kmax - 1.0);
    }

    ~GDAConfiguration() {
        std::for_each(pointStateSpaces.begin(), pointStateSpaces.end(), bind(delete_ptr(), _1));
        std::for_each(pointStateProbabilities.begin(), pointStateProbabilities.end(), bind(delete_ptr(), _1));
        std::for_each(pointStateDistances.begin(), pointStateDistances.end(), bind(delete_ptr(), _1));
    }

    void run() {
        int progressIndicator = 1;
        int numIterations = static_cast<int>(ceil(log(tFinal / tInitial) / log(tau)));
        int iterationCount = 0;
        int iterationsPerTick = (numIterations + 3) / 4;

#ifdef HAVE_LIBGLEW
        if (UseGPU) {
            configureGPUTextures(kMax, pointStateSpaces.size());
        }
#endif

        tCurrent = tInitial;

        while (kMax > 1 && tCurrent > tFinal) {
            const double epsilon = 1.0 / kMax;
            const unsigned int eta =
                static_cast<unsigned int>(ceil(log(epsilon)
                                               / log(((kMax - 2.0) / (2.0 * kMax) * exp(-tCurrent / deltaEMax))
                                                     + 0.5)));

            if (Verbose >= VERBOSE_GDA_MESSAGES) {
                const std::ios_base::fmtflags ioFlags(cerr.flags());
                cerr << "\n"
                     << command
                     << ": info: t = " << std::scientific << std::setprecision(3) << tCurrent
                     << ", eta = " << std::setw(4) << eta
                     << ", k_max = " << std::setw(3) << kMax;
                cerr.flush();
                cerr.flags(ioFlags);
            }

            for (unsigned int i = 0; i < eta; i++) {
                iterate();
            }

            tCurrent *= tau;

            if (Verbose >= VERBOSE_GDA_MESSAGES) {
                int numConvergedPoints = 0;
                for (unsigned int i = 0; i < convergedPoints.size(); i++) {
                    if (convergedPoints[i]) {
                        numConvergedPoints++;
                    }
                }
                cerr << ", " << numConvergedPoints
                     << " of " << convergedPoints.size()
                     << " points converged";
                cerr.flush();
            }
            else if (Verbose >= VERBOSE_MASK_MESSAGES && iterationCount % iterationsPerTick == 0) {
                cerr << " " << progressIndicator << "/4";
                progressIndicator++;
                cerr.flush();
            }

            iterationCount++;
        }

#ifdef HAVE_LIBGLEW
        if (UseGPU) {
            clearGPUTextures();
        }
#endif

        if (visualizeStateSpaceImage) {
            // Remaining unconverged state space points
            for (unsigned int i = 0; i < pointStateSpaces.size(); ++i) {
                std::vector<vigra::Point2D>* stateSpace = pointStateSpaces[i];
                for (unsigned int j = 0; j < stateSpace->size(); ++j) {
                    vigra::Point2D point = (*stateSpace)[j];
                    if (visualizeStateSpaceImage->isInside(point)) {
                        (*visualizeStateSpaceImage)[point] = VISUALIZE_STATE_SPACE_UNCONVERGED;
                    }
                }
            }
        }

        if (Verbose >= VERBOSE_GDA_MESSAGES) {
            cerr << endl;
            for (unsigned int i = 0; i < convergedPoints.size(); i++) {
                if (!convergedPoints[i]) {
                    cerr << command
                         << ": info: unconverged point: "
                         << endl;
                    std::vector<vigra::Point2D>* stateSpace = pointStateSpaces[i];
                    std::vector<double>* stateProbabilities = pointStateProbabilities[i];
                    const unsigned int localK = stateSpace->size();
                    for (unsigned int state = 0; state < localK; ++state) {
                        cerr << command
                             << ": info:    state " << (*stateSpace)[state]
                             << ", weight = " << (*stateProbabilities)[state]
                             << endl;
                    }
                    cerr << command
                         << ": info:    mfEstimate = " << mfEstimates[i]
                         << endl;
                }
            }
        }
    }

    const std::vector<vigra::Point2D>& getCurrentPoints() const {
        return mfEstimates;
    }

    void setOptimizerWeights(double aDistanceWeight, double aMismatchWeight) {
        // IMPLEMENTATION NOTE: We normalize to 2.0, because we
        // want to reproduce the results of Enblend-3.2.  Up to
        // Enblend-3.2 the distanceWeight and mismatchWeight were both
        // fixed at 1.0.
        const double sum = 0.5 * (aDistanceWeight + aMismatchWeight);
        assert(aDistanceWeight >= 0.0);
        assert(aMismatchWeight >= 0.0);
        assert(sum > 0.0);
        distanceWeight = aDistanceWeight / sum;
        mismatchWeight = aMismatchWeight / sum;
    }

protected:
    void calculateStateProbabilities() {
        const int mf_size = static_cast<int>(mfEstimates.size());

#ifdef OPENMP
#pragma omp parallel
#endif
        {
            int* E = new int[kMax];
            double* Pi = new double[kMax];

#ifdef OPENMP
#pragma omp for nowait schedule(guided)
#endif
            for (int index = 0; index < mf_size; ++index) {
                // Skip updating points that have already converged.
                if (convergedPoints[index]) {
                    continue;
                }

                const std::vector<vigra::Point2D>* stateSpace = pointStateSpaces[index];
                std::vector<double>* stateProbabilities = pointStateProbabilities[index];
                const std::vector<int>* stateDistances = pointStateDistances[index];
                const unsigned int localK = stateSpace->size();

                const int lastIndex = index == 0 ? mf_size - 1 : index - 1;
                const unsigned int nextIndex = (index + 1) % mf_size;
                const vigra::Point2D lastPointEstimate = mfEstimates[lastIndex];
                const bool lastPointInCostImage = costImage->isInside(lastPointEstimate);
                const vigra::Point2D nextPointEstimate = mfEstimates[nextIndex];
                const bool nextPointInCostImage = costImage->isInside(nextPointEstimate);

                // Calculate E values.
                // exp_a scaling factor is part of the Schraudolph approximation.
                // for all e_i, e_j, in E: -700 < e_j-e_i < 700
                const double exp_a = 1512775.0 / tCurrent; // = (1048576 / M_LN2) / tCurrent;
                for (unsigned int i = 0; i < localK; ++i) {
                    const vigra::Point2D currentPoint = (*stateSpace)[i];
                    const int distanceCost = (*stateDistances)[i];
                    int mismatchCost = 0;
                    if (lastPointInCostImage) {
                        mismatchCost += costImageCost(lastPointEstimate, currentPoint);
                    }
                    if (nextPointInCostImage) {
                        mismatchCost += costImageCost(currentPoint, nextPointEstimate);
                    }

                    const double cost =
                        distanceWeight * static_cast<double>(distanceCost) +
                        mismatchWeight * static_cast<double>(mismatchCost);
                    E[i] = vigra::NumericTraits<int>::fromRealPromote(cost * exp_a);
                    Pi[i] = 0.0;
                }

                // Calculate new stateProbabilities
                // An = 1 / (1 + exp((E[j] - E[i]) / tCurrent))
                // I am using an approximation of the exp function from:
                // Nicol N. Schraudolph. A Fast, Compact Approximation of the Exponential Function.
                // Neural Computation, vol. 11, pages 853--862, 1999.
                union {
                    double d;
#ifdef WORDS_BIGENDIAN
                    struct { int hi, lo; } n;
#else
                    struct { int lo, hi; } n;
#endif
                } eco;
                eco.n.lo = 0;

                // An = 1 / (1 + exp( (E[j] - E[i]) / T )
                // pi[j]' = 1/K * sum_(0)_(k-1) An(i,j) * (pi[i] + pi[j])
                for (unsigned int j = 0; j < localK; ++j) {
                    const double piTj = (*stateProbabilities)[j];
                    Pi[j] += piTj;
                    const int ej = E[j];
                    for (unsigned int i = j + 1; i < localK; ++i) {
                        const double piT = (*stateProbabilities)[i] + piTj;
                        eco.n.hi = (ej - E[i]) + (0x3ff00000 - 60801);
                        // FIXME eco.n.hi is overflowing into NaN range!
                        double piTAn = piT / (1.0 + eco.d);
                        if (
#if defined(_MSC_VER) || defined(__sun__)
                            isnan(piTAn)
#else
                            std::isnan(piTAn)
#endif
                            ) {
                            // exp term is infinity or zero.
                            piTAn = ej > E[i] ? 0.0 : piT;
                        }
                        Pi[j] += piTAn;
                        Pi[i] += piT - piTAn;
                    }
                    (*stateProbabilities)[j] = Pi[j] / localK;
                }
            }

            delete [] E;
            delete [] Pi;
        } // omp parallel
    }

#ifdef HAVE_LIBGLEW
    void calculateStateProbabilitiesGPU() {
        const unsigned int mf_size = mfEstimates.size();
        unsigned int unconvergedPoints = 0;

        float* EF = new float[kMax * mfEstimates.size()];
        float* PiF = new float[kMax * mfEstimates.size()];

        for (unsigned int index = 0; index < mf_size; ++index) {
            // Skip updating points that have already converged.
            if (convergedPoints[index]) {
                continue;
            }

            const unsigned int rowIndex = unconvergedPoints / 4;
            const unsigned int vectorIndex = unconvergedPoints % 4;
            float* EFbase = &(EF[(rowIndex * kMax * 4) + vectorIndex]);
            float* PiFbase = &(PiF[(rowIndex * kMax * 4) + vectorIndex]);

            const std::vector<vigra::Point2D>* stateSpace = pointStateSpaces[index];
            std::vector<double>* stateProbabilities = pointStateProbabilities[index];
            const std::vector<int>* stateDistances = pointStateDistances[index];
            const unsigned int localK = stateSpace->size();

            const unsigned int lastIndex = index == 0 ? mf_size - 1 : index - 1;
            const unsigned int nextIndex = (index + 1) % mf_size;
            const vigra::Point2D lastPointEstimate = mfEstimates[lastIndex];
            const bool lastPointInCostImage = costImage->isInside(lastPointEstimate);
            const vigra::Point2D nextPointEstimate = mfEstimates[nextIndex];
            const bool nextPointInCostImage = costImage->isInside(nextPointEstimate);

            // Calculate E values.
            for (unsigned int i = 0; i < localK; ++i) {
                const vigra::Point2D currentPoint = (*stateSpace)[i];
                const int distanceCost = (*stateDistances)[i];
                int mismatchCost = 0;
                if (lastPointInCostImage) {
                    mismatchCost += costImageCost(lastPointEstimate, currentPoint);
                }
                if (nextPointInCostImage) {
                    mismatchCost += costImageCost(currentPoint, nextPointEstimate);
                }
                const float cost =
                    distanceWeight * static_cast<float>(distanceCost) +
                    mismatchWeight * static_cast<float>(mismatchCost);
                EFbase[4 * i] = cost;
                PiFbase[4 * i] = static_cast<float>((*stateProbabilities)[i]);
            }

            for (unsigned int i = localK; i < kMax; ++i) {
                PiFbase[4 * i] = 0.0f;
            }

            unconvergedPoints++;
        }

        // Calculate all of the new PiF values on the GPU in parallel
        gpuGDAKernel(kMax, unconvergedPoints, tCurrent, EF, PiF, PiF);

        // Write the results back to pointStateProbabilities
        unconvergedPoints = 0;
        for (unsigned int index = 0; index < mf_size; ++index) {
            // Skip updating points that have already converged.
            if (convergedPoints[index]) {
                continue;
            }

            const unsigned int rowIndex = unconvergedPoints / 4;
            const unsigned int vectorIndex = unconvergedPoints % 4;
            float* PiFbase = &(PiF[(rowIndex * kMax * 4) + vectorIndex]);

            std::vector<double>* stateProbabilities = pointStateProbabilities[index];
            const unsigned int localK = stateProbabilities->size();

            for (unsigned int i = 0; i < localK; ++i) {
                (*stateProbabilities)[i] = static_cast<double>(PiFbase[4 * i]);
            }

            unconvergedPoints++;
        }

        delete [] EF;
        delete [] PiF;
    }
#endif

    void iterate() {
#ifdef HAVE_LIBGLEW
        if (UseGPU) {
            calculateStateProbabilitiesGPU();
        } else {
            calculateStateProbabilities();
        }
#else
        calculateStateProbabilities();
#endif

        kMax = 1;
        size_t kmax_local = 1;
#ifdef OPENMP
#pragma omp parallel firstprivate(kmax_local)
#endif
        {
#ifdef OPENMP
#pragma omp for nowait schedule(guided)
#endif
            for (int index = 0; index < static_cast<int>(pointStateSpaces.size()); ++index) {
                if (convergedPoints[index]) {
                    continue;
                }

                std::vector<vigra::Point2D>* stateSpace = pointStateSpaces[index];
                std::vector<double>* stateProbabilities = pointStateProbabilities[index];
                std::vector<int>* stateDistances = pointStateDistances[index];
                unsigned int localK = stateSpace->size();
                double estimateX = 0.0;
                double estimateY = 0.0;

                // Make new mean field estimates.
                double totalWeight = 0.0;
                bool hasHighWeightState = false;
                for (unsigned int k = 0; k < localK; ++k) {
                    const double weight = (*stateProbabilities)[k];
                    totalWeight += weight;
                    if (weight > 0.99) {
                        hasHighWeightState = true;
                    }
                    const vigra::Point2D state = (*stateSpace)[k];
                    estimateX += weight * static_cast<double>(state.x);
                    estimateY += weight * static_cast<double>(state.y);
                }
                estimateX /= totalWeight;
                estimateY /= totalWeight;

                vigra::Point2D newEstimate(vigra::NumericTraits<int>::fromRealPromote(estimateX),
                                           vigra::NumericTraits<int>::fromRealPromote(estimateY));

                // Sanity check
                if (!costImage->isInside(newEstimate)) {
#ifdef OPENMP
#pragma omp critical(write_to_cerr)
#endif
                    {
                        cerr << command
                             << ": warning: new mean field estimate outside cost image"
                             << endl;
                        for (unsigned int state = 0; state < localK; ++state) {
                            cerr << command
                                 << ": info:    state " << (*stateSpace)[state]
                                 << " weight = "
                                 << (*stateProbabilities)[state]
                                 << endl;
                        }
                        cerr << command
                             << ": info:    new estimate = " << newEstimate
                             << endl;
                    } // omp critical

                    // Skip this point from now on.
                    convergedPoints[index] = true;
                    continue;
                }

                mfEstimates[index] = newEstimate;

                // Remove improbable solutions from the search space
                double totalWeights = 0.0;
                const double cutoffWeight = hasHighWeightState ? 0.50 : 0.00001;
                for (unsigned int k = 0; k < stateSpace->size(); ) {
                    const double weight = (*stateProbabilities)[k];
                    if (weight < cutoffWeight) {
                        // Replace this state with last state
                        (*stateProbabilities)[k] = (*stateProbabilities)[stateProbabilities->size() - 1];
                        (*stateSpace)[k] = (*stateSpace)[stateSpace->size() - 1];
                        (*stateDistances)[k] = (*stateDistances)[stateDistances->size() - 1];

                        // Delete last state
                        stateProbabilities->pop_back();
                        stateSpace->pop_back();
                        stateDistances->pop_back();
                    } else {
                        totalWeights += weight;
                        ++k;
                    }
                }

                // Renormalize
                for (unsigned int k = 0; k < stateSpace->size(); ++k) {
                    (*stateProbabilities)[k] /= totalWeights;
                }

                localK = stateSpace->size();
                if (localK < 2) {
                    convergedPoints[index] = true;
                }

                kmax_local = std::max(kmax_local, stateProbabilities->size());
            }
#ifdef OPENMP
#pragma omp critical(update_kMax)
#endif
            kMax = std::max(kMax, static_cast<unsigned int>(kmax_local));
        } // omp parallel
    }

    int costImageCost(const vigra::Point2D& start_point, const vigra::Point2D& end_point) const {
        typedef typename CostImage::ConstIterator CostIterator;

        const int shortLineThreshold = 8; // We penalize lines below this limit.
        const CostIterator end(costImage->upperLeft() + end_point);

        vigra::LineIterator<CostIterator> lineEnd(end, end);
        vigra::LineIterator<CostIterator> line(costImage->upperLeft() + start_point, end);
        int cost = 0;

        while (line != lineEnd) {
            cost += *line;
            ++line;
        }

        const int lineLength =
            std::max(std::abs(end_point.x - start_point.x), std::abs(end_point.y - start_point.y));

        if (lineLength < shortLineThreshold) {
            cost += vigra::NumericTraits<CostImagePixelType>::max() * (shortLineThreshold - lineLength);
        }

        return cost;
    }

    bool segmentIntersect(const vigra::Point2D& l1a, const vigra::Point2D& l1b,
                          const vigra::Point2D& l2a, const vigra::Point2D& l2b) const {
        int denom = (l2b.y - l2a.y) * (l1b.x - l1a.x) - (l2b.x - l2a.x) * (l1b.y - l1a.y);
        if (denom == 0) {
            return false;       // lines are parallel or coincident
        }

        int uaNum = (l2b.x - l2a.x) * (l1a.y - l2a.y) - (l2b.y - l2a.y) * (l1a.x - l2a.x);
        int ubNum = (l1b.x - l1a.x) * (l1a.y - l2a.y) - (l1b.y - l1a.y) * (l1a.x - l2a.x);
        if (denom < 0) {
            uaNum *= -1;
            ubNum *= -1;
            denom *= -1;
        }
        if (uaNum > 0 && uaNum < denom && ubNum > 0 && ubNum < denom) {
            return true;
        }
        return false;
    }

    const CostImage *costImage;
    VisualizeImage *visualizeStateSpaceImage;

    // Mean-field estimates of current point locations
    std::vector<vigra::Point2D> mfEstimates;

    // State spaces of each point
    std::vector<std::vector<vigra::Point2D>*> pointStateSpaces;

    // Probability vectors for each state space
    std::vector<std::vector<double>*> pointStateProbabilities;

    std::vector<std::vector<int>*> pointStateDistances;

    // Flags indicate which points have converged
    std::vector<bool> convergedPoints;

    // Initial Temperature
    double tInitial;

    // Final Temperature
    double tFinal;

    // Current Temperature
    double tCurrent;

    // Cooling constant
    double tau;

    // Maximum cost change possible by any single annealing move
    double deltaEMax;

    // Minimum cost change possible by any single annealing move
    double deltaEMin;

    // Largest state space over all points
    unsigned int kMax;

    // Weight factors for the distance of a point from the initial
    // seam line and the total mismatch accumulated along the seam
    // line segment.
    double distanceWeight;;
    double mismatchWeight;
};


template <typename CostImage, typename VisualizeImage>
void annealSnake(const CostImage* const ci,
                 const std::pair<double, double>& optimizerWeights,
                 Segment* snake,
                 VisualizeImage* const vi)
{
    GDAConfiguration<CostImage, VisualizeImage> cfg(ci, snake, vi);

    cfg.setOptimizerWeights(optimizerWeights.first, optimizerWeights.second);
    cfg.run();

    std::vector<vigra::Point2D>::const_iterator annealedPoint = cfg.getCurrentPoints().begin();
    for (Segment::iterator snakePoint = snake->begin();
         snakePoint != snake->end();
         ++snakePoint, ++annealedPoint) {
        snakePoint->second = *annealedPoint;
    }
}

} // namespace enblend

#endif /* __ANNEAL_H__ */

// Local Variables:
// mode: c++
// End:
