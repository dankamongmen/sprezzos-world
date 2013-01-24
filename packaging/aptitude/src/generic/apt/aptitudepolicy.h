// aptitudepolicy.h                  -*-c++-*-
//
//  Copyright 2001 Daniel Burrows
//

#ifndef APTITUDEPOLICY_H
#define APTITUDEPOLICY_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <apt-pkg/policy.h>

/** \brief A policy class that allows Recommends and Suggests to be treated as
 *  "always important", "important for new installs", or "never important".
 * 
 *  \file aptitudepolicy.h
 */

class aptitudePolicy:public pkgPolicy
{
public:
  aptitudePolicy(pkgCache *Owner)
    :pkgPolicy(Owner) {}

  bool IsImportantDep(pkgCache::DepIterator dep);
};

#endif
