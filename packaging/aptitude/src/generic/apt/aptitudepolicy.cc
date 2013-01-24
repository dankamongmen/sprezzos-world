// aptitudepolicy.cc
//
//  Copyright 2001 Daniel Burrows
//
//  A policy class that allows Recommends to be treated as "always
// important", "important for new installs", or "never important".

#include "aptitudepolicy.h"

#include <aptitude.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "apt.h"
#include "config_signal.h"

#include <apt-pkg/pkgcache.h>
#include <apt-pkg/error.h>

bool aptitudePolicy::IsImportantDep(pkgCache::DepIterator dep)
{
  return pkgPolicy::IsImportantDep(dep);
}
