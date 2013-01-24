// infer_reason.h
//
//  Copyright 2004 Daniel Burrows

#ifndef INFER_DEPS_H
#define INFER_DEPS_H

#include <set>

#include <apt-pkg/pkgcache.h>

/** \brief Code for inferring why a package is in its current state.
 *
 *  \file infer_reason.h
 */

/** A structure representing a package and a dependency of that
 *  package.  It is used below to return the reasons for a package's
 *  current state.
 */

struct reason
{
  pkgCache::PkgIterator pkg;

  pkgCache::DepIterator dep;

  reason(const pkgCache::PkgIterator &_pkg, const pkgCache::DepIterator &_dep)
    :pkg(_pkg), dep(_dep) {}
};

/** Compares two reasons; this is meant to support, eg, sorting
 *  reasons into a sensible order for output.
 */

bool operator<(const reason &a, const reason &b);

/** Return the reason for a package's current state.  The
 *  interpretation of each output reason can vary depending on the
 *  state of the package:
 *
 * - If the package is being automatically installed, each reason
 *   refers to a package which depends on, recommends, or suggests
 *   the package being installed.
 *
 * - If the package is being automatically removed, each reason refers
 *   EITHER to a package which this package depends upon or conflicts
 *   with, OR to a package which conflicts with this package.
 *
 * - If the package was removed because it was unused, each reason
 *   refers to a package which depends upon (or recommends) this package,
 *   and is being removed (or upgraded to a version which no longer has
 *   the dependency).
 *
 * - If the package was automatically held back from upgrading, each
 *   reason refers to a dependency which would be broken if the package
 *   was upgraded.
 *
 * - If the package is not installed and will not be installed, each
 *   reason refers to a suggestion or recommendation made by a package
 *   which is being installed.
 *
 * - If the package is broken, each reason refers either to another
 *   package that conflicts with this package, or to a dependency of
 *   this package that is not satisfied.
 *
 * \param pkg the package to analyze
 * \param reasons the reasons for the package's state will be sent to this 
 *                set.
 */
void infer_reason(pkgCache::PkgIterator pkg, std::set<reason> &reasons);


/** Do the opposite of infer_reason: instead of finding reasons for
 *  why \b this package is in its present state, find reasons (if any)
 *  for why it breaks other packages.
 * 
 *  Possible reasons for a package A (this is meant to be an
 *  exhaustive enumeration):
 * 
 *  - B depends on A, but A is [being removed|not going to be installed]
 *
 *  - B conflicts with A, but A is [installed|going to be installed]
 *
 *  - B depends on A (version), but A is being upgraded from that version.
 *
 *  - B conflicts with A (version), and A is being upgraded to that version.
 */
void infer_reverse_breakage(pkgCache::PkgIterator pkg,
			    std::set<reason> &reasons);

#endif

