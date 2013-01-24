// trust.h
//
//  Copyright 2004 Daniel Burrows
//

#ifndef TRUST_H
#define TRUST_H

#include <apt-pkg/pkgcache.h>

/** \brief Various routines dealing with messages about package trust.
 * 
 *  \file trust.h
 */

namespace cwidget
{
  class fragment;
}

/** Create a new cwidget::fragment, suitable as a BIG FAT WARNING to the user
 *  that a single package is not trusted.
 *
 *  \param ver the version to which the cwidget::fragment refers
 *  \return the new cwidget::fragment, or NULL if ver is trusted.
 */
cwidget::fragment *make_untrusted_warning(const pkgCache::VerIterator &ver);

#endif
