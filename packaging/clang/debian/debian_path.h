//===----------------------------------------------------------------------===//
//
// Debian paths declaration management
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef DEBIAN_PATH_H
#define DEBIAN_PATH_H

// To create the full path to libgcc and other.
// For example: /usr/lib/i386-linux-gnu/gcc/i486-linux-gnu/4.5/libgcc.a

#define DEB_HOST_MULTIARCH_TRIPLET "@DEB_HOST_MULTIARCH@"
#define DEB_HOST_GNU_TYPE_TRIPLET "@DEB_HOST_GNU_TYPE@"

// Provides the debian revision
#define DEB_PATCHSETVERSION  "@DEB_PATCHSETVERSION@"

#endif
