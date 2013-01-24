// dpkg.h  -*-c++-*-
//
//  Copyright 2012 Daniel Hartwig
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; see the file COPYING.  If not, write to
//  the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//  Boston, MA 02111-1307, USA.
//
//  Helpers for interfacing with dpkg.
//  Based on APT: dpkgpm.cc, apt-mark.cc

// FIXME: These should really go in libapt-pkg.

#ifndef APTITUDE_GENERIC_APT_DPKG_H
#define APTITUDE_GENERIC_APT_DPKG_H

#include <apt-pkg/pkgcache.h>

#include <string>
#include <vector>

namespace aptitude {
namespace apt {

/** \brief Base arguments that should be passed to dpkg.  Append
 *         additional arguments, followed by NULL, then pass to
 *         execvp(3).
 */
std::vector<const char *> dpkg_base_args(const bool cached = true);

/** \brief Returns \b true if dpkg is multi-arch aware.
 */
bool dpkg_multi_arch(const bool cached = true);

/** \brief Returns the name by which dpkg knows /b pkg.  Note that
 *         this is different to PkgIterator::FullName.
 */
std::string dpkg_package_name(const pkgCache::PkgIterator &pkg);

}} // namespace aptitude { namespace apt {

#endif // APTITUDE_GENERIC_APT_DPKG_H
