// dump_packages.h         -*-c++-*-
//
//   Copyright (C) 2007 Daniel Burrows
//
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.

#ifndef DUMP_PACKAGES_H
#define DUMP_PACKAGES_H

#include <apt-pkg/pkgcache.h>

#include <set>
#include <vector>

/** \brief Functions to write information about packages to a stream
 *  in Packages-file format.
 *
 *  \file dump_packages.h
 */

namespace aptitude
{
  namespace apt
  {
    /** \brief Write the given Packages file entry to the given
     *  stream.
     *
     *  \param vf   the Packages file entry to write.
     *  \param out  the stream to which to write the entry.
     */
    void dump_verfile(const pkgCache::VerFileIterator &vf,
		      std::ostream &out);

    /** \brief Write all the entries associated with the given
     *  package version.
     *
     *  \param ver  the version whose entries should be written.
     *  \param out  the stream to which the entries should be written.
     */
    void dump_version(const pkgCache::VerIterator &ver,
		      std::ostream &out);

    /** \brief Write all the entries associated with the given
     *         package versions.
     *
     *  \param versions  the versions whose Packages entries should
     *                   be written.
     *  \param out       the stream to which the Packages entries
     *                   should be written.
     */
    void dump_versions(const std::vector<pkgCache::VerIterator> &versions,
		       std::ostream &out);

    /** \brief Write all the entries associated with the given
     *         packages, dropping outgoing dependencies.
     *
     *  Dependencies that point to a package not in the given set
     *  will be stripped from the output file.  This is used to
     *  drop irrelevant dependencies when generating resolver test
     *  cases.
     */
    void dump_truncated_packages(const std::set<pkgCache::PkgIterator> &versions,
				 std::ostream &out);



    /** Make a truncated state snapshot in the given directory.
     *
     *  We need to copy:
     *
     *  - $(Dir::State::lists)/
     *  - $(Dir::State::status)/
     *  - $(Dir::Etc::sourceparts)/
     *  - $(Dir::Etc::vendorlist)
     *  - $(Dir::Etc::vendorparts)/
     *  - $(Dir::Etc::main)
     *  - $(Dir::Etc::parts)/
     *  - $(Dir::Etc::preferences)
     *
     * Dir::State::* are truncated copies; the others are copied
     * literally.
     *
     *  \todo check that outDir doesn't exist.
     *
     *  \note This calls _error in many places; instead it should
     *        throw exceptions (all else aside, _error isn't
     *        thread-safe!).
     *
     *  \param out_dir   the target directory, which should not
     *                   exist.
     *  \param visited_packages    the packages that should
     *                             be contained in the truncated cache.
     */
    void make_truncated_state_copy(const std::string &out_dir,
				   const std::set<pkgCache::PkgIterator> &visited_packages);
  }
}

#endif
