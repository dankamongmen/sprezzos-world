// solution_fragment.h            -*-c++-*-
//
//   Copyright (C) 2005, 2009-2010 Daniel Burrows
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
//

#ifndef SOLUTION_FRAGMENT_H
#define SOLUTION_FRAGMENT_H

#include <apt-pkg/pkgcache.h>

// For aptitude_solution::action
#include <generic/problemresolver/solution.h>

// So passing aptitude_solution::action to a function is legal
#include <generic/apt/aptitude_resolver_universe.h>

/** \brief Code to convert an aptitude resolver solution and some of its
 *  individual components to fragments.
 * 
 *  \file solution_fragment.h
 */

namespace cwidget
{
  class fragment;
}
class aptitude_universe;

cwidget::fragment *solution_fragment(const generic_solution<aptitude_universe> &solution);

/** \return a fragment describing the given solution.
 *
 *  \param solution  The solution to render.
 *  \param ids       A map in which bindings from string values to the
 *                   choices in the solution will be stored; the strings
 *                   can be used later on to act on entries in the
 *                   solution.
 */
cwidget::fragment *solution_fragment_with_ids(const generic_solution<aptitude_universe> &solution,
					      std::map<std::string, generic_choice<aptitude_universe> > &ids);

/** \return a list of the archives to which a version
 *  belongs in the form "archive1,archive2,..."
 *
 *  The output will be sorted and contain each archive name at most
 *  once.
 *
 *  \param v   the version whose archives are to be returned.
 *  \param suppress_now  If \b true, the "now" archive will be
 *                       discarded from the list.
 *  \param inter_archive_string   The string to place between archives.
 */
std::string archives_text(const pkgCache::VerIterator &v,
                          bool suppress_now = false,
                          const std::string &inter_archive_string = ", ");

/** \return a cwidget::fragment describing the given choice. */
cwidget::fragment *choice_fragment(const generic_choice<aptitude_universe> &c);

/** \return a cwidget::fragment with a flag giving the state of the
 *  given choice (rejected/accepted), or NULL if the state is blank.
 */
cwidget::fragment *choice_state_fragment(const generic_choice<aptitude_universe> &c);

/** \return descriptive text about a single dependency. */
std::wstring dep_text(const pkgCache::DepIterator &d);

/** \return descriptive text about a conflict through a provides.
 *
 *  \param conflict any dependency
 *  \param p a provides iterator corresponding to conflict.  If conflict
 *           is not a Conflict, then p is ignored and conflict_text is
 *           identical to dep_text.  Otherwise, p is taken to be
 *           the provides through which the conflict was discovered.
 */
std::wstring conflict_text(const pkgCache::DepIterator &conflict,
			   const pkgCache::PrvIterator &p);

/** \return descriptive text about the targets of a dependency. */
std::string dep_targets(const pkgCache::DepIterator &start);

#endif // SOLUTION_FRAGMENT_H
