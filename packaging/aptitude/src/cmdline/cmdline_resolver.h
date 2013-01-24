// cmdline_resolver.h                              -*-c++-*-
//
//   Copyright (C) 2005, 2007-2008, 2010 Daniel Burrows
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

#ifndef CMDLINE_RESOLVER_H
#define CMDLINE_RESOLVER_H

// Local includes:
#include "cmdline_common.h"

// We need these two to declare get_current_solution().
//#include <generic/apt/aptitude_resolver_universe.h>
#include <generic/problemresolver/solution.h>


// System includes:
#include <boost/shared_ptr.hpp>

/** \file cmdline_resolver.h
 */

class aptitude_universe;

class pkgPolicy;
namespace aptitude
{
  namespace cmdline
  {
    class terminal_metrics;

    /** \brief Represents the termination state of the
     *  command-line resolver.
     */
    enum cmdline_resolver_result
      {
	/** \brief The resolver produced and applied a solution
	 *  to the broken dependencies.
	 */
	resolver_success,
	/** \brief The resolver encountered a fatal (possibly
	 *  internal) error or was cancelled by the user; the caller
	 *  should fall back to manual resolution.
	 */
	resolver_incomplete,
	/** \brief The user asked to quit (i.e., pressed "q");
	 *  the caller should terminate the program.
	 */
	resolver_user_exit
      };
  }
}

/** \brief Compute the current solution with a command-line-appropriate
 *  UI.
 *
 *  \param print_resolving_dependencies   if \b true, print a message
 *  saying "Resolving dependencies..." before running the resolver.
 *  This is used by safe-upgrade to suppress the message on subsequent
 *  resolver runs.
 *
 *  \param term  The terminal object to use for I/O.
 *
 * \return the resolver manager's current solution; if it needs to be
 *          calculated first, run the calculation in the background
 *          and display a spinner in the foreground.
 *
 *  \note The exceptions are the same as the exceptions of the
 *  resolver manager's get_solution.
 *
 *  \throw NoMoreSolutions if the list of solutions is exhausted
 *  \throw NoMoreTime if time is exhausted while searching for
 *                    the solution (time here is counted separately
 *                    at each step).
 *  \throw ResolverManagerThreadClashException if a new solution
 *         would be generated and a background thread exists.
 *  \throw Exception if the background thread aborted with an exception.
 */

generic_solution<aptitude_universe> calculate_current_solution(bool print_resolving_dependencies,
                                                               const boost::shared_ptr<aptitude::cmdline::terminal_metrics> &term_metrics);

/** \brief Write the resolver state to a file as appropriate.
 *
 *  If the configuration option Aptitude::CMdLine::Resolver-Dump is
 *  set, its value is taken to be the name of a file to which the
 *  resolver state should be written.
 */
void cmdline_dump_resolver();

/** Run the resolver once, possibly prompting the user in the process.
 *
 *  \param to_install a list of packages which the user explicitly
 *  asked to install
 *
 *  \param to_hold a list of packages which the user explicitly asked
 *  to hold back
 *
 *  \param to_remove a list of packages which the user explicitly
 *  asked to remove
 *
 *  \param to_purge a list of packages which the user explicitly asked
 *  to purge
 *
 *  \param assume_yes if \b true, try to find a single solution
 *  (regardless of how long it takes) and accept it immediately.
 *
 *  \param force_no_change if \b true, assign extra version scores to
 *  heavily bias the resolver against changing any packages in the
 *  supplied sets.
 *  \param verbose the verbosity level set by the user
 *
 *  \param policy the package policy object used to look up priorities.
 *  \param arch_only if \b true, architecture-independent build-dependencies
 *  are ignored.
 */
aptitude::cmdline::cmdline_resolver_result
cmdline_resolve_deps(pkgset &to_install,
		     pkgset &to_hold,
		     pkgset &to_remove,
		     pkgset &to_purge,
		     bool assume_yes,
		     bool force_no_change,
		     int verbose,
		     pkgPolicy &policy,
		     bool arch_only,
                     const boost::shared_ptr<aptitude::cmdline::terminal_metrics> &term_metrics);

namespace aptitude
{
  namespace cmdline
  {
    /** \brief Try to resolve packages without removing anything.
     *
     *  \param verbose   The verbosity level (increase to get more
     *                   warnings about being unable to resolve
     *                   deps).
     *  \param no_new_installs  If true, packages not currently
     *                          on the system will not be installed.
     *  \param no_new_upgrades  If true, packages not currently
     *                          flagged for upgrade will not be
     *                          upgraded.
     *  \param show_story  If true, an explanation of the arrived-at
     *                     solution as a sequence of dependency
     *                     resolutions will be displayed.
     *
     *  \return \b true iff a solution was found and applied.
     */
    bool safe_resolve_deps(int verbose,
			   bool no_new_installs,
			   bool no_new_upgrades,
			   bool show_story,
                           const boost::shared_ptr<terminal_metrics> &term_metrics);
  }
}

#endif // CMDLINE_RESOLVER_H
