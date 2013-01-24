// cmdline_simulate.h               -*-c++-*-
//
// Copyright (C) 2004, 2010 Daniel Burrows
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

#ifndef CMDLINE_SIMULATE_H
#define CMDLINE_SIMULATE_H

// Local includes:
#include "cmdline_common.h"


// System includes:
#include <boost/shared_ptr.hpp>

/** \file cmdline_simulate.h
 */

class pkgPolicy;

namespace aptitude
{
  namespace cmdline
  {
    class terminal_metrics;
  }
}

/** Simulate an install run.  to_install and friends are meant to be
 *  the sets the user explicitly selected (so the prompt can be
 *  displayed only when extra stuff is added or removed).
 *
 *  \param as_upgrade currently ignored; meant to control how the
 *                    preview and prompting are handled.
 *  \param to_install a set of packages to install.
 *  \param to_hold a set of packages to hold.
 *  \param to_remove a set of packages to remove.
 *  \param to_purge a set of packages to purge.
 *  \param showvers \b true to show version information in the preview.
 *  \param showdeps \b true to show dependency information in the preview.
 *  \param showsize \b true to show size information in the preview.
 *  \param showwhy  \b true to show root causes in the preview.
 *  \param verbose the current verbosity level
 *  \param assume_yes if \b true, assume the user entered "yes"
 *                    at the prompt.
 *  \param fornce_no_change if \b true, make an effort to avoid
 *                          undoing the user's explicit requests as
 *                          given in to_install et al.
 *  \param policy  the policy object used to look up version priorities.
 *  \param arch_only   if \b true, architecture-independent build-dependencies
 *                     will be ignored when the user installs build-dependencies.
 */

int cmdline_simulate(bool as_upgrade,
		     pkgset &to_install, pkgset &to_hold, pkgset &to_remove,
		     pkgset &to_purge,
		     bool showvers, bool showdeps,
		     bool showsize, bool showwhy,
		     bool always_prompt, int verbose,
		     bool assume_yes, bool force_no_change,
		     pkgPolicy &policy, bool arch_only,
                     const boost::shared_ptr<aptitude::cmdline::terminal_metrics> &term_metrics);


#endif // CMDLINE_SIMULATE_H
