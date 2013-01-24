// cmdline_show.h                                      -*-c++-*-
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

#ifndef CMDLINE_SHOW_H
#define CMDLINE_SHOW_H

// System includes:
#include <apt-pkg/pkgcache.h>

#include <boost/shared_ptr.hpp>

#include <iosfwd>
#include <string>

/** \file cmdline_show.h
 */

namespace cwidget
{
  class fragment;
  class fragment_contents;
}

namespace aptitude
{
  namespace cmdline
  {
    class terminal_metrics;
  }
}

/** \brief Render the description of a single version as found in a
 *  particular index file.
 *
 *  \param ver  The version to render.
 *  \param vf   The index file in which to find information about ver.
 *  \param verbose  How much information to render (scale of 0 to 2 inclusive).
 */
cwidget::fragment *version_file_fragment(const pkgCache::VerIterator &ver,
					 const pkgCache::VerFileIterator &vf,
					 int verbose);

/** Run the "show" operation on a single argument, presented as a string. */
bool do_cmdline_show(std::string s, int verbose,
                     const boost::shared_ptr<aptitude::cmdline::terminal_metrics> &term_metrics);

/** The "show" user command. */
int cmdline_show(int argc, char *argv[], int verbose);

std::ostream &operator<<(std::ostream &out, const cwidget::fragment_contents &contents);

#endif // CMDLINE_SHOW_H
