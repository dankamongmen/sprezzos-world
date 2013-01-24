// browser.h        -*-c++-*-
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

#ifndef BROWSER_H
#define BROWSER_H

/** \brief Start a browser to view the given URL.
 *
 *  The browser is taken from the Aptitude::Browser configuration item
 *  if set, from the BROWSER environment variable if it exists; if
 *  not, we try to execute sensible-browser, firefox, epiphany,
 *  konqueror, links and w3m (in that order) as fallbacks.  These are
 *  The browser is executed in the foreground; if a browser should run
 *  in the background, a wrapper script will have to be used
 *  (otherwise text-based browsers couldn't be invoked).
 *
 *  \todo not currently implemented, but my thoughts on what it should
 *  do are saved for future reference.
 *
 *  NOTE: the programs are executed from the path.  If the user is
 *  running as root, this could be a security issue.  On the other
 *  hand,
 */
void spawn_browser(const std::string &url);

#endif // BROWSER_H
