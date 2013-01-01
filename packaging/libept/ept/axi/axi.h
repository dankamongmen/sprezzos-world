#ifndef EPT_TEXTSEARCH_TEXTSEARCH_H
#define EPT_TEXTSEARCH_TEXTSEARCH_H

/** @file
 * @author Enrico Zini <enrico@enricozini.org>
 * Fast full-text search
 */

/*
 * Copyright (C) 2007  Enrico Zini <enrico@debian.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <xapian.h>
#include <string>

namespace ept {

/**
 * Maintains and accesses a Xapian index of package descriptions.
 *
 * Contrarily to Debtags and Popcon, TextSearch does not attempt to create the
 * index in the home directory if no system index is found and it is not
 * running as root: this is to avoid secretly building large indexes (>50Mb)
 * in the home directory of users.
 *
 * The idea then is to have root keep the index up to date, possibly running a
 * reindexing tool once a day, or after an apt-get update.
 *
 * This works because the full text search index is useful even if it is
 * slightly out of date.
 */
namespace axi {

// Allocate value indexes for known values
const Xapian::valueno VAL_APT_INSTALLED_SIZE      =  1;
const Xapian::valueno VAL_APT_PACKAGE_SIZE        =  2;
const Xapian::valueno VAL_POPCON                  = 10;
const Xapian::valueno VAL_ITERATING_RATING        = 20;
const Xapian::valueno VAL_ITERATING_FUNCTIONALITY = 21;
const Xapian::valueno VAL_ITERATING_USABILITY     = 22;
const Xapian::valueno VAL_ITERATING_SECURITY      = 23;
const Xapian::valueno VAL_ITERATING_PERFORMANCE   = 24;
const Xapian::valueno VAL_ITERATING_QUALITY       = 25;
const Xapian::valueno VAL_ITERATING_SUPPORT       = 26;
const Xapian::valueno VAL_ITERATING_ADOPTION      = 27;
// If you need to index a value and cannot edit this file, feel free to use any
// value starting from 1000000

/// Return the path to the Apt Xapian index root directory
std::string path_dir();

/// Return the path to the Apt Xapian index Xapian database
std::string path_db();

/// Return the last update timestamp of the index
time_t timestamp();


/**
 * RAII temporary override of the location of the index root
 * directory, used for tests
 */
class OverrideIndexDir
{
	std::string old;
public:
	OverrideIndexDir(const std::string& path);
	~OverrideIndexDir();
};

}
}

// vim:set ts=4 sw=4:
#endif
