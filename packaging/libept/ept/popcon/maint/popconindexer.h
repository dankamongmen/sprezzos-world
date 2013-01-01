#ifndef EPT_DEBTAGS_PKGIDINDEXER_H
#define EPT_DEBTAGS_PKGIDINDEXER_H

/** @file
 * @author Enrico Zini <enrico@enricozini.org>
 * Rebuild and maintain the map from package IDs to package names
 */

/*
 * Copyright (C) 2003-2007  Enrico Zini <enrico@debian.org>
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

#include <ept/popcon/maint/sourcedir.h>
#include <string>

namespace ept {
namespace popcon {

struct InfoStruct
{
	size_t submissions;
};

struct PopconIndexer
{
	SourceDir mainSource;
	SourceDir userSource;
	time_t ts_main_src;
	time_t ts_user_src;
	time_t ts_main_sco;
	time_t ts_user_sco;
	time_t ts_main_idx;
	time_t ts_user_idx;

	time_t sourceTimestamp() const
	{
		time_t res = ts_main_src;
		if (ts_user_src > res) res = ts_user_src;
		return res;

	}
	bool needsRebuild() const;
	bool rebuild(const std::string& scofname, const std::string& idxfname);
	bool rebuildIfNeeded();
	bool getUpToDatePopcon(std::string& scofname, std::string& idxfname);

	bool userIndexIsRedundant() const;
	bool deleteRedundantUserIndex();

	void rescan();

	PopconIndexer();

	static bool obtainWorkingPopcon(std::string& scofname, std::string& idxfname);
};

}
}

// vim:set ts=4 sw=4:
#endif
