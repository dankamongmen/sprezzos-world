// -*- mode: c++; tab-width: 4; indent-tabs-mode: t -*-

/** @file
 * @author Enrico Zini <enrico@enricozini.org>
 * Quick map from package IDs to package names
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

#include <ept/popcon/popcon.h>
#include <ept/popcon/maint/popconindexer.h>
#include <ept/popcon/maint/path.h>

//#include <iostream>

using namespace std;

namespace ept {
namespace popcon {

size_t Popcon::GeneralInfo::submissions() const
{
	if (!m_buf) return 0;
	return ((InfoStruct*)m_buf)->submissions;
}

Popcon::Popcon()
{
	std::string scofname, idxfname;

	if (!PopconIndexer::obtainWorkingPopcon(scofname, idxfname))
	{
		m_timestamp = 0;
		return;
	}

	//cerr << "GOT " << scofname << " " << idxfname << endl;

	m_timestamp = Path::timestamp(idxfname);

	mastermmap.init(idxfname);
	tagcoll::diskindex::MMap::init(mastermmap, 0);

	m_info.init(mastermmap, 1);

	//cerr << "SIZE " << size() << endl;
	//for (size_t i = 0; i < size(); ++i)
	//{
	//	cerr << "EL " << i << ": " << ((Score*)m_buf)[i].offset << " " << ((Score*)m_buf)[i].score << endl;
	//}
}

float Popcon::scoreByName(const std::string& name) const
{
	// Binary search the index to find the package ID
	int begin, end;

	/* Binary search */
	begin = -1, end = size();
	while (end - begin > 1)
	{
		int cur = (end + begin) / 2;
		if (this->name(cur) > name)
			end = cur;
		else
			begin = cur;
	}

	if (begin == -1 || this->name(begin) != name)
		//throw NotFoundException(string("looking for the ID of string ") + str);
		return 0;
	else
		return score(begin);
}

}
}

// vim:set ts=4 sw=4:
