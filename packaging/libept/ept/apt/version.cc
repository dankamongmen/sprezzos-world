/** \file
 * Provide a very lightweight Version class that represent a package with a
 * version, with very cheap value copy operations.
 */

/* 
 * Copyright (C) 2007  Enrico Zini <enrico@enricozini.org>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */

#include <ept/apt/version.h>
#include <apt-pkg/debversion.h>

using namespace std;

namespace ept {
namespace apt {

std::string Version::upstreamVersion() const
{
	// Skip the epoch, if it is there
	size_t start = m_version.find(':');
	if (start == string::npos)
		start = 0;
	else
		++start;

	// Skip everything after the trailing '-', if it is there
	size_t end = m_version.rfind('-');
	if (end == string::npos)
		end = m_version.size();

	return m_version.substr(start, end-start);
}

/* Version comparison by Debian policy */

bool Version::operator<=(const Version& pkg) const
{
	if (name() < pkg.name())
		return true;
	if (name() == pkg.name())
		return debVS.CmpVersion(version(), pkg.version()) <= 0;
	return false;
}
bool Version::operator<(const Version& pkg) const
{
	if (name() < pkg.name())
		return true;
	if (name() == pkg.name())
		return debVS.CmpVersion(version(), pkg.version()) < 0;
	return false;
}
bool Version::operator>=(const Version& pkg) const
{
	if (name() > pkg.name())
		return true;
	if (name() == pkg.name())
		return debVS.CmpVersion(version(), pkg.version()) >= 0;
	return false;
}
bool Version::operator>(const Version& pkg) const
{
	if (name() > pkg.name())
		return true;
	if (name() == pkg.name())
		return debVS.CmpVersion(version(), pkg.version()) > 0;
	return false;
}

}
}

// vim:set ts=4 sw=4:
