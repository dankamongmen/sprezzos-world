/** \file
 * Parser for APT records, with specialised accessors for package records
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

#include <ept/apt/packagerecord.h>

#include <cctype>
#include <cstdlib>

//#include <iostream>

using namespace std;

namespace ept {
namespace apt {

size_t PackageRecord::parseSize(size_t def, const std::string& str) const
{
	if (str == string())
		return def;
	return (size_t)strtoul(str.c_str(), NULL, 10);
}

std::string PackageRecord::parseShortDescription(const std::string& def, const std::string& str) const
{
	if (str == std::string())
		return def;
	size_t pos = str.find("\n");
	if (pos == std::string::npos)
		return str;
	else
		return str.substr(0, pos);
}

std::string PackageRecord::parseLongDescription(const std::string& def, const std::string& str) const
{
	if (str == std::string())
		return def;
	size_t pos = str.find("\n");
	if (pos == std::string::npos)
		return str;
	else
	{
		// Trim trailing spaces
		for (++pos; pos < str.size() && isspace(str[pos]); ++pos)
			;
		return str.substr(pos);
	}
}

std::set<std::string> PackageRecord::parseTags(const std::set<std::string>& def, const std::string& str) const
{
	if (str == string())
		return def;

	set<string> res;

	size_t pos = 0;
	while (pos < str.size())
	{
		string tag;
		size_t i = str.find(", ", pos);
		if (i == string::npos)
			tag = str.substr(pos);
		else
			tag = str.substr(pos, i-pos);

		// Check if we need curly brace expansion
		if (tag[tag.size() - 1] == '}')
		{
			size_t begin = tag.find('{');
			if (begin != string::npos)
			{
				string prefix(tag, 0, begin);
				++begin;
				size_t end;
				while ((end = tag.find(',', begin)) != string::npos)
				{
					res.insert(prefix + tag.substr(begin, end-begin));
					begin = end + 1;
				}
				res.insert(prefix + tag.substr(begin, tag.size() - 1 - begin));
			}
		} else {
			res.insert(tag);
		}

		if (i == string::npos)
			break;
		else
			pos = i + 2;
	}

	return res;
}

}
}

// vim:set ts=4 sw=4:
