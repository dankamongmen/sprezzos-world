/** \file
 * Parser for APT records
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

#include <ept/apt/recordparser.h>

#include <algorithm>
#include <cctype>

//#include <iostream>

using namespace std;

namespace ept {
namespace apt {

struct rpcompare
{
	const RecordParser& rp;
	rpcompare(const RecordParser& rp) : rp(rp) {}
	bool operator()(size_t a, size_t b)
	{
		return rp.name(a) < rp.name(b);
	}
};

void RecordParser::scan(const std::string& str)
{
	buffer = str;
	ends.clear();
	sorted.clear();

	//cerr << "PARSE " << endl << buffer << "*****" << endl;

	// Scan the buffer, taking note of all ending offsets of the various fields
	size_t pos = 0;
	size_t idx = 0;
	while (pos < buffer.size() - 1)
	{
		//cerr << "PREPOS " << pos << " left: " << buffer.substr(pos, 10) << endl;
		pos = buffer.find("\n", pos);
		//cerr << "POSTPOS " << pos << " left: " << (pos == string::npos ? "NONE" : buffer.substr(pos, 10)) << endl;

		// The buffer does not end with a newline
		if (pos == string::npos)
		{
			//cerr << "ENDNOTEOL" << endl;
			pos = buffer.size();
			ends.push_back(pos);
			sorted.push_back(idx++);
			break;
		}

		++pos;
		//cerr << "POSTPOSINC " << pos << " left: " << buffer.substr(pos, 10) << endl;

		// The buffer ends with a newline
		if (pos == buffer.size())
		{
			//cerr << "ENDEOL" << endl;
			ends.push_back(pos);
			sorted.push_back(idx++);
			break;
		}

		// Terminate parsing on double newlines
		if (buffer[pos] == '\n')
		{
			//cerr << "ENDDOUBLENL" << endl;
			ends.push_back(pos);
			sorted.push_back(idx++);
			break;
		}

		// Mark the end of the field if it's not a continuation line
		if (!isspace(buffer[pos]))
		{
			//cerr << "INNERFIELD" << endl;
			ends.push_back(pos);
			sorted.push_back(idx++);
		} //else
			//cerr << "CONTLINE" << endl;
	}

	// Sort the sorted array
	sort(sorted.begin(), sorted.end(), rpcompare(*this));

	//for (size_t i = 0; i < ends.size(); ++i)
	//	cerr << ends[i] << "\t" << name(i) << "\t" << sorted[i] << "\t" << name(sorted[i]) << endl;
}

std::string RecordParser::field(size_t idx) const
{
	if (idx >= ends.size())
		return string();
	if (idx == 0)
		return buffer.substr(0, ends[0]);
	else
		return buffer.substr(ends[idx-1], ends[idx]-ends[idx-1]);
}

std::string RecordParser::name(size_t idx) const
{
	string res = field(idx);
	size_t pos = res.find(":");
	if (pos == string::npos)
		return res;
	return res.substr(0, pos);
}

std::string RecordParser::lookup(size_t idx) const
{
	string res = field(idx);
	size_t pos = res.find(":");
	if (pos == string::npos)
		return res;
	// Skip initial whitespace after the :
	for (++pos; pos < res.size() && isspace(res[pos]); ++pos)
		;
	res = res.substr(pos);
	// Trim spaces at the end
	while (!res.empty() && isspace(res[res.size() - 1]))
		res.resize(res.size() - 1);
	return res;
}

size_t RecordParser::index(const std::string& str) const
{
	int begin, end;

	/* Binary search */
	begin = -1, end = size();
	while (end - begin > 1)
	{
		int cur = (end + begin) / 2;
		//cerr << "Test " << cur << " " << str << " < " << name(cur) << endl;
		if (name(sorted[cur]) > str)
			end = cur;
		else
			begin = cur;
	}

	if (begin == -1 || name(sorted[begin]) != str)
		return size();
	else
		return sorted[begin];
}

}
}

// vim:set ts=4 sw=4:
