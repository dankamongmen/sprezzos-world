/** @file
 * @author Enrico Zini <enrico@enricozini.org>
 * Correlate popcon data with local popcon information
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

#include <ept/popcon/local.h>
#include <ept/popcon/popcon.h>
#include <ept/popcon/maint/path.h>

#include <wibble/exception.h>

#include <algorithm>
#include <fstream>
#include <cmath>

//#include <iostream>

using namespace std;

namespace ept {
namespace popcon {

// Split a string where there are separators
static vector<string> split(const std::string& str, char sep = ' ')
{
	vector<string> res;
	size_t start = 0;
	while (start < str.size())
	{
		size_t end = str.find(sep, start);
		if (end == string::npos)
		{
			res.push_back(str.substr(start));
			break;
		}
		else
		{
			res.push_back(str.substr(start, end-start));
			start = end + 1;
		}
	}
	return res;
}

// Reverse sort pairs by comparing their second element
struct secondsort
{
	bool operator()(const pair<string, float>& a, const pair<string, float>& b) const
	{
		if (a.second == b.second)
			return a.first > b.first;
		else
			return a.second > b.second;
	}
};

Local::Local(const std::string& file)
{
	m_timestamp = Path::timestamp(file);
	if (m_timestamp == 0)
		return;
	
	ifstream in;
	in.open(file.c_str());
	if (!in.good())
		throw wibble::exception::File(file, "opening file for reading");

	while (!in.eof())
	{
		std::string line;
		getline(in, line);
		if (line.substr(0, 10) == "POPULARITY")
			continue;
		if (line.substr(0, 14) == "END-POPULARITY")
			continue;
		vector<string> data = split(line);
		if (data.size() < 4)
			continue;
		if (data[3] == "<NOFILES>")
			// This is an empty / virtual package
			m_scores.insert(make_pair(data[2], 0.1));
		else if (data.size() == 4)
			// Package normally in use
			m_scores.insert(make_pair(data[2], 1.0));
		else if (data[4] == "<OLD>")
			// Unused packages
			m_scores.insert(make_pair(data[2], 0.3));
		else if (data[4] == "<RECENT-CTIME>")
			// Recently installed packages
			m_scores.insert(make_pair(data[2], 0.5));
	}
}

float Local::score(const std::string& pkg) const
{
	std::map<std::string, float>::const_iterator i = m_scores.find(pkg);
	if (i == m_scores.end())
		return 0;
	else
		return i->second;
}

/**
 * Return the TFIDF score of the package computed against the popcon
 * information.
 */
float Local::tfidf(const Popcon& popcon, const std::string& pkg) const
{
	float popconScore = popcon.score(pkg);
	//cerr << pkg << ": " << score(pkg) << " * log(" << (float)popcon.submissions() << " / " << popconScore << ") = " << score(pkg) * log((float)popcon.submissions() / popconScore) << endl;
	if (popconScore == 0)
		return 0;
	else
		return score(pkg) * log((float)popcon.submissions() / popconScore);
	
}

std::vector< std::pair<std::string, float> > Local::scores() const
{
	vector< pair<string, float> > res;
	// Copy the scores in res
	copy(m_scores.begin(), m_scores.end(), back_inserter(res));
	// Sort res by score
	sort(res.begin(), res.end(), secondsort());
	return res;
}

std::vector< std::pair<std::string, float> > Local::tfidf(const Popcon& popcon) const
{
	vector< pair<string, float> > res;
	// Compute the tfidf scores and store them into res
	for (std::map<std::string, float>::const_iterator i = m_scores.begin();
			i != m_scores.end(); ++i)
	{
		float popconScore = popcon.score(i->first);
		if (popconScore == 0)
			res.push_back(make_pair(i->first, 0.0f));
		else
			res.push_back(make_pair(i->first,
						i->second * log((float)popcon.submissions() / popconScore)));
	}
	// Sort res by score
	sort(res.begin(), res.end(), secondsort());
	return res;
}

}
}

// vim:set ts=4 sw=4:
