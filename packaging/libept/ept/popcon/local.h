#ifndef EPT_POPCON_LOCAL_H
#define EPT_POPCON_LOCAL_H

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

#include <string>
#include <vector>
#include <map>

namespace ept {
namespace popcon {

class Popcon;

/**
 * Access the results of the local daily popcon scan.
 */
class Local
{
protected:
	std::map<std::string, float> m_scores;
	time_t m_timestamp;

public:
	Local(const std::string& file = std::string("/var/log/popularity-contest"));

	/// Get the timestamp of the local popcon information
	time_t timestamp() const { return m_timestamp; }

	/// Return true if this data source has data, false if it's empty
	bool hasData() const { return m_timestamp != 0; }

	/**
	 * Return the local score of the package
	 */
	float score(const std::string& pkg) const;

	/**
	 * Return the TFIDF score of the package computed against the popcon
	 * information.
	 *
	 * The TFIDF score is high when a package is representative of this system,
	 * that is, it is used in this system and not much used in other systems.
	 */
	float tfidf(const Popcon& popcon, const std::string& pkg) const;

	/**
	 * Read the local popcon vote and return the list of packages and their
	 * local scores, sorted by ascending score.
	 */
	std::vector< std::pair<std::string, float> > scores() const;

	/**
	 * Read the local popcon vote and return the list of packages and their
	 * TFIDF scores computed against the popcon information.
	 *
	 * The packages will be sorted by ascending score.
	 */
	std::vector< std::pair<std::string, float> > tfidf(const Popcon& popcon) const;
};

}
}

// vim:set ts=4 sw=4:
#endif
