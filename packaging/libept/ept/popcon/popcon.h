// -*- mode: c++; tab-width: 4; indent-tabs-mode: t -*-
#ifndef EPT_POPCON_POPCON_H
#define EPT_POPCON_POPCON_H

/** @file
 * @author Enrico Zini <enrico@enricozini.org>
 * Access popcon data
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

#include <tagcoll/diskindex/mmap.h>
#include <string>

namespace ept {
namespace apt {
class Apt;
}

namespace popcon {

/**
 * Store the score information in the popcon cache.
 *
 * Currently, this is only one float; more can be added in the future.
 */
class Score
{
protected:
	unsigned offset;

public:
	float score;

	Score(float score) : offset(offset), score(score) {}

	friend class Popcon;
	friend class PopconIndexer;
	friend class PopconGenerator;
};

/**
 * Maps Packages to IDs and vice-versa.
 *
 * This is used in building the Debtags fast index, which works representing
 * tags and packages as int IDs.
 *
 * Index building works like this:
 *  1. The file all-popcon-results.txt.gz is downloaded from
 *     http://popcon.debian.org/all-popcon-results.txt.gz
 *  2. The file is put in either ~/.popcon/all-popcon-results.txt.gz
 *     or in /var/lib/popcon/all-popcon-results.txt.gz
 *  3. If the file is newer than the index, it will be automatically used to
 *     recompute the scores and rebuild the index.
 */
class Popcon : public tagcoll::diskindex::MMap
{
	struct GeneralInfo : public tagcoll::diskindex::MMap
	{
		size_t submissions() const;
	};

	tagcoll::diskindex::MasterMMap mastermmap;
	time_t m_timestamp;

	GeneralInfo m_info;

	/// Get the score structure by index
	const Score* structByIndex(size_t idx) const
	{
		if (idx >= 0 && idx < size())
			return (Score*)m_buf + idx;
		return 0;
	}

public:
	Popcon();

	/// Get the timestamp of when the index was last updated
	time_t timestamp() const { return m_timestamp; }

	/// Return true if this data source has data, false if it's empty
	bool hasData() const { return m_timestamp != 0; }

	/// Return the total number of popcon submissions
	size_t submissions() const { return m_info.submissions(); }

	/// Get the number of packages in the index
	size_t size() const
	{
		if (m_buf)
			return ((Score*)m_buf)->offset / sizeof(Score);
		else
			return 0;
	}

	/**
	 * Get a package name by index
	 *
	 * If the index is not valid, returns the empty string.
	 */
	std::string name(size_t idx) const
	{
		const Score* s = structByIndex(idx);
		if (s == 0) return std::string();
		return std::string(m_buf + s->offset);
	}

	/// Get the score by index
	float scoreByIndex(size_t idx) const
	{
		const Score* s = structByIndex(idx);
		if (!s) return 0;
		return s->score;
	}

	/// Get the score structure by package name
	float scoreByName(const std::string& name) const;

	/// Get the score by index
	float score(size_t idx) const { return scoreByIndex(idx); }

	/// Get the score by index
	float operator[](int idx) const { return scoreByIndex(idx); }

	/// Get the score by name
	float score(const std::string& name) const { return scoreByName(name); }

	/// Get the score structure by package name
	float operator[](const std::string& name) const { return scoreByName(name); }
};

}
}

// vim:set ts=4 sw=4:
#endif
