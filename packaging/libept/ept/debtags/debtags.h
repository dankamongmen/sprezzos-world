// -*- mode: c++; tab-width: 4; indent-tabs-mode: t -*-
/* @file
 * @author Enrico Zini (enrico) <enrico@enricozini.org>
 */

/*
 * libpkg Debtags data provider
 *
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

#ifndef EPT_DEBTAGS_DEBTAGS_H
#define EPT_DEBTAGS_DEBTAGS_H

#include <tagcoll/coll/base.h>
#include <tagcoll/coll/fast.h>
#include <string>

namespace ept {
namespace debtags {
class Debtags;
}
}

namespace ept {
namespace debtags {

/**
 * Access the on-disk Debtags tag database.
 *
 * The database is normally found in /var/lib/debtags.
 *
 * Tags and Facets are returned as std::strings.  The objects follow
 * the flyweight pattern and access the data contained in the Vocabulary
 * instantiated inside Debtags.
 *
 * It is possible to get a reference to the Vocabulary object using the
 * vocabulary() method.
 */
class Debtags : public tagcoll::coll::Fast<std::string, std::string>
{
protected:
	// User rc directory to store patches
	std::string rcdir;

	// Last modification timestamp of the index
	time_t m_timestamp;

public:
	typedef tagcoll::coll::Fast<std::string, std::string> coll_type;
	typedef std::pair< std::string, std::set<std::string> > value_type;

	/**
	 * Create a new accessor for the on-disk Debtags database
	 *
	 * \param editable
	 * Specifies if recording of modifications should be enabled.  If editable
	 * is true, then the local state directory will be created when the object
	 * is instantiated.
	 */
	Debtags(bool editable = false);
	~Debtags() {}

	/// Get the timestamp of when the index was last updated
	time_t timestamp() const { return m_timestamp; }

	/// Return true if this data source has data, false if it's empty
	bool hasData() const { return m_timestamp != 0; }

	coll_type& tagdb() { return *this; }
	const coll_type& tagdb() const { return *this; }
	tagcoll::PatchList<std::string, std::string> changes() const;

#if 0
	/**
	 * Get the changes that have been applied to this collection
	 */
	const Patches& changes() const { return m_changes; }

	/**
	 * Throw away all changes previously applied to this collection
	 */
	void resetChanges() { m_changes.clear(); }

	/**
	 * Set the changes list to a specific patch list
	 */
	void setChanges(const Patches& changes);

	/**
	 * Add a specific patch list to the changes list
	 */
	void addChanges(const Patches& changes);
#endif

#if 0
	ItemSet getTaggedItems() const;
#endif

	/**
	 * Check if the tag database has been created (i.e. if something
	 * equivalend to debtags update has been run)
	 */
	//static bool hasTagDatabase();


	/**
	 * Save in the state storage directory a patch that can be used to turn
	 * the system database into the collection given
	 */
	void savePatch();

	/**
	 * Save in the state storage directory a patch to turn the system database
	 * into the collection given
	 */
	void savePatch(const tagcoll::PatchList<std::string, std::string>& patch);

	/**
	 * Send to the central archive a patch that can be used to turn
	 * the system database into the collection given
	 */
	void sendPatch();

	/**
	 * Send the given patch to the central archive
	 */
	void sendPatch(const tagcoll::PatchList<std::string, std::string>& patch);

	/**
	 * Output the current Debian tags database to a consumer of <std::string, std::string>
	 *
	 * \note The collection is sent to 'cons' without merging repeated items
	 */
	template<typename OUT>
	void outputSystem(const OUT& cons);

	/**
	 * Output the given tag file to a consumer of <std::string, std::string>
	 *
	 * \note The collection is sent to 'cons' without merging repeated items
	 */
	template<typename OUT>
	void outputSystem(const std::string& filename, const OUT& out);

	/**
	 * Output the current Debian tags database, patched with local patch,
	 * to a Consumer of <std::string, std::string>
	 *
	 * \note The collection is sent to 'cons' without merging repeated items
	 */
	template<typename OUT>
	void outputPatched(const OUT& cons);

	/**
	 * Output the given tag file, patched with local patch,
	 * to a Consumer of <std::string, std::string>
	 *
	 * \note The collection is sent to 'cons' without merging repeated items
	 */
	template<typename OUT>
	void outputPatched(const std::string& filename, const OUT& out);
};


}
}

// vim:set ts=4 sw=4:
#endif
