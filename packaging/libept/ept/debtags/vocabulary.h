#ifndef EPT_DEBTAGS_VOCABULARY_H
#define EPT_DEBTAGS_VOCABULARY_H

/** @file
 * @author Enrico Zini <enrico@enricozini.org>
 * Debtags vocabulary access
 */

/*
 * Copyright (C) 2003,2004,2005,2006,2007  Enrico Zini <enrico@debian.org>
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
#include <set>
#include <map>

namespace tagcoll {
namespace input {
struct Input;
}
}

namespace ept {
namespace debtags {
namespace voc {

/// Extract the facet name from a tag name
std::string getfacet(const std::string& tagname);

/// Base class for facet and tag data
struct Data : public std::map<std::string, std::string>
{
protected:
	// Cache the parsed short description
	mutable std::string m_desc;

public:
	std::string name;

	/**
	 * Return the short description of the tag
	 * @throws std::out_of_range if the tag is not valid
	 */
	std::string shortDescription() const;

	/**
	 * Return the long description of the tag
	 *
	 * @throws std::out_of_range if the tag is not valid
	 */
	std::string longDescription() const;
};

/**
 * Representation of a tag.
 *
 * ept::debtags::Tag represents a Tag with all its informations.
 * It is guaranteed to have fast value-copy semantics, so it can be passed
 * around freely and efficiently without worrying about memory management
 * issues.
 *
 * The class is normally instantiated using a Vocabulary:
 * \code
 * 	Tag tag = vocabulary.tagByName("made-of::lang:c++");
 * \endcode
 *
 * Tags can contain an "invalid" value, in which case using any of their
 * methods will likely produce segfault.  The "invalid" facets are useful as
 * "none" return values:
 *
 * \code
 *    Tag tag = vocabulary.tagByName("made-of");
 *    if (!tag)
 *       throw SomeException("tag \"mytag\" has not been defined");
 * \endcode
 */
struct TagData : public Data
{
	TagData() {}

	// Facet facet() const;
};

/**
 * Representation of a facet.
 *
 * ept::debtags::Facet represents a Facet with all its informations.
 * It is guaranteed to have fast value-copy semantics, so it can be passed
 * around freely and efficiently without worrying about memory management
 * issues.
 *
 * The class is normally instantiated using a Vocabulary:
 * \code
 * 	Facet facet = vocabulary.faceByName("made-of");
 * \endcode
 *
 * Facets can contain an "invalid" value, in which case using any of their
 * methods will likely produce segfault.  The "invalid" facets are useful as
 * "none" return values:
 *
 * \code
 *    Facet facet = vocabulary.facetByName("made-of");
 *    if (!facet)
 *       throw SomeException("facet \"made-of\" has not been defined");
 * \endcode
 */
class FacetData : public Data
{
public:
	std::map<std::string, TagData> m_tags;

	FacetData() {}

	TagData& obtainTag(const std::string& fullname);

	/**
	 * Return true if the facet has a tag with the given name (name, not fullname)
	 */
	bool hasTag(const std::string& name) const;

	/**
	 * Return the tag data for the given tag, or 0 if not found
	 */
	const TagData* tagData(const std::string& name) const;

	/**
	 * Return the list of tags in this facet
	 */
	std::set<std::string> tags() const;
};

}

class Vocabulary
{
protected:
	std::map<std::string, voc::FacetData> m_facets;

	time_t m_timestamp;

	// Empty parsed data to return when data is asked for IDs == -1
	std::map<std::string, std::string> emptyData;

	void parseVocBuf(std::map<std::string, std::string>& res, size_t ofs, size_t len) const;

	voc::FacetData& obtainFacet(const std::string& name);
	voc::TagData& obtainTag(const std::string& fullname);
	
public:
	/**
	 * Instantiate the Debtags vocabulary
	 *
	 * @param empty
	 *   false if the Debtags vocabulary should be loaded,
	 *   true if it should start as an empty vocabulary
	 */
	Vocabulary(bool empty=false);
	~Vocabulary();

	/// Get the timestamp of when the index was last updated
	time_t timestamp() const { return m_timestamp; }

	/// Return true if this data source has data, false if it's empty
	bool hasData() const { return m_timestamp != 0; }

	/**
	 * Check if there is any data in the merged vocabulary
	 */
	bool empty() const { return m_facets.empty(); }

	/**
	 * Check if the vocabulary contains the facet `name'
	 */
	bool hasFacet(const std::string& name) const;

	/**
	 * Check if the vocabulary contains the tag `fullname'
	 */
	bool hasTag(const std::string& name) const;

	/**
	 * Return the facet with the given name
	 */
	const voc::FacetData* facetData(const std::string& name) const;

	/**
	 * Return the tag with the given full name
	 */
	const voc::TagData* tagData(const std::string& fullname) const;

	/**
	 * Return all the facets in the vocabulary
	 */
	std::set<std::string> facets() const;

	/**
	 * Return all the tags in the vocabulary
	 */
	std::set<std::string> tags() const;

	/**
	 * Return the tags in the given facet
	 */
	std::set<std::string> tags(const std::string& facet) const;

#if 0
	/// Get the DerivedTagList with the Equates: expressions read from the vocabulary
	const DerivedTagList& getEquations() const throw () { return equations; }
	
	/// Get a set with all the facets present in the vocabulary that are matched by `filter'
	FacetSet facets(const FacetMatcher& filter) const throw () { return getFiltered(filter); }
#endif

	/**
	 * Parse and import the vocabulary from `input', merging the data with the
	 * previously imported ones
	 */
	void read(tagcoll::input::Input& input);

	/**
	 * Atomically update the system vocabulary
	 */
	void write();

	/**
	 * Atomically write the vocabulary data to the given file
	 */
	void write(const std::string& fname);

	/**
	 * Write the vocabulary data to the given output stream
	 */
	void write(FILE* out);
};

}
}

// vim:set ts=4 sw=4:
#endif
