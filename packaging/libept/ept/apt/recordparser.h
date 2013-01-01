#ifndef EPT_APT_RECORDPARSER_H
#define EPT_APT_RECORDPARSER_H

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

#include <vector>
#include <string>

namespace ept {
namespace apt {

/**
 * Access the fields of a package record contained inside a std::string.
 *
 * Implementation note: this implementation should take advantage of
 * std::string sharing buffer space among them.
 */
class RecordParser
{
	/// Buffer containing the whole record
	std::string buffer;

	/// End offsets of the various fields in the record
	std::vector<size_t> ends;

	/// Indexes on the ends vector, sorted by field name
	std::vector<size_t> sorted;

public:
	RecordParser() {}
	RecordParser(const std::string& str) { scan(str); }

	/// Index a new record
	void scan(const std::string& str);

	/**
	 * Get the index of the field with the given name.
	 *
	 * size() is returned if not found
	 */
	size_t index(const std::string& str) const;

	/// Return the field by its index
	std::string field(size_t idx) const;

	/// Return the name of a field by its index
	std::string name(size_t idx) const;

	/// Return the content of a field by its index
	std::string lookup(size_t idx) const;

	/// Return the content of a field by its name
	std::string lookup(const std::string& name) const { return lookup(index(name)); }

	/// Return the content of a field by its index
	std::string operator[](size_t idx) const { return lookup(idx); }

	/// Return the content of a field by its name
	std::string operator[](const std::string& name) const { return lookup(name); }

	/// Return the entire record
	const std::string& record() const { return buffer; }

	/// Return the entire record
	std::string record() { return buffer; }

	/// Return the number of fields in the record
	size_t size() const { return ends.size(); }
};

}
}

// vim:set ts=4 sw=4:
#endif
