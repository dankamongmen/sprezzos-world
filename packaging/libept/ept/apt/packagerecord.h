#ifndef EPT_APT_PACKAGERECORD_H
#define EPT_APT_PACKAGERECORD_H

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

#include <ept/apt/recordparser.h>
#include <set>

namespace ept {
namespace apt {

/**
 * RecordParser specialised with access methods for common Debian package
 * information.
 */
class PackageRecord : public RecordParser
{
	bool parseBool(bool& def, const std::string& str) const
	{
		// Believe it or not, this is what apt does to interpret bool fields
		if (str == "no" || str == "false" || str == "without" ||
			str == "off" || str == "disable")
			return false;

		if (str == "yes" || str == "true" || str == "with" ||
			str == "on" || str == "enable")
			return true;

		return def;
	}
	std::string parseString(const std::string& def, const std::string& str) const
	{
		if (str == std::string())
			return def;
		return str;
	}
	std::string parseShortDescription(const std::string& def, const std::string& str) const;
	std::string parseLongDescription(const std::string& def, const std::string& str) const;
	size_t parseSize(size_t def, const std::string& str) const;
	std::set<std::string> parseTags(const std::set<std::string>& def, const std::string& str) const;

public:
	PackageRecord() : RecordParser() {}
	PackageRecord(const std::string& str) : RecordParser(str) {}

	std::string package(const std::string& def = std::string()) const
	{
		return parseString(def, lookup("Package"));
	}
	std::string priority(const std::string& def = std::string()) const
	{
		return parseString(def, lookup("Priority"));
	}
	std::string section(const std::string& def = std::string()) const
	{
		return parseString(def, lookup("Section"));
	}
	size_t installedSize(size_t def = 0) const
	{
		return parseSize(def, lookup("Installed-Size"));
	}
	std::string maintainer(const std::string& def = std::string()) const
	{
		return parseString(def, lookup("Maintainer"));
	}
	std::string architecture(const std::string& def = std::string()) const
	{
		return parseString(def, lookup("Architecture"));
	}
	std::string source(const std::string& def = std::string()) const
	{
		return parseString(def, lookup("Source"));
	}
	std::string version(const std::string& def = std::string()) const
	{
		return parseString(def, lookup("Version"));
	}
	std::string replaces(const std::string& def = std::string()) const
	{
		return parseString(def, lookup("Replaces"));
	}
	std::string depends(const std::string& def = std::string()) const
	{
		return parseString(def, lookup("Depends"));
	}
	std::string preDepends(const std::string& def = std::string()) const
	{
		return parseString(def, lookup("Pre-Depends"));
	}
	std::string recommends(const std::string& def = std::string()) const
	{
		return parseString(def, lookup("Recommends"));
	}
	std::string suggests(const std::string& def = std::string()) const
	{
		return parseString(def, lookup("Suggests"));
	}
	std::string enhances(const std::string& def = std::string()) const
	{
		return parseString(def, lookup("Enhances"));
	}
	std::string provides(const std::string& def = std::string()) const
	{
		return parseString(def, lookup("Provides"));
	}
	std::string conflicts(const std::string& def = std::string()) const
	{
		return parseString(def, lookup("Conflicts"));
	}
	std::string filename(const std::string& def = std::string()) const
	{
		return parseString(def, lookup("Filename"));
	}
	size_t packageSize(size_t def = 0) const
	{
		return parseSize(def, lookup("Size"));
	}
	std::string md5sum(const std::string& def = std::string()) const
	{
		return parseString(def, lookup("MD5sum"));
	}
	std::string sha1(const std::string& def = std::string()) const
	{
		return parseString(def, lookup("SHA1"));
	}
	std::string sha256(const std::string& def = std::string()) const
	{
		return parseString(def, lookup("SHA256"));
	}
	std::string description(const std::string& def = std::string()) const
	{
		return parseString(def, lookup("Description"));
	}
	std::string shortDescription(const std::string& def = std::string()) const
	{
		return parseShortDescription(def, lookup("Description"));
	}
	std::string longDescription(const std::string& def = std::string()) const
	{
		return parseLongDescription(def, lookup("Description"));
	}
	bool buildEssential(bool def = false) const
	{
		return parseBool(def, lookup("Build-Essential"));
	}
	std::set<std::string> tag(const std::set<std::string>& def = std::set<std::string>()) const
	{
		return parseTags(def, lookup("Tag"));
	}
};

}
}

// vim:set ts=4 sw=4:
#endif
