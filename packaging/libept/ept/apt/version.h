#ifndef EPT_APT_VERSION_H
#define EPT_APT_VERSION_H

/** \file
 * Representation of a package with a version
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

#include <string>

namespace ept {
namespace apt {

/**
 * Lightweight Version class that represent a package with a version, with very
 * cheap value copy operations.
 *
 * This class can be used to query package information from various information
 * sources.  The purpose is create a middle ground that makes sure that all
 * sort of different information sources about packages are referring to the
 * same package.
 */
class Version
{
protected:
	std::string m_name;
	std::string m_version;

public:
	/**
	 * Create an invalid Version
	 */
	Version() {}

	/**
	 * Create a Version from strings
	 */
	Version(const std::string& name, const std::string& version)
		: m_name(name), m_version(version) {}

	/**
	 * Return the package name
	 */
	std::string name() const { return m_name; }

	/**
	 * Return the package version, or the empty string if this is a
	 * versionless package.
	 */
	std::string version() const { return m_version; }

	/**
	 * Return the upstream part of the version
	 */
	std::string upstreamVersion() const;

	/**
	 * Return true if this package contains a valid value
	 */
	bool isValid() const { return !m_name.empty() && !m_version.empty(); }

	/**
	 * Comparison operators
	 */
	bool operator==(const Version& pkg) const { return m_name == pkg.m_name && m_version == pkg.m_version; }
	bool operator!=(const Version& pkg) const { return m_name != pkg.m_name || m_version != pkg.m_version; }
	bool operator<=(const Version& pkg) const;
	bool operator<(const Version& pkg) const;
	bool operator>=(const Version& pkg) const;
	bool operator>(const Version& pkg) const;
};

}
}

// vim:set ts=4 sw=4:
#endif
