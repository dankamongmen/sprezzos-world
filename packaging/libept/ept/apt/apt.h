// -*- C++ -*-
#ifndef EPT_APT_APT_H
#define EPT_APT_APT_H

/** \file
 * High-level front-end to libapt-pkg, as a data provider for the ept framework.
 */

/*
 * Copyright (C) 2007,2008  Enrico Zini <enrico@enricozini.org>
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

#include <wibble/exception.h>
#include <ept/apt/version.h>

#include <iterator>

class pkgCache;

namespace ept {
namespace apt {

class Exception : public wibble::exception::Generic
{
protected:
	std::string m_message;

public:
	Exception(const std::string& context) throw ();
	~Exception() throw () {}

	virtual const char* type() const throw () { return "Apt"; }
	virtual std::string desc() const throw () { return m_message; }
};

class Apt;
class AptImplementation;
class RecordIteratorImpl;

struct PackageState {
    enum Query {
        Install = 1 << 0,
        Upgrade = 1 << 1,
        Keep = 1 << 2,
        Remove = 1 << 3,
        Installed = 1 << 4,
        Upgradable = 1 << 5,
        NowBroken = 1 << 6,
        WillBreak = 1 << 7,
        ReInstall = 1 << 8,
        Purge = 1 << 9,
        Hold = 1 << 10,
        Valid = 1 << 11
    };

    typedef unsigned state;
    
    operator unsigned() { return m_state; };
    
    PackageState &operator=( unsigned i ) {
        m_state = i;
        return *this;
    }
    
    PackageState &operator|=( const PackageState &s ) {
        m_state |= s.m_state;
        return *this;
    }
    
    PackageState( unsigned a ) {
        m_state = a;
    }
    
    PackageState() : m_state( 0 ) {}

    // FIXME this probably needs to be used consistently in core and out of core
    bool isValid() const { return m_state & Valid; }
    // FIXME compatibility API for non-core apt
    bool isInstalled() const { return installed(); }

    bool install() const { return m_state & Install; }
    // reinstall() implies install()
    bool reinstall() const { return m_state & ReInstall; }
    bool remove() const { return m_state & Remove; }
    // purge() implies remove()
    bool purge() const { return m_state & Purge; }
    bool keep() const { return m_state & Keep; }
    bool willBreak() const { return m_state & WillBreak; }
    // upgrade() implies install()
    bool upgrade() const { return hasNewVersion() && install(); }
    // newInsstal() implies install()
    bool newInstall() const { return !installed() && install(); }
    bool hold() const { return m_state & Hold; }

    bool installed() const { return m_state & Installed; }
    bool hasNewVersion() const { return m_state & Upgradable; }
    bool upgradable() const { return hasNewVersion() && !hold(); }
    bool held() const { return hasNewVersion() && hold(); }
    bool nowBroken() const { return m_state & NowBroken; }

    bool modify() const { return install() || remove(); }
    
protected:
    unsigned m_state;
};

/**
 * High-level access to the Apt cache, as a data provider for the ept
 * framework.
 *
 * This class wraps the Apt cache and allows to query it in various ways.
 */
class Apt
{
protected:
	AptImplementation* impl;

public:
	// Iterate Packages in the Apt cache
	class Iterator : public std::iterator<std::input_iterator_tag, std::string, void, void, void>
	{
		void* cur;

	protected:
		// Construct a valid iterator
		Iterator(void* cur) : cur(cur) {}

		// Construct and end iterator
		Iterator() : cur(0) {}

	public:
		// Copy constructor
		Iterator(const Iterator&);
		~Iterator();
		std::string operator*();
		Iterator& operator++();
		Iterator& operator=(const Iterator&);
		bool operator==(const Iterator&) const;
		bool operator!=(const Iterator&) const;

		// FIXME: Iterator operator++(int); cannot be easily implemented
		// because of how Apt's pkgIterator works

		friend class Apt;
	};

	// Iterate Package records in the Apt cache
	class RecordIterator : public std::iterator<std::input_iterator_tag, std::string, void, void, void>
	{
		RecordIteratorImpl* impl;
		size_t pos;
		std::string cur;
		size_t cur_pos;

	protected:
		// Construct a valid iterator
		RecordIterator(RecordIteratorImpl* cur, size_t pos = 0);

		// Construct and end iterator
		RecordIterator() : impl(0), pos(0), cur_pos(0) {}

	public:
		// Copy constructor
		RecordIterator(const RecordIterator& r);

		~RecordIterator();
		std::string operator*();
		std::string* operator->();
		RecordIterator& operator++();
		RecordIterator& operator=(const RecordIterator& r);
		bool operator==(const RecordIterator&) const;
		bool operator!=(const RecordIterator&) const;

		// FIXME: Iterator operator++(int); cannot be easily implemented
		// because of how Apt's pkgIterator works

		friend class Apt;
	};

	typedef Iterator iterator;
	typedef RecordIterator record_iterator;

	/**
	 * Create the Apt data provider
	 */
	Apt();
	~Apt();


	iterator begin() const;
	iterator end() const;

	record_iterator recordBegin() const;
	record_iterator recordEnd() const;


	/// Return the number of packages in the archive
	size_t size() const;

	/**
	 * Validate a package name, returning trye if it exists in the APT database,
	 * or false if it does not.
	 */
	bool isValid(const std::string& pkg) const;

	/// Validate a package name, returning it if it exists in the APT database,
	/// or returning the empty string if it does not.
	std::string validate(const std::string& pkg) const
	{
		if (isValid(pkg))
			return pkg;
		return std::string();
	}

	/// Validate a Version, returning it if it exists in the APT database, or
	/// returning the invalid version if it does not.
	Version validate(const Version& ver) const;

	/// Return the installed version for a package
	Version installedVersion(const std::string& pkg) const;

	/// Return the candidate version for a package
	Version candidateVersion(const std::string& pkg) const;

	/**
	 * Return the candidate version for a package, if available, or the
	 * installed version otherwise
	 */
	Version anyVersion(const std::string& pkg) const;

	/// Return state information on a package
	PackageState state(const std::string& pkg) const;

	/**
	 * Perform a package search.
	 *
	 * All packages for which the functor filter returns true, are passed to
	 * the functor out.
	 */
	//template<typename FILTER, typename OUT>
	//void search(const FILTER& filter, OUT& out);

	/// Get the raw package record for the given Version
	std::string rawRecord(const std::string& pkg) const;

	/// Get the raw package record for the given Version
	std::string rawRecord(const Version& ver) const;

	/// Returns the pointer to the internal libapt pkgCache object used.
	const pkgCache* aptPkgCache() const;

	
	
	/// Timestamp of when the apt index was last modified
	time_t timestamp();

	/**
	 * Check if the cache has been changed by another process, and reopen it if
	 * that is the case.
	 *
	 * Note that this method can invalidate all existing iterators.
	 */
	void checkCacheUpdates();

	/**
	 * Invalidate the cache timestamp used to track cache updates.
	 *
	 * @warning Do not use this method: it is here only to support the test
	 * cases, and may disappear in any future version.
	 */
	void invalidateTimestamp();
};

}
}

// vim:set ts=4 sw=4:
#endif
