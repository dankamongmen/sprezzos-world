// -*- mode: c++; indent-tabs-mode: t -*-
/** \file
 * debtags paths
 */

/*
 * Copyright (C) 2005,2006,2007  Enrico Zini <enrico@debian.org>
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

struct stat;

#ifndef EPT_DEBTAGS_PATH_H
#define EPT_DEBTAGS_PATH_H

namespace ept {
namespace debtags {

/**
 * Singleton class to configure and access the various Debtags paths
 */
class Path
{
public:
	static std::string vocabulary();
	static std::string userVocabulary();
	static std::string tagdb();
	static std::string userTagdb();

	static std::string debtagsSourceDir();
	static std::string debtagsIndexDir();
	static std::string debtagsUserSourceDir();
	static std::string debtagsUserIndexDir();

	// Directory where Debtags source data is found
	static void setDebtagsSourceDir( const std::string &s );

	// Directory where Debtags indexes are kept
	static void setDebtagsIndexDir( const std::string &s );

	// User-specific directory for Debtags source data
	static void setDebtagsUserSourceDir( const std::string &s );

	// User-specific directory for Debtags index data
	static void setDebtagsUserIndexDir( const std::string &s );

	static int access( const std::string &, int );
	static time_t timestamp( const std::string& );

	// RAII-style classes to temporarily override directories
	class OverrideDebtagsSourceDir
	{
		std::string old;
	public:
		OverrideDebtagsSourceDir(const std::string& path) : old(Path::debtagsSourceDir())
		{
			Path::setDebtagsSourceDir(path);
		}
		~OverrideDebtagsSourceDir() { Path::setDebtagsSourceDir(old); }
	};
	class OverrideDebtagsIndexDir
	{
		std::string old;
	public:
		OverrideDebtagsIndexDir(const std::string& path) : old(Path::debtagsIndexDir())
		{
			Path::setDebtagsIndexDir(path);
		}
		~OverrideDebtagsIndexDir() { Path::setDebtagsIndexDir(old); }
	};
	class OverrideDebtagsUserSourceDir
	{
		std::string old;
	public:
		OverrideDebtagsUserSourceDir(const std::string& path) : old(Path::debtagsUserSourceDir())
		{
			Path::setDebtagsUserSourceDir(path);
		}
		~OverrideDebtagsUserSourceDir() { Path::setDebtagsUserSourceDir(old); }
	};
	class OverrideDebtagsUserIndexDir
	{
		std::string old;
	public:
		OverrideDebtagsUserIndexDir(const std::string& path) : old(Path::debtagsUserIndexDir())
		{
			Path::setDebtagsUserIndexDir(path);
		}
		~OverrideDebtagsUserIndexDir() { Path::setDebtagsUserIndexDir(old); }
	};
protected:
	static Path *s_instance;
	static Path &instance();

	// Directory where Debtags source data is found
	std::string m_debtagsSourceDir;

	// Directory where Debtags indexes are kept
	std::string m_debtagsIndexDir;

	// User-specific directory for Debtags source data
	std::string m_debtagsUserSourceDir;

	// User-specific directory for Debtags index data
	std::string m_debtagsUserIndexDir;
};

}
}

// vim:set ts=4 sw=4:
#endif
