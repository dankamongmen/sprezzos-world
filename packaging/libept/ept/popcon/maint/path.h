// -*- mode: c++; indent-tabs-mode: t -*-
/** \file
 * popcon paths
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

#ifndef EPT_POPCON_PATH_H
#define EPT_POPCON_PATH_H

#include <string>

struct stat;

namespace ept {
namespace popcon {

/**
 * Singleton class to configure and access the various Popcon paths
 */
class Path
{
public:
	static std::string scores();
	static std::string scoresIndex();
	static std::string userScores();
	static std::string userScoresIndex();

	static std::string popconSourceDir();
	static std::string popconIndexDir();
	static std::string popconUserSourceDir();
	static std::string popconUserIndexDir();

	// Directory where Popcon source data is found
	static void setPopconSourceDir( const std::string &s );

	// Directory where Popcon indexes are kept
	static void setPopconIndexDir( const std::string &s );

	// User-specific directory for Popcon source data
	static void setPopconUserSourceDir( const std::string &s );

	// User-specific directory for Popcon index data
	static void setPopconUserIndexDir( const std::string &s );

	static int access( const std::string &, int );
	static time_t timestamp( const std::string& );

	// RAII-style classes to temporarily override directories
	class OverridePopconSourceDir
	{
		std::string old;
	public:
		OverridePopconSourceDir(const std::string& path) : old(Path::popconSourceDir())
		{
			Path::setPopconSourceDir(path);
		}
		~OverridePopconSourceDir() { Path::setPopconSourceDir(old); }
	};
	class OverridePopconIndexDir
	{
		std::string old;
	public:
		OverridePopconIndexDir(const std::string& path) : old(Path::popconIndexDir())
		{
			Path::setPopconIndexDir(path);
		}
		~OverridePopconIndexDir() { Path::setPopconIndexDir(old); }
	};
	class OverridePopconUserSourceDir
	{
		std::string old;
	public:
		OverridePopconUserSourceDir(const std::string& path) : old(Path::popconUserSourceDir())
		{
			Path::setPopconUserSourceDir(path);
		}
		~OverridePopconUserSourceDir() { Path::setPopconUserSourceDir(old); }
	};
	class OverridePopconUserIndexDir
	{
		std::string old;
	public:
		OverridePopconUserIndexDir(const std::string& path) : old(Path::popconUserIndexDir())
		{
			Path::setPopconUserIndexDir(path);
		}
		~OverridePopconUserIndexDir() { Path::setPopconUserIndexDir(old); }
	};
protected:
	static Path *s_instance;
	static Path &instance();

	// Directory where Popcon source data is found
	std::string m_popconSourceDir;

	// Directory where Popcon indexes are kept
	std::string m_popconIndexDir;

	// User-specific directory for Popcon source data
	std::string m_popconUserSourceDir;

	// User-specific directory for Popcon index data
	std::string m_popconUserIndexDir;
};

}
}

// vim:set ts=4 sw=4:
#endif
