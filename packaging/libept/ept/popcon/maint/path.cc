// -*- mode: c++; indent-tabs-mode: t -*-

/** \file
 * popcon paths
 */

/*
 * Copyright (C) 2005,2006,2007  Enrico Zini <enrico@debian.org>, Peter Rockai <me@mornfall.net>
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

#include <ept/config.h>
#include <ept/popcon/maint/path.h>

#include <wibble/sys/fs.h>
#include <wibble/string.h>

#include <sys/types.h>	// getpwuid, stat, mkdir, getuid
#include <sys/stat.h>	// stat, mkdir
#include <pwd.h>		// getpwuid
#include <unistd.h>		// stat, getuid

using namespace wibble;

namespace ept {
namespace popcon {

static std::string userdir()
{
	std::string rcdir;

	struct passwd* udata = getpwuid(getuid());
	rcdir = str::joinpath(udata->pw_dir, ".popcon");

	return rcdir;
}


Path &Path::instance() {
	if (!s_instance) {
		s_instance = new Path;
		instance().m_popconSourceDir = POPCON_DB_DIR;
		instance().m_popconIndexDir = POPCON_DB_DIR;
		instance().m_popconUserSourceDir = userdir();
		instance().m_popconUserIndexDir = userdir();
	}
	return *s_instance;
}

int Path::access( const std::string &s, int m ) {
	return ::access( s.c_str(), m );
}

time_t Path::timestamp( const std::string& file ) {
	std::auto_ptr<struct stat> st = wibble::sys::fs::stat(file);
	return st.get() == NULL ? 0 : st->st_mtime;
}

void Path::setPopconSourceDir( const std::string &s )
{
	instance().m_popconSourceDir = s;
}
void Path::setPopconIndexDir( const std::string &s )
{
	instance().m_popconIndexDir = s;
}
void Path::setPopconUserSourceDir( const std::string &s )
{
	instance().m_popconUserSourceDir = s;
}
void Path::setPopconUserIndexDir( const std::string &s )
{
	instance().m_popconUserIndexDir = s;
}

std::string Path::popconSourceDir() { return instance().m_popconSourceDir; }
std::string Path::popconIndexDir() { return instance().m_popconIndexDir; }
std::string Path::popconUserSourceDir() { return instance().m_popconUserSourceDir; }
std::string Path::popconUserIndexDir() { return instance().m_popconUserIndexDir; }

std::string Path::scores() {
	return str::joinpath(popconIndexDir(), "scores");
}

std::string Path::scoresIndex() {
	return str::joinpath(popconIndexDir(), "scores.idx");
}

std::string Path::userScores() {
	return str::joinpath(popconUserIndexDir(), "scores");
}

std::string Path::userScoresIndex() {
	return str::joinpath(popconUserIndexDir(), "scores.idx");
}

Path *Path::s_instance = 0;

}
}

// vim:set ts=4 sw=4:
