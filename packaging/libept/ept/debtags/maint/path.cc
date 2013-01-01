// -*- mode: c++; indent-tabs-mode: t -*-

/** \file
 * debtags paths
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

#include <ept/debtags/maint/path.h>
#include <ept/config.h>

#include <wibble/sys/fs.h>
#include <wibble/string.h>

#include <sys/types.h>	// getpwuid, stat, mkdir, getuid
#include <sys/stat.h>	// stat, mkdir
#include <pwd.h>		// getpwuid
#include <unistd.h>		// stat, getuid

using namespace wibble;

namespace ept {
namespace debtags {

static std::string userdir()
{
	std::string rcdir;

	struct passwd* udata = getpwuid(getuid());
	rcdir = str::joinpath(udata->pw_dir, ".debtags");

	return rcdir;
}


Path &Path::instance() {
	if (!s_instance) {
		s_instance = new Path;
		instance().m_debtagsSourceDir = DEBTAGS_DB_DIR;
		instance().m_debtagsIndexDir = DEBTAGS_DB_DIR;
		instance().m_debtagsUserSourceDir = userdir();
		instance().m_debtagsUserIndexDir = userdir();
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

void Path::setDebtagsSourceDir( const std::string &s )
{
	instance().m_debtagsSourceDir = s;
}
void Path::setDebtagsIndexDir( const std::string &s )
{
	instance().m_debtagsIndexDir = s;
}
void Path::setDebtagsUserSourceDir( const std::string &s )
{
	instance().m_debtagsUserSourceDir = s;
}
void Path::setDebtagsUserIndexDir( const std::string &s )
{
	instance().m_debtagsUserIndexDir = s;
}

std::string Path::debtagsSourceDir() { return instance().m_debtagsSourceDir; }
std::string Path::debtagsIndexDir() { return instance().m_debtagsIndexDir; }
std::string Path::debtagsUserSourceDir() { return instance().m_debtagsUserSourceDir; }
std::string Path::debtagsUserIndexDir() { return instance().m_debtagsUserIndexDir; }

std::string Path::vocabulary() {
	return str::joinpath(debtagsIndexDir(), "vocabulary");
}

std::string Path::userVocabulary() {
	return str::joinpath(debtagsUserIndexDir(), "vocabulary");
}

std::string Path::tagdb() {
	return str::joinpath(debtagsIndexDir(), "package-tags");
}

std::string Path::userTagdb() {
	return str::joinpath(debtagsUserIndexDir(), "package-tags");
}

Path *Path::s_instance = 0;

}
}

// vim:set ts=4 sw=4:
