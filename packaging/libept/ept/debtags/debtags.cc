/** -*- C++ -*-
 * @file
 * @author Enrico Zini (enrico) <enrico@enricozini.org>
 */

/*
 * System tag database
 *
 * Copyright (C) 2003-2008  Enrico Zini <enrico@debian.org>
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

#include <ept/debtags/debtags.h>
#include <ept/debtags/maint/path.h>
#include <ept/debtags/maint/sourcedir.h>

#include <tagcoll/patch.h>
#include <tagcoll/coll/simple.h>
#include <tagcoll/input/stdio.h>
#include <tagcoll/TextFormat.h>

#include <wibble/sys/fs.h>
#include <wibble/string.h>

#include <iostream>
#include <sstream>

#include <sys/wait.h>	// WIFEXITED WEXITSTATUS
#include <sys/types.h>	// getpwuid, stat, mkdir, getuid
#include <sys/stat.h>	// stat, mkdir
#include <pwd.h>	// getpwuid
#include <unistd.h>	// stat, getuid


using namespace std;
using namespace tagcoll;
using namespace wibble;

namespace ept {
namespace debtags {

Debtags::Debtags(bool editable)
{
	// Read and merge tag data
	SourceDir mainSource(Path::debtagsSourceDir());
	SourceDir userSource(Path::debtagsUserSourceDir());

	mainSource.readTags(inserter(*this));
	userSource.readTags(inserter(*this));

	time_t ts_main_src = mainSource.tagTimestamp();
        time_t ts_user_src = userSource.tagTimestamp();
        m_timestamp = ts_main_src > ts_user_src ? ts_main_src : ts_user_src;

	// Initialize the patch collection layer
	rcdir = Path::debtagsUserSourceDir();

	string patchFile = str::joinpath(rcdir, "patch");
	if (Path::access(patchFile, F_OK) == 0)
	{
		input::Stdio in(patchFile);
		PatchList<string, string> patch;
		textformat::parsePatch(in, inserter(patch));
		applyChange(patch);
	}
}

tagcoll::PatchList<std::string, std::string> Debtags::changes() const
{
	// Read original tag data
	SourceDir mainSource(Path::debtagsSourceDir());
	SourceDir userSource(Path::debtagsUserSourceDir());
	coll::Simple<string, string> orig;
	mainSource.readTags(inserter(orig));
	userSource.readTags(inserter(orig));

	tagcoll::PatchList<std::string, std::string> res;
	res.addPatch(orig, *this);
	return res;
}


#if 0
bool Debtags::hasTagDatabase()
{
	if (Path::access(Path::tagdb(), R_OK) == -1)
	{
		std::cerr << "Missing tag database " << Path::tagdb() << std::endl;
		return false;
	}
	if (Path::access(Path::tagdbIndex(), R_OK) == -1)
	{
		std::cerr << "Missing tag database index " << Path::tagdbIndex() << std::endl;
		return false;
	}
	if (Path::access(Path::vocabulary(), R_OK) == -1)
	{
		std::cerr << "Missing tag vocabulary " << Path::vocabulary() << std::endl;
		return false;
	}
	if (Path::access(Path::vocabularyIndex(), R_OK) == -1)
	{
		std::cerr << "Missing index for tag vocabulary " << Path::vocabularyIndex() << std::endl;
		return false;
	}
	return true;
}
#endif


void Debtags::savePatch()
{
	PatchList<std::string, std::string> spatch;
	changes().output(tagcoll::inserter(spatch));
	savePatch(spatch);
}

void Debtags::savePatch(const PatchList<std::string, std::string>& patch)
{
	std::string patchFile = str::joinpath(rcdir, "patch");
	std::string backup = patchFile + "~";

	wibble::sys::fs::mkFilePath(patchFile);

	if (access(patchFile.c_str(), F_OK) == 0)
		if (rename(patchFile.c_str(), backup.c_str()) == -1)
			throw wibble::exception::System("Can't rename " + patchFile + " to " + backup);

	try {
		FILE* out = fopen(patchFile.c_str(), "w");
		if (out == 0)
			throw wibble::exception::System("Can't write to " + patchFile);

		textformat::outputPatch(patch, out);

		fclose(out);
	} catch (std::exception& e) {
		if (rename(backup.c_str(), patchFile.c_str()) == -1)
            std::cerr << "Warning: Cannot restore previous backup copy: " << e.what() << std::endl;
		throw;
	}
}

void Debtags::sendPatch()
{
	PatchList<std::string, std::string> spatch;
	changes().output(tagcoll::inserter(spatch));
	if (!spatch.empty())
	{
		sendPatch(spatch);
	}
}

void Debtags::sendPatch(const PatchList<std::string, std::string>& patch)
{
	static const char* cmd = "/usr/sbin/sendmail -t";
	FILE* out = popen(cmd, "w");
	if (out == 0)
		throw wibble::exception::System(std::string("trying to run `") + cmd + "'");

	struct passwd* udata = getpwuid(getuid());

	fprintf(out,
			"To: enrico-debtags@debian.org\n"
			"Bcc: %s\n"
			"Subject: Tag patch\n"
			"Mime-Version: 1.0\n"
			"Content-Type: multipart/mixed; boundary=\"9amGYk9869ThD9tj\"\n"
			"Content-Disposition: inline\n"
			"X-Mailer: debtags-edit\n\n"
			"This mail contains a Debtags patch for the central archive\n\n"
			"--9amGYk9869ThD9tj\n"
			"Content-Type: text/plain; charset=utf-8\n"
			"Content-Disposition: inline\n\n"
			"-- DEBTAGS DIFF V0.1 --\n", udata->pw_name);

	textformat::outputPatch(patch, out);

	fprintf(out, "\n--9amGYk9869ThD9tj\n");

	int res = pclose(out);
	if (!WIFEXITED(res) || WEXITSTATUS(res) != 0)
	{
		std::stringstream str;
		str << res;
		throw wibble::exception::Consistency("checking mailer exit status", "sendmail returned nonzero (" + str.str() + "): the mail may have not been sent");
	}
}

}
}

#include <ept/debtags/maint/sourcedir.tcc>
#include <tagcoll/patch.tcc>
#include <tagcoll/coll/simple.tcc>
#include <tagcoll/coll/fast.tcc>
#include <tagcoll/TextFormat.tcc>
//#include <tagcoll/stream/filters.tcc>

// vim:set ts=4 sw=4:
