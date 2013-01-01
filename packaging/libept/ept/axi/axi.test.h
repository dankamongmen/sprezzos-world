// -*- mode: c++; tab-width: 4; indent-tabs-mode: t -*-
/*
 * popcon test
 *
 * Copyright (C) 2007  Enrico Zini <enrico@debian.org>
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

#include <ept/test.h>
#include <ept/axi/axi.h>
#include <ept/apt/apt.h>
#include <wibble/sys/fs.h>
#include <set>

using namespace std;
using namespace ept;

struct DirMaker
{
	DirMaker(const std::string& name)
	{
		wibble::sys::fs::mkdirIfMissing(name, 0755);
	}
};

struct TestAxi : AptTestEnvironment
{
	DirMaker md;
	axi::OverrideIndexDir oid;
	apt::Apt apt;

	TestAxi()
		: md( TEST_ENV_DIR "xapian"), oid( TEST_ENV_DIR "xapian")
	{
	}

// Access an empty index
	Test empty()
	{
		axi::OverrideIndexDir oid("./empty");
		assert_eq(axi::timestamp(), 0);
	}
};

// vim:set ts=4 sw=4:
