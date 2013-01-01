// -*- mode: c++; tab-width: 4; indent-tabs-mode: t -*-
/*
 * popcon/local test
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

#include <ept/popcon/local.h>
#include <ept/popcon/popcon.h>
#include <ept/popcon/maint/path.h>

#include <ept/test.h>

using namespace std;
using namespace ept;
using namespace ept::popcon;

struct TestPopconLocal
{
	Path::OverridePopconSourceDir odsd;
	Path::OverridePopconIndexDir odid;
	Path::OverridePopconUserSourceDir odusd;
	Path::OverridePopconUserIndexDir oduid;

	Popcon popcon;
	Local local;

	TestPopconLocal()
		: odsd( TEST_ENV_DIR "popcon" ),
		  odid( TEST_ENV_DIR "popcon" ),
		  odusd( TEST_ENV_DIR "popcon" ),
		  oduid( TEST_ENV_DIR "popcon" ),
		  local( TEST_ENV_DIR "popcon/popularity-contest" )
	{}

    // Very basic access
	Test basicAccess()
	{
		assert(local.score("apt") > 0);
		assert(local.tfidf(popcon, "apt") > 0);
	}

#if 0 // mornfall: apparently left out by enrico, leaving as it is
// Check that every valid index is accessible
template<> template<>
void to::test< 2 >()
{
	for (size_t i = 0; i < popcon.size(); ++i)
	{
		//cerr << popcon.name(i) << " " << popcon.score(i) << endl;
		assert(popcon.score(i) > 0);
	}
}

// Check that we can get a score for every package
template<> template<>
void to::test< 3 >()
{
	int has = 0;
	for (Apt::iterator i = apt.begin(); i != apt.end(); ++i)
	{
		float score = popcon.score(*i);
		if (score > 0)
			++has;
	}
	// At least 1000 packages should have a score
	assert(has > 1000);
}

// Check that scores are meaningful
template<> template<>
void to::test< 4 >()
{
	assert(popcon["apt"] > popcon["libapt-pkg-dev"]);
}

// If there is no data, Popcon should work as if all scores were 0
template<> template<>
void to::test<5>()
{
	Path::OverridePopconSourceDir odsd("./empty");
	Path::OverridePopconIndexDir odid("./empty");
	Path::OverridePopconUserSourceDir odusd("./empty");
	Path::OverridePopconUserIndexDir oduid("./empty");
	Popcon empty;

	assert_eq(empty.timestamp(), 0);
	assert(!empty.hasData());

	assert(empty.size() == 0);
	assert(empty.score("apt") == 0.0);
}
#endif

};

// vim:set ts=4 sw=4:
