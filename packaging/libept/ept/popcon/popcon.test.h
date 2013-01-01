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

#include <ept/popcon/popcon.h>
#include <ept/popcon/maint/path.h>
#include <ept/apt/apt.h>
#include <set>

#include <ept/test.h>

using namespace std;
using namespace ept;
using namespace ept::popcon;
using namespace ept::apt;

struct TestPopcon
{
	popcon::Path::OverridePopconSourceDir odsd;
	popcon::Path::OverridePopconIndexDir odid;
	popcon::Path::OverridePopconUserSourceDir odusd;
	popcon::Path::OverridePopconUserIndexDir oduid;

	Apt apt;
	Popcon popcon;

	TestPopcon()
		: odsd( TEST_ENV_DIR "popcon" ),
		  odid( TEST_ENV_DIR "popcon" ),
		  odusd( TEST_ENV_DIR "popcon" ),
		  oduid( TEST_ENV_DIR "popcon" )
	{}

	Test basicAccess()
	{
		assert_eq(popcon.submissions(), 52024);
		assert(popcon.size() > 0);
		assert(popcon.score(0) > 0);
		assert(!popcon.name(0).empty());
	}

    // Check that every valid index is accessible
	Test accessibility()
	{
		for (size_t i = 0; i < popcon.size(); ++i)
		{
			//cerr << popcon.name(i) << " " << popcon.score(i) << endl;
			assert(popcon.score(i) > 0);
		}
	}
	
	// Check that we can get a score for every package
	Test haveScores()
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
	Test validScores()
	{
		assert(popcon["apt"] > popcon["libapt-pkg-dev"]);
	}

    // If there is no data, Popcon should work as if all scores were 0
	Test fallbackValues()
	{
		popcon::Path::OverridePopconSourceDir odsd("./empty");
		popcon::Path::OverridePopconIndexDir odid("./empty");
		popcon::Path::OverridePopconUserSourceDir odusd("./empty");
		popcon::Path::OverridePopconUserIndexDir oduid("./empty");
		Popcon empty;
		
		assert_eq(empty.timestamp(), 0);
		assert(!empty.hasData());

		assert_eq(empty.submissions(), 0);
		assert(empty.size() == 0);
		assert(empty.score("apt") == 0.0);
	}

};

// vim:set ts=4 sw=4:
