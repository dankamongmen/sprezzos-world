/*
 * Match tag expressions against sets of Debtags Tags
 *
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

#include <wibble/test.h>
#include <ept/debtags/maint/path.h>
#include <tagcoll/expression.h>
#include <ept/debtags/vocabulary.h>

#include "debtags.test.h"

using namespace tagcoll;
using namespace std;
using namespace ept::debtags;

struct TestExpression : DebtagsTestEnvironment {
	Vocabulary voc;

        Test _1()
{
	set<std::string> test;
	test.insert("use::editing");
	test.insert("use::viewing");
	test.insert("works-with::text");

	assert_eq(test.size(), 3u);

	Expression e1("use::editing");
	assert(e1(test));

	Expression e2("use::editing && use::viewing");
	assert(e2(test));

	e1 = Expression("!use::editing");
	assert(!e1(test));

	e1 = Expression("use::editing || sugo");
	assert(e1(test));

	e1 = Expression("use::editing && !sugo");
	assert(e1(test));

	e1 = Expression("use::editing && !use::viewing");
	assert(!e1(test));

	e1 = Expression("(use::editing || sugo) && (use::viewing && works-with::text)");
	assert(e1(test));

	e1 = Expression("!(use::editinuse::editingra && works-with::text)");
	assert(e1(test));

	e1 = Expression("works-with::*");
	assert(e1(test));

	e1 = Expression("*::text");
	assert(e1(test));

	e1 = Expression("!*::antani");
	assert(e1(test));

	e1 = Expression("*::antani");
	assert(!e1(test));
}

};

// vim:set ts=4 sw=4:
