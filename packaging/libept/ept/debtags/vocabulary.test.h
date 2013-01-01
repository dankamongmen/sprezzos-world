/*
 * Tag vocabulary access
 *
 * Copyright (C) 2003--2007  Enrico Zini <enrico@debian.org>
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
#include <ept/debtags/vocabulary.h>
#include <ept/debtags/maint/path.h>
#include <tagcoll/utils/set.h>
#include <tagcoll/input/stdio.h>

#include "ept/test.h"

using namespace std;
using namespace tagcoll::utils;
using namespace ept::debtags;

struct TestVocabulary : DebtagsTestEnvironment
{
	Vocabulary  m_tags;
	Vocabulary& tags() { return m_tags; }

        Test _1()
{
    tags(); // this will throw if the open above didn't work
}

        Test _2()
{
    assert( tags().hasFacet( "works-with" ) );
    assert( !tags().hasFacet( "blah" ) );
}

        Test _3()
{
    assert( tags().hasTag( "works-with::people" ) );
    assert( !tags().hasTag( "works-with::midgets" ) );
}

        Test _4()
{
    const voc::TagData *people = tags().tagData( "works-with::people" ),
                       *midgets = tags().tagData( "works-with::midgets" ),
                       *blahg = tags().tagData( "works-with::blahg" ),
                       *text = tags().tagData( "works-with::text" ),
                       *people2 = tags().tagData( "works-with::people" );
    assert( people != midgets );
    assert( people != text );
    assert( people != blahg );
    assert( midgets == blahg );
    assert( midgets == midgets );
    assert( people == people2 );
    assert( people == people );
}

        Test _5()
{
    std::string a = "works-with::people",
                b = "works-with::midgets";
    std::set<std::string> s = tags().tags(),
                          f = tags().tags( "works-with" ),
                          n = tags().tags( "nonsense" );
    assert( set_contains(s, a) );
    assert( set_contains(f, a) );
    assert( set_contains(s, f) );
    assert( !set_contains(s, b) );
    assert( !set_contains(f, b) );
    assert( n.empty() );
}

        Test _6()
{
	const voc::FacetData* f = tags().facetData( "works-with" );
	assert(f);
	assert_eq(f->name, "works-with");

	const voc::TagData* t = tags().tagData( "works-with::people" );
	assert(t);
	assert_eq(t->name, "works-with::people");
}

        Test _7()
{
	const voc::FacetData* f = tags().facetData( "works-with" );
	std::set<std::string> x = tags().tags( "works-with" );
	assert( x == f->tags() );
}

        Test _8()
{
    const voc::FacetData* f = tags().facetData( "does-not-work-with" );
    assert(!f);
}

        Test _9()
{
    const voc::FacetData* f = tags().facetData( "legacy" );
    assert(f);
    assert_eq(f->shortDescription(), "");
    assert_eq(f->longDescription(), "");
    //assert_eq(f.shortDescription( "weehee" ), "weehee");
}

        Test _10()
{
	// assert that one-character tag names are parsed correctly
	assert( tags().hasTag( "implemented-in::c" ) );
}

        Test _11()
{
	// assert that all facets are somehow working
	std::set<std::string> facets = tags().facets();

	for (std::set<std::string>::const_iterator i = facets.begin();
			i != facets.end(); i++)
	{
		const voc::FacetData* f = tags().facetData(*i);
		assert(f);
	}
}

        Test _12()
{
	// assert that all tags are somehow working
	std::set<std::string> tags = this->tags().tags();
	for (std::set<std::string>::const_iterator i = tags.begin();
			i != tags.end(); i++)
	{
		const voc::TagData* t = this->tags().tagData(*i);
		assert(t);
	}
}

// Check for correctness of the first and last tag in the vocabulary
        Test _13()
{
	Vocabulary& tags = this->tags();

	const voc::TagData* first = tags.tagData("accessibility::TODO");
	assert(first);
	assert_eq(first->name, string("accessibility::TODO"));
	assert_eq(first->shortDescription(), string("Need an extra tag"));

	const voc::TagData* last = tags.tagData("x11::xserver");
	assert(last);
	assert_eq(last->name, string("x11::xserver"));
	assert_eq(last->shortDescription(), string("X Server"));
}

        Test _14()
{
	// assert that it's possible to go from facet to ID and back
	// we don't use IDs anymore
}

        Test _15()
{
	// assert that it's possible to go from tag to ID and back
	// we don't use IDs anymore
}

        Test _16()
{
	// assert that facet IDs are distinct
	// we don't use IDs anymore
}

        Test _17()
{
	// assert that tag IDs are distinct
	// we don't use IDs anymore
}

        Test _18()
{
	// assert that all the tags are indexed
	// we don't use the index anymore
}

        Test _19()
{
	// test the tagcmp function
	// we don't have tagcmp anymore
}

        Test _20()
{
	// check that we're seeing all the tags for a facet
	std::set<std::string> t = tags().tags("accessibility");
	assert_eq(t.size(), 10u);

	t = tags().tags("works-with-format");
	assert_eq(t.size(), 33u);
}

// If there is no data, Vocabulary should work as an empty vocabulary
        Test _21()
{
	Path::OverrideDebtagsSourceDir odsd("./empty");
	Path::OverrideDebtagsIndexDir odid("./empty");
	Path::OverrideDebtagsUserSourceDir odusd("./empty");
	Path::OverrideDebtagsUserIndexDir oduid("./empty");
	Vocabulary empty;

	assert(!empty.hasData());

	set<std::string> facets = empty.facets();
	assert_eq(facets.size(), 0u);

	set<std::string> tags = empty.tags();
	assert_eq(tags.size(), 0u);
}

};

// vim:set ts=4 sw=4:
