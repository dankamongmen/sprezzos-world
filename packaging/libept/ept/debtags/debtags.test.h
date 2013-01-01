// -*- mode: c++; tab-width: 4; indent-tabs-mode: t -*-
/**
 * @file
 * @author Enrico Zini (enrico) <enrico@enricozini.org>
 */

/*
 * Test for the Debtags data provider
 *
 * Copyright (C) 2003-2007  Enrico Zini <enrico@debian.org>
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

#include <tagcoll/coll/simple.h>
#include <tagcoll/stream/sink.h>
#include <tagcoll/patch.h>

#include <wibble/operators.h>

#include <ept/test.h>
#include <cstdio>

#ifndef EPT_DEBTAGS_TESTH
#define EPT_DEBTAGS_TESTH

using namespace tagcoll;
using namespace std;
using namespace ept;
using namespace ept::debtags;
using namespace wibble::operators;

struct TestDebtags : DebtagsTestEnvironment
{
	Debtags debtags;

    TestDebtags() {}

	Test _1() {
		for (Debtags::const_iterator i = debtags.begin(); i != debtags.end(); ++i)
		{
			*i;
			i->first;
			i->second;
		}
		int items = 0, tags = 0;
		debtags.outputSystem(stream::countingSink(items, tags));
		
		int pitems = 0, ptags = 0;
	debtags.outputPatched(stream::countingSink(pitems, ptags));

	assert(items > 10);
	assert(tags > 10);
	assert(items <= pitems);
	assert(tags <= ptags);
}

	Test _2()
{
	string p("debtags");
	std::set<std::string> tags = debtags.getTagsOfItem(p);
	assert( !tags.empty() );

#if 0
	for ( std::set< Tag >::iterator i = tags.begin(); i != tags.end(); ++ i ) {
		std::cerr << i->id() << ": " << i->fullname() << std::endl;
	}
	std::cerr << "---" << std::endl;
	Tag t = voc().tagByName( "interface::commandline" );
	std::cerr << t.id() << ": " << t.fullname() << std::endl;
#endif

	assert_eq( tags.size(), 8u );
	assert( tags.find("devel::buildtools") != tags.end() );
	assert( tags.find("implemented-in::c++") != tags.end() );
	assert( tags.find("interface::commandline") != tags.end() );
	assert( tags.find("role::program") != tags.end() );
	assert( tags.find("scope::application") != tags.end() );
	assert( tags.find("suite::debian") != tags.end() );
	assert( tags.find("use::searching") != tags.end() );
	assert( tags.find("works-with::software:package") != tags.end() );
}

	Test _3()
{
	using namespace std;

	/* Get the 'debtags' package */
	string p("debtags");

	/* Get its tags */
	std::set<std::string> tags = debtags.getTagsOfItem(p);
	assert(!tags.empty());

	/*
	cerr << "Intersection size: " << endl;
	using namespace wibble::operators;
	std::set<Tag>::const_iterator dbgi = tags.begin();
	cerr << "* " << dbgi->fullname() << ": " << dbgi->id() << endl;
	std::set<int> dbgres = debtags.tagdb().getItemsHavingTag(dbgi->id());
	std::set<Package> dbgpres = debtags.getItemsHavingTag(*dbgi);
	cerr << " #pkgs " << dbgres.size() << " == " << dbgpres.size() << endl;
	cerr << " #isec " << dbgres.size() << " == " << dbgpres.size() << endl;
	cerr << "  "; ppset(dbgpres); cerr << endl;
	cerr << "  "; piset(dbgres); cerr << endl;
	for (++dbgi ; dbgi != tags.end(); ++dbgi)
	{
		cerr << "* " << dbgi->fullname() << ": " << dbgi->id() << endl;
		std::set<Package> dbgpkgs = debtags.getItemsHavingTag(*dbgi);
		std::set<int> dbgids = debtags.tagdb().getItemsHavingTag(dbgi->id());
		cerr << "  "; ppset(dbgpkgs); cerr << endl;
		cerr << "  "; piset(dbgids); cerr << endl;
		cerr << " #pkgs " << dbgpkgs.size() << " == " << dbgids.size() << endl;
		dbgres &= dbgids;
		dbgpres &= dbgpkgs;
		cerr << " #isec " << dbgres.size() << " == " << dbgpres.size() << endl;
	}
	cerr << " " << dbgres.size() << endl << "Results: " << endl;
	for (std::set<int>::const_iterator i = dbgres.begin(); i != dbgres.end(); ++i)
		cerr << "   " << *i << endl;
	*/


//	cerr << "Tags of debtags: ";
//	for (std::set<Tag>::const_iterator i = tags.begin(); i != tags.end(); ++i)
//	{
//		cerr << " " + i->fullname() << endl;
//		std::set<Package> packages = debtags.getItemsHavingTag(*i);
//		for (std::set<Package>::const_iterator p = packages.begin();
//				p != packages.end(); ++p)
//			cerr << "   PKG " << p->name() << endl;
//	}
//	cerr << endl;

    /* Get the items for the tagset of 'debtags' */
	std::set<string> packages = debtags.getItemsHavingTags(tags);
	//cerr << packages.size() << endl;
    assert(!packages.empty());
	/*
	for ( std::set< Package >::iterator i = packages.begin(); i != packages.end(); ++ i )
		std::cerr << i->name() << std::endl;
	std::cerr << "---" << std::endl;
	std::cerr << p.name() << std::endl;
	*/
    /* They should at least contain 'debtags' */
    assert( p <= packages );

    /* Get one of the tags of 'debtags' */
    std::string tag = *tags.begin();

    /* Get its items */
    {
        /* Need this workaround until I figure out how to tell the new GCC
         * that TagDB is a TDBReadonlyDiskIndex and should behave as such
         */
		std::set<std::string> ts;
		ts.insert(tag);
        packages = debtags.getItemsHavingTags(ts);
    }
    //packages = c.debtags().tagdb().getItems(tag);
    assert(!packages.empty());
    /* They should at least contain 'debtags' */
    assert( p <= packages );

    //c.debtags().getTags(""); // XXX HACK AWW!
}

	Test _4()
{
	std::string patchfile = Path::debtagsUserSourceDir() + "patch";
	unlink(patchfile.c_str());

	string p("debtags");

    /* Get its tags */
	std::set<std::string> tags = debtags.getTagsOfItem(p);
    assert(!tags.empty());

	// Ensure that it's not tagged with gameplaying
    std::string t = "use::gameplaying";
    assert(tags.find(t) == tags.end());

	// Add the gameplaying tag
	PatchList<string, string> change;
	change.addPatch(Patch<string, string>(p, wibble::singleton(t), wibble::Empty<string>()));
	debtags.applyChange(change);

	// See that the patch is non empty
	PatchList<string, string> tmp = debtags.changes();
	assert(tmp.size() > 0);
	assert_eq(tmp.size(), 1u);

	// Ensure that the tag has been added
	tags = debtags.getTagsOfItem(p);
    assert(!tags.empty());

	t = "use::gameplaying";
    assert(tags.find(t) != tags.end());

	// Save the patch
	debtags.savePatch();

	// Check that the saved patch is correct
	FILE* in = fopen(patchfile.c_str(), "r");
	string writtenPatch;
	int c;
	while ((c = getc(in)) != EOF)
		writtenPatch += c;
	fclose(in);

	assert_eq(writtenPatch, string("debtags: +use::gameplaying\n"));

	unlink(patchfile.c_str());

	// Reapply the patch and see that it doesn't disrept things
	debtags.applyChange(change);

	// The patch should not have changed
	tmp = debtags.changes();
	assert_eq(tmp.size(), 1u);
	assert_eq(tmp.begin()->first, p);
	assert_eq(tmp.begin()->second.item, p);
}

// If there is no data, Debtags should work as an empty collection
	Test _5()
{
	Path::OverrideDebtagsSourceDir odsd("./empty");
	Path::OverrideDebtagsIndexDir odid("./empty");
	Path::OverrideDebtagsUserSourceDir odusd("./empty");
	Path::OverrideDebtagsUserIndexDir oduid("./empty");
	Debtags empty;

	assert(empty.begin() == empty.end());
	assert_eq(empty.timestamp(), 0);
	assert(!empty.hasData());

	tagcoll::PatchList<std::string, std::string> patches = empty.changes();
	assert(patches.empty());

	set<std::string> res = empty.getTagsOfItem("apt");
	assert(res.empty());
	// TODO: currently does not compile because of a bug in tagcoll
	//res = empty.getTagsOfItems(wibble::singleton(string("apt")));
	//assert(res.empty());

	res = empty.getAllTags();
	assert(res.empty());

	tagcoll::coll::Simple<string, std::string> coll;
	empty.outputSystem(tagcoll::coll::inserter(coll));
	assert_eq(coll.itemCount(), 0u);

	coll.clear();

	empty.outputPatched(tagcoll::coll::inserter(coll));
	assert_eq(coll.itemCount(), 0u);
}

};

#include <ept/debtags/debtags.tcc>
#include <tagcoll/coll/simple.tcc>

#endif

// vim:set ts=4 sw=4:
