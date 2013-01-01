/*
 * Copyright (C) 2007  Enrico Zini <enrico@enricozini.org>
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

#include <ept/test.h>
#include <ept/apt/apt.h>
#include <set>
#include <algorithm>

using namespace std;
using namespace ept;
using namespace ept::apt;

struct TestApt : AptTestEnvironment {
    Apt apt;

    // Check that iterations iterates among some packages
    Test iterators()
    {
        Apt::iterator i = apt.begin();
        assert(i != apt.end());
        
        size_t count = 0;
        for (; i != apt.end(); ++i)
            ++count;

        assert(count > 100);
    }

    // Check that iteration gives some well-known packages
    Test aptExists()
    {
        set<string> packages;
 
        std::copy(apt.begin(), apt.end(), inserter(packages, packages.begin()));

        assert(packages.find("libsp1") != packages.end());
        // TODO this exposes a bug somewhere... sp definitely is among
        // the packages
        // assert(packages.find("sp") != packages.end());
        assert(packages.find("") == packages.end());
    }

    // Check that timestamp gives some meaningful timestamp
    Test timestamp()
    {
        time_t ts = apt.timestamp();
        assert(ts > 1000000);
    }

    // Check the package validator
    Test validity()
    {
        assert(apt.isValid("apt"));
        assert(!apt.isValid("this-package-does-not-really-exists"));
    }

    // Check the version instantiators
    Test versions()
    {
        std::string pkg("apt");
        Version ver = apt.candidateVersion(pkg);
        assert(ver.isValid());
        
        ver = apt.installedVersion(pkg);
        assert(ver.isValid());

        ver = apt.anyVersion(pkg);
        assert(ver.isValid());

        std::string pkg1("this-package-does-not-really-exists");
        ver = apt.candidateVersion(pkg1);
        assert(!ver.isValid());
        
        ver = apt.installedVersion(pkg1);
        assert(!ver.isValid());

        ver = apt.anyVersion(pkg1);
        assert(!ver.isValid());
    }

    // Check the version validator
    Test versionValidity()
    {
        Version ver = apt.candidateVersion("apt");
        assert(apt.validate(ver) == ver);

        ver = Version("this-package-does-not-really-exists", "0.1");
        assert(!apt.validate(ver).isValid());

        ver = Version("apt", "0.31415");
        assert(!apt.validate(ver).isValid());
    }

    // Check the raw record accessor
    Test rawRecord()
    {
        string pkg("sp");
        Version ver = apt.candidateVersion(pkg);
        assert(ver.isValid());
        assert(apt.validate(ver) == ver);

        string record = apt.rawRecord(ver);
        assert(record.find("Package: sp") != string::npos);
        assert(record.find("Section: text") != string::npos);

        record = apt.rawRecord(Version("sp", "0.31415"));
        assert_eq(record, string());

        assert_eq(apt.rawRecord(pkg), apt.rawRecord(apt.anyVersion(pkg)));
    }

    // Check the package state accessor
    Test state()
    {
        PackageState s = apt.state("kdenetwork");
        assert(s.isValid());
        assert(s.isInstalled());

        s = apt.state("this-package-does-not-really-exists");
        assert(!s.isValid());
    }

    // Check the record iterator (accessing with *)
    Test recordIteration()
    {
        size_t count = 0;
        for (Apt::record_iterator i = apt.recordBegin();
             i != apt.recordEnd(); ++i)
            {
                assert((*i).size() > 8);
                assert_eq((*i).substr(0, 8), "Package:");
                ++count;
            }
        assert(count > 200);
    }

    // Check the record iterator (accessing with ->)
    Test recordIteration2()
    {
        size_t count = 0;
        for (Apt::record_iterator i = apt.recordBegin();
             i != apt.recordEnd(); ++i)
            {
                assert(i->size() > 8);
                assert_eq(i->substr(0, 8), "Package:");
                ++count;
            }
        assert(count > 200);
    }

    // Check that the iterators can be used with the algorithms
    Test stlIteration()
    {
        vector<string> out;
        std::copy(apt.begin(), apt.end(), back_inserter(out));
    }

    // Check that the iterators can be used with the algorithms
    Test stlRecordIteration()
    {
        vector<string> out;
        std::copy(apt.recordBegin(), apt.recordEnd(), back_inserter(out));
    }

	// Check that checkUpdates will keep a working Apt object
	Test checkUpdates()
	{
        assert(apt.isValid("apt"));
		apt.checkCacheUpdates();
        assert(apt.isValid("apt"));
		apt.invalidateTimestamp();		
		apt.checkCacheUpdates();
        assert(apt.isValid("apt"));
	}

};

// vim:set ts=4 sw=4:
