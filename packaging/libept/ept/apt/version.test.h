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
#include <ept/apt/version.h>

using namespace std;
using namespace ept::apt;

struct TestAptVersion {

    // Basic test for invalid version
    Test invalid()
    {
        Version test;

        assert_eq(test.name(), "");
        assert_eq(test.version(), "");
        assert_eq(test.isValid(), false);

        string p = test.name();

        assert_eq(p, string());
    }

    // Basic test for version
    Test basic()
    {
        Version test("test", "1.0");

        assert_eq(test.name(), "test");
        assert_eq(test.version(), "1.0");
        assert_eq(test.isValid(), true);

        string p = test.name();

        assert_eq(p, "test");

        Version v(p, "1.1");
        assert_eq(v.name(), "test");
        assert_eq(v.version(), "1.1");
        assert_eq(v.isValid(), true);
    }

    // Comparison semanthics
    Test comparison()
    {
        Version test("test", "1.0");
        Version test1("test", "1.0");

        assert(test == test1);
        assert(! (test != test1));
        assert(! (test < test1));
        assert(! (test > test1));
        assert(test <= test1);
        assert(test >= test1);


        Version test2("test2", "1.0");

        assert(test != test2);
        assert(test != test2);
        assert(test < test2);
        assert(! (test > test2));
        assert(test <= test2);
        assert(! (test >= test2));


        Version test3("test", "2.0");

        assert(test != test3);
        assert(test != test3);
        assert(test < test3);
        assert(! (test > test3));
        assert(test <= test3);
        assert(! (test >= test3));
    }

    // Value-copy semanthics
    Test valueCopy()
    {
        Version test("test", "1.0");
        Version test1 = test;

        assert(test == test1);

        Version test2;
        test2 = test;
        assert(test == test2);
        assert(test1 == test2);

        Version test3("test", "1.0");
        assert(test == test3);
        assert(test1 == test3);
        assert(test2 == test3);
    }

    // Extraction of upstream version
    Test upstreamVersion()
    {
        assert_eq(Version("a", "10.0").upstreamVersion(), "10.0");
        assert_eq(Version("a", "10.0-1").upstreamVersion(), "10.0");
        assert_eq(Version("a", "10.0~foo.1-1.0").upstreamVersion(), "10.0~foo.1");
        assert_eq(Version("a", "1.0:10.0~foo.1-1.0").upstreamVersion(), "10.0~foo.1");
    }

    // Debian policy comparison semanthics
    Test policyComparison()
    {
        assert(Version("a", "10.0") > Version("a", "2.1"));
        assert(Version("a", "1:10.0") < Version("a", "2:2.1"));
        assert(Version("a", "10.0-1") < Version("a", "10.0-2"));
        assert(Version("a", "10.0-2") > Version("a", "10.0-1"));
        assert(Version("a", "1:10.0-1") <= Version("a", "1:10.0-1"));
        assert(Version("a", "1:10.0-1") >= Version("a", "1:10.0-1"));
        // TODO: add more
    }

};

// vim:set ts=4 sw=4:
