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
#include <ept/apt/recordparser.h>

//#include <iostream>

using namespace std;
using namespace ept;
using namespace ept::apt;

struct TestAptRecordparser {
    std::string record;
    TestAptRecordparser()
    {
        record =
            "A:\n"
            "D: da de di do du\n"
            "B: b\n"
            "C: c \n"
            "Desc: this is the beginning\n"
            " this is the continuation\n"
            " this is the end\n";
    }

    // Check that the fields are identified and broken up correctly
    Test parsing()
    {
        RecordParser p(record);
        
        assert_eq(p.record(), record);
        assert_eq(p.size(), 5u);
    }

    Test fieldTuples()
    {
        RecordParser p(record);
        assert_eq(p.field(0), "A:\n");
        assert_eq(p.field(1), "D: da de di do du\n");
        assert_eq(p.field(2), "B: b\n");
        assert_eq(p.field(3), "C: c \n");
        assert_eq(p.field(4), "Desc: this is the beginning\n this is the continuation\n this is the end\n");
    }

    Test fieldKeys()
    {
        RecordParser p(record);
        assert_eq(p.name(0), "A");
        assert_eq(p.name(1), "D");
        assert_eq(p.name(2), "B");
        assert_eq(p.name(3), "C");
        assert_eq(p.name(4), "Desc");
    }

    Test fieldValues()
    {
        RecordParser p(record);
        assert_eq(p[0], "");
        assert_eq(p[1], "da de di do du");
        assert_eq(p[2], "b");
        assert_eq(p[3], "c");
        assert_eq(p[4], "this is the beginning\n this is the continuation\n this is the end");
    }

    // Check that the field search by name finds all the fields
    Test findByName()
    {
        RecordParser p(record);

        assert_eq(p.index("A"), 0u);
        assert_eq(p.index("D"), 1u);
        assert_eq(p.index("B"), 2u);
        assert_eq(p.index("C"), 3u);
        assert_eq(p.index("Desc"), 4u);

        assert_eq(p.name(p.index("A")), "A");
        assert_eq(p.name(p.index("B")), "B");
        assert_eq(p.name(p.index("C")), "C");
        assert_eq(p.name(p.index("D")), "D");
        assert_eq(p.name(p.index("Desc")), "Desc");
    }

    Test indexing()
    {
        RecordParser p(record);
        assert_eq(p["A"], "");
        assert_eq(p["B"], "b");
        assert_eq(p["C"], "c");
        assert_eq(p["D"], "da de di do du");
        assert_eq(p["Desc"], "this is the beginning\n this is the continuation\n this is the end");
    }

    Test missingBehaviour()
    {
        RecordParser p(record);
        // Missing fields give empty strings
        assert_eq(p.field(100), "");
        assert_eq(p.name(100), "");
        assert_eq(p[100], "");
        assert_eq(p["Missing"], "");
    }

    // Check that scanning twice replaces the old fields
    Test rescan()
    {
        std::string record =
            "A: a\n"
            "B: b\n"
            "C: c\n";

        RecordParser p(record);
        assert_eq(p.size(), 3u);
        assert_eq(p["A"], "a");
        assert_eq(p["B"], "b");
        assert_eq(p["C"], "c");

        std::string record1 =
            "Foo: bar\n"
            "A: different\n";

        p.scan(record1);

        //for (size_t i = 0; i < p.size(); ++i)
        //      cerr << ">> " << i << "==" << p.index(p.name(i)) << " " << p.name(i) << " " << p[i] << endl;

        assert_eq(p.size(), 2u);
        assert_eq(p["A"], "different");
        assert_eq(p["B"], "");
        assert_eq(p["C"], "");
        assert_eq(p["Foo"], "bar");
    }

    // Real-life example
    Test realLife()
    {
        string record = 
            "Package: apt\n"
            "Priority: important\n"
            "Section: admin\n"
            "Installed-Size: 4368\n"
            "Maintainer: APT Development Team <deity@lists.debian.org>\n"
            "Architecture: amd64\n"
            "Version: 0.6.46.4-0.1\n"
            "Replaces: libapt-pkg-doc (<< 0.3.7), libapt-pkg-dev (<< 0.3.7)\n"
            "Provides: libapt-pkg-libc6.3-6-3.11\n"
            "Depends: libc6 (>= 2.3.5-1), libgcc1 (>= 1:4.1.1-12), libstdc++6 (>= 4.1.1-12), debian-archive-keyring\n"
            "Suggests: aptitude | synaptic | gnome-apt | wajig, dpkg-dev, apt-doc, bzip2\n"
            "Filename: pool/main/a/apt/apt_0.6.46.4-0.1_amd64.deb\n"
            "Size: 1436478\n"
            "MD5sum: 1776421f80d6300c77a608e77a9f4a15\n"
            "SHA1: 1bd7337d2df56d267632cf72ac930c0a4895898f\n"
            "SHA256: b92442ab60046b4d0728245f39cc932f26e17db9f7933a5ec9aaa63172f51fda\n"
            "Description: Advanced front-end for dpkg\n"
            " This is Debian's next generation front-end for the dpkg package manager.\n"
            " It provides the apt-get utility and APT dselect method that provides a\n"
            " simpler, safer way to install and upgrade packages.\n"
            " .\n"
            " APT features complete installation ordering, multiple source capability\n"
            " and several other unique features, see the Users Guide in apt-doc.\n"
            "Build-Essential: yes\n"
            "Tag: admin::package-management, filetransfer::ftp, filetransfer::http, hardware::storage:cd, interface::commandline, network::client, protocol::{ftp,http,ipv6}, role::program, suite::debian, use::downloading, use::searching, works-with::software:package\n";
        RecordParser p(record);

        assert_eq(p.size(), 19u);

        string rec1;
        for (size_t i = 0; i < p.size(); ++i)
            rec1 += p.field(i);
        assert_eq(record, rec1);
    }

    // Various buffer termination patterns
    Test bufferTermination()
    {
        std::string record =
            "A: a\n"
            "B: b";

        RecordParser p(record);
        assert_eq(p.size(), 2u);
        assert_eq(p["A"], "a");
        assert_eq(p["B"], "b");
    }

    Test bufferTermination2()
    {
        std::string record =
            "A: a\n"
            "B: b\n\n";

        RecordParser p(record);
        assert_eq(p.size(), 2u);
        assert_eq(p["A"], "a");
        assert_eq(p["B"], "b");
    }

    Test bufferTermination3()
    {
        std::string record =
            "A: a\n"
            "B: b\n\n"
            "C: c\n";

        RecordParser p(record);
        assert_eq(p.size(), 2u);
        assert_eq(p["A"], "a");
        assert_eq(p["B"], "b");
    }

};

// vim:set ts=4 sw=4:
