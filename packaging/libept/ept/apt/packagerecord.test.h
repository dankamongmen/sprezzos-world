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
#include <ept/apt/packagerecord.h>

namespace std {
    ostream& operator<<(ostream& out, const set<string>& s)
        {
            for (set<string>::const_iterator i = s.begin();
                 i != s.end(); ++i)
                if (i == s.begin())
                    out << *i;
                else
                    out << ", " << *i;
            return out;
        }
}

using namespace std;
using namespace ept;
using namespace ept::apt;

struct TestAptPackagerecord {

    // Check that the supported fields are understood
    Test supportedFields()
    {
        string record =
            "Package: apt\n"
            "Priority: important\n"
            "Section: admin\n"
            "Installed-Size: 4368\n"
            "Maintainer: APT Development Team <deity@lists.debian.org>\n"
            "Architecture: amd64\n"
            "Source: apt\n"
            "Version: 0.6.46.4-0.1\n"
            "Replaces: libapt-pkg-doc (<< 0.3.7), libapt-pkg-dev (<< 0.3.7)\n"
            "Provides: libapt-pkg-libc6.3-6-3.11\n"
            "Depends: libc6 (>= 2.3.5-1), libgcc1 (>= 1:4.1.1-12), libstdc++6 (>= 4.1.1-12), debian-archive-keyring\n"
            "Pre-Depends: debtags (maybe)\n"
            "Suggests: aptitude | synaptic | gnome-apt | wajig, dpkg-dev, apt-doc, bzip2\n"
            "Recommends: debtags (maybe)\n"
            "Enhances: debian\n"
            "Conflicts: marameo\n"
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

        PackageRecord p(record);

        assert_eq(p.size(), 24u);

        assert_eq(p.package(), "apt");
        assert_eq(p.priority(), "important");
        assert_eq(p.section(), "admin");
        assert_eq(p.installedSize(), 4368u);
        assert_eq(p.maintainer(), "APT Development Team <deity@lists.debian.org>");
        assert_eq(p.architecture(), "amd64");
        assert_eq(p.source(), "apt");
        assert_eq(p.version(), "0.6.46.4-0.1");
        assert_eq(p.replaces(), "libapt-pkg-doc (<< 0.3.7), libapt-pkg-dev (<< 0.3.7)");
        assert_eq(p.provides(), "libapt-pkg-libc6.3-6-3.11");
        assert_eq(p.depends(), "libc6 (>= 2.3.5-1), libgcc1 (>= 1:4.1.1-12), libstdc++6 (>= 4.1.1-12), debian-archive-keyring");
        assert_eq(p.preDepends(), "debtags (maybe)");
        assert_eq(p.recommends(), "debtags (maybe)");
        assert_eq(p.suggests(), "aptitude | synaptic | gnome-apt | wajig, dpkg-dev, apt-doc, bzip2");
        assert_eq(p.enhances(), "debian");
        assert_eq(p.conflicts(), "marameo");
        assert_eq(p.filename(), "pool/main/a/apt/apt_0.6.46.4-0.1_amd64.deb");
        assert_eq(p.packageSize(), 1436478u);
        assert_eq(p.md5sum(), "1776421f80d6300c77a608e77a9f4a15");
        assert_eq(p.sha1(), "1bd7337d2df56d267632cf72ac930c0a4895898f");
        assert_eq(p.sha256(), "b92442ab60046b4d0728245f39cc932f26e17db9f7933a5ec9aaa63172f51fda");
        assert_eq(p.description(), "Advanced front-end for dpkg\n"
                      " This is Debian's next generation front-end for the dpkg package manager.\n"
                      " It provides the apt-get utility and APT dselect method that provides a\n"
                      " simpler, safer way to install and upgrade packages.\n"
                      " .\n"
                      " APT features complete installation ordering, multiple source capability\n"
                      " and several other unique features, see the Users Guide in apt-doc.");
        assert_eq(p.shortDescription(), "Advanced front-end for dpkg");
        assert_eq(p.longDescription(),
                      "This is Debian's next generation front-end for the dpkg package manager.\n"
                      " It provides the apt-get utility and APT dselect method that provides a\n"
                      " simpler, safer way to install and upgrade packages.\n"
                      " .\n"
                      " APT features complete installation ordering, multiple source capability\n"
                      " and several other unique features, see the Users Guide in apt-doc.");
        assert_eq(p.buildEssential(), true);
        
        std::set<std::string> tags;
        tags.insert("admin::package-management");
        tags.insert("filetransfer::ftp");
        tags.insert("filetransfer::http");
        tags.insert("hardware::storage:cd");
        tags.insert("interface::commandline");
        tags.insert("network::client");
        tags.insert("protocol::ftp");
        tags.insert("protocol::http");
        tags.insert("protocol::ipv6");
        tags.insert("role::program");
        tags.insert("suite::debian");
        tags.insert("use::downloading");
        tags.insert("use::searching");
        tags.insert("works-with::software:package");
        assert_eq(p.tag(), tags);
    }

};

// vim:set ts=4 sw=4:
