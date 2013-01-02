#!/usr/bin/python
# Update dependencies based on info.py
# Copyright (C) 2010 Jelmer Vernooij <jelmer@debian.org>
# Licensed under the GNU GPL, version 2 or later.

import ConfigParser
import optparse
import os
import sys

parser = optparse.OptionParser("%s <--minimum-library-version|--update-control> <source-tree>" % sys.argv[0])
parser.add_option("--minimum-library-version",
    help="Print argument for --minimum-library-version", action="store_true")
parser.add_option("--update-control", help="Update debian/control", action="store_true")

(opts, args) = parser.parse_args()

if len(args) != 1:
    if os.path.exists("source4"):
        tree = os.getcwd()
    else:
        parser.print_usage()
        sys.exit(1)
else:
    tree = args[0]

def update_relation(line, pkg, kind, version):
    """Update a relation in a control field.

    :param line: Depends-like dependency list
    :param pkg: Package name
    :param kind: Version requirement kind ("==", ">=", "<<", etc)
    :param version: Required version
    """
    found = False
    for pr in line:
        for e in pr:
            if e["name"] == pkg and e["version"] and e["version"][0] == kind:
                e["version"] = (kind, version)
                found = True
    if not found:
        line.append([{"version": (kind, version), "name": pkg, "arch": None}])


class LibraryEquivalents(object):
    """Lookup table for equivalent library versions."""

    def __init__(self, path):
        self.config = ConfigParser.ConfigParser()
        self.config.readfp(open(path))

    def find_equivalent(self, package, version):
        """Find an equivalent version for a specified package version.

        :param package: Package name
        :param version: Package version as int-tuple.
        :return: Equivalent version as int-tuple.
        :raise KeyError: Raised if there was no equivalent version found
        """
        try:
            version = self.config.get(package, ".".join(str(x) for x in version))
            return tuple([int(x) for x in version.split(".")])
        except (ConfigParser.NoSectionError, ConfigParser.NoOptionError):
            raise KeyError

    def find_oldest_compatible(self, package, version):
        try:
            return self.find_equivalent(package, version)
        except KeyError:
            return version


def find_version(path):
    """Find a version in a waf file.

    :param path: waf script to read
    :return: Version as int-tuple.
    """
    v = open(path, 'r')
    try:
        for l in v.readlines():
            if l.startswith("VERSION = '"):
                return tuple([int(x) for x in l.strip()[len("VERSION = '"):-1].split(".")])
        raise KeyError
    finally:
        v.close()


def update_control():
    """Update the debian control file.
    """
    from debian.deb822 import Deb822, PkgRelation
    f = open('debian/control', 'r')
    iter = Deb822.iter_paragraphs(f)
    source = iter.next()

    def update_deps(control, field, package, min_version):
        bdi = PkgRelation.parse_relations(control[field])
        update_relation(bdi, package, ">=", "%d.%d.%d~" % min_version)
        control[field] = PkgRelation.str(bdi)

    talloc_version = find_version(os.path.join(tree, "lib/talloc/wscript"))

    eq_config = LibraryEquivalents('debian/library-equivalents')
    min_talloc_version = eq_config.find_oldest_compatible("talloc", talloc_version)
    update_deps(source, "Build-Depends", "libtalloc-dev", min_talloc_version)
    update_deps(source, "Build-Depends", "python-talloc-dev", min_talloc_version)

    o = open("debian/control", "w+")
    source.dump(o)

    for binary in iter:
        o.write("\n")
        binary.dump(o)

    o.close()


def forced_minimum_library_versions():
    libraries = [
        ("tdb", "lib/tdb/wscript"),
        ("talloc", "lib/talloc/wscript"),
        ("ldb", "source4/lib/ldb/wscript"),
        ("tevent", "lib/tevent/wscript")]
    eq_config = LibraryEquivalents('debian/library-equivalents')
    for (name, path) in libraries:
        version = find_version(os.path.join(tree, path))
        try:
            min_version = eq_config.find_equivalent(name, version)
        except KeyError:
            continue
        yield "%s:%s" % (name, ".".join([str(x) for x in min_version]))


if opts.minimum_library_version:
    print ",".join(forced_minimum_library_versions())
elif opts.update_control:
    update_control()
else:
    parser.print_usage()
    sys.exit(1)
