#!/usr/bin/python
# this is a example how to access the build dependencies of a package

import apt
import apt_pkg
import sys

# main
cache = apt.Cache()
srcrecords = apt_pkg.SourceRecords()

# base package that we use for build-depends calculation
if len(sys.argv) < 2:
    print "need a package name as argument"
    sys.exit(1)
try:
    pkg = base = cache[sys.argv[1]]
except KeyError:
    print "No package %s found" % sys.argv[1]
    sys.exit(1)
all_build_depends = set()

# get the build depdends for the package itself
srcpkg_name = base.candidate.source_name
print "srcpkg_name: %s " % srcpkg_name
if not srcpkg_name:
    print "Can't find source package for '%s'" % pkg.Name
srcrec = srcrecords.Lookup(srcpkg_name)
if srcrec:
    print "Files:"
    print srcrecords.Files
    bd = srcrecords.BuildDepends
    print "build-depends of the package: %s " % bd
    for b in bd:
        all_build_depends.add(b[0])

# calculate the build depends for all dependencies
depends = base.candidate.dependencies
for or_dep in depends:
    for dep in or_dep.or_dependencies:
        pkg = cache[dep.name]
        srcpkg_name = pkg.candidate.source_name
        if not srcpkg_name:
            print "Can't find source package for '%s'" % pkg.Name
            continue
        srcrec = srcrecords.Lookup(srcpkg_name)
        if srcrec:
            #print srcrecords.Package
            #print srcrecords.Binaries
            bd = srcrecords.BuildDepends
            #print "%s: %s " % (srcpkg_name, bd)
            for b in bd:
                all_build_depends.add(b[0])


print "\n".join(all_build_depends)
