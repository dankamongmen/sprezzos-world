#!/usr/bin/python
#
# Copyright (C) 2010 Julian Andres Klode <jak@debian.org>
#               2010 Michael Vogt <mvo@ubuntu.com>
#
# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.
"""Unit tests for verifying the correctness of check_dep, etc in apt_pkg."""
import os
import tempfile
import unittest

from test_all import get_library_dir
import sys
sys.path.insert(0, get_library_dir())

import apt
import apt_pkg
import shutil
import glob
import logging

def if_sources_list_is_readable(f):
    def wrapper(*args, **kwargs):
        if os.access("/etc/apt/sources.list", os.R_OK):
            f(*args, **kwargs)
        else:
            logging.warn("skipping '%s' because sources.list is not readable" % f)
    return wrapper

class TestAptCache(unittest.TestCase):
    """ test the apt cache """

    def setUp(self):
        # reset any config manipulations done in the individual tests
        apt_pkg.init_config()
        # save/restore the apt config
        self._cnf = {}
        for item in apt_pkg.config.keys():
            self._cnf[item] = apt_pkg.config.find(item)

    def tearDown(self):
        for item in self._cnf:
            apt_pkg.config.set(item, self._cnf[item])

    @if_sources_list_is_readable
    def test_apt_cache(self):
        """cache: iterate all packages and all dependencies """
        cache = apt.Cache()
        # number is not meaningful and just need to be "big enough",
        # the important bit is the test against __len__
        self.assertTrue(len(cache) > 100)
        # go over the cache and all dependencies, just to see if
        # that is possible and does not crash
        for pkg in cache:
            if pkg.candidate:
                for or_deps in pkg.candidate.dependencies:
                    for dep in or_deps:
                        self.assertTrue(dep.name)
                        self.assertTrue(isinstance(dep.relation, str))
                        self.assertTrue(dep.pre_depend in (True, False))

                # accessing record should take a reasonable time; in
                # particular, when using compressed indexes, it should not use
                # tons of seek operations
                r = pkg.candidate.record
                self.assertEqual(r['Package'], pkg.shortname)
                self.assertTrue('Version' in r)
                self.assertTrue(len(r['Description']) > 0)
                self.assertTrue(str(r).startswith('Package: %s\n' % pkg.shortname))

    def test_get_provided_packages(self):
        apt.apt_pkg.config.set("Apt::architecture", "i386")
        cache = apt.Cache(rootdir="./data/test-provides/")
        cache.open()
        if len(cache) == 0:
            logging.warn("skipping test_get_provided_packages, cache empty?!?")
            return
        # a true virtual pkg
        l = cache.get_providing_packages("mail-transport-agent")
        self.assertTrue(len(l) > 0)
        self.assertTrue("postfix" in [p.name for p in l])
        self.assertTrue("mail-transport-agent" in cache["postfix"].candidate.provides)

    def test_low_level_pkg_provides(self):
        apt.apt_pkg.config.set("Apt::architecture", "i386")
        # create highlevel cache and get the lowlevel one from it
        highlevel_cache = apt.Cache(rootdir="./data/test-provides")
        if len(highlevel_cache) == 0:
            logging.warn("skipping test_log_level_pkg_provides, cache empty?!?")
            return
        # low level cache provides list of the pkg
        cache = highlevel_cache._cache
        l = cache["mail-transport-agent"].provides_list
        # arbitrary number, just needs to be higher enough
        self.assertEqual(len(l), 2)
        for (providesname, providesver, version) in l:
            self.assertEqual(providesname, "mail-transport-agent")
            if version.parent_pkg.name == "postfix":
                break
        else:
            self.assertNotReached()
   
    @if_sources_list_is_readable
    def test_dpkg_journal_dirty(self):
        # create tmp env
        tmpdir = tempfile.mkdtemp()
        dpkg_dir = os.path.join(tmpdir,"var","lib","dpkg")
        os.makedirs(os.path.join(dpkg_dir,"updates"))
        open(os.path.join(dpkg_dir,"status"), "w").close()
        apt_pkg.config.set("Dir::State::status",
                       os.path.join(dpkg_dir,"status"))
        cache = apt.Cache()
        # test empty
        self.assertFalse(cache.dpkg_journal_dirty)
        # that is ok, only [0-9] are dpkg jounral entries
        open(os.path.join(dpkg_dir,"updates","xxx"), "w").close()
        self.assertFalse(cache.dpkg_journal_dirty)        
        # that is a dirty journal
        open(os.path.join(dpkg_dir,"updates","000"), "w").close()
        self.assertTrue(cache.dpkg_journal_dirty)

    @if_sources_list_is_readable
    def test_apt_update(self):
        rootdir = "./data/tmp"
        if os.path.exists(rootdir):
            shutil.rmtree(rootdir)
        try:
            os.makedirs(os.path.join(rootdir, "var/lib/apt/lists/partial"))
        except OSError:
            pass
        state_dir = os.path.join(rootdir, "var/lib/apt")
        lists_dir = os.path.join(rootdir, "var/lib/apt/lists")
        old_state = apt_pkg.config.find("dir::state")
        apt_pkg.config.set("dir::state", state_dir)
        # set a local sources.list that does not need the network
        base_sources = os.path.abspath(os.path.join(rootdir, "sources.list"))
        old_source_list = apt_pkg.config.find("dir::etc::sourcelist")
        old_source_parts = apt_pkg.config.find("dir::etc::sourceparts")
        apt_pkg.config.set("dir::etc::sourcelist", base_sources)
        apt_pkg.config.set("dir::etc::sourceparts", "/tmp")
        # main sources.list
        sources_list = base_sources
        with open(sources_list, "w") as f:
            repo = os.path.abspath("./data/test-repo2")
            f.write("deb copy:%s /\n" % repo)

        # test single sources.list fetching
        sources_list = os.path.join(rootdir, "test.list")
        with open(sources_list, "w") as f:
            repo_dir = os.path.abspath("./data/test-repo")
            f.write("deb copy:%s /\n" % repo_dir)

        self.assertTrue(os.path.exists(sources_list))
        # write marker to ensure listcleaner is not run
        open("./data/tmp/var/lib/apt/lists/marker", "w").close()

        # update a single sources.list
        cache = apt.Cache()
        cache.update(sources_list=sources_list)
        # verify we just got the excpected package file 
        needle_packages = glob.glob(
            lists_dir+"/*tests_data_test-repo_Packages*")
        self.assertEqual(len(needle_packages), 1)
        # verify that we *only* got the Packages file from a single source
        all_packages = glob.glob(lists_dir+"/*_Packages*")
        self.assertEqual(needle_packages, all_packages)
        # verify that the listcleaner was not run and the marker file is
        # still there
        self.assertTrue("marker" in os.listdir(lists_dir))
        
        # now run update again (without the "normal" sources.list that
        # contains test-repo2 and verify that we got the normal sources.list
        cache.update()
        needle_packages = glob.glob(lists_dir+"/*tests_data_test-repo2_Packages*")
        self.assertEqual(len(needle_packages), 1)
        all_packages = glob.glob(lists_dir+"/*_Packages*")
        self.assertEqual(needle_packages, all_packages)

        # and another update with a single source only
        cache = apt.Cache()
        cache.update(sources_list=sources_list)
        all_packages = glob.glob(lists_dir+"/*_Packages*")
        self.assertEqual(len(all_packages), 2)
        apt_pkg.config.set("dir::state", old_state)
        apt_pkg.config.set("dir::etc::sourcelist", old_source_list)
        apt_pkg.config.set("dir::etc::sourceparts", old_source_parts)

    def test_package_cmp(self):
        cache = apt.Cache(rootdir="/")
        l = []
        l.append(cache["intltool"])
        l.append(cache["python3"])
        l.append(cache["apt"])
        l.sort()
        self.assertEqual([p.name for p in l],
                         ["apt", "intltool", "python3"])

    def test_get_architectures(self):
        main_arch = apt.apt_pkg.config.get("APT::Architecture")
        arches = apt_pkg.get_architectures()
        self.assertTrue(main_arch in arches)


if __name__ == "__main__":
    unittest.main()
