#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Regression test for LP: #981896, LP: #659438"""
# Copyright (C) 2012 Sebastian Heinlein <devel@glatzor.de>
#
# Licensed under the GNU General Public License Version 2
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
# Licensed under the GNU General Public License Version 2

__author__  = "Sebastian Heinlein <devel@glatzor.de>"

import os
import shutil
import tempfile
import unittest

import apt_pkg
import apt


class RegressionTestCase(unittest.TestCase):

    """Test suite for LP: #981896, LP: #659438
    'Cannot locate a file for package X'
    """

    def setUp(self):
        apt_pkg.init_config()
        self.chroot_path = chroot_path = tempfile.mkdtemp()
        # Create a damaged status file
        self.cache = apt.cache.Cache(rootdir=chroot_path)
        with open(apt_pkg.config.find_file("Dir::State::status"),
                  "a") as status:
            status.write("""Package: abrowser
Status: install reinstreq half-installed
Priority: optional
Section: admin
Architecture: all
Version: 3.6.9+build1+nobinonly-0ubuntu1""")
        sources_list_path = apt_pkg.config.find_file("Dir::Etc::sourcelist")
        repo_path = os.path.abspath("./data/test-repo")
        with open(sources_list_path, "w") as sources_list:
            sources_list.write("deb copy:%s /\n" % repo_path)
        # os.makedirs(os.path.join(chroot_path, "etc/apt/sources.list.d/"))
        self.cache.update(sources_list=sources_list_path)
        self.cache.open()

    def tearDown(self):
        # this resets the rootdir apt_pkg.config to ensure it does not
        # "pollute" the later tests
        cache = apt.cache.Cache(rootdir="/")
        shutil.rmtree(self.chroot_path)

    def test_survive_reqreinst(self):
        """Test that we survive a package in require reinstallation state"""
        # this should be 82324L but python3.2 gets unhappy about the "L"
        self.assertEqual(self.cache.required_download, 82324)

if __name__ == "__main__":
    unittest.main()

# vim: ts=4 et sts=4
