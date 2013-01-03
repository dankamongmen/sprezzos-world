#!/usr/bin/python
# -*- coding: utf-8 -*-
#
# Copyright (C) 2010 Michael Vogt <mvo@ubuntu.com>
# Copyright (C) 2012 Canonical Ltd.
# Author: Colin Watson <cjwatson@ubuntu.com>
#
# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.
"""Unit tests for verifying the correctness of apt_pkg.TagFile"""

from __future__ import print_function, unicode_literals

import io
import glob
import os
import shutil
import sys
import tempfile
import unittest

from test_all import get_library_dir
sys.path.insert(0, get_library_dir())

import apt_pkg

class TestTagFile(unittest.TestCase):
    """ test the apt_pkg.TagFile """

    def setUp(self):
        apt_pkg.init()
        self.temp_dir = tempfile.mkdtemp()

    def tearDown(self):
        shutil.rmtree(self.temp_dir)

    def test_tag_file(self):
        basepath = os.path.dirname(__file__)
        tagfilepath = os.path.join(basepath, "./data/tagfile/*")
        # test once for compressed and uncompressed
        for testfile in glob.glob(tagfilepath):
            # test once using the open() method and once using the path
            for f in [testfile, open(testfile)]:
                tagfile = apt_pkg.TagFile(f)
                for i, stanza in enumerate(tagfile):
                    pass
                self.assertEqual(i, 2)

    def test_errors(self):
        # Raises SystemError via lbiapt
        self.assertRaises(SystemError, apt_pkg.TagFile, "not-there-no-no")
        # Raises Type error
        self.assertRaises(TypeError, apt_pkg.TagFile, object())

    def test_utf8(self):
        value = "Tést Persön <test@example.org>"
        packages = os.path.join(self.temp_dir, "Packages")
        with io.open(packages, "w", encoding="UTF-8") as packages_file:
            print("Maintainer: %s" % value, file=packages_file)
            print("", file=packages_file)
        if sys.version < '3':
            # In Python 2, test the traditional file interface.
            with open(packages) as packages_file:
                tagfile = apt_pkg.TagFile(packages_file)
                tagfile.step()
                self.assertEqual(
                    value.encode("UTF-8"), tagfile.section["Maintainer"])
        with io.open(packages, encoding="UTF-8") as packages_file:
            tagfile = apt_pkg.TagFile(packages_file)
            tagfile.step()
            if sys.version < '3':
                self.assertEqual(
                    value.encode("UTF-8"), tagfile.section["Maintainer"])
            else:
                self.assertEqual(value, tagfile.section["Maintainer"])

    def test_latin1(self):
        value = "Tést Persön <test@example.org>"
        packages = os.path.join(self.temp_dir, "Packages")
        with io.open(packages, "w", encoding="ISO-8859-1") as packages_file:
            print("Maintainer: %s" % value, file=packages_file)
            print("", file=packages_file)
        if sys.version < '3':
            # In Python 2, test the traditional file interface.
            with open(packages) as packages_file:
                tagfile = apt_pkg.TagFile(packages_file)
                tagfile.step()
                self.assertEqual(
                    value.encode("ISO-8859-1"), tagfile.section["Maintainer"])
        with io.open(packages) as packages_file:
            tagfile = apt_pkg.TagFile(packages_file, bytes=True)
            tagfile.step()
            self.assertEqual(
                value.encode("ISO-8859-1"), tagfile.section["Maintainer"])
        if sys.version >= '3':
            # In Python 3, TagFile can pick up the encoding of the file
            # object.
            with io.open(packages, encoding="ISO-8859-1") as packages_file:
                tagfile = apt_pkg.TagFile(packages_file)
                tagfile.step()
                self.assertEqual(value, tagfile.section["Maintainer"])

    def test_mixed(self):
        value = "Tést Persön <test@example.org>"
        packages = os.path.join(self.temp_dir, "Packages")
        with io.open(packages, "w", encoding="UTF-8") as packages_file:
            print("Maintainer: %s" % value, file=packages_file)
            print("", file=packages_file)
        with io.open(packages, "a", encoding="ISO-8859-1") as packages_file:
            print("Maintainer: %s" % value, file=packages_file)
            print("", file=packages_file)
        if sys.version < '3':
            # In Python 2, test the traditional file interface.
            with open(packages) as packages_file:
                tagfile = apt_pkg.TagFile(packages_file)
                tagfile.step()
                self.assertEqual(
                    value.encode("UTF-8"), tagfile.section["Maintainer"])
                tagfile.step()
                self.assertEqual(
                    value.encode("ISO-8859-1"), tagfile.section["Maintainer"])
        with io.open(packages) as packages_file:
            tagfile = apt_pkg.TagFile(packages_file, bytes=True)
            tagfile.step()
            self.assertEqual(
                value.encode("UTF-8"), tagfile.section["Maintainer"])
            tagfile.step()
            self.assertEqual(
                value.encode("ISO-8859-1"), tagfile.section["Maintainer"])

if __name__ == "__main__":
    unittest.main()
