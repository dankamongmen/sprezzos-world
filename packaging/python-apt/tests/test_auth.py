#!/usr/bin/env python

import os
import shutil
import sys
import tempfile
import time
import unittest

if sys.version_info[0] > 2:
    from http.server import HTTPServer
    from http.server import SimpleHTTPRequestHandler as HTTPRequestHandler
else:
    from BaseHTTPServer import HTTPServer
    from SimpleHTTPServer import SimpleHTTPRequestHandler as HTTPRequestHandler


if sys.version_info[0] == 2 and sys.version_info[1] == 6:
    from unittest2 import TestCase
else:
    from unittest import TestCase


import apt_pkg
import apt.auth

WHEEZY_KEY = """-----BEGIN PGP PUBLIC KEY BLOCK-----
Version: GnuPG v1.4.12 (GNU/Linux)

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
DHcut3Yey8o=
=id4q
-----END PGP PUBLIC KEY BLOCK-----"""


class TestAuthKeys(TestCase):

    """Test handling of keys for signed repositories."""

    if sys.version_info[0] == 2 and sys.version_info[1] < 7:
        def addCleanup(self, function, *args, **kwds):
            try:
                self.cleanup.append(lambda: function(*args, **kwds))
            except AttributeError:
                self.cleanup = [lambda: function(*args, **kwds)]

        def tearDown(self):
            for f in self.cleanup:
                f()
            self.cleanup = []

    def setUp(self):
        # reset any config manipulations done in the individual tests
        apt_pkg.init_config()
        # save the apt config to restore later
        cnf = {}
        for item in apt_pkg.config.keys():
            cnf[item] = apt_pkg.config.find(item)
        self.addCleanup(self._restore_apt_config, cnf)

        self.tmpdir = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.tmpdir)
        apt_pkg.config.set("Dir", self.tmpdir)
        apt_pkg.config.set("Dir::Bin::Apt-key", "fakeroot-apt-key")
        apt_pkg.config.set("Dir::Etc", "etc/apt/")
        trustedparts_dir = apt_pkg.config.find_dir("Dir::Etc::Trustedparts")
        confparts_dir = apt_pkg.config.find_dir("Dir::Etc::parts")
        self.assertTrue(trustedparts_dir.startswith(self.tmpdir))
        os.makedirs(trustedparts_dir)
        os.makedirs(confparts_dir)
        shutil.copy("fakeroot-apt-key", self.tmpdir)

    def _restore_apt_config(self, cnf):
        """Restore previous apt configuration."""
        for item in cnf:
            apt_pkg.config.set(item, cnf[item])

    def testAddAndExportKey(self):
        """Add an example key."""
        apt.auth.add_key(WHEEZY_KEY)
        # Strip the headers from the keys to avoid test errors because
        # the exported key used a differenct GnuPG version than the
        # original example key
        self.assertEqual(apt.auth.export_key("46925553").split("\n")[2:],
                         WHEEZY_KEY.split("\n")[2:])

    def testAddAndListKey(self):
        """Add an example key and test if it is correctly returned by
        list_keys()
        """
        apt.auth.add_key(WHEEZY_KEY)
        ret = apt.auth.list_keys()
        self.assertEqual(len(ret), 1)
        key = ret[0]
        self.assertEqual(key.name,
                         "Debian Archive Automatic Signing Key (7.0/wheezy) "
                         "<ftpmaster@debian.org>")
        self.assertEqual(key.keyid, "46925553")
        self.assertEqual(key.date, "2012-04-27")

    def testAddKeyFromFile(self):
        """Test adding a key from file."""
        keyfd, keyname = tempfile.mkstemp()
        self.addCleanup(os.close, keyfd)
        os.write(keyfd, WHEEZY_KEY.encode("UTF-8"))

        apt.auth.add_key_from_file(keyname)

        ret = apt.auth.list_keys()
        self.assertEqual(len(ret), 1)
        key = ret[0]
        self.assertEqual(key.name,
                         "Debian Archive Automatic Signing Key (7.0/wheezy) "
                         "<ftpmaster@debian.org>")
        self.assertEqual(key.keyid, "46925553")
        self.assertEqual(key.date, "2012-04-27")

    def test_add_key_from_keyserver_too_short(self):
        """Ensure that short keyids are not imported"""
        with self.assertRaises(apt.auth.AptKeyError):
            apt.auth.add_key_from_keyserver("46925553", "hkp://localhost:19191")

    def test_add_key_from_server_mitm(self):
        """Verify that the key fingerprint is verified after download"""
        self._start_keyserver()
        self.addCleanup(self._stop_keyserver)
        with self.assertRaises(apt.auth.AptKeyError) as cm:
            apt.auth.add_key_from_keyserver(
                "0101010178F7FE5C3E65D8AF8B48AD6246925553",
                "hkp://localhost:19191")
        self.assertTrue(
            str(cm.exception).startswith("Fingerprints do not match"))

    def testAddKeyFromServer(self):
        """Install a GnuPG key from a remote server."""
        self._start_keyserver()
        self.addCleanup(self._stop_keyserver)

        apt.auth.add_key_from_keyserver(
            "0xa1bD8E9D78F7FE5C3E65D8AF8B48AD6246925553", 
            "hkp://localhost:19191")

        ret = apt.auth.list_keys()
        self.assertEqual(len(ret), 1)
        key = ret[0]
        self.assertEqual(key.name,
                         "Debian Archive Automatic Signing Key (7.0/wheezy) "
                         "<ftpmaster@debian.org>")
        self.assertEqual(key.keyid, "46925553")
        self.assertEqual(key.date, "2012-04-27")

    def _start_keyserver(self):
        """Start a fake keyserver on http://localhost:19191
        Thanks pitti.
        """
        dir = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, dir)
        os.mkdir(os.path.join(dir, "pks"))
        with open(os.path.join(dir, "pks", "lookup"), "w") as key_file:
            key_file.write(WHEEZY_KEY)

        self.keyserver_pid = os.fork()
        if self.keyserver_pid == 0:
            # quiesce server log
            os.dup2(os.open('/dev/null', os.O_WRONLY), sys.stderr.fileno())
            os.chdir(dir)
            httpd = HTTPServer(('localhost', 19191), HTTPRequestHandler)
            httpd.serve_forever()
            os._exit(0)

        # wait a bit until server is ready
        time.sleep(0.5)

    def _stop_keyserver(self):
        '''Stop fake keyserver'''
        assert self.keyserver_pid

        os.kill(self.keyserver_pid, 15)
        os.wait()


if __name__ == "__main__":
    unittest.main()
