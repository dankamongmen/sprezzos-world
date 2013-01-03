#!/usr/bin/python
#
# Copyright (C) 2012 Michael Vogt <mvo@ubuntu.com>
#
# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.

import apt
import unittest

class TestAptPolicy(unittest.TestCase):

    def test_apt_policy_lowlevel(self):
        # get a policy
        cache = apt.Cache()
        policy = cache._depcache.policy
        self.assertNotEqual(policy, None)
        # basic tests
        pkg = cache["apt"]
        self.assertEqual(policy.get_priority(pkg._pkg), 0)
        # get priority for all pkgfiles
        for ver in pkg.versions:
            lowlevel_ver = ver._cand
            for pkgfile, i in lowlevel_ver.file_list:
                #print pkgfile, i, policy.get_priority(pkgfile)
                self.assertTrue(policy.get_priority(pkgfile) >= 1)
                self.assertTrue(policy.get_priority(pkgfile) < 1001)

    def test_apt_policy_highlevel(self):
        cache = apt.Cache()
        pkg = cache["apt"]
        self.assertTrue(pkg.candidate.policy_priority > 1 and
                        pkg.candidate.policy_priority < 1001)



if __name__ == "__main__":
    unittest.main()
