#!/usr/bin/python
#
# Copyright (C) 2010 Michael Vogt <michael.vogt@ubuntu.com>
#
# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.

import datetime
import os
import sys
import unittest

import apt_pkg
import apt.utils

from apt.utils import (
    get_maintenance_end_date,
    get_release_date_from_release_file,
    )

class TestUtils(unittest.TestCase):

    def test_get_release_date_from_release_file(self):
        path = os.path.join(os.path.dirname(__file__), 
                            "data", "misc", "foo_Release")
        t = get_release_date_from_release_file(path)
        self.assertEqual(str(datetime.datetime.utcfromtimestamp(t)),
                         "2012-04-25 22:49:23")

    def test_maintenance_time(self):
        months_of_support = 18
        # test historic releases, jaunty
        release_date = datetime.datetime(2009, 4, 23)
        (end_year, end_month) = get_maintenance_end_date(release_date, months_of_support)
        self.assertEqual(end_year, 2010)
        self.assertEqual(end_month, 10)
        # test historic releases, karmic
        release_date = datetime.datetime(2009, 10, 29)
        (end_year, end_month) = get_maintenance_end_date(release_date, months_of_support)
        self.assertEqual(end_year, 2011)
        self.assertEqual(end_month, 4)
        # test maverick
        release_date = datetime.datetime(2010, 10, 10)
        (end_year, end_month) = get_maintenance_end_date(release_date, months_of_support)
        self.assertEqual(end_year, 2012)
        self.assertEqual(end_month, 4)

        # test with modulo zero
        release_date = datetime.datetime(2010, 6, 10)
        (end_year, end_month) = get_maintenance_end_date(release_date, months_of_support)
        self.assertEqual(end_year, 2011)
        self.assertEqual(end_month, 12)

        # test dapper
        months_of_support = 60
        release_date = datetime.datetime(2008, 4, 24)
        (end_year, end_month) = get_maintenance_end_date(release_date, months_of_support)
        self.assertEqual(end_year, 2013)
        self.assertEqual(end_month, 4)
        
        # what datetime says
        #d = datetime.timedelta(18*30)
        #print "end date: ", release_date + d

if __name__ == "__main__":
    unittest.main()
