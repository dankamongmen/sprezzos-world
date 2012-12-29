#!/usr/bin/python

import unittest
import subprocess

class TestParseVersions(unittest.TestCase):
    @staticmethod
    def get_output(args):
        subp = subprocess.Popen(args, stdout=subprocess.PIPE)
        output = subp.communicate()[0]
        if subp.returncode != 0:
            raise RuntimeError, subp.returncode
        return output

    def pv_output(self, control):
        return TestParseVersions.get_output(["parseversions", "--pycentral", control])

    def test_regular(self):
        self.assertEqual(self.pv_output('testparseversions.regular.control'),
                         TestParseVersions.get_output(['pyversions', '--supported', '-v']))

    def test_nonexported(self):
        self.assertEqual(self.pv_output('testparseversions.nonexported.control'),
                         TestParseVersions.get_output(['pyversions', '--supported', '-v']))

    def test_leading_newline(self):
        self.assertEqual(self.pv_output('testparseversions.leading-newline.control'),
                         TestParseVersions.get_output(['pyversions', '--supported', '-v']))

    def test_missing(self):
        self.assertRaises(RuntimeError, self.pv_output, 'testparseversions.missing.control')

    def test_second_paragraph(self):
        self.assertRaises(RuntimeError, self.pv_output, 'testparseversions.second-paragraph.control')

    def test_spaces(self):
        self.assertRaises(RuntimeError, self.pv_output, 'testparseversions.spaces.control')

if __name__ == '__main__':
    unittest.main()
