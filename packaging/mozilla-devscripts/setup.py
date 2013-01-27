#!/usr/bin/python

import glob
import os
import re

from distutils.core import setup

def get_debian_version():
    """look what Debian version we have"""
    version = None
    changelog = "debian/changelog"
    if os.path.exists(changelog):
        head = open(changelog).readline()
        match = re.compile(".*\((.*)\).*").match(head)
        if match:
            version = match.group(1)
    return version

SCRIPTS = [
    'dh_xul-ext',
    'install-xpi',
    'xpi-pack',
    'xpi-repack',
    'xpi-unpack',
    'moz-version',
]

if __name__ == '__main__':
    setup(name='mozilla-devscripts',
          version=get_debian_version(),
          scripts=SCRIPTS,
          py_modules=['moz_version'],
          data_files=[('share/doc/mozilla-devscripts', ['README']),
                      ('share/man/man1', glob.glob("man/*.1")),
                      ('share/mozilla-devscripts',
                       ['data/xpi.mk'] + glob.glob('data/xul-app-data.csv.*')),
                      ('share/perl5/Debian/Debhelper/Buildsystem',
                       ['perl/Debian/Buildsystem/xul_ext.pm']),
                      ('share/perl5/Debian/Debhelper/Sequence',
                       ['perl/Debian/Sequence/xul_ext.pm']),
                     ],
    )
