'''apport package hook for simple-scan

(c) 2010 Canonical Ltd.
Author: Robert Ancell <robert.ancell@canonical.com>
'''

import os.path
from apport.hookutils import *

LOG_FILE = os.path.expanduser('~/.cache/simple-scan/simple-scan.log')
driver_packages = ['libsane', 'libsane-extras', 'hplip', 'hpoj']

def add_info(report):
    attach_hardware(report)
    versions = ''
    for package in driver_packages:
        try:
            version = packaging.get_version(package)
        except ValueError:
            version = 'N/A'
        versions += '%s %s\n' % (package, version)
    report['DriverPackageVersions'] = versions
    attach_file_if_exists(report, LOG_FILE, 'SimpleScanLog')
