'''apport package hook for apparmor

(c) 2009 Canonical Ltd.
Author: Steve Beattie <sbeattie@ubuntu.com>
License: GPLv2
'''

from apport.hookutils import *
from os import path
import re

def recent_kernlog(pattern):
    '''Extract recent messages from kern.log or message which match a regex.

       pattern should be a "re" object.  '''
    lines = ''
    if os.path.exists('/var/log/kern.log'):
        file = '/var/log/kern.log'
    elif os.path.exists('/var/log/messages'):
        file = '/var/log/messages'
    else:
        return lines

    for line in open(file):
        if pattern.search(line):
            lines += line
    return lines

def add_info(report):
    attach_file(report, '/proc/version_signature', 'ProcVersionSignature')
    attach_file(report, '/proc/cmdline', 'ProcKernelCmdline')

    sec_re = re.compile('audit\(|apparmor|selinux|security', re.IGNORECASE)
    report['KernLog'] = recent_kernlog(sec_re)

    packages=['apparmor', 'apparmor-utils', 'libapparmor1',
	'libapparmor-dev', 'libapparmor-perl', 'apparmor-utils',
	'apparmor-docs', 'apparmor-profiles', 'libapache2-mod-apparmor',
	'libpam-apparmor', 'auditd', 'libaudit0']

    versions = ''
    for package in packages:
        try:
            version = packaging.get_version(package)
        except ValueError:
            version = 'N/A'
        if version is None:
            version = 'N/A'
        versions += '%s %s\n' % (package, version)
    report['ApparmorPackages'] = versions

    # These need to be run as root
    report['ApparmorStatusOutput'] = root_command_output(['/usr/sbin/apparmor_status'])
    report['PstreeP'] = command_output(['/usr/bin/pstree', '-p'])
    attach_file_if_exists(report, '/var/log/audit/audit.log', 'audit.log')
