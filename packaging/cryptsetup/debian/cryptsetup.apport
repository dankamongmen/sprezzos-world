'''apport package hook for cryptsetup

(c) 2009 Author: Reinhard Tartler <siretart@tauware.de>
'''

from apport.hookutils import *

def add_info(report):
	attach_file(report, '/etc/fstab', 'fstab')
	attach_file_if_exists(report, '/etc/crypttab', 'crypttab')

