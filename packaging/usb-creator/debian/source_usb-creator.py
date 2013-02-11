'''apport package hook for usb-creator

(c) 2010 Canonical Ltd.
Author: Brian Murray <brian@ubuntu.com>
'''

from apport.hookutils import *
from os import path, getenv

def add_info(report):

    cache_dir = getenv('XDG_CACHE_HOME', path.expanduser('~/.cache'))
    log_file = path.join(cache_dir, 'usb-creator.log')
    attach_file_if_exists(report, log_file, 'UsbCreatorLog')

    report['UsbDevices'] = usb_devices()
    report['UDisksDump'] = apport.hookutils.command_output(['udisks', '--dump'])
