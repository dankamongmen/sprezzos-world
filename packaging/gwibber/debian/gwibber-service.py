'''apport package hook for gwibber-service

(c) 2010 Canonical Ltd.
Author: Ken VanDine <ken.vandine@ubuntu.com>
'''

from apport.hookutils import *
from os import path, getenv

def add_info(report):
    cache_dir = getenv('XDG_CACHE_HOME', path.expanduser('~/.cache/gwibber/'))
    log_file = path.join(cache_dir, 'gwibber.log')
    attach_file_if_exists(report, log_file, 'gwibber.log')
