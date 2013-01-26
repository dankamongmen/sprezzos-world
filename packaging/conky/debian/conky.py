'''apport package hook for conky

Collect conky conf file, conky user's conf file and any specified lua script

Copyright 2009 Cesare Tirabassi <norsetto@ubuntu.com>

'''

from apport.hookutils import *
from os import path
import re

def add_info(report):
	attach_conffiles(report, 'conky')
        conkyrc_path = path.expanduser('~/.conkyrc')
        if path.exists(conkyrc_path):
            attach_file(report, conkyrc_path)
            for file in re.findall("^lua_load\s+(.*?)$",
                                   open(conkyrc_path).read(),
                                   re.MULTILINE):
                attach_file_if_exists(report, file)
