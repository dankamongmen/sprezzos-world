from apport.hookutils import *

def add_info(report, ui=None):
    if not apport.packaging.is_distro_package(report['Package'].split()[0]):
        report['ThirdParty'] = 'True'
        report['CrashDB'] = 'signon'
