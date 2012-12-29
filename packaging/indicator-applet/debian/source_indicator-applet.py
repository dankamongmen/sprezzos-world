import os.path
from apport.hookutils import *
from xdg.BaseDirectory import xdg_cache_home

APPLET_LOG = os.path.join(xdg_cache_home, 'indicator-applet.log')
APPLET_SESSION_LOG = os.path.join(xdg_cache_home, 'indicator-applet-session.log')
COMPLETE_LOG = os.path.join(xdg_cache_home, 'indicator-applet-complete.log')
APPMENU_LOG = os.path.join(xdg_cache_home, 'indicator-applet-appmenu.log')

def add_info(report, ui):
    if not apport.packaging.is_distro_package(report['Package'].split()[0]):
        report['ThirdParty'] = 'True'
        report['CrashDB'] = 'indicator_applet'

    response = ui.choice("How would you describe the issue?", ["None of the indicator menus are working correctly", "The Sound Menu is not working correctly", "The Social Networking Menu is not working correctly", "The login/logout menu is not working correctly", "The Networking Menu is not working correctly", "The date/time menu is not working correctly", "The application testing menu is not working correctly", "The all-in-one (complete) applet is not working correctly", "Another menu is not working correctly"], False)
    try:
        if response == None: # user cancelled
            raise StopIteration
        if response[0] == 0: # an issue with indicator-applet
            report.add_package_info("indicator-applet")
        if response[0] == 1: # the issue is with indicator-sound
            report.add_package_info("indicator-sound")
        if response[0] == 2: # the issue is with indicator-me
            report.add_package_info("indicator-me")
        if response[0] == 3: # the issue is with indicator-session
            report.add_package_info("indicator-session")
        if response[0] == 4: # the issue is with indicator-network in UNE
            report.add_package_info("indicator-network")
        if response[0] == 5: # the issue is with indicator-datetime in "unity"
            report.add_package_info("indicator-datetime")
        if response[0] == 6: # the issue is with indicator-applet-appmenu
            report.add_package_info("indicator-applet-appmenu")
        if response[0] == 7: # the issue is with indicator-applet-complete
            report.add_package_info("indicator-applet-complete")            
        if response[0] == 8: # the issue is with indicator-application
            report.add_package_info("indicator-application")
    except:
        print 'Unfortunately, that package is not installed. Please refer to https://wiki.ubuntu.com/Bugs/FindRightPackage'

    attach_file_if_exists(report, APPLET_LOG, 'indicator-applet.log')
    attach_file_if_exists(report, APPLET_SESSION_LOG, 'indicator-applet-session.log')
    attach_file_if_exists(report, COMPLETE_LOG, 'indicator-applet-complete.log')
    attach_file_if_exists(report, APPMENU_LOG, 'indicator-applet-appmenu.log')
