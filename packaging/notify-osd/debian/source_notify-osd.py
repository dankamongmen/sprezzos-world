#!/usr/bin/python

'''Notify-OSD Apport interface

Copyright (C) 2009 Canonical Ltd.
Author: Ara Pulido <ara.pulido@canonical.com>

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.  See http://www.gnu.org/copyleft/gpl.html for
the full text of the license.
'''

import os.path
import subprocess
import os
import apport.hookutils

HOME = os.getenv("HOME")
NOTIFYOSD_LOG = HOME + '/.cache/notify-osd.log'
RELATED_PACKAGES = ['xserver-xorg', 'libgl1-mesa-glx', 'libdrm2', 'xserver-xorg-video-intel', 'xserver-xorg-video-ati']


def installed_version(pkg):
    script = subprocess.Popen(['apt-cache', 'policy', pkg], stdout=subprocess.PIPE)
    output = script.communicate()[0]
    return output.split('\n')[1].replace("Installed: ", "")

def add_info(report):
    
    apport.hookutils.attach_related_packages(report, RELATED_PACKAGES)
    apport.hookutils.attach_hardware(report)
    
    try:
        report['XorgConf'] = open('/etc/X11/xorg.conf').read()
    except IOError:
        pass

    try:
        report['XorgLog']  = open('/var/log/Xorg.0.log').read()
    except IOError:
        pass

    try:
        report['XorgLogOld']  = open('/var/log/Xorg.0.log.old').read()
    except IOError:
        pass    

    report['Lsmod'] = apport.hookutils.command_output(['lsmod'])

    try:
        script = subprocess.Popen(['grep', 'fglrx', '/var/log/kern.log', '/proc/modules'], stdout=subprocess.PIPE)
        matches = script.communicate()[0]
        if (matches):
            report['fglrx-loaded'] = matches
    except OSError:
        pass

    report['Xrandr'] = apport.hookutils.command_output(['xrandr', '--verbose'])

    try:
        monitors_config = os.path.join(os.environ['HOME'], '.config/monitors.xml')
        report['monitors.xml']  = open(monitors_config).read()
    except IOError:
        pass

    report['xdpyinfo'] = apport.hookutils.command_output(['xdpyinfo'])
    report['glxinfo'] = apport.hookutils.command_output(['glxinfo'])
    report['setxkbmap'] = apport.hookutils.command_output(['setxkbmap', '-print'])
    report['setxkbmap'] = apport.hookutils.command_output(['xkbcomp', ':0', '-w0', '-'])

    report['WindowManager'] = apport.hookutils.command_output(['gconftool-2','--get','/desktop/gnome/session/required_components/windowmanager'])
    report['IconTheme'] = apport.hookutils.command_output(['gconftool-2','--get','/desktop/gnome/interface/icon_theme'])
    report['GtkTheme'] = apport.hookutils.command_output(['gconftool-2','--get','/desktop/gnome/interface/gtk_theme'])


## DEBUGING ##
if __name__ == '__main__':
    report = {}
    add_info(report)
    for key in report:
        print '[%s]\n%s' % (key, report[key])
