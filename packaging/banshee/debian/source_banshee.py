import os
import re

import apport.packaging
import apport.hookutils

def mask_string (str):
    MASK = '##MASKED##'
    return str.group(1) + MASK

def mask_values(gconfinfo):
    """ strip personal/private information from the GConf entries """
    pattrn = re.compile ('((library-location|country|file_pattern|folder_pattern|share_password|share_name|username|password)=)(.*)$',
    re.IGNORECASE)
    newReport = ""
    for line in gconfinfo.splitlines():
        line = pattrn.sub (mask_string, line)
        newReport += line + '\n'
    return newReport

def add_info(report, ui):

    # Check to see if the stacktrace is a webkit or libubuntuone issue
    # if it is, then reassign to the Ubuntu One Music Store
    stacktrace = report.get("Stacktrace", None)
    if stacktrace:
        if "ubuntuone" in stacktrace or "webkit" in stacktrace:
            report.add_package_info("banshee-extension-ubuntuonemusicstore")
            return
    

    response = ui.choice("How would you describe the issue?", ["The Banshee interface is not working correctly", "No sound is being played", "Some audio files are not being played correctly", "The Ubuntu One Music Store is not working correctly"], False)

    if response == None: # user cancelled
        raise StopIteration
    if response[0] == 0: # an issue about banshee interface
        apport.hookutils.attach_gconf(report, 'banshee-1')
        report['GConfNonDefault'] = mask_values(report['GConfNonDefault'])
    if response[0] == 1: # the issue is a sound one
        os.execlp('apport-bug', 'apport-bug', 'audio')
    if response[0] == 2: # the issue is a codec one
        report.add_package_info("libgstreamer0.10-0")
        return
    if response[0] == 3: # problem with the music store
        report.add_package_info("banshee-extension-ubuntuonemusicstore")
        return

    report["LogAlsaMixer"] = apport.hookutils.command_output(["/usr/bin/amixer"])
    report["GstreamerVersions"] = apport.hookutils.package_versions("gstreamer*")
    report["XorgLog"] = apport.hookutils.read_file("/var/log/Xorg.0.log")
