'''apport package hook for libmtp

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.  See http://www.gnu.org/copyleft/gpl.html for
the full text of the license.

(c) 2009 Sense Hofstede <sense@qense.nl>
'''

def add_info(report, ui):
    
    ui.information('Please make sure the affected device is connected and on before continuing.')
    
    attach_related_packages(report, [
        "hal",
        "udev",
        ])
        
    # Try using the mtp-detect command to obtain more information
    if command_available("mtp-detect"):
        report['MTPDetect'] = command_output("mtp-detect")
    else:
        ui.information("Please install the package 'mtp-tools' so we can gather "\
                        "more detailed debugging information. Afterwards, rerun " \
                        "the command 'ubuntu-bug libmtp8' or add more information "\
                        "to an existing bug report by running the command "\
                        "'apport-collect -p libmtp8 '<bugnumber>', replacing "\
                        "<bugnumber> with the number of your bug report.")
    
    # Obtain information about changes to udev configuration files
    attach_conffiles(report, "udev")
    # Attach the udev log file
    attach_file_if_exists(report, '/var/log/udev', 'UdevLog')
    
    # Get all connected USB devices
    report['USBDevices'] = usb_devices()
