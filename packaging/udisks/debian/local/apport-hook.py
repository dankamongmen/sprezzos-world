'''apport package hook for udisks

(c) 2009 Canonical Ltd.
Author: Martin Pitt <martin.pitt@ubuntu.com>
'''

import os
import os.path
import apport.hookutils
import dbus

UDISKS = 'org.freedesktop.UDisks'

def add_info(report):
    apport.hookutils.attach_hardware(report)

    user_rules = []
    for f in os.listdir('/etc/udev/rules.d'):
        if not f.startswith('70-persistent-') and f != 'README':
            user_rules.append(f)

    if user_rules:
        report['CustomUdevRuleFiles'] = ' '.join(user_rules)

    report['UDisksDump'] = apport.hookutils.command_output(['udisks', '--dump'])
    report['Mounts'] = apport.hookutils.command_output(['mount'])

    # grab SMART blobs
    dkd = dbus.Interface(dbus.SystemBus().get_object(UDISKS,
        '/org/freedesktop/UDisks'), UDISKS)
    for d in dkd.EnumerateDevices():
        dev_props = dbus.Interface(dbus.SystemBus().get_object(UDISKS, d),
                dbus.PROPERTIES_IFACE)
        blob = dev_props.Get(UDISKS, 'DriveAtaSmartBlob')
        if len(blob) > 0:
            report['AtaSmartBlob_' + os.path.basename(d)] = ''.join(map(chr, blob))

if __name__ == '__main__':
    r = {}
    add_info(r)
    for k, v in r.items():
        print('%s: "%s"' % (k, v))

