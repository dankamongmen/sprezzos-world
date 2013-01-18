#!/usr/bin/python
#
#    postfix apport package hook
#
#    Copyright (C) 2011 Canonical Ltd. All Rights Reserved.
#    Author: Clint Byrum <clint.byrum@canonical.com>
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

import os
import apport
import re
import gettext

gettext.install('postfix-apport-hook')

msg = _('In order for the developers to determine the cause of this, some'
        ' potentially sensitive information from your system configuration may'
        ' be helpful. Specifically, your hostname and DNS configuration. Please'
        ' note that this will be included in a *PUBLIC* bug report.' )

msg2 = _('Do you want to add this extra information to the bug report?')

host_re = re.compile('^[a-zA-Z0-9][a-zA-Z0-9\-\.]*$')

def add_info(report, ui):
    extra_info=dict()

    if os.path.exists('/etc/mailname'):
        extra_info['EtcMailname'] = open('/etc/mailname').read().strip()
    else:
        extra_info['EtcMailname'] = _('*** /etc/mailname does not exist ***')
    extra_info['Hostname'] = apport.hookutils.command_output(['hostname','--fqdn'])
    extra_info['PostconfMyhostname'] = apport.hookutils.command_output(['/usr/sbin/postconf','-h','myhostname'])
    extra_info['PostconfMydomain'] = apport.hookutils.command_output(['/usr/sbin/postconf','-h','mydomain'])

    """ Note that even if the user opts not to send the info, we get this key """
    for k,v in extra_info.iteritems():
        if not host_re.match(v):
            report['DuplicateSignature'] = 'InvalidHostOrDomain'
            break

    # Do not include this in the dupes since it is usually "/etc/mailname"
    extra_info['PostconfMyorigin'] = apport.hookutils.command_output(['/usr/sbin/postconf','-h','myorigin'])

    if os.path.exists('/etc/resolv.conf'):
        extra_info['ResolvConf'] = open('/etc/resolv.conf').read()
    else:
        extra_info['ResolvConf'] = _('*** /etc/resolv.conf does not exist ***')

    eeinfo = [("%s: %s" % (k,v)) for k,v in extra_info.iteritems()]
    answer = ui.yesno("%s\n\n%s\n\n%s" % (msg, msg2 ,"\n".join(eeinfo)))
    if answer:
        report.update(extra_info)
        return
