#vim:set fileencoding=utf-8:
#
#   apt-listchanges - Show changelog entries between the installed versions
#                     of a set of packages and the versions contained in
#                     corresponding .deb files
#
#   Copyright (C) 2000-2006  Matt Zimmerman  <mdz@debian.org>
#   Copyright (C) 2006       Pierre Habouzit <madcoder@debian.org>
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public
#   License along with this program; if not, write to the Free
#   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
#   MA 02111-1307 USA
#

import sys
import os
import os.path
import re
import locale
import email.Message
import email.Header
import email.Charset
import cStringIO
import tempfile
from ALChacks import *

# TODO:
# newt-like frontend, or maybe some GUI bit
# keep track of tar/dpkg-deb errors like in pre-2.0

def read_apt_pipeline(config):
    if config.debug:
        sys.stderr.write("APT pipeline messages:\n")
    version = sys.stdin.readline().rstrip()
    if config.debug:
        sys.stderr.write("\t%s\n" % version)
    if version != "VERSION 2":
        sys.stderr.write(_('''Wrong or missing VERSION from apt pipeline
(is Dpkg::Tools::Options::/usr/bin/apt-listchanges::Version set to 2?)
'''))
        sys.exit(1)

    while 1:
        line = sys.stdin.readline().rstrip()
        if config.debug:
            sys.stderr.write("\t%s\n" % line)
        if not line:
            break
        if line.startswith('quiet='):
            config.quiet = int(line[len('quiet='):])

    filenames = {}
    toconfig = []
    toremove = []

    for pkgline in sys.stdin.readlines():
        pkgline = pkgline.rstrip()
        if config.debug:
            sys.stderr.write("\t%s\n" % pkgline)
        if not pkgline:
            break

        (pkgname, oldversion, compare, newversion, filename) = pkgline.split()
        if compare != '<' or oldversion == '-':
            continue

        if filename == '**CONFIGURE**':
            toconfig.append(pkgname)
        elif filename == '**REMOVE**' or filename == '**ERROR**':
            toremove.append(pkgname)
        else:
            filenames[pkgname] = filename

    # Sort by configuration order.  THIS IS IMPORTANT.  Sometimes, a
    # situation exists where package X contains changelog.gz (upstream
    # changelog) and depends on package Y which contains
    # changelog.Debian.gz (Debian changelog).  Until we have a more
    # reliable method for determining whether a package is Debian
    # native, this allows things to work, since Y will always be
    # configured first.

    # apt doesn't explicitly configure everything anymore, so sort
    # the things to be configured first, and then do everything else
    # in alphabetical order.  Also, drop from the list everything
    # that's to be removed.
    for pkg in toremove:
        del filenames[pkg]

    ordered_filenames = []
    for pkg in toconfig:
        if pkg in filenames:
            ordered_filenames.append(filenames[pkg])
            del filenames[pkg]

    ordered_filenames.extend(sorted(filenames.values()))
    return ordered_filenames

def mail_changes(address, changes, subject):
    print "apt-listchanges: " + _("Mailing %s: %s") % (address, subject)

    charset = email.Charset.Charset('utf-8')
    charset.body_encoding = '8bit'
    charset.header_encoding = email.Charset.QP
    message = email.Message.Message()
    message.set_charset(charset)
    subject = unicode(subject.decode(locale.getpreferredencoding() or 'ascii', 'replace'))
    message['Auto-Submitted'] = 'auto-generated'
    message['Subject'] = email.Header.Header(subject, 'utf-8')
    message['To'] = address
    message.set_payload(changes)

    fh = os.popen('/usr/sbin/sendmail -oi -t', 'w')
    fh.write(message.as_string())
    fh.close()

def make_frontend(name, packages, config):
    frontends = { 'text' : text,
                  'pager' : pager,
                  'debconf': debconf,
                  'mail' : mail,
                  'browser' : browser,
                  'xterm-pager' : xterm_pager,
                  'xterm-browser' : xterm_browser }

    if name in ('newt', 'w3m', 'xterm-w3m'):
        sys.stderr.write((_("The %s frontend is deprecated, using pager") + '\n') % name)
        name = 'pager'

    if name == "mail" and not os.path.exists("/usr/sbin/sendmail"):
        sys.stderr.write((_("The mail frontend needs a installed 'sendmail', using pager") + '\n'))
        name = 'pager'

    # TODO: it would probably be nice to have a frontends subdir and
    # import from that. that would mean a uniform mechanism for all
    # frontends (that would become small files inside
    if name == "gtk":
        if os.environ.has_key("DISPLAY"):
            try:
                gtk = __import__("AptListChangesGtk")
                frontends[name] = gtk.gtk2
            except ImportError, e:
                sys.stderr.write(_("The gtk frontend needs a working python-gtk2 "
                                   "and python-glade2.\n"
                                   "Those imports can not be found. Falling back "
                                   "to pager.\n"
                                   "The error is: %s\n") % e)
                name = 'pager'
        else:
            name = 'pager'

    if not frontends.has_key(name):
        return None
    return frontends[name](packages, config)

class frontend:
    def __init__(self, packages, config):
        self.packages = packages
        self.config = config

    def update_progress(self):
        pass

    def progress_done(self):
        pass

    def display_output(self, text):
        pass

    def _render(self, text):
        newtext = []
        for line in text.split('\n'):
            try:
                # changelogs are supposed to be in UTF-8
                uline = line.decode('utf-8')
            except UnicodeError:
                # ... but handle gracefully if they aren't.
                # (That's also the reason we do it line by line.)
                # This is possibly wrong, but our best guess.
                uline = line.decode('iso8859-1')
            newtext.append(uline.encode(locale.getpreferredencoding() or 'ascii', 'replace'))
        return '\n'.join(newtext)

    def confirm(self):
        return 1

    def set_title(self, text):
        pass

class debconf(frontend):
    def set_title(self, text):
        self.title = text

    def display_output(self, text):
        import socket
        import debconf as dc
        if 'DEBIAN_FRONTEND' not in os.environ or os.environ['DEBIAN_FRONTEND'] != 'passthrough':
            return
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM, 0)
        sock.connect(os.environ['DEBCONF_PIPE'])
        dcfd = sock.makefile()
        sock.close()
        db = dc.Debconf(read=dcfd, write=dcfd)
        tmp = tempfile.NamedTemporaryFile(prefix="apt-listchanges-tmp")
        os.fchmod(tmp.fileno(), 0644)
        tmp.write('''Template: apt-listchanges/info
Type: title
Description: NEWS

Template: apt-listchanges/title
Type: title
Description: ${title}

Template: apt-listchanges/news
Type: note
Description: ${packages} packages''')
        for line in text.split('\n'):
            if line.strip():
                tmp.write('  ' + line + '\n')
            else:
                tmp.write(' .\n')
        tmp.flush()
        db.command('x_loadtemplatefile', tmp.name)
        tmp.close()
        db.info('apt-listchanges/info')
        db.subst('apt-listchanges/title', 'title', self.title)
        db.subst('apt-listchanges/news', 'packages', self.packages)
        db.settitle('apt-listchanges/title')
        db.fset('apt-listchanges/news', 'seen', 'false')
        db.input('high', 'apt-listchanges/news')
        db.go()
        dcfd.close()

class ttyconfirm:
    def confirm(self):
        try:
            tty = open('/dev/tty', 'r+')
        except IOError, e:
            return -1
        tty.write('apt-listchanges: ' + _('Do you want to continue? [Y/n] '))
        tty.flush()
        response = tty.readline()
        return response == '\n' or re.search(locale.nl_langinfo(locale.YESEXPR),
                                             response)

class simpleprogress:
    def update_progress(self):
        if self.config.quiet > 1:
            return

        if not hasattr(self, 'message_printed'):
            self.message_printed = 1
            sys.stderr.write(_("Reading changelogs") + "...\n")

    def progress_done(self):
        pass

class mail(simpleprogress, frontend):
    pass

class text(simpleprogress, ttyconfirm, frontend):
    def display_output(self, text):
        sys.stdout.write(text)

class fancyprogress:
    def update_progress(self):
        if not hasattr(self, 'progress'):
            # First call
            self.progress = 0
            self.line_length = 0

        self.progress += 1
        line = _("Reading changelogs") + "... %d%%" % (self.progress * 100 / self.packages)
        self.line_length = len(line)
        sys.stdout.write(line + '\r')
        sys.stdout.flush()

    def progress_done(self):
        if hasattr(self, 'line_length'):
            sys.stdout.write(' ' * self.line_length + '\r')
            sys.stdout.write(_("Reading changelogs") + "... " + _("Done") + "\n")
            sys.stdout.flush()

class runcommand:
    mode = os.P_WAIT
    suffix = ''

    def display_output(self, text):
        if self.mode == os.P_NOWAIT:
            if os.fork() != 0:
                return

        tmp = tempfile.NamedTemporaryFile(prefix="apt-listchanges-tmp", suffix=self.suffix)
        tmp.write(self._render(text))
        tmp.flush()
        shellcommand = self.get_command() + ' ' + tmp.name

        status = os.spawnl(os.P_WAIT, '/bin/sh', 'sh', '-c', shellcommand)
        if status != 0:
            raise OSError('Subprocess ' + shellcommand + ' exited with status ' + str(status))

        if self.mode == os.P_NOWAIT:
            # We are a child; exit
            sys.exit(0)

    def get_command(self):
        return self.command

class pager(runcommand, ttyconfirm, fancyprogress, frontend):
    def __init__(self, *args):
        if not 'LESS' in os.environ:
            os.environ['LESS'] = "-P?e(q to quit)"
        apply(frontend.__init__, [self] + list(args))
        self.command = self.config.get('pager', 'sensible-pager')

class xterm(runcommand, ttyconfirm, fancyprogress, frontend):
    def __init__(self, *args):
        apply(frontend.__init__, [self] + list(args))
        self.mode = os.P_NOWAIT
        self.xterm = self.config.get('xterm', 'x-terminal-emulator')

    def get_command(self):
        return self.xterm + ' -T apt-listchanges -e ' + self.xterm_command

class xterm_pager(xterm):
    def __init__(self, *args):
        apply(xterm.__init__, [self] + list(args))
        self.xterm_command = self.config.get('pager', 'sensible-pager')

class html:
    suffix = '.html'

    # LP bug-closing format requires the colon after "LP", but many people
    # say "LP #123456" when talking informally about bugs.
    lp_bug_stanza_re = re.compile(r'(?:lp:?\s+\#\d+(?:,\s*\#\d+)*)', re.I)
    lp_bug_re        = re.compile('(?P<linktext>#(?P<bugnum>\d+))', re.I)
    lp_bug_fmt       = r'<a href="https://launchpad.net/bugs/\g<bugnum>">\g<linktext></a>'
    bug_stanza_re = re.compile(r'(?:closes:\s*(?:bug)?\#?\s?\d+(?:,\s*(?:bug)?\#?\s?\d+)*|(?<!">)#\d+)', re.I)
    bug_re        = re.compile('(?P<linktext>#?(?P<bugnum>\d+))', re.I)
    bug_fmt       = r'<a href="http://bugs.debian.org/\g<bugnum>">\g<linktext></a>'
    # regxlib.com
    email_re = re.compile(r'([a-zA-Z0-9_\-\.]+)@(([[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.)|(([a-zA-Z0-9\-]+\.)+))([a-zA-Z]{2,4}|[0-9]{1,3})(\]?)')

    title = '''apt-listchanges output'''

    def _render(self, text):
        htmltext = cStringIO.StringIO()
        htmltext.write('''<html>
        <head>
        <title>''')
        htmltext.write(self.title)
        htmltext.write('''</title>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        </head>

        <body>
        <pre>''')

        for line in text.split('\n'):
            try:
                # changelogs are supposed to be in UTF-8
                uline = line.decode('utf-8')
            except UnicodeError:
                # ... but handle gracefully if they aren't.
                # This is possibly wrong, but our best guess.
                uline = line.decode('iso8859-1')
            line = uline.encode('utf-8').replace(
                '&', '&amp;').replace(
                '<', '&lt;').replace(
                '>', '&gt;')
            line = self.lp_bug_stanza_re.sub(lambda m: self.lp_bug_re.sub(self.lp_bug_fmt, m.group(0)), line)
            line = self.bug_stanza_re.sub(lambda m: self.bug_re.sub(self.bug_fmt, m.group(0)), line)
            line = self.email_re.sub(r'<a href="mailto:\g<0>">\g<0></a>', line)
            htmltext.write(line + '\n')
        htmltext.write('</pre></body></html>')

        return htmltext.getvalue()

class browser(html, pager):
    def __init__(self, *args):
        apply(pager.__init__, [self] + list(args))
        self.command = self.config.get('browser', 'sensible-browser')
    def set_title(self, text):
        self.title = text

class xterm_browser(html, xterm):
    def __init__(self, *args):
        apply(xterm.__init__, [self] + list(args))
        self.xterm_command = self.config.get('browser', 'sensible-browser')

