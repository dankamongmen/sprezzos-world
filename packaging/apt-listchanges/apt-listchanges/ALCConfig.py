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

import ConfigParser
import getopt
import sys, os
import re

from ALChacks import *

class ALCConfig:
    def __init__(self):
        # Defaults
        self.frontend = 'pager'
        self.email_address = None
        self.verbose = False
        self.quiet = 0
        self.show_all = False
        self.confirm = False
        self.headers = False
        self.debug = False
        self.save_seen = None
        self.apt_mode = False
        self.profile = None
        self.which = 'both'
        self.allowed_which = ('both', 'news', 'changelogs')
        self.since = None
        self.reverse = False

    def read(self, file):
        self.parser = ConfigParser.ConfigParser()
        self.parser.read(file)

    def expose(self):
        if self.parser.has_section(self.profile):
            for option in self.parser.options(self.profile):
                value = None
                if self.parser.has_option(self.profile, option):
                    if option in ('confirm', 'run', 'show_all', 'headers', 'verbose', 'reverse'):
                        value = self.parser.getboolean(self.profile, option)
                    else:
                        value = self.parser.get(self.profile, option)
                setattr(self, option, value)

    def get(self, option, defvalue=None):
        return getattr(self, option, defvalue)

    def usage(self, exitcode):
        if exitcode == 0:
            fh = sys.stdout
        else:
            fh = sys.stderr

        fh.write(_("Usage: apt-listchanges [options] {--apt | filename.deb ...}\n"))
        sys.exit(exitcode)


    def getopt(self, argv):
        try:
            (optlist, args) = getopt.getopt(argv[1:], 'vf:s:cah', [
                "apt", "verbose", "frontend=", "email-address=", "confirm",
                "all", "headers", "save_seen=", "since=", "debug", "which=",
                "help", "profile=", "reverse"])
        except getopt.GetoptError:
            return None

        # Determine mode and profile before processing other options
        for opt, arg in optlist:
            if opt == '--profile':
                self.profile = arg
            elif opt == '--apt':
                self.apt_mode = True

        # Provide a default profile if none has been specified
        if self.profile is None:
            if self.apt_mode:
                self.profile = 'apt'
            else:
                self.profile = 'cmdline'

        # Expose defaults from config file
        self.expose()

        # Environment variables override config file
        self.frontend = os.getenv('APT_LISTCHANGES_FRONTEND', self.frontend)

        # Command-line options override environment and config file
        for opt, arg in optlist:
            if opt == '--help':
                self.usage(0)
            elif opt in ('-v', '--verbose'):
                self.verbose = 1
            elif opt in ('-f', '--frontend'):
                self.frontend = arg
            elif opt == '--email-address':
                self.email_address = arg
            elif opt in ('-c', '--confirm'):
                self.confirm = 1
            elif opt in ('--since'):
                self.since = arg
            elif opt in ('-a', '--all'):
                self.show_all = 1
            elif opt in ('-h', '--headers'):
                self.headers = 1
            elif opt == '--save_seen':
                self.save_seen = arg
            elif opt == '--which':
                if arg in self.allowed_which:
                    self.which = arg
                else:
                    print _('Unknown option %s for --which.  Allowed are: %s.') % \
                        (arg, ', '.join(self.allowed_which))
                    sys.exit(1)
            elif opt == '--debug':
                self.debug = 1
            elif opt == '--reverse':
                self.reverse = 1

        if self.email_address == 'none':
            self.email_address = None
        if self.save_seen == 'none':
            self.save_seen = None

        if self.since is not None:
            if len(args) is not 1:
                print _('--since=<version> expects a only path to a .deb')
                sys.exit(1)
            self.save_seen = None
        return args

__all__ = [ 'ALCConfig' ]
