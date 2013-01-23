#vim:set fileencoding=utf-8:
#
#   apt-listchanges - Show changelog entries between the installed versions
#                     of a set of packages and the versions contained in
#                     corresponding .deb files
#
#   Copyright (C) 2007       Pierre Habouzit <madcoder@debian.org>
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

import gettext
import locale
import sys

try:
    locale.setlocale(locale.LC_ALL, '')
except locale.Error:
    sys.stderr.write("Can't set locale; make sure $LC_* and $LANG are correct!\n")
    locale.setlocale(locale.LC_ALL, 'C')

def _(x):
    try:
        return gettext.translation('apt-listchanges').lgettext(x)
    except: return x

# forcibly export _
__all__ = ['_']
