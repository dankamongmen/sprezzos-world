# Copyright © 2011 Raphaël Hertzog <hertzog@debian.org>
# Copyright © 2012 Guillem Jover <guillem@debian.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

package Dpkg::File;

use strict;
use warnings;

our $VERSION = "0.01";

use Fcntl qw(:flock);
use Dpkg::Gettext;
use Dpkg::ErrorHandling;

use base qw(Exporter);
our @EXPORT = qw(file_lock);

sub file_lock($$) {
    my ($fh, $filename) = @_;

    # A strict dependency on libfile-fcntllock-perl being it an XS module,
    # and dpkg-dev indirectly making use of it, makes building new perl
    # package which bump the perl ABI impossible as these packages cannot
    # be installed alongside.
    eval 'use File::FcntlLock';
    if ($@) {
        warning(_g("File::FcntlLock not available; using flock which is not NFS-safe"));
        flock($fh, LOCK_EX) ||
            syserr(_("failed to get a write lock on %s"), $filename);
    } else {
        eval q{
            my $fs = File::FcntlLock->new(l_type => F_WRLCK);
            $fs->lock($fh, F_SETLKW) ||
                syserr(_("failed to get a write lock on %s"), $filename);
        }
    }
}

1;
