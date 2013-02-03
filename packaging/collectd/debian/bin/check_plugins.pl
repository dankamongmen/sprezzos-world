#! /usr/bin/perl --
#
# collectd - check_plugins.pl
# Copyright (C) 2006, 2007 Sebastian Harl
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; only version 2 of the License is applicable.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
#
# Author:
#   Sebastian Harl <sh at tokkee.org>

# This script checks each plugin and reports the address of each plugin's
# registered functions. It uses src/.libs/*.so for its checks.  This can be
# used to check what kind of operations each plugin supports after it has been
# built.

use strict;
use warnings;

my $srcdir = 'src/';
my $libdir = 'src/.libs';

my $plugins = {};

my ($srcs, $libs) = (undef, undef);

if (! opendir($srcs, $srcdir)) {
	print STDERR "Could not open directory '$srcdir': $!\n"
		. "Make sure you are in the toplevel source directory.\n";
	exit 1;
}

while (my $dirent = readdir($srcs)) {
	if ($dirent !~ m/^(.*)\.c$/) {
		next;
	}

	my $name = $1;
	my $src  = undef;

	if (! open($src, "<", "$srcdir/$dirent")) {
		print STDERR "Unable to open '$srcdir/$dirent': $!\n";
		next;
	}

	while (my $line = <$src>) {
		if ($line =~ m/plugin_register_(\w+)\s*\("([^"]+)",\s*(\w+)/) {
			my ($t, $n, $f) = ($1, $2, $3);

			$plugins->{$name}->{$n}->{$t} = $f;
		}
	}

	close($src);
} # while (my $dirent = readdir($srcs))

closedir($srcs);

if (! opendir($libs, $libdir)) {
	print STDERR "Could not open directory '$libdir': $!\n"
		. "Make sure you ran 'make'.\n";
	exit 1;
}

while (my $dirent = readdir($libs)) {
	if ($dirent !~ m/^(.*)\.so$/) {
		next;
	}

	my $name = $1;
	my $nm   = undef;

	if (! defined $plugins->{$name}) {
		print STDERR "No information available for plugin '$name'!\n";
		next;
	}

	if (! open($nm, "-|", "nm $libdir/$dirent")) {
		print STDERR "Unable to open pipe from nm(1): $!\n";
		next;
	}

	while (my $line = <$nm>) {
		if ($line !~ m/^([0-9a-fA-F]{8,}) [tT] (\w+)$/) {
			next;
		}

		my $adr = $1;
		my $sym = $2;

		for my $n (keys %{$plugins->{$name}}) {
			for my $t (keys %{$plugins->{$name}->{$n}}) {
				if (defined $plugins->{$name}->{$n}->{$t}
						&& ($sym eq $plugins->{$name}->{$n}->{$t})) {
					$plugins->{$name}->{$n}->{$t} = "0x" . $adr;
				}
			}
		}
	}

	close($nm);
} # while (my $dirent = readdir($libs))

closedir($libs);

print 'plugin name     config   init     read     write    log      shutdown';
print $/ . '-' x 70 . $/;

for my $name (sort keys %$plugins) {
	if (! -f "$libdir/$name.so") {
		print "$name.c has not been compiled.\n";
		next;
	}

	for my $n (sort keys %{$plugins->{$name}}) {
		dump_plugin_data($n, $plugins->{$name}->{$n});
	}
}

exit 0;

sub dump_plugin_data {
	my $name  = shift || return;
	my $funcs = shift || return;

	if (length($name) > 15) {
		$name = substr($name, 0, 12) . '...';
	}

	printf '%-15s ', $name;

	foreach my $t ("config", "init", "read", "write", "log", "shutdown") {
		if (! defined $funcs->{$t}) {
			print '-        ';
		}
		elsif ($funcs->{$t} =~ m/^0x[A-Fa-f0-9]{8,}$/) {
			print substr($funcs->{$t}, -8, 8) . " ";
		}
		else {
			print 'nA       ';
		}
	}

	print $/;
	return 1;
} # sub dump_plugin_data

# vim: set sw=4 ts=4 tw=78 noexpandtab :
