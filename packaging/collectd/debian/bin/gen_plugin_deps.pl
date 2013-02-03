#! /usr/bin/perl
#
# collectd - gen_plugin_deps.pl
# Copyright (C) 2007 Sebastian Harl
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

use strict;
use warnings;

my $extra_deps = {
	sensors => [ 'lm-sensors' ],
};

my $infile  = "debian/README.Debian.plugins.in";
my $outfile = "debian/README.Debian.plugins";

my ($ifile, $ofile);

if (! open($ifile, "<", $infile)) {
	print STDERR "Could not open file '$infile': $!\n";
	exit 1;
}

if (! open($ofile, ">", $outfile)) {
	print STDERR "Could not open file '$outfile': $!\n";
	exit 1;
}

while (my $line = <$ifile>) {
	if ($line !~ m/^\@PLUGIN_DEPS\@\n$/) {
		print $ofile $line;
	}
	else {
		print_plugin_deps($ofile);
	}
}

close($ofile);
close($ifile);

sub print_plugin_deps
{
	my $fh   = shift;
	my $pdir = undef;
	my $i    = 0;

	my $plugindir = "debian/collectd-core/usr/lib/collectd/";

	if (! opendir($pdir, $plugindir)) {
		print STDERR "Could not open directory '$plugindir': $!\n";
		exit 1;
	}

	foreach my $dirent (sort readdir($pdir)) {
		if ($dirent !~ m/^(\w+).so$/) {
			next;
		}

		my $name = $1;
		my $deps = `dpkg-shlibdeps -O $plugindir/$dirent`;

		chomp $deps;

		$deps =~ s/^shlibs:Depends=//;

		my @deps = grep !m/^libc6\b/, split m/, /, $deps;

		if (scalar @deps) {
			if (0 < $i) {
				print $fh "\n";
			}

			++$i;

			print $fh "$name:\n";

			if (defined $extra_deps->{$name}) {
				unshift @deps, @{$extra_deps->{$name}};
			}

			foreach my $dep (@deps) {
				print $fh " * $dep\n";
			}
		}
	}
}

# vim: set tw=78 sw=4 ts=4 noexpandtab :

