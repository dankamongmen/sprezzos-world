#!/usr/bin/perl -wl

use strict;
use File::Glob ':glob';

my $confdir = shift || "/etc/lighttpd/";
my $enabled = "conf-enabled/*.conf";

chdir($confdir);
my @files = bsd_glob($enabled);

for my $file (@files)
{
	print "include \"$file\"";
}
