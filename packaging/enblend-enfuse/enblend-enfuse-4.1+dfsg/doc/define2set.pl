#! /usr/bin/env perl

use strict;

while ( my $l = <> ) {
    next if ($l !~ /^\s*#define/);
    chomp($l);
    if($l =~ s/^\s*#define\s+(\S+)\s+"?([^"]*)"?/\@set CFG::$1 $2/) {
	print "$l\n";
    }
}
exit(0);
