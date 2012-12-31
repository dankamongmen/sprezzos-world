#! /usr/bin/env perl

use strict;

while (my $l = <> ) {
    next if ($l !~ /---BEGIN-TEXT---/);
    last;
}

while (my $l = <> ) {
    last if ($l =~ /---END-TEXT---/);
    $l =~ s/^# //;
    print $l;
}
exit(0);
