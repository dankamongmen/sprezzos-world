#!/usr/bin/perl

for my $i (0..$#ARGV) {
    $_ = $ARGV[$i];
    /(.*)-(.*)/m;

    $debver = $1;
    $devrev = $2;

    @revs = split('\.', $devrev);

    $devrev = $revs[0];
    $devrev = "$devrev.$revs[1]" if defined($revs[1]) and $revs[1] ne "0";

    print ", " if $i > 0;
    print "glibc-$debver-$devrev";
}
print "\n";
