#!/usr/bin/perl

# AptPkg::Cache auto-init test

BEGIN { print "1..3\n" }

use AptPkg::Cache;

if (my $cache = AptPkg::Cache->new)
{
    print "ok 1\n";
    if (my $apt = $cache->{apt})
    {
        print "ok 2\n";
        print 'not ' unless $apt->{Name} eq 'apt'; print "ok 3\n";
    }
    else
    {
        print "not ok 2\n";
        print "ok 3 # skip\n";
    }
}
else
{
    print "not ok 1\n";
    print "ok $_ # skip\n" for 2..3;
}

