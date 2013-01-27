#!/usr/bin/perl

# AptPkg::System tests

BEGIN { print "1..2\n" }

use AptPkg::Config '$_config';
use AptPkg::System '$_system';

print 'not ' unless $_config->init and $_system = $_config->system;
print "ok 1\n";

my $type;
$type ||= 'dpkg' if -f '/etc/debian_version';
$type ||= 'rpm'  if -f '/etc/redhat-release';
unless ($type)
{
    print "# unknown system type, defaulting to dpkg\n";
    $type = 'dpkg';
}

print 'not ' unless $_system->label =~ /$type/;
print "ok 2\n";

1;
