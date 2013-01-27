#!/usr/bin/perl

# AptPkg::Version tests

BEGIN { print "1..21\n" }

use AptPkg::Config '$_config';
use AptPkg::System '$_system';
use AptPkg::Version;

print 'not ' unless $_config->init and $_system = $_config->system;
print "ok 1\n";

my $v = $_system->versioning;
print 'not ' unless $v;
print "ok 2\n";

print 'not ' unless $v->compare('1.0', '1.1') < 0;	print "ok 3\n";
print 'not ' unless $v->compare('1.1', '1.0') > 0;	print "ok 4\n";
print 'not ' unless $v->compare('1.1', '1.1') == 0;	print "ok 5\n";
print 'not ' unless $v->compare('1.2-1', '1.2-2') < 0;	print "ok 6\n";
print 'not ' unless $v->compare('1.2.3', '1.20.2') < 0;	print "ok 7\n";
print 'not ' unless $v->compare('1.2.3', '1:1.0') < 0;	print "ok 8\n";

print 'not ' unless $v->check_dep('1', '<<', '2');	print "ok 9\n";
print 'not ' unless $v->check_dep('1', '<=', '2');	print "ok 10\n";
print 'not ' unless $v->check_dep('2', '<=', '2');	print "ok 11\n";
print 'not ' unless $v->check_dep('1', '=',  '1');	print "ok 12\n";
print 'not ' unless $v->check_dep('2', '>=', '2');	print "ok 13\n";
print 'not ' unless $v->check_dep('2', '>=', '1');	print "ok 14\n";
print 'not ' unless $v->check_dep('2', '>>', '1');	print "ok 15\n";

print 'not ' if $v->check_dep('1', '>>', '2');		print "ok 16\n";
print 'not ' if $v->check_dep('1', '>=', '2');		print "ok 17\n";
print 'not ' if $v->check_dep('1', '=',  '2');		print "ok 18\n";
print 'not ' if $v->check_dep('2', '<=', '1');		print "ok 19\n";
print 'not ' if $v->check_dep('2', '<<', '1');		print "ok 20\n";

print 'not ' unless $v->upstream("1:6.12-3-1.2") eq '6.12-3';
print "ok 21\n";

1;
