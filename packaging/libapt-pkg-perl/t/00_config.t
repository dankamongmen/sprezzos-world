#!/usr/bin/perl

# AptPkg::Config tests

BEGIN { print "1..23\n" }

use AptPkg::Config;

# create
my $c = AptPkg::Config->new;
print "ok 1\n";

# set
print 'not ' unless $c->set(a => 42) == 42;
print "ok 2\n";

# get
print 'not ' unless $c->get('a') == 42;
print "ok 3\n";

# default
print 'not ' unless $c->get('not there', 'default') eq 'default';
print "ok 4\n";

# get_file, get_dir
$c->set(b => "/tmp/");
$c->set(b::c => "foo");
print 'not ' unless $c->get_file('b::c') eq '/tmp/foo';
print "ok 5\n";

print 'not ' unless $c->get_dir('b::c') eq '/tmp/foo/';
print "ok 6\n";

# using /f, /d suffix (FindAny)
print 'not ' unless $c->get('b::c/f') eq '/tmp/foo';
print "ok 7\n";

print 'not ' unless $c->get('b::c/d') eq '/tmp/foo/';
print "ok 8\n";

# get_bool
$c->set(c => 'false');
$c->set(d => 'yes');
print 'not ' if $c->get_bool('c');
print "ok 9\n";

print 'not ' unless $c->get_bool('d');
print "ok 10\n";

# exists
print 'not ' unless $c->exists('c');
print "ok 11\n";

print 'not ' if $c->exists('not there');
print "ok 12\n";

# read_file
{
    local $SIG{__WARN__} = sub { die @_ };
    eval { $c->read_file('t/config.conf') };
}
print 'not ' if $@;
print "ok 13\n";

print 'not ' unless $c->get('e') eq 'e_val' and $c->get('e::f') eq 'f_val';
print "ok 14\n";

# parse_cmdline
my @r = $c->parse_cmdline([
	['q', 'qtest', 'qtest', 'int_level'],
	['r', 'rtest', 'rtest', 'int_level'],
	['s', 'stest', 'stest', 'int_level'],
	['t', 'ttest', 'ttest', 'has_arg'],
	['o', 'otest', '', 'arb_item'],
    ], '-q', '-rr', '-s=3', '--ttest=t', '-ofoo=bar', 'cmd');

print 'not ' if $c->get('qtest') != 1 or
		$c->get('rtest') != 2 or
		$c->get('stest') != 3 or
		$c->get('ttest') ne 't' or
		$c->get('foo') ne 'bar' or
		"@r" ne 'cmd';

print "ok 15\n";

# XS methods
print 'not ' unless $c->Find('a') == 42;
print "ok 16\n";

# $_config, init
$c = $AptPkg::Config::_config;
print 'not ' unless $c->init;
print "ok 17\n";

print 'not ' unless $c->get_dir('Dir::Etc') eq '/etc/apt/';
print "ok 18\n";

# hash access
$c = AptPkg::Config->new;
$c->{a} = 42;
$c->{b} = 'foo';

print 'not ' unless $c->{a} == 42 and $c->{b} eq 'foo';
print "ok 19\n";

@r = keys %$c;
print 'not ' unless "@r" eq 'a b';
print "ok 20\n";

@r = values %$c;
print 'not ' unless "@r" eq '42 foo';
print "ok 21\n";

# explicit iterator
@r = $c->keys;
print 'not ' unless "@r" eq 'a b';
print "ok 22\n";

@r = ();
for (my $i = $c->keys; my $k = $i->next; ) { push @r, $k }
print 'not ' unless "@r" eq 'a b';
print "ok 23\n";

1;
