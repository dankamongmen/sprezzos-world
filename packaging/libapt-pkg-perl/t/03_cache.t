#!/usr/bin/perl

# AptPkg::Cache tests

BEGIN { print "1..62\n" }

sub check_keys
{
    my ($test, $thing, $expect) = @_;
    for my $key (keys %$expect)
    {
	print $thing->{$key} eq $expect->{$key}
	    ? "ok $test\n"
	    : "not ok $test # $thing->{$key} != $expect->{$key}\n";

	$test++;
    }

    $test;
}

use AptPkg::Config '$_config';
use AptPkg::System '$_system';
use AptPkg::Cache;

$_config->init;
$_config->read_file('t/cache/etc/apt.conf');
$_system = $_config->system;

print "ok 1\n";

my $cache = AptPkg::Cache->new;

# cache created
unless ($cache)
{
    print "not ok 2\n";
    exit;
}

my $verfile; # for ->packages test

# package "b" exists
if (my $b = $cache->{b})
{
    print "ok 2\n";

    # check some string values
    check_keys 3, $b, {
	Name => 'b',
	Section => 'test',
	SelectedState => 'Install',
	InstState => 'Ok',
	CurrentState => 'Installed',
    };

    # expected current version
    print 'not ' unless $b->{CurrentVer}{VerStr} eq '0.2-1';
    print "ok 8\n";

    # installed and two new versions are available
    print 'not ' unless @{$b->{VersionList}} == 3;
    print "ok 9\n";

    # newest version is first
    print 'not ' unless $b->{VersionList}[0]{VerStr} eq '0.3-2';
    print "ok 10\n";

    # stash verfile from stable version
    if (my $f = $b->{VersionList}[1]{FileList})
    {
	$verfile = $f->[0];
    }
    else
    {
	print 'not ';
    }

    print "ok 11\n";

    # last is current version
    print 'not ' unless $b->{VersionList}[-1]{VerStr} eq '0.2-1';
    print "ok 12\n";

    # check rev depends
    my $r;
    if (@{$b->{RevDependsList}} == 2)
    {
	($r) = grep {
		$_->{ParentPkg}{Name} eq 'a' and
		$_->{ParentVer}{VerStr} eq '0.1'
	    } @{$b->{RevDependsList}};
    }

    if ($r)
    {
	print "ok 13\n";

	print 'not ' unless $r->{DepType} eq 'Depends';
	print "ok 14\n";

	print 'not ' unless $r->{TargetPkg}{Name} eq 'b';
	print "ok 15\n";

	print 'not ' unless $r->{CompType} eq '>=';
	print "ok 16\n";

	print 'not ' unless $r->{TargetVer} eq '0.2-3';
	print "ok 17\n";
    }
    else
    {
	print "not ok 13 # no rev depends\n";
	print "ok $_ # skip\n" for 14..17;
    }
}
else
{
    print "not ok 2 # package b missing\n";
    print "ok $_ # skip\n" for 3..17;
}

# package "a" exists
if (my $a = $cache->{a})
{
    print "ok 18\n";

    # check some string values
    check_keys 19, $a, {
	Name => 'a',
	Section => 'test',
	SelectedState => 'Purge',
	InstState => 'Ok',
	CurrentState => 'NotInstalled',
    };

    # expect two versions to be available
    print 'not ' unless @{$a->{VersionList}} == 2;
    print "ok 24\n";

    # check version (stable version)
    print 'not ' unless $a->{VersionList}[1]{VerStr} eq '0.1';
    print "ok 25\n";

    # check provides
    print 'not ' unless @{$a->{VersionList}[1]{ProvidesList}} == 1;
    print "ok 26\n";

    my $p = $a->{VersionList}[1]{ProvidesList}[0];
    print 'not ' unless $p->{OwnerPkg}{Name} eq 'a';
    print "ok 27\n";

    print 'not ' unless $p->{OwnerVer}{VerStr} eq '0.1';
    print "ok 28\n";

    # check depends
    my $d = $a->{VersionList}[1]{DependsList};
    my $depend;
    my $conflict;

    if ($d and @$d == 2)
    {
	for my $i (0, 1)
	{
	    for ($d->[$i]{DepType})
	    {
		/^Depends$/   and $depend = $i;
		/^Conflicts$/ and $conflict = $i;
	    }
	}
    }

    if (defined $depend and defined $conflict)
    {
	print "ok 29\n";

	print 'not ' unless $d->[$depend]{TargetPkg}{Name} eq 'b';
	print "ok 30\n";

	print 'not ' unless $d->[$depend]{CompType} eq '>=';
	print "ok 31\n";

	print 'not ' unless $d->[$depend]{TargetVer} eq '0.2-3';
	print "ok 32\n";

	print 'not ' unless $d->[$conflict]{TargetPkg}{Name} eq 'foo';
	print "ok 33\n";
    }
    else
    {
	print "not ok 29 # bad depends\n";
	print "ok $_ # skip\n" for 30..33;
    }
}
else
{
    print "not ok 18 # package b missing\n";
    print "ok $_ # skip\n" for 19..33;
}

# test files
my $f = $cache->files;
my $status;
my $packages;

if ($f and @$f == 3)
{
    for (my $i = 0; $i < @$f; $i++)
    {
	for ($f->[$i]{FileName})
	{
	    /\bstatus$/  and $status = $i;
	    /stable_.*_Packages$/ and $packages = $i;
	}
    }
}

if (defined $status and defined $packages)
{
    print "ok 34\n";

    check_keys 35, $f->[$status], {
	FileName => 't/cache/var/status',
	IsOk => 1,
	Archive => 'now',
	IndexType => 'Debian dpkg status file',
    };

    check_keys 39, $f->[$packages], {
	FileName => 't/cache/var/lists/_test_dists_stable_main_binary-i386_Packages',
	IsOk => 1,
	Origin => 'Debian',
	Label => 'Debian',
	Archive => 'stable',
	Version => '3.0',
	Component => 'main',
	IndexType => 'Debian Package Index',
    };
}
else
{
    print "not ok 34 # bad ->files\n";
    print "ok $_ # skip\n" for 35..46;
}

if (my $p = $cache->packages)
{
    # check by name
    if (my $a = $p->lookup('a'))
    {
	print "ok 47\n";

	check_keys 48, $a, {
	    Name => 'a',
	    Section => 'test',
	    VerStr => '0.5',
	    Maintainer => 'Brendan O\'Dea <bod@debian.org>',
	    FileName => 'pool/main/a/a/a_0.5_i386.deb',
	    MD5Hash => '0123456789abcdef0123456789abcdef',
	    ShortDesc => 'Test Package "a"',
	    LongDesc => qq/Test Package "a"\n This is a bogus package for the AptPkg::Cache test suite./,
	};
    }
    else
    {
	print "not ok 47 # lookup by name failed\n";
	print "ok $_ # skip\n" for 47..55;
    }

    # check by verfile
    if ($verfile)
    {
	if (my $b = $p->lookup($verfile))
	{
	    print "ok 56\n";

	    check_keys 57, $b, {
		Name => 'b',
		Maintainer => 'Brendan O\'Dea <bod@debian.org>',
		FileName => 'pool/main/b/b/b_0.3-1_i386.deb',
		MD5Hash => '0123456789abcdef0123456789abcdef',
		ShortDesc => 'Test Package "b"',
		LongDesc => qq/Test Package "b"\n This is a bogus package for the AptPkg::Cache test suite./,
	    };
	}
	else
	{
	    print "not ok 56 # lookup by verfile failed\n";
	    print "ok $_ # skip\n" for 57..62;
	}
    }
    else
    {
	print "ok $_ # skip\n" for 56..62;
    }
}
else
{
    print "not ok 47 # bad ->packages\n";
    print "ok $_ # skip\n" for 48..62;
}
