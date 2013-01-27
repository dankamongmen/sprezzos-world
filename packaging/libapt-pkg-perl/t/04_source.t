#!/usr/bin/perl

# AptPkg::Source tests

BEGIN { print "1..14\n" }

use AptPkg::Config '$_config';
use AptPkg::Source;

$_config->init;
$_config->read_file('t/cache/etc/apt.conf');
$_config->{quiet} = 0;

print "ok 1\n";

my $src = AptPkg::Source->new;

# cache created
unless ($src)
{
    print "not ok 2\n";
    exit;
}

sub check
{
    my ($test, $t) = @_;
    print 'not ' unless $t;
    print "ok $test\n";
}

# package "a" exists
if (my $a = $src->{a})
{
    print "ok 2\n";

    check 3, @$a == 1;

    $a = $a->[0];
    check 4, $a->{Package} eq 'a';
    check 5, $a->{Section} eq 'test';
    check 6, $a->{Version} eq '0.1';
    if ($a->{Binaries} and ref $a->{Binaries} eq 'ARRAY')
    {
    	check 7, "@{$a->{Binaries}}" eq 'a';
    }
    else
    {
    	print "not ok 7 # no binaries\n";
    }

    if ($a->{BuildDepends} and ref $a->{BuildDepends} eq 'HASH'
	and my $b = $a->{BuildDepends}{'Build-Depends'})
    {
	check  8, $b->[0][0] eq 'b';
	check  9, $b->[0][1] == AptPkg::Dep::GreaterEq;
	check 10, $b->[0][2] eq '0.2-42';
    }
    else
    {
	print "not ok 8 # build depends\n";
	print "ok $_ # skip\n" for 9..10;
    }

    if ($a->{Files} and ref $a->{Files} eq 'ARRAY')
    {
    	if (my ($dsc) = grep $_->{Type} eq 'dsc', @{$a->{Files}})
	{
	    check 11, $dsc->{ArchiveURI} =~ m!pool/main/a/a/a_0\.1\.dsc$!;
	    check 12, $dsc->{MD5Hash} eq '8202ae7d918948c192bdc0f183ab26ca';
	}
	else
	{
	    print "not ok 11 # no dsc\n";
	    print "ok 12 # skip\n";
	}

    	if (my ($tgz) = grep $_->{Type} eq 'tar', @{$a->{Files}})
	{
	    check 13, $tgz->{ArchiveURI} =~ m!pool/main/a/a/a_0\.1\.tar\.gz$!;
	    check 14, $tgz->{MD5Hash} eq 'a54a02be45314a8eea38058b9bbea7da';
	}
	else
	{
	    print "not ok 13 # no tar\n";
	    print "ok 14 # skip\n";
	}
    }
}
else
{
    print "not ok 2 # source a missing\n";
    print "ok $_ # skip\n" for 3..14;
}
