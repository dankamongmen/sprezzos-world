#!/usr/bin/perl -w
use strict;
use Test::More tests => 8;

my $upstream_version;
ok(open(P, "dpkg-parsechangelog |"), "successfully piping from dpkg-parsechangelog");
while (<P>) {
    /^Version: (.+)-[^-]+$/ or next;
    $upstream_version = $1;
    last;
}
isnt($upstream_version, "", "found upstream version from dpkg-parsechangelog output");
ok(close P, "dpkg-parsechangelog exited normally");

my $checked_version;
ok(open(C, "<debian/copyright"), "successfully opened debian/copyright");
while (<C>) {
    next if !/^ Last checked against: Perl (.+)/;
    $checked_version = $1;
    last;
}
isnt($checked_version, "", "found checked version from debian/copyright");
close C;

is($checked_version, $upstream_version,
    "debian/copyright last checked for the current upstream version");

SKIP: {
    system("which config-edit >/dev/null 2>&1");
    skip("config-edit not available", 2) if $?;
    diag("parsing debian/copyright with config-edit...");
    is( qx/config-edit -application dpkg-copyright -ui none 2>&1/, "",
        "no error messages from config-edit when parsing debian/copyright");
    is($?, 0, "config-edit exited normally");
}

