#!/bin/false /usr/bin/perl -w
# shebang disabled while this script resides in debian/

# similar to dh_install; needs some documentation

use strict;
use File::Find;
use Debian::Debhelper::Dh_Lib;

init();

# list of sourcedirs
my @sourcedirs = @ARGV;

my @installed;

my $srcdir = '.';

# dh_install code, but not doing any installation, just building @installed
# and stripping sourcedirs from these
foreach my $package (@{$dh{DOPACKAGES}}) {
    my $tmp=tmpdir($package);
    my $file=pkgfile($package,"install");

    my @install;
    if ($file) {
        @install=filedoublearray($file); # no globbing yet
    }

    # Support for -X flag.
    my $exclude = '';
    if ($dh{EXCLUDE_FIND}) {
        $exclude = '! \( '.$dh{EXCLUDE_FIND}.' \)';
    }

    foreach my $set (@install) {
        if (! defined $dh{AUTODEST} && @$set > 1) {
            pop @$set;
        }
        foreach my $src (map { glob "$srcdir/$_" } @$set) {
            next if excludefile($src);

            # strip source dir
            foreach my $d (@sourcedirs) {
                $src=~s/^\Q$srcdir\E\/\Q$d\E\///;
            }

            # Keep track of what's installed.
            # Kill any extra slashes. Makes the @installed stuff more robust.
            $src=~y:/:/:s;
            $src=~s:/+$::;
            $src=~s:^(\./)*::;
            push @installed, "\Q$src\E\/.*|\Q$src\E";
        }
    }
}

# dh_install code, but stripping sourcedirs
my @missing;
my $installed=join("|", @installed);
$installed=qr{^($installed)$};
find(sub {
    -f || -l || return;
    $_="$File::Find::dir/$_";
    foreach my $d (@sourcedirs) {
        s/^\Q$d\E\///;
    }
    if (! /$installed/ && ! excludefile($_)) {
        my $file=$_;
        push @missing, $file;
    }
}, @sourcedirs);
if (@missing) {
    warning "$_ has been installed upstream but is not in any package" foreach @missing;
    if ($dh{FAIL_MISSING}) {
        error("missing files, aborting");
    }
}

