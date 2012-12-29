#!/usr/bin/perl
# debhelper sequence file for python-central

use warnings;
use strict;
use Debian::Debhelper::Dh_Lib;

insert_after("dh_perl", "dh_pycentral");
remove_command("dh_pysupport");

1
