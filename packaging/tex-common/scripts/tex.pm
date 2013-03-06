#!/usr/bin/perl
# debhelper sequence file for tex-common script

use warnings;
use strict;
use Debian::Debhelper::Dh_Lib;

insert_after("dh_link", "dh_installtex");

1;
