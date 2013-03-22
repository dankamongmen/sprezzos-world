#!/usr/bin/perl
# debhelper sequence file for dh_scour

use warnings;
use strict;
use Debian::Debhelper::Dh_Lib;

insert_after("dh_link", "dh_scour");

1;
