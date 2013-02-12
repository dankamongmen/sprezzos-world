#!/usr/bin/perl
use warnings;
use strict;
use Debian::Debhelper::Dh_Lib;

insert_before("dh_auto_configure", "dh_autotools-dev_updateconfig");
insert_before("dh_clean", "dh_autotools-dev_restoreconfig");

1;
