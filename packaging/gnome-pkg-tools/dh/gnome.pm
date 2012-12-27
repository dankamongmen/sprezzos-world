#!/usr/bin/perl

use warnings;
use strict;
use Debian::Debhelper::Dh_Lib;

insert_before("dh_gconf", "dh_gnome");
insert_before("dh_clean", "dh_gnome_clean");

1;
