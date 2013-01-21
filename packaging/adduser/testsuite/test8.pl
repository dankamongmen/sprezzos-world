#!/usr/bin/perl -w

# expect:
#  - a new system user $USER
#  - Added to all groups in extra_groups
#  - a new group
#  - $USER added to new group
#  - Removal of $USER works
#  - removal of new group works
#  - system users do not get added to extra_groups

use strict;
use lib_test;

my $username = find_unused_name(); 
my $cmd = "adduser --gecos test --disabled-password --add_extra_groups $username";

my %config;

preseed_config(("/etc/adduser.conf"),\%config);

if (!defined (getpwnam($username))) {
	print "Testing $cmd... ";
	`$cmd`;
	my $error = ($?>>8);
	if ($error) {
	  print "failed\n  adduser returned an errorcode != 0 ($error)\n";
	  exit $error;
	}
	assert(check_user_exist ($username));

        foreach my $group (split ' ', $config{"extra_groups"}) {
          assert(check_user_in_group($username,$group));
        }
	print "ok\n";
}

my $newgroup = find_unused_name();

$cmd = "addgroup $newgroup";
unless (defined getgrnam($newgroup)) {
        print "Testing $cmd... ";
        `$cmd`;
        my $error = ($?>>8);
        if ($error) {
            print "failed\n  addgroup returned an errorcode != 0 ($error)\n";
            exit $error;
        }
        assert(check_group_exist ($newgroup));
        print "ok\n";
}

$cmd = "adduser $username $newgroup";
if (defined (getpwnam($username))) {
   print "Testing $cmd... ";
   `$cmd`;
   my $error = ($?>>8);
   if ($error) {
     print "failed\n  adduser returned an errorcode != 0 ($error)\n";
     exit $error;
   }
   assert(check_user_in_group ($username,$newgroup));
   print "ok\n";
}

$cmd = "deluser --remove-home $username";
if (defined (getpwnam($username))) {
	print "Testing $cmd... ";
	`$cmd`;
	my $error = ($?>>8);
	if ($error) {
	  print "failed\n  adduser returned an errorcode != 0 ($error)\n";
	  exit $error;
	}
	assert(check_user_not_exist ($username));
	print "ok\n";
}

$cmd = "delgroup $newgroup";
unless (!defined getgrnam($newgroup)) {
        print "Testing $cmd... ";
        `$cmd`;
        my $error = ($?>>8);
        if ($error) {
            print "failed\n  delgroup returned an errorcode != 0 ($error)\n";
            exit $error;
        }
        assert(!check_group_exist ($newgroup));
        print "ok\n";
}

my $sysusername = find_unused_name(); 
$cmd = "adduser --system --gecos test --disabled-password --add_extra_groups $sysusername";

if (!defined (getpwnam($sysusername))) {
	print "Testing $cmd... ";
	`$cmd`;
	my $error = ($?>>8);
	if ($error) {
	  print "failed\n  adduser returned an errorcode != 0 ($error)\n";
	  exit $error;
	}
	assert(check_user_exist ($sysusername));

        foreach my $group (split ' ', $config{"extra_groups"}) {
          assert(!check_user_in_group($username,$group));
        }
	print "ok\n";
}
