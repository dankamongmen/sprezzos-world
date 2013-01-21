#!/usr/bin/perl -w

# expect:
#  - a new system user $USER
#  - added to group nogroup
#  - home directory /home/$USER
#  - removal of home directory works

use strict;
use lib_test;

my $groupname = "nogroup";
my $username = find_unused_name(); 
my $cmd = "adduser --system $username";

if (!defined (getpwnam($username))) {
	print "Testing $cmd... ";
	`$cmd`;
	my $error = ($?>>8);
	if ($error) {
	  print "failed\n  adduser returned an errorcode != 0 ($error)\n";
	  exit $error;
	}
	assert(check_user_exist ($username));
	assert(check_homedir_exist($username));	
	assert(check_group_exist($groupname));
	assert(check_user_in_group($username,$groupname));
	print "ok\n";
}

$cmd = "deluser --remove-home $username";
if (defined (getpwnam($username))) {
  	my $homedir = (getpwnam($username))[7];
	print "Testing $cmd... ";
	`$cmd`;
	my $error = ($?>>8);
	if ($error) {
	  print "failed\n  adduser returned an errorcode != 0 ($error)\n";
	  exit $error;
	}
	assert(check_user_not_exist ($username));
	assert(check_homedir_not_exist($homedir));	
	print "ok\n";
}

