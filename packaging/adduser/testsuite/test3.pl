#!/usr/bin/perl -w

# expect:
#  - a new user $USER
#  - added to group nogroup
#  - no home directory /home/$USER

use strict;
use lib_test;

my $groupname = "nogroup";
my $username = find_unused_name();
my $cmd = "adduser --system --no-create-home $username";

my $homedir = "/home/$username";

if (!defined (getpwnam($username))) {
	print "Testing $cmd... ";
	`$cmd`;
	my $error = ($?>>8);
	if ($error) {
	  print "failed\n  adduser returned an errorcode != 0 ($error)\n";
	  exit $error;
	}

	assert(check_user_exist ($username));
	assert(check_group_exist($groupname));
	assert(check_user_in_group($username,$groupname));
	assert(check_homedir_not_exist($homedir));	
	print "ok\n";
}
  
