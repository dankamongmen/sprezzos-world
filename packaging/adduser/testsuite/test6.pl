#!/usr/bin/perl -w

# expect:
#  - a new user $USER with uid $want_uid and gid 0
#  - added to group nogroup
#  - no home directory /home/$USER

use strict;
use lib_test;

my $username = find_unused_name();
my $want_uid = find_unused_uid("system");
my $want_gid = 0;

my $cmd = "adduser --system --uid $want_uid --gid $want_gid $username";

if (!defined (getpwnam($username))) {
	print "Testing $cmd... ";
	`$cmd`;
	my $error = ($?>>8);
	if ($error) {
	  print "failed\n  adduser returned an errorcode != 0 ($error)\n";
	  exit $error;
	}

	assert(check_user_exist ($username, $want_uid));
	assert(check_homedir_exist ($username));
	assert(check_user_has_gid($username,$want_gid));
	print "ok\n";
}
  
