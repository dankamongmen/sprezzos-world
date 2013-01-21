#!/usr/bin/perl -w

# expect:
#  - a new non-system group $groupname
#  - readding the group fails
#  - readding the group as a system group fails
#  - a new system group $groupname
#  - readding the group succeeds
#  - readding the group as a non-system group fails

use strict;

use lib_test;

my $error;
my $output;
my $groupname = find_unused_name();
my $cmd = "addgroup $groupname";

if (!defined (getgrnam($groupname))) {
	print "Testing (9.1) $cmd... ";
	$output=`$cmd 2>&1`;
	$error = ($?>>8);
	if ($error) {
	  print "failed\n  $cmd returned an errorcode != 0 ($error)\n";
	  exit $error;
	}
        if ($output !~ /^Adding group `addusertest\d+' \(GID \d+\) ...\nDone\.\n$/) {
          print "failed\n  $cmd returned unexpected output ($output)\n";
	  exit 1;
	}
	assert(check_group_exist ($groupname));

	print "ok\n";
}

# now testing whether adding the group again fails as it should

print "Testing (9.2) $cmd... ";
$output=`$cmd 2>&1`;
$error = ($?>>8);
if ($error ne 1) {
  print "failed\n  $cmd returned an errorcode != 1 ($error)\n";
  exit 1;
}
if ($output !~ /^addgroup: The group `addusertest\d+' already exists\.\n$/ ) {
  print "failed\n  $cmd returned unexpected output ($output)\n";
  exit 1;
}
print "ok\n";

# now testing whether adding the group again (as a system group)
# fails as it should (#405905)

$cmd = "addgroup --system $groupname";
print "Testing (9.3) $cmd... ";
$output=`$cmd 2>&1`;
$error = ($?>>8);
if ($error ne 1) {
  print "failed\n  $cmd returned an errorcode != 1 ($error)\n";
  exit $error;
}
if ($output !~ /^addgroup: The group `addusertest\d+' already exists and is not a system group. Exiting.$/ ) {
  print "failed\n  $cmd returned unexpected output ($output)\n";
  exit 1;
}
print "ok\n";

my $sysgroupname = find_unused_name();
$cmd = "addgroup --system $sysgroupname";

if (!defined (getgrnam($sysgroupname))) {
	print "Testing (9.4) $cmd... ";
	$output=`$cmd 2>&1`;
	$error = ($?>>8);
	if ($error) {
	  print "failed\n  $cmd returned an errorcode != 0 ($error)\n";
	  exit $error;
	}
        if ($output !~ /^Adding group `addusertest\d+' \(GID \d+\) ...\nDone\.\n$/ ) {
	  print "failed\n  $cmd returned unexpected output ($output)\n";
	  exit 1;
	}
	assert(check_group_exist ($sysgroupname));

	print "ok\n";
}

# now testing whether adding the group again passes as it should
# ("already exists as a system group")

$cmd = "addgroup --system $sysgroupname" ;
print "Testing (9.5) $cmd... ";
$output=`$cmd 2>&1`;
$error = ($?>>8);
if ($error) {
  print "failed\n  $cmd returned an errorcode != 0 ($error)\n";
  exit $error;
}
if ($output !~ /^addgroup: The group `addusertest\d+' already exists as a system group\. Exiting\.\n$/ ) {
  print "failed\n  $cmd returned unexpected output ($output)\n";
  exit 1;
}
print "ok\n";

# now testing whether adding the group again (as a normal group)
# fails as it should

$cmd = "addgroup $sysgroupname";
print "Testing (9.6) $cmd... ";
$output=`$cmd 2>&1`;
$error = ($?>>8);
if ($error ne 1) {
  print "failed\n  $cmd returned an errorcode != 1 ($error)\n";
  exit 1;
}
if ($output !~ /^addgroup: The group `addusertest\d+' already exists\.$/ ) {
  print "failed\n  $cmd returned unexpected output ($output)\n";
  exit 1;
}
print "ok\n";

