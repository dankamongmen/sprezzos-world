#!/usr/bin/perl -w

use strict;
use Debian::AdduserCommon;


# helper routines

my %add_config;
my %del_config;

preseed_config(("/etc/adduser.conf"),\%add_config);
preseed_config(("/etc/deluser.conf"),\%del_config);

my $user_prefix = "addusertest";



sub assert {
  my ($cond) = @_;
  if ($cond) {
    print "Test failed; aborting test suite\n";
    exit 1;
  }
}

sub find_unused_uid {
  my ($mode) = @_;
  my $low_uid, my $high_uid;
  if ($mode =~ /"user"/i) {
    $low_uid = $add_config{"first_uid"};
    $high_uid = $add_config{"last_uid"};
  } else {
    $low_uid = $add_config{"first_system_uid"};
    $high_uid = $add_config{"last_system_uid"};
  }
  setpwent();
  my $uid = $low_uid;
  while (($uid <= $high_uid) && (defined(getpwuid($uid)))) {$uid++;}
  endpwent();
  
  if (($uid <= $high_uid) && (! defined(getpwuid($uid)))) {
    return $uid;
  }
  else {
    print "Haven't found a unused uid in range ($low_uid - $high_uid)\nExiting ...\n";
    exit 1;
  }
}

sub find_unused_name {
  my $i = 1;
  setpwent();
  while (my $name = getpwent) {
    if ($name =~ /$user_prefix(\d+)/) {
      $i = $1>$i?$1:$i;
    }
  }
  endpwent();
  my $j = 1;
  setgrent();
  while (my $name = getgrent) {
    if ($name =~ /$user_prefix(\d+)/) {
      $j = $1>$j?$1:$j;
    }
  }
  endgrent();
  return "$user_prefix".(($i>$j)?++$i:++$j);
}

sub find_unused_gid {
  my ($mode) = @_;
  my $low_gid, my $high_gid;
  if ($mode =~ /"user"/i) {
    $low_gid = $add_config{"first_gid"};
    $high_gid = $add_config{"last_gid"};
  } else {
    $low_gid = $add_config{"first_system_gid"};
    $high_gid = $add_config{"last_system_gid"};
  }
  setgrent();
  my $gid = $low_gid;
  while (($gid <= $high_gid) &&  (defined(getgrgid($gid)))) { $gid++;}
  endgrent();
  
  if (($gid <= $high_gid) && (! defined(getgrgid($gid)))) {
    return $gid;
  }
  else {
    print "Haven't found a unused gid in range ($low_gid - $high_gid)\nExiting ...\n";
    exit 1;
  }
}

# checking routines

sub check_user_exist {
  my ($username,$uid) = @_;
 
  my @ent = getpwnam ($username);
  if (!@ent) {
	print "user $username does not exist\n";
	exit 1;
  }
  if (( defined($uid)) && ($ent[2] != $uid)) {
	printf "uid $uid does not match %s\n",$ent[2];
	return 1;
  }
  return 0;
}

sub check_user_not_exist {
  my ($username) = @_;

  if (defined(getpwnam($username))) {
    return 1;
  }
  return 0;
}


#####################
sub check_homedir_exist {
  my ($username, $homedir) = @_;
  my $dir = (getpwnam($username))[7];
  if ((defined($homedir)) && (! $dir eq $homedir)) {
    print "check_homedir_exist: wrong homedir ($homedir != $dir)\n";
    return 1;
  }
  if (! -d $dir) {
    print "check_homedir_exist: there's no home directory $dir\n";
    return 1;
  }
  return 0;
}


sub check_homedir_not_exist {
  my ($homedir) = @_;
  if ( -d $homedir) {
    print "check_homedir_not_exist: there's a home directory $homedir\n";
    return 1;
  }
  return 0;
}



sub check_group_exist {
  my ($groupname) = @_;
  if (!defined(getgrnam($groupname))) {
    print "check_group_exist: Group $groupname does not exist\n";
    return 1;
  }
  return 0;
}

sub check_user_in_group {
  my ($user,$group) = @_;
  my ($name,$passwd,$gid,$members) = getgrnam ($group);
  #print "check_user_in_group: group $group = $members\n";
  foreach  my $u (split(" ",$members)) {
    #print "check_user_in_group: Testing user $u for group $group\n";
    if ( $u eq $user) { return 0; }
  }
  # ok, but $group is maybe $user's primary group ...
  my @pw = getpwnam($user);
  my $primary_gid = $pw[3];
  if (getgrgid($primary_gid) eq $group) {
    return 0;
  }
  
  print "check_user_in_group: User $user not in group $group\n";
  return 1;
}


sub check_user_has_gid {
  my ($user,$gid) = @_;
  my ($name,$passwd,$group_gid,$members) = getgrgid($gid);
  #print "check_user_has_gid: group $group = $members\n";
  foreach  my $u (split(" ",$members)) {
    #print "check_user_has_gid: Testing user $u for group $group\n";
    if ( $u eq $user) { return 0; }
  }
  # ok, but $group is maybe $user's primary group ...
  my @pw = getpwnam($user);
  my $primary_gid = $pw[3];
  if (getgrgid($primary_gid) eq $name) {
    return 0;
  }
  
  print "check_user_has_gid: User $user has no gid $gid\n";
  return 1;
}


return 1
