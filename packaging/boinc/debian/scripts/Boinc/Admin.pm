#
# Module Boinc::Admin - controlling and administering Boinc projects
#
# Author: Gabor Gombas <gombasg@sztaki.hu>
#
# This is free software; you can redistribute it and/or modify it under the
# terms of the GNU Lesser General Public License as published by the Free
# Software Foundation; either version 2.1 of the License, or (at your option)
# any later version.

package Boinc::Admin;

use IO::File;
use Boinc::Common;
use Boinc::Config;
use strict;

use vars qw(@undolist);

BEGIN {
	use Exporter();
	our ($VERSION, @ISA, @EXPORT, @EXPORT_OK, %EXPORT_TAGS);

	$VERSION = 1.0;
	@ISA = qw(Exporter);
	@EXPORT = qw(ctl_project add_admin delete_admin list_from_sudoers);

	%EXPORT_TAGS = ();
	@EXPORT_OK = qw();
}

sub need_su($) {
	my $name = shift;

	my $user = username($name);
	my $euser = getpwuid($<);
	return $user ne $euser;
}

sub ctl_project($$) {
	my ($name, $cmd) = @_;

	die("Unknown command\n")
		unless $cmd =~ m/(start|stop|status)/;

	my $projectroot = projectroot($name);
	die("$projectroot/bin/start does not exist or is not executable\n")
		unless -x "$projectroot/bin/start";

	if (need_su($name)) {
		spawn('/bin/su', '-', username($name), '-c', "start --$cmd");
	}
	else {
		spawn("start", "--$cmd");
	}
}

sub add_to_sudoers($$) {
	my ($name, $admin) = @_;

	notice("Adding $admin to /etc/sudoers");

	# Initial sanity check
	my $res = spawn('/usr/sbin/visudo', '-c');
	die("/etc/sudoers contains errors - not modified\n") if $res;

	my $fh = new IO::File '/etc/sudoers', O_RDONLY
		or die("Cannot open /etc/sudoers\n");
	my $tmpsudo = "/etc/sudoers.$$";
	my $ofh = new IO::File $tmpsudo, O_WRONLY | O_CREAT, 0440
		or die("Cannot create $tmpsudo\n");

	my $user = username($name);

	while (my $line = $fh->getline) {
		if ($line =~ m!\#\# DO NOT REMOVE - boinc_admin: $name/$admin!) {
			$ofh->close;
			unlink "/etc/sudoers.$$";
			die("User $admin is already an administrator for project $name\n");
		}
		$ofh->print($line);
	}

	$ofh->print(<<"EOF");
## DO NOT REMOVE - boinc_admin: $name/$admin
$admin	ALL = ($user) ALL
## DO NOT REMOVE - boinc_admin: $name/$admin
$admin	ALL = (root) /bin/su - $user
EOF
	$fh->close;
	$ofh->close;

	# Second sanity check
	$res = spawn('/usr/sbin/visudo', '-c', '-f', $tmpsudo);
	if ($res) {
		unlink $tmpsudo;
		die("New /etc/sudoers contains errors - not modified\n");
	}

	rename $tmpsudo, '/etc/sudoers';

	push @undolist, \&delete_from_sudoers;
}

sub delete_from_sudoers($$) {
	my ($name, $admin) = @_;

	notice("Removing $admin from /etc/sudoers");

	# Initial sanity check
	my $res = spawn('/usr/sbin/visudo', '-c');
	die("/etc/sudoers contains errors - not modified\n") if $res;

	my $fh = new IO::File '/etc/sudoers', O_RDONLY
		or die("Cannot open /etc/sudoers\n");
	my $tmpsudo = "/etc/sudoers.$$";
	my $ofh = new IO::File $tmpsudo, O_WRONLY | O_CREAT, 0440
		or die("Cannot create $tmpsudo\n");

	my $user = username($name);

	while (my $line = $fh->getline) {
		if ($line =~ m!^\#\# DO NOT REMOVE - boinc_admin: $name/$admin!) {
			# If the mark is found, remove the next line - but check it first

			my $oldline = $line;
			$line = $fh->getline;
			next if $line =~ m/^$admin/;

			# No, the next line does not look right - put back both lines
			$ofh->print($oldline);
		}
		$ofh->print($line);
	}

	$fh->close;
	$ofh->close;

	# Second sanity check
	$res = spawn('/usr/sbin/visudo', '-c', '-f', $tmpsudo);
	if ($res) {
		unlink $tmpsudo;
		die("New /etc/sudoers contains errors - not modified\n");
	}

	rename $tmpsudo, '/etc/sudoers';
}

sub list_from_sudoers($) {
	my $name = shift;
	my @admins;

	my $fh = new IO::File '/etc/sudoers', O_RDONLY
		or die("Cannot open /etc/sudoers: $@\n");
	while (my $line = $fh->getline) {
		chomp $line;
		next unless $line =~ m!^\#\# DO NOT REMOVE - boinc_admin: $name/(.*)!;
		my $admin = $1;
		$line = $fh->getline;
		next unless $line =~ m/^$admin/;
		push @admins, $admin unless grep $_ eq $admin, @admins;
	}

	$fh->close;

	return @admins;
}

sub add_to_forward($$) {
	my ($name, $admin) = @_;
	my @admins;

	notice("Adding $admin to .forward");

	my $home = homedir($name);
	my $fh = new IO::File $home . '/.forward', O_RDONLY;
	if ($fh) {
		my $list = $fh->getline;
		chomp $list;

		@admins = split(',', $list);
		$fh->close;
	}

	push @admins, $admin;
	$fh = new IO::File $home . '/.forward', O_WRONLY | O_CREAT, 0644
		or die("Cannot write $home/.forward\n");
	$fh->print(join(',', @admins), "\n");
	$fh->close;

	push @undolist, \&delete_from_forward;
}

sub delete_from_forward($$) {
	my ($name, $admin) = @_;

	notice("Removing $admin from .forward");

	my $home = homedir($name);
	my $fh = new IO::File $home . '/.forward', O_RDONLY;
	unless ($fh) {
		notice("$home/.forward does not exist");
		return;
	}

	my $list = $fh->getline;
	chomp $list;

	my @admins = split(',', $list);
	$fh->close;

	my @newadmins = grep {$_ ne $admin} @admins;

	if (@newadmins) {
		$fh = new IO::File $home . '/.forward', O_WRONLY | O_CREAT, 0644
			or die("Cannot write $home/.forward\n");
		$fh->print(join(',', @newadmins), "\n");
		$fh->close;
	} else {
		unlink "$home/.forward";
	}
}

sub list_from_forward($) {
	my $name = shift;

	my $home = homedir($name);
	my $fh = new IO::File $home . '/.forward', O_RDONLY;
	return unless $fh;

	my $list = $fh->getline;
	chomp $list;

	my @admins = split(',', $list);
	$fh->close;

	return @admins;
}

sub add_to_htpasswd($$) {
	my ($name, $admin) = @_;

	my $projectroot = projectroot($name);
	my $filename = $projectroot . '/admin_users.htpasswd';

	notice("Setting the password for $admin");

	print <<"EOF";
Please enter a password below for accessing the admin parts of the web page.
It is recommended to use a different password here than the normal login
password.
EOF
	my $res = spawn('/usr/bin/htpasswd', '-m', $filename, $admin);
	die("Failed to set the admin password\n") if ($res);

	push @undolist, \&delete_from_htpasswd;
}

sub delete_from_htpasswd($$) {
	my ($name, $admin) = @_;

	my $projectroot = projectroot($name);
	my $filename = $projectroot . '/admin_users.htpasswd';

	notice("Disabling web access for $admin");

	spawn('/usr/bin/htpasswd', '-D', $filename, $admin);
}

sub list_from_htpasswd($) {
	my $name = shift;

	my $projectroot = projectroot($name);
	my $filename = $projectroot . '/admin_users.htpasswd';
	my $fh = new IO::File $filename, O_RDONLY;
	return unless $fh;

	my @admins;
	while (my $line = $fh->getline) {
		$line =~ m/^([^:]+):/ && push @admins, $1;
	}
	$fh->close;
	return @admins;
}

sub add_admin($$) {
	my ($name, $admin) = @_;

	die("This command requires root privileges\n")
		if $> != 0;

	die("No such user: $admin\n") unless getpwnam $admin;
	die("No such project: $name\n") unless -d projectroot($name) && -d homedir($name);

	eval {
		add_to_sudoers($name, $admin);
		add_to_forward($name, $admin);
		add_to_htpasswd($name, $admin);
	};
	if ($@) {
		foreach my $sub (@undolist) {
			$sub->($name, $admin);
		}
		die($@);
	}
}

sub delete_admin($$) {
	my ($name, $admin) = @_;

	die("This command requires root privileges\n")
		if $> != 0;

	delete_from_sudoers($name, $admin);
	delete_from_forward($name, $admin);
	delete_from_htpasswd($name, $admin);
}

# Signal that the module was parsed successfully
1;
