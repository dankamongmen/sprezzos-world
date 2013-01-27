#!/usr/bin/perl
#
# makedesc directory file
#
# Scan the directory for files, and use the files to generate a task
# description file. The format of the task description file is described in
# data.c. The format of the input files is:
#
# Task: desktop
# Section: user
# Description: Provide a basic GUI system
#  This task provides functionality for a basic desktop; whether Gnome
#   based, KDE based or customised. With this task, your system will boot
#   into a graphical login screen, at which point you can choose which of
#   these desktops you wish to use on a per-user basis. You can further
#   customise your desktop once installed.
# Packages-list:
#  kdebase
#  gdm
#  ...
#
# Hash-comments are allowed in the files, but must be on their own lines.

my $dir=shift or die "no directory specified\n";
my $file=shift or die "no file specified\n";

my %package;
my %notmain;
my $dolint=1;
{
	local $/="\n\n";
	if (! open (AVAIL, "apt-cache dumpavail |")) {
		warn "cannot real available info, so disabling lint check\n";
		$dolint=0;
	}
	while (<AVAIL>) {
		my ($package)=/Package:\s*(.*?)\n/;
		$package{$package}=1;
		if (/Section:\s*(contrib|non-free)/) {
			$notmain{$package}=$1;
		}
	}
	close AVAIL;
}

open (OUT, ">$file") or die ">$file: $!";

use File::Find;
find({ wanted => \&processfile, preprocess => sub { return sort @_}}, $dir);

sub processfile {
	my $file=$_; # File::Find craziness.
	$file eq 'po' && -d $file && ($File::Find::prune = 1);
	return if $File::Find::dir=~/\.(svn|git)/;
	return unless $file =~ /^[-+_.a-z0-9]+$/ and -f $file;
	open (IN, $file) or die "$file: $!";
	my %fields;
	my $field="";
	while (<IN>) {
		chomp;
		next if /^\s*#/;
		s/#.*//;

		if (/^\s/) {
			$fields{$field}.="\n$_";
		}
		else {
			($field, my $value)=split(/:\s*/, $_, 2);
			$field=lc($field);
			$fields{$field}=$value;
		}
	}
	close IN;

	# Basic lint of the listed packages.
	# TODO: a better lint would incloude checks for conflicting
	# packages. Hard tho.
	if ($dolint) {
		foreach my $field (qw(key packages-list)) {
			foreach (split ' ', $fields{$field}) {
				if (! $package{$_}) {
					print STDERR "$file: $_ is not a valid package.\n";
					if ($field eq 'key') {
						print STDERR "MISSING KEY PACKAGE, TASK BROKEN\n";
					}
				}
				if ($notmain{$_}) {
					print STDERR "$file: $_ is in $notmain{$_}.\n";
					if ($field eq 'key') {
						print STDERR "MISSING KEY PACKAGE, TASK BROKEN\n";
					}
				}
			}
		}
	}

	foreach (qw{task section relevance description key enhances provides depends packages}, 
	         grep(/^test-(.*)/, keys %fields)) {
		print OUT ucfirst($_).": ".$fields{$_}."\n" if length $fields{$_};
	}
	print OUT "\n";
}

close OUT;
