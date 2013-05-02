#
# Module Boinc::Common - common utility functions
#
# Author: Gabor Gombas <gombasg@sztaki.hu>
#
# This is free software; you can redistribute it and/or modify it under the
# terms of the GNU Lesser General Public License as published by the Free
# Software Foundation; either version 2.1 of the License, or (at your option)
# any later version.

package Boinc::Common;

use IO::File;
use File::Temp;
use POSIX qw(strftime);
use Boinc::Config;
use strict;

BEGIN {
	use Exporter();
	our ($VERSION, @ISA, @EXPORT, @EXPORT_OK, %EXPORT_TAGS);

	$VERSION = 1.0;
	@ISA = qw(Exporter);
	@EXPORT = qw(error warning notice debug set_logfile set_quiet spawn);

	%EXPORT_TAGS = ();
	@EXPORT_OK = qw();
}

use vars qw($logfilename $logfh $quiet);

sub set_logfile($) {
	my $filename = shift;

	$logfh->close if $logfh;
	$logfilename = $filename;

	$logfh = new IO::File $filename, O_WRONLY | O_APPEND | O_CREAT, 0600
		or die("Could not write to $filename\n");
}

sub set_quiet($) {
	$quiet = shift;
}

sub logit($) {
	my $msg = shift;

	return unless $logfh;

	chomp $msg;

	my $prog = $0;
	$prog =~ s!.*/!!;
	$prog =~ s/^boinc_//;

	my $time = strftime('%Y-%m-%d %H:%M:%S', localtime(time));

	$logfh->print("$time $prog $msg\n");
}

sub debug($) {
	my $msg = shift;

	logit("Debug: $msg");
}

sub notice($) {
	my $msg = shift;

	chomp $msg;
	logit("Notice: $msg");
	print($msg . "\n") unless $quiet;
}

sub warning {
	my $msg = shift;

	chomp $msg;
	logit("Warning: $msg");
	print STDERR 'Warning: ' . $msg . "\n" unless $quiet;
}

sub error {
	my $msg = shift;

	chomp $msg;
	logit("Error: $msg");
	print STDERR 'Error: ' . $msg . "\n";
}

sub spawn {
	my @args = @_;

	$logfh->flush if $logfh;

	debug('Running ' . join(' ', map({'\'' . $_ . '\''} @args)));
	debug('-- Begin command output --');

	my $pid = fork();

	unless ($pid) {
		# Child process
		open STDERR, ">&=", $logfh->fileno if $logfh;
		open STDOUT, ">&=", $logfh->fileno if $logfh;

		exec @args or exit(1);
	}

	waitpid($pid, 0);
	my $res = $? >> 8;

	debug('-- End command output, exit code: ' . $res . ' --');
	return $res;
}

# Signal that the module was parsed successfully
1;
