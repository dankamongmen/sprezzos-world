#! /usr/bin/perl -wT

# Postfix mailq file reformatter, (C) 2003 by Matthias Andree

# This file is licensed to you under the conditions of the
# GNU General Public License v2.

# $Id: mailqfmt.pl,v 0.6 2004/01/20 00:30:26 emma Exp $

use strict;

my ($rec, $rsn);
use HTTP::Date;

my $cmd = '/usr/bin/mailq';

my %q = ( ' ' => 'normal',
	  '!' => 'hold  ',
	  '*' => 'active' );

delete $ENV{'PATH'};

if ($ENV{'SSH_CLIENT'} and not $ENV{'SSH_TTY'}) {
    print STDERR "Warning: If you aren't redirecting input, type EOF (^D) now and re-run me with ssh -t.\n";
}

if (@ARGV == 0 and -t STDIN) {
	open STDIN, "$cmd|" or die "cannot run $cmd: $!";
}

while(<>) {
	if (/^Mail queue is empty/) { print; next; }
	if (/^--/) { print; next; }	# print trailer
	if (/^-/) { next; }		# skip header
	# empty line
	if (/^$/) {
		if ($rsn) { $rec .= " reason=$rsn"; }
		print "$rec\n";
		$rec = $rsn = '';
		next;
	}
	# line with queue id
	if (/^([0-9A-F]+)\s*([ !*])\s+(\d+)\s+(\S+\s+\S+\s+\d+\s+\d+:\d+:\d+)\s+(.+)$/)
	{
		my ($qid, $qfl, $siz, $dat, $from) = ($1, $2, $3, $4, $5);
		$dat = HTTP::Date::time2isoz(str2time($dat));
		$dat =~ s/ /T/g;
		$siz = sprintf "%08d", $siz;
		$rec="$qid queue=$q{$qfl} size=$siz date=$dat from=$from";
		next;
	}
	if (/^\s*\((.+)\)$/) { $rsn = $1; $rsn =~ tr/ /_/; next; }
	if (/^\s+(.+)$/) { $rec .= " to=$1"; next; }
}

exit
__END__
# $Log: mailqfmt.pl,v $
# Revision 0.6  2004/01/20 00:30:26  emma
# When in an SSH session without pseudo terminal,
# warn the user that program expects input
#
# Revision 0.5  2003/12/19 13:38:18  emma
# Do not require space before a bounce reason (which made mailqfmt.pl
# ignore long bounce reasons.)
#
# Revision 0.4  2003/01/09 11:59:47  ma
# Pass "Mail queue is empty".
#
# Revision 0.3  2003/01/09 11:55:59  ma
# Use delete, not undef, to dispose of $ENV{PATH}.
#
# Revision 0.2  2003/01/09 11:53:11  ma
# Add -wT to shebang line. Undefine $ENV{PATH}. Fix __end__ to __END__.
#
# Revision 0.1  2003/01/09 11:50:56  ma
# first release
#
