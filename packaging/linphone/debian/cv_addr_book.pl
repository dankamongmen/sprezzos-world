#!/usr/bin/perl -w

# Convert linphone's address book from version 0.12 to version 1.0.0.
#
# Copyright (C) 2005 Samuel Mimram
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

use strict;
use Env '$HOME';

my $conffile = "$HOME/.gnome2/linphone";
my $tmpfile = "/tmp/linphone.tmp";
my $friend_nb = 0;
my $old_nb = 0;

system "cp $conffile $tmpfile";

open CONF, $conffile or die "Cannot open configuration file: $!";
open OUT, ">>$tmpfile" or die "Cannot open temp file: $!";

while (<CONF>)
{
    $friend_nb = ($1 + 1) if (/\[friend_(\d+)\]/);
}

print "$friend_nb existing new entries were found\n";

seek CONF, 0, 0;

while (<CONF>)
{
    last if (/^\[address_book\]$/);
}

if (<CONF> =~ /^entry_count=(\d+)$/)
{
    $old_nb = $1;
    print "$old_nb old entries were found\n";
}
else
{
    die "Wrong file format (entry_count not found)";
}


my $i;
my ($name, $user, $domain);

for ($i = 0; $i < $old_nb; $i++)
{
    if (<CONF> =~ /^entry$i=(.*) <sip:(.*)\@(.*)>$/)
    {
	$name = $1;
	$user = $2;
	$domain = $3;
	print OUT ("[friend_".($friend_nb + $i)."]\n");
	print OUT "url=$name <sip:$user\@$domain>\n";
	print OUT "pol=accept\nsubscribe=1\nproxy=-1\n\n";
    }
    else
    {
	die "Wrong file format (entry$i not found or invalid)";
    }
}

close CONF;
close OUT;

system "cp $tmpfile $conffile";

print "Successfully added old entries\n";
