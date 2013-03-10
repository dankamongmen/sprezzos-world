#!/usr/bin/perl

#	BSDI	$Id: checksendmail.perl,v 1.1.1.1 1994/01/13 21:15:31 polk Exp $

# checksendmail
#
# 	The checksendmail program is a perl script that aids the testing
#	of sendmail's various configuration filse.  checksendmail 
#	passes typical addresses (supplied in input files) through
#	sendmail and prints the result of the resolution and 
#	transformation routines. 
#
#			Gene Kim & Rob Kolstad & Jeff Polk, 7/11/90

# --    -C config file
# --    -r resolve_addresses_file
# --	-T test one address
# --    -b sendmail binary

# XXX deficiencies:  ``/etc/sendmail'' should be a parameter

# defaults:
	$resolve  = "address.resolve";
	$cffile   = "/etc/sendmail.cf";
	$sendmail   = "/usr/sbin/sendmail";

sub usage {
	die "Usage: checksendmail [-C cffile.cf] [-r resolve_addrs_file [-T one\@test.addr\n";
}

for (unshift (@ARGV, "XX"); $#ARGV > 0; shift ARGV) {
	if ($ARGV[1] eq "-C") 
	{
		shift @ARGV;
		if ($#ARGV >= 1) {$cffile = $ARGV[1]; }
		else { &usage(); }
	}
	elsif ($ARGV[1] eq "-r")
	{
		if ($setfile & 1) { die "Can only set `resolve' once"; }
		shift @ARGV;
		if ($#ARGV < 1) { &usage(); }
		$resolve = $ARGV[1];
		$setfile += 1;
	}
	elsif ($ARGV[1] eq "-T")
	{
		if ($setfile) { die "Can't set other files and use -T"; }
		shift @ARGV;
		if ($#ARGV < 1) { &usage(); }
		open (OUT, ">/tmp/csm.in$$") ||
					die "Can't open >/tmp/csm.in$$";
		$resolve = "/tmp/csm.in$$";
		print OUT "$ARGV[1]\n";
		close (OUT);
		$setfile = 7;
	}
	elsif ($ARGV[1] eq "-b")
	{
		shift @ARGV;
		if ($#ARGV >= 1) {$sendmail = $ARGV[1]; }
		else { &usage(); }
	}
	else { &usage(); }
}

if (!-e $cffile)   { die "Can't find configuration file $cffile\n"; }
if (!-e $resolve)  { die "Can't find name file $resolve\n"; }

# make sure the user can access the ../mqueue directory

$maildir = `grep ^OQ $cffile`;
chop $maildir;
$maildir =~ s/^OQ//;
if ((!-r $maildir) || (!-x $maildir) || (!-w $maildir)) { 
	warn "checksendmail: can't access $maildir!  trying anyway...\n";
}

chop($hostname = `hostname`);
chop($pwd = `pwd`);
print "system: $hostname\t\tcurrent dir: $pwd\n";
print "config file: $cffile\t\tresolve file: $resolve\n";

sub handler {  # 1st argument is signal name
     local($sig) = @_;
     print STDERR "Caught a SIG$sig--shutting down\n";
     unlink("/tmp/csm$$") || die "Can't unlink /tmp/csm$$";
     unlink("/tmp/csm.in$$");
     exit(0);
}

$SIG{'INT'} = 'handler';
$SIG{'QUIT'} = 'handler';


# glean the mailers used from the rule 0 tests

sub parseresolve
{
     while (<MAIL>)
     {
        if (/^ADDRESS TEST MODE/) { next; }
        if (/^Enter <ruleset>/) { next; }
        if (/^>/) 			# INPUT LINE! (and last output is done)
        {
	  chop;
     	  if ($prevline) {	# OUTPUT LINE!
	    &printparseresolve;
     	  }
     	  s/> *[0-9,]* *//g;
     	  s/.*input: *//;
     	  s/ //g;
     	  s/"//g;
     	  $input = $_;
        }
	else { chop; }
        $prevline = $_;
     }
    &printparseresolve;
}

sub printparseresolve {
     	    $prevline =~ s/.*returns:  *//;
     	    $prevline =~ s/"//g;
     	    $prevline =~ s/ //g;
	    # Don't strip spaces off error messages
	    $prevline =~ s/ *([.!%<>]) */$1/g;
	    $prevline =~ s/ +([;?\$])/$1/g;
	    $prevline =~ s/([#\@:]) +/$1/g;

# non-Sun test mode delimiters -> easily-readable style
# Mark Sausville, MIT Media Laboratory, saus@media-lab.media.mit.edu
     	    $prevline =~ s/\^V/\$#/;
     	    $prevline =~ s/\^W/\$@/;
     	    $prevline =~ s/\^X/\$:/;

     ####   $#ether$@sun-barr.Ebay.Sun.COM$:site!user@uunet.uu.NET
     	    @t = split( '\$', $prevline);
     	    $address = "XXX";
     	    $remote = "XXX";
     	    $mailer = "XXX";
     	    for ($i = 1; $i <= $#t; $i++)
     	    {
     		if (substr($t[$i], 0, 1) eq ":")
     		    { $address = substr($t[$i], 1,999); }
     		if (substr($t[$i], 0, 1) eq "@")
     		    { $remote = substr($t[$i], 1,999); }
     		if (substr($t[$i], 0, 1) eq "#")
     		    { $mailer =  substr($t[$i], 1,999);
			$mailer =~ tr/A-Z/a-z/; }
     	    }
	    printf("%-27.27s --(%-6.6s)-->  %s[%s]\n", $input, $mailer,
     			$address, $remote);
	    if ($mailer ne "XXX") { $themailers{$mailer}.="$input ";}
}

# Parse output of sendmail name resolution 

sub parseaddress
{
##   ADDRESS TEST MODE
##   Enter <ruleset> <address>
##   > rewrite: ruleset  3   input: "user"

     open(MAIL, $_[0]) || die "Can't open $_[0]...\n";
     local ($k, $address, $prevline, $mailer, $remote, $input);
     print $_[1];			# the title

    $done = 0;
     while (<MAIL>)
     {
	if (/^>  *$/) { $done = 1; }
        if (/^ADDRESS TEST MODE/) { next; }
        if (/^Enter <ruleset>/) { next; }
        chop;
        if (/^>/) 			# INPUT LINE! (and last output is done)
        {
         	if ($prevline) {	# OUTPUT LINE!
         	    $prevline =~ s/.*returns:  *//;
         	    $prevline =~ s/"//g;
         	    $prevline =~ s/ //g;
#> rewrite: ruleset  3   input: "site" "!" "user" "@" "uunet" "." "uu" "." "net"
#rewrite: ruleset  8   input: "site" "!" "user" "@" "uunet" "." "uu" "." "net"
#> 
    	            printf("%-27.27s ---->  %s\n", $input, $prevline);
         	}
     	    s/> *[0-9,]* *//g;
     	    s/.*input: *//;
     	    s/ //g;
     	    s/"//g;
     	    $input = $_;
        }
	last if $done;
        $prevline = $_;
     }
    close(MAIL);
}


# pass names from resolve files to sendmail

open(IN, $resolve) || die "can't open $resolve\n";
open(OUT, ">/tmp/csm$$") || die "can't open >/tmp/csm$$\n";
while (<IN>) 
{
    /^$/ && next;
    /^#/ && next;
    print OUT "3,0 $_";
    chop;
    $allinput .= " $_";
}
close(OUT);
close(IN);
$sendmailprog = "$sendmail -bt -C$cffile < /tmp/csm$$|";
open(MAIL, $sendmailprog) || die "can't exec $sendmailprog...\n";
print "Mail address resolution (rule 0)\n\n";
&parseresolve();

# calculate $f
    $sendmailprog = "|$sendmail -bt -C$cffile > /tmp/csm$$";
    open (OUT, $sendmailprog);
    open (IN, $resolve);
    $rules = "3,1,4";
    while (<IN>) {
        chop;
        /^$/ && next;
        /^#/ && next;
	$address = $_;
        print OUT "$rules $address\n";
    }
    print OUT "\n";			# stuff last line into sendmail
    close(IN);
    close(OUT);
    &parseaddress ("/tmp/csm$$", "\n\n\$f address header transformations\n\n");

# Get the R= and S= special rulesets from the configuration

open(FID, "grep ^M $cffile|");
while (<FID>)
{
#    Mether,	P=[IPC], F=msDFMueCX, S=11, R=21, A=IPC $h
    chop;
    $thisline = $_; $thisline =~ s/M//; $thisline =~ s/,.*//;
    $thisline =~ tr/A-Z/a-z/;
    $sendrule = $_; $sendrule =~ s/.*S=//; $sendrule =~ s/,.*//;
	if ($sendrule=~q!/!) {
		($esendrule, $hsendrule) = split (q!/!, $sendrule);
	}
	else { $esendrule = $hsendrule = $sendrule; }
    $recvrule = $_; $recvrule =~ s/.*R=//; $recvrule =~ s/,.*//;
	if ($recvrule=~q!/!) {
		($erecvrule, $hrecvrule) = split (q!/!, $recvrule);
	}
	else { $erecvrule = $hrecvrule = $recvrule; }
    $esendrules{$thisline} = $esendrule;
    $hsendrules{$thisline} = $hsendrule;
    $erecvrules{$thisline} = $erecvrule;
    $hrecvrules{$thisline} = $hrecvrule;
}
close(FID);

# pass names from various mailers through sendmail

foreach $i (keys %themailers)
{
    $sendmailprog = "|$sendmail -bt -C$cffile > /tmp/csm$$";
    if ($erecvrules{$i} == 0) {
	print "\n\nCan't find recv rule for `$i', skipping this mailer <-------------------\n";
	next;
    }
    open (OUT, $sendmailprog);
    @namelist = split(/ /, $allinput);
    $rules = "3,2,$erecvrules{$i},4";
    foreach (@namelist) {
        print OUT "$rules $_\n";
    }
    print OUT "\n";			# stuff last line into sendmail
    close(OUT);
    $note = "";
    if ($erecvrules{$i} eq $hrecvrules{$i}) { $note = "and header "; }
    &parseaddress ("/tmp/csm$$", "\n\n`To' envelope ${note}address transformations for mailer $i [$rules]:\n\n");
}
foreach $i (keys %themailers)
{
    $sendmailprog = "|$sendmail -bt -C$cffile > /tmp/csm$$";
    if ($hrecvrules{$i} == 0) {
	print "\n\nCan't find recv rule for `$i', skipping this mailer <-------------------\n";
	next;
    }
    if ($hrecvrules{$i} ne $erecvrules{$i}) {
        open (OUT, $sendmailprog);
        @namelist = split(/ /, $allinput);
        $rules = "3,2,$hrecvrules{$i},4";
        foreach (@namelist) {
            print OUT "$rules $_\n";
        }
        print OUT "\n";			# stuff last line into sendmail
        close(OUT);
        &parseaddress ("/tmp/csm$$", "\n\n`To' header address transformations for mailer $i [$rules]:\n\n");
    }
}

# pass names from various mailers through sendmail
foreach $i (keys %themailers)
{
    $sendmailprog = "|$sendmail -bt -C$cffile > /tmp/csm$$";
    if ($esendrules{$i} == 0) {
	print "\n\nCan't find send rule for `$i', skipping this mailer <-------------------\n";
	next;
    }
    open (OUT, $sendmailprog);
    open (IN, $resolve);
    $rules = "3,1,$esendrules{$i},4";
    while (<IN>) {
        chop;
        /^$/ && next;
        /^#/ && next;
	$address = $_;
        print OUT "$rules $address\n";
    }
    print OUT "\n";			# stuff last line into sendmail
    close(IN);
    close(OUT);
    $note = "";
    if ($esendrules{$i} eq $hsendrles{$i}) {$note = "and header ";}
    &parseaddress ("/tmp/csm$$", "\n\n`From' envelope ${note}address transformations for mailer $i [$rules]:\n\n");
}
foreach $i (keys %themailers)
{
    $sendmailprog = "|$sendmail -bt -C$cffile > /tmp/csm$$";
    if ($hsendrules{$i} == 0) {
	print "\n\nCan't find send rule for `$i', skipping this mailer <-------------------\n";
	next;
    }
    open (OUT, $sendmailprog);
    open (IN, $resolve);
    $rules = "3,1,$hsendrules{$i},4";
    while (<IN>) {
        chop;
        /^$/ && next;
        /^#/ && next;
	$address = $_;
        print OUT "$rules $address\n";
    }
    print OUT "\n";			# stuff last line into sendmail
    close(IN);
    close(OUT);
    &parseaddress ("/tmp/csm$$", "\n\n`From' address header transformations for mailer $i [$rules]:\n\n");
}
foreach $i (keys %themailers)
{
    $sendmailprog = "|$sendmail -bt -C$cffile > /tmp/csm$$";
    if ($esendrules{$i} == 0) {
	print "\n\nCan't find send rule for `$i', skipping this mailer <-------------------\n";
	next;
    }
    open (OUT, $sendmailprog);
    open (IN, $resolve);
    $rules = "3,1,4,3,1,$hsendrules{$i},4"; # XXX esendrules? book p. 431???
    while (<IN>) {
        chop;
        /^$/ && next;
        /^#/ && next;
	$address = $_;
        print OUT "$rules $address\n";
    }
    print OUT "\n";			# stuff last line into sendmail
    close(IN);
    close(OUT);
    $note = "";
    if ($esendrules{$i} eq $hsendrles{$i}) {$note = "and header ";}
    &parseaddress ("/tmp/csm$$", "\n\n\$g address transformations for mailer $i [$rules]:\n\n");
}

unlink("/tmp/csm$$") || die "Can't unlink /tmp/csm$$";
unlink("/tmp/csm.in$$");

