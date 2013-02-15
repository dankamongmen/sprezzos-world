#!/usr/bin/perl
# vim:ts=4:et:sts=4:sw=4:ai
use strict;
use warnings;

# Script reads debian/control to get list of the binary packages. Then
# for each package reads  or debian/packages.d/${package}.in
# it at the line which are equal to %filename%, where filename can be any file name.
#
# When called without "generate" argument, the text  between that line and the next %filename%
# are written to the file debian/package.filename
#
# When called with the "clean" argument it removes debian/package.filename files


my %descriptions = 
(
'ibritish'         => 'British English',
'ibritish-small'   => 'British English - small',
'ibritish-large'   => 'British English - large',
'ibritish-huge'    => 'British English - huge',
'ibritish-insane'  => 'British English - insane',

'iamerican'        => 'American English',
'iamerican-small'  => 'American English - small',
'iamerican-large'  => 'American English - large',
'iamerican-huge'   => 'American English - huge',
'iamerican-insane' => 'American English - insane'

);

my %suffixes =
(
    ''          => 'med+',
    'small'     => 'sml+',
    'large'     => 'lrg+',
    'huge'      => 'xlg+',
    'insane'    => 'xxl+'
);

my %locales =
(
    'british'  => 'en_GB',
    'american' => 'en_US'
);



die "Usage: $0 clean | generate\n" if $#ARGV != 0;
my $action = $ARGV[0];
my $builddirval = $ARGV[1];
my $cleanonly =  $action eq "clean";

die "Invalid action $action" unless $cleanonly or $action eq "generate";

my @debdirs = grep { -d $_ } ("./debian", "../debian", "../../debian");
die "Cannot find debian dir" unless $#debdirs < 1;
chdir "$debdirs[0]/.." or die "Cannot chdir to $debdirs[0]/..: $!\n";

my $dir="debian/packages.d";

my @packages=`dh_listpackages`;
while (@packages)
{
    chomp (my $package = shift(@packages));
    my ($locale, $dictionary, $description, $munchlist);
    if (exists $descriptions{$package} && $package =~ /^i(([^-]+)-?([^-]+)?)$/)
    {
        ($locale, $dictionary, $description, $munchlist) 
         = ($locales{$2}, $1, $descriptions{$package}, $2.'.'.$suffixes{$3 ? $3 :''}); 
    }

    my $master_file = $description ? "$dir/dictionaries.in" : "$dir/$package.in";
    my $fh = undef;

    open IN, "<", $master_file or die "Cannot open $master_file: $!\n";
    print $cleanonly ? "Removing" : "Generating";
    while (<IN>)
    {
        next if /^\%#/;
        if (/^\%([^:]*):?(.*)\%$/)
        {
            close $fh if $fh;
            $fh = undef;
            my $file="debian/$package.$1";
            if ($2)
            {
                next unless grep { $_ eq $package } split(' ', $2);
            }    

            print " $file";
            unlink $file or die "Cannot unlink $file: $!\n" if -e $file;
            next if $cleanonly;

            open $fh, ">", $file or die "Cannot open $file for writing: $!\n";
            chmod 0444, $file or die "Cannot chmod $file: $!\n";
            print $fh "### Generated from $master_file for $package ###\n" unless $file =~ /(info-ispell|inst|rm)$/;
        }
        elsif ($fh && !$cleanonly)
        {
            s/#PACKAGE#/$package/g;
            if ($description)
            {
                s/#DICTIONARY#/$dictionary/g   ;
                s/#DESCRIPTION#/$description/g ;
                s/#MUNCHLIST#/$munchlist/g     ;
                s/#LOCALE#/$locale/g           ;
            }
            print $fh $_;
        }
    }
    print "\n";
    close $fh if $fh;
    close IN;
}
