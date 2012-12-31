#! /usr/bin/env perl

use File::stat;

my %MapMonth = (
    "01" => "January",
    "02" => "February",
    "03" => "March",
    "04" => "April",
    "05" => "May",
    "06" => "June",
    "07" => "July",
    "08" => "August",
    "09" => "September",
    "10" => "October",
    "11" => "November",
    "12" => "December");

my ($testfile, $version, $usea4) = @ARGV;
my ($day, $month, $year) = &getDateFromFile($testfile);

print "\@set UPDATED $day $month $year\n";
print "\@set UPDATED-MONTH $month $year\n";
print "\@set EDITION $version\n";
print "\@set VERSION $version\n";

#if ($usea4 =~ /(ON|TRUE)/i) {
#    print "\@afourpaper\n";
#}

##########################################################

sub getDateFromFile($)
{
    my ($file) = @_;

    my ($m, $d, $j);
    my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst);

    my $sb = stat($file);
    ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime($sb->mtime);
    $m = sprintf("%02d", $mon+1);
    $j = $year+1900;
    $d = sprintf("%02d", $mday);

    return($d, $MapMonth{$m}, $j);
}
