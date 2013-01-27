#!/usr/bin/perl
#
# listpackages directory [field]
#
# This program spits out a list of all the packages listed in the tasks.
#
# If you go to auric, this command is then useful:
#
# for package in $(listpackages); do
#   madison -s testing -a "i386 all" $package >/dev/null || echo "No $package!"
# done
#
# Or to see just key packages:
#
# listpackages tasks key

my $dir=shift or die "no directory specified\n";
my @toshow=qw{packages-list key};
@toshow=@ARGV if @ARGV;

use File::Find;
find(\&processfile, $dir);

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

	my @list;
	push @list, split(' ', $fields{$_}) foreach @toshow;
	print join("\n", @list)."\n" if @list;
}
