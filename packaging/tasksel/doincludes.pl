#!/usr/bin/perl
#
# doincludes directory
#
# Expands #include directives in files in the directory. This is used
# to let task package pull in the contents of metapackages, keeping the
# contents up-to-date, w/o actually pulling in the metapackages themselves,
# since some metapackages are rather prone to breakage near release time.

my $dir=shift or die "no directory specified\n";

my %depends;
{
	local $/="\n\n";
	if (! open (AVAIL, "apt-cache dumpavail |")) {
		warn "cannot real available info, so not exanding includes\n";
		exit;
	}
	while (<AVAIL>) {
		my ($package)=/Package:\s*(.*?)\n/;
		my ($depends)=/Depends:\s*(.*?)\n/;
		$depends{$package}=$depends;
	}
	close AVAIL;
}

use File::Find;
find(\&processfile, $dir);

sub processfile {
	my $file=$_; # File::Find craziness.
	$file eq 'po' && -d $file && ($File::Find::prune = 1);
	return if $File::Find::dir=~/\.(svn|git)/;
	return unless $file =~ /^[-+_.a-z0-9]+$/ and -f $file;
	my @lines;
	open (IN, $file) or die "$file: $!";
	while (<IN>) {
		if (/#\s*endinclude/) {
			if ($skipping == 0) {
				die "$file: #endinclude without #include\n";
			}
			$skipping=0;
		}
		
		push @lines, $_ unless $skipping == 1;

		if (/^#\s*include\s+(\w+)/) {
			my $pkg=$1;
			if ($skipping) {
				die "$file: nested includes near $_\n";
			}
			if (! exists $depends{$pkg}) {
				warn "$file: #include $1 skipped; no such package. Leaving what was there alone.\n";
				$skipping=-1;
			}
			else {
				push @lines, "#Automatically added by doincludes.pl; do not edit.\n";
				# Split deps and remove alternates and versioned
				# deps. Include the metapackage on the list.
				push @lines, map { s/[|(].*//; "  $_\n" }
				             split(/,\s+/, $depends{$pkg}), $pkg;
				$skipping=1;
			}
		}
	}
	close IN;
	if ($skipping == 1) {
		die "$file: #include without #endinclude";
	}
	
	open (OUT, ">$file") or die "$file: $!";
	print OUT @lines;
	close OUT;
}
