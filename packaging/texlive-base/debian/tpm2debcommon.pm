#!/usr/bin/perl
#
# tpm2deb-common.pl
# machinery to create debian packages from TeX Live depot
# (c) 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012 Norbert Preining
#
# configuration is done via the file tpm2deb.cfg
#
#
# please see the README file in the pkg-texlive svn repository of the
# debian-tex project on alioth.debian.org

package tpm2debcommon;
use Exporter ();
@ISA = qw( Exporter );
@EXPORT_OK = qw (
	build_data_hash check_consistency
	initialize_config_file_data is_blacklisted get_all_files 
	get_all_executes myopen mycopy ismember
	%TeXLive %Config
);
@EXPORT = @EXPORT_OK;

# keys(%TeXLive{$package}) = extra_format, uploaders, section,
#       standards, build_dep_indep, build_dep, priority, description, title,
#       license, sourcefiles, runfiles, docfiles, binfiles,
#       realtype, type, replaces, provides, recommends, suggests,
#       conflicts, depends, includedpackages, executes
#
# keys(%Config) = 
#                 add_execute, title, description, depends, conflicts,
#                 suggests, recommends provides, maintainer

use strict;
no strict 'refs';
use warnings;
no warnings 'uninitialized';


#use Strict;
use Getopt::Long;
use File::Basename;
use File::Copy;
use File::Path;
use File::Temp qw/ tempfile tempdir /;
use Cwd;


our (%TeXLive,%Config);

my $opt_debug;
my $opt_onlyscripts;
my $Master;
sub use_global_vars {
  $opt_debug = $main::opt_debug;
  $opt_onlyscripts = $main::opt_onlyscripts;
  $Master = $main::Master;
}


sub build_data_hash {
	
	sub tpm2debname {
		my @list_of_debnames = ();
		foreach my $pkg (@_) {
			if (defined($Config{'name_mapping'}{$pkg})) {
				push @list_of_debnames, $Config{'name_mapping'}{$pkg};
			} else {
				push @list_of_debnames, $pkg;
			};
		}
		return(@list_of_debnames);
	}
	# start real work
	print "Building data hash ...\n";
	#
	# we now build the information used for building the packages
	# this should merge the tpm data and the config file data
	# after this we check on consistency
	#
	# The data will be stored in %TeXLive{'binary'}{$package}{'item'}
	# where item is in: depend, suggest, conflict, execute, description
	# title, packages, docfiles, binfiles, runfiles, sourcefiles, relocated
	# remotefiles, tltype, license for binary packages, and in
	# %TeXLive{'source'}{$package}{'item'} .... for source packages.
	#
	# Note that the keys in %TeXLive are *different* package names than
	# the collection names!
	#
	# First the binary part
	my @collections = ();
	foreach my $bin_pkg ($::tlpdb->list_packages()) {
		next if ($bin_pkg =~ m/^00texlive/);
		if ($bin_pkg =~ m/\.(.*)$/) {
			next if ("$1" ne "i386-linux");
		}
		#
		# TODO TODO TODO
		# what todo with texlive.infra!!!!
		next if is_blacklisted ($bin_pkg, "");
		if ($bin_pkg =~ m/^(.*)\.i386-linux/) {
			next if is_blacklisted( $1, "");
		}
		my $tlp = $::tlpdb->get_package($bin_pkg);
		die "Cannot get $bin_pkg from tlpdb!" unless defined($tlp);
		$tlp->cancel_reloc_prefix;
		my ($pkg) = tpm2debname($bin_pkg);
		my $realtype = $tlp->category;
		next if ($realtype eq "Scheme");
		if ($realtype eq "Collection") {
			push @collections, $pkg;
		}
		# we consider everything either as collection or as package, no
		# difference what so ever
		my $faketype = ($realtype eq "Collection") ? $realtype : "Package";
		#
		# we consider TLCore packages as normal packages
		# since what we actually ship are the collections
		$TeXLive{'binary'}{$pkg}{'type'} 	       = $faketype;
		$TeXLive{'binary'}{$pkg}{'realtype'}        = $realtype;
		my %foo = %{$tlp->binfiles};
		if (defined($foo{'i386-linux'})) {
			$TeXLive{'binary'}{$pkg}{'binfiles'}    = [ @{$foo{'i386-linux'}} ];
		} else {
			$TeXLive{'binary'}{$pkg}{'binfiles'}    = [ ];
		}
		$TeXLive{'binary'}{$pkg}{'docfiles'}    = [ $tlp->docfiles ];
		$TeXLive{'binary'}{$pkg}{'runfiles'}    = [ $tlp->runfiles ];
		$TeXLive{'binary'}{$pkg}{'sourcefiles'} = [ $tlp->srcfiles ];
		$TeXLive{'binary'}{$pkg}{'license'}     = $tlp->cataloguedata->{'license'};
		$TeXLive{'binary'}{$pkg}{'relocated'}   = $tlp->relocated;
		# items that can be overwritten by the configuration file
		$TeXLive{'binary'}{$pkg}{'title'} =
			$Config{'shortdesc'}{$pkg} ? 
				$Config{'shortdesc'}{$pkg} : $tlp->shortdesc;
		$TeXLive{'binary'}{$pkg}{'description'} =
			$Config{'description'}{$pkg} ?
				$Config{'description'}{$pkg} : $tlp->longdesc;
		#
		# executes
		#
		my @executes = $tlp->executes;
		if (defined($Config{'add_execute'}{$pkg})) {
			foreach my $e (@{$Config{'add_execute'}{$pkg}}) {
				if (grep(/$e/,@executes) == 0) {
					push (@executes, $e);
				}
			}
		}
		$TeXLive{'binary'}{$pkg}{'executes'} = [ @executes ];
		#
		# included packages
		#
		my @requires = $tlp->depends;
		my @packs = ();
		my @depends = ();
		foreach my $dep (@requires) {
			$dep =~ s/\.ARCH/.i386-linux/;
			my $deptlp = $::tlpdb->get_package($dep);
			if (!defined($deptlp)) {
				printf STDERR "Cannot find $dep!\n";
				next;
			}
			my $depcat = $deptlp->category;
			#
			# dependencies on collections are handled below
			if ($depcat eq "Collection") {
				push @depends, $dep;
				next;
			}
			#
			# if the package is moved somewhere else, ignore it
			if (defined($Config{'moved_to'}{$dep})) { 
				next; 
			}
			if (!is_blacklisted($dep,$pkg)) { push @packs, $dep; } 
		}
		# normally collections contain all sub-dependencies, ie we
		# dont have deps of deps to consider.
		# One exception is the .ARCH dep. collection-basicbin depends
		# on bin-bibtex which in turn depends on bin-bibtex.ARCH
		# but the original collection does not depend on bin-bibtex.ARCH
		if ($realtype eq "Collection") {
			push @packs, @{$Config{'extra_packages'}{$pkg}}
				if defined($Config{'extra_packages'}{$pkg});
			# work through all @packs and see if we find a .ARCH one
			my @addpacks;
			foreach my $p (@packs) {
				next if (!defined($::tlpdb->get_package($p)));
				foreach my $d ($::tlpdb->get_package($p)->depends) {
					if ($d =~ m/\.ARCH/) {
						if (defined($::tlpdb->get_package("$p.i386-linux"))) {
							push @addpacks, "$p.i386-linux";
						}
					}
				}
			}
			push @packs, @addpacks;
		}
		$TeXLive{'binary'}{$pkg}{'includedpackages'} = [ @packs ];
		#
		# depends
		#
		# we have to collect the depends from the config file and the 
		# direct tpm dependencies
		@depends = tpm2debname(@depends);
		if ($pkg ne "texlive-common") {
			push @depends, "texlive-common (>= $TeXLive{'all'}{'tl_common_version'})";
		}
		#
		if (defined($Config{'depends'}{$pkg})) {
			push @depends, @{$Config{'depends'}{$pkg}};
		}
		# what about ${shlibs:Depends}
		$TeXLive{'binary'}{$pkg}{'depends'} = [ @depends ];
		#
		# other relations
		#
		if (defined($Config{'conflicts'}{$pkg})) {
			$TeXLive{'binary'}{$pkg}{'conflicts'} = [ @{$Config{'conflicts'}{$pkg}} ];
		}
		if (defined($Config{'suggests'}{$pkg})) {
			$TeXLive{'binary'}{$pkg}{'suggests'} = [ @{$Config{'suggests'}{$pkg}} ];
		}
		if (defined($Config{'recommends'}{$pkg})) {
			$TeXLive{'binary'}{$pkg}{'recommends'} = [ @{$Config{'recommends'}{$pkg}} ];
		}
		if (defined($Config{'provides'}{$pkg})) {
			$TeXLive{'binary'}{$pkg}{'provides'} = [ @{$Config{'provides'}{$pkg}} ];
		}
		if (defined($Config{'replaces'}{$pkg})) {
			$TeXLive{'binary'}{$pkg}{'replaces'} = [ @{$Config{'replaces'}{$pkg}} ];
		}
		if (defined($Config{'breaks'}{$pkg})) {
			$TeXLive{'binary'}{$pkg}{'breaks'} = [ @{$Config{'breaks'}{$pkg}} ];
		}
		if (defined($Config{'bin-section'}{$pkg})) {
			$TeXLive{'binary'}{$pkg}{'section'} = $Config{'bin-section'}{$pkg};
		}
		if (defined($Config{'bin-priority'}{$pkg})) {
			$TeXLive{'binary'}{$pkg}{'priority'} = $Config{'bin-priority'}{$pkg};
		}
	}
	#
	# we do the doc splitting now
	foreach my $coll (@collections) {
		if (is_blacklisted($coll,"")) { next; }
		my ($bin_pkg) = tpm2debname("$coll");
		if (defined($Config{'docsplitting'}{$bin_pkg})) {
			my $doc_pkg = "$bin_pkg-doc";
			# first creat new bin package and add doc files

			$TeXLive{'binary'}{$doc_pkg}{'section'} = "doc" ; 
			$TeXLive{'binary'}{$doc_pkg}{'type'}        = 'Collection';
			$TeXLive{'binary'}{$doc_pkg}{'realtype'}    = 'Collection';
			# we don't want to move man pages!!!
			my @p = ();
			my @pd = ();
			foreach my $f (@{$TeXLive{'binary'}{$bin_pkg}{'docfiles'}}) {
				if ($f =~ m;texmf[^/]*/doc/man/man.*/.*;) {
					push @p, $f;
				} else {
					push @pd, $f;
				}
			}
			$TeXLive{'binary'}{$doc_pkg}{'docfiles'}    = [ @pd ];
			$TeXLive{'binary'}{$bin_pkg}{'docfiles'} = [ @p ];
			$TeXLive{'binary'}{$doc_pkg}{'license'}     = $TeXLive{'binary'}{$bin_pkg}{'license'};
			$TeXLive{'binary'}{$doc_pkg}{'title'} = "Documentation files for $bin_pkg";
			$TeXLive{'binary'}{$doc_pkg}{'description'} = "This package provides the documentation for $bin_pkg";
			# what else do we have to set here ????
			#
			# the doc package needs to depend on texlive-common, it
			# doesn't get this dependency as ordinary packages do
			$TeXLive{'binary'}{$doc_pkg}{'depends'} = 
				[ @{$TeXLive{'binary'}{$doc_pkg}{'depends'}}, 
				  "texlive-common (>= $TeXLive{'all'}{'tl_common_version'})" 
				];
			# add a recommends for the normal package on the doc pkg.
			$TeXLive{'binary'}{$bin_pkg}{'recommends'} = [ @{$TeXLive{'binary'}{$bin_pkg}{'recommends'}}, "$bin_pkg-doc" ];
			# add source mapping
			my $srcpkg = $TeXLive{'binary'}{$bin_pkg}{'source_package'};
			$TeXLive{'source'}{$srcpkg}{'binary_packages'} = [ @{$TeXLive{'source'}{$srcpkg}{'binary_packages'}}, $doc_pkg ];
			#
			# necessary relations from the config file
			#
			# we need texlive-common (for tex-common)
			push @{$TeXLive{'binary'}{$doc_pkg}{'depends'}}, "texlive-common (>= $TeXLive{'all'}{'tl_common_version'})";
			if (defined($Config{'depends'}{$doc_pkg})) {
				$TeXLive{'binary'}{$doc_pkg}{'depends'} = [ @{$Config{'depends'}{$doc_pkg}} ];
			}
			if (defined($Config{'conflicts'}{$doc_pkg})) {
				$TeXLive{'binary'}{$doc_pkg}{'conflicts'} = [ @{$Config{'conflicts'}{$doc_pkg}} ];
			}
			if (defined($Config{'suggests'}{$doc_pkg})) {
				$TeXLive{'binary'}{$doc_pkg}{'suggests'} = [ @{$Config{'suggests'}{$doc_pkg}} ];
			}
			if (defined($Config{'recommends'}{$doc_pkg})) {
				$TeXLive{'binary'}{$doc_pkg}{'recommends'} = [ @{$Config{'recommends'}{$doc_pkg}} ];
			}
			if (defined($Config{'provides'}{$doc_pkg})) {
				$TeXLive{'binary'}{$doc_pkg}{'provides'} = [ @{$Config{'provides'}{$doc_pkg}} ];
			}
			if (defined($Config{'replaces'}{$doc_pkg})) {
				$TeXLive{'binary'}{$doc_pkg}{'replaces'} = [ @{$Config{'replaces'}{$doc_pkg}} ];
			}
			if (defined($Config{'breaks'}{$doc_pkg})) {
				$TeXLive{'binary'}{$doc_pkg}{'breaks'} = [ @{$Config{'breaks'}{$doc_pkg}} ];
			}
			#
			# now we have to move all the DocFiles of all INCLUDED
			# package/tpms into $bin_pkg and remove them from the original
			# packages
			#
			# For an explanation of the code see above!
			foreach my $incpkg (@{$TeXLive{'binary'}{$bin_pkg}{'includedpackages'}}) {
				my @p = ();
				my @pd = ();
				foreach my $f (@{$TeXLive{'binary'}{$incpkg}{'docfiles'}}) {
					if (defined($TeXLive{'all'}{'file_map_actions'}{$f}) && 
								($TeXLive{'all'}{'file_map_actions'}{$f} eq "move")) {
						push @p, $f;
					} else {
						if ($f =~ m;texmf[^/]*/doc/man/man.*/.*;) {
							push @p, $f;
						} else {
							push @pd, $f;
						}
					}
				}
				$TeXLive{'binary'}{$doc_pkg}{'docfiles'}    = [ @{$TeXLive{'binary'}{$doc_pkg}{'docfiles'}}, @pd ];
				$TeXLive{'binary'}{$incpkg}{'docfiles'} = [ @p ];
			}
		}
	}
	#
	# Now for the source part
	#
	for my $srcpkg (@{$TeXLive{'all'}{'sources'}}) {
		$TeXLive{'source'}{$srcpkg}{'uploaders'}   = 
			$Config{'uploaders'}{$srcpkg}     ? 
				$Config{'uploaders'}{$srcpkg} : $TeXLive{'all'}{'uploaders'};
		$TeXLive{'source'}{$srcpkg}{'maintainer'}  = 
			$Config{'maintainer'}{$srcpkg}    ? 
				$Config{'maintainer'}{$srcpkg} : $TeXLive{'all'}{'maintainer'};
		$TeXLive{'source'}{$srcpkg}{'priority'} = 
			$Config{'priority'}{$srcpkg};
		$TeXLive{'source'}{$srcpkg}{'build_dep'} = 
			$Config{'build_dep'}{$srcpkg};
		$TeXLive{'source'}{$srcpkg}{'build_dep_indep'} =
			$Config{'build_dep_indep'}{$srcpkg};
		$TeXLive{'source'}{$srcpkg}{'standards'} =
			$Config{'standards'}{$srcpkg};
 		$TeXLive{'source'}{$srcpkg}{'section'} =
			$Config{'section'}{$srcpkg};
	}
	# we let texlive-common CONFLICT with all texlive packages << then the 
	# values set in latest-version
	my @conflictpkgs = ();
	foreach my $source_package (@{$TeXLive{'all'}{'sources'}}) {
		foreach my $bin_pkg (@{$TeXLive{'source'}{$source_package}{'binary_packages'}}) {
			push @conflictpkgs, "$bin_pkg (<< $TeXLive{'source'}{$source_package}{'latest_version'})";
		}
	}
	# finally we let the package "texlive-full" depend on all texlive-* packages
	my @allpkgs = ();
	foreach my $source_package (@{$TeXLive{'all'}{'sources'}}) {
		foreach my $bin_pkg (@{$TeXLive{'source'}{$source_package}{'binary_packages'}}) {
			my $addthis = 1;
			foreach my $depends_not (@{$Config{'depends_not'}{'texlive-full'}}) {
				$addthis = 0 if ($bin_pkg eq $depends_not);
			};
# 			next if ($bin_pkg eq "texlive-full");
# 			next if ($bin_pkg eq "texlive");
# 			next if ($bin_pkg eq "texlive-lang-all");
# 			next if ($bin_pkg eq "tetex-bin");
# 			next if ($bin_pkg eq "tetex-base");
# 			next if ($bin_pkg eq "tetex-extra");
			$addthis && push @allpkgs, "$bin_pkg (>= $TeXLive{'source'}{$source_package}{'latest_version'})";
		}
	}

	# Additional (meta)packages
	$TeXLive{'all'}{'meta_packages'} = [ @{$Config{'add_packages'}} ];
	foreach my $meta_package (@{$Config{'add_packages'}}) {
		$TeXLive{'mbinary'}{$meta_package}{'type'} = "TLCore" ;

		# Dependencies
		if ($meta_package eq "texlive-full") {
			$TeXLive{'mbinary'}{$meta_package}{'depends'}   = [ @{$Config{'depends'}{$meta_package}}, @allpkgs ];
		} elsif ($meta_package eq "texlive-lang-all") {
			my @foo = ();
			foreach my $a (@allpkgs) {
				if ($a =~ /^texlive-lang-/) { push @foo, $a; }
			}
			$TeXLive{'mbinary'}{$meta_package}{'depends'}   = [ @{$Config{'depends'}{$meta_package}}, @foo ];
		} else {
			$TeXLive{'mbinary'}{$meta_package}{'depends'}   = [ @{$Config{'depends'}{$meta_package}} ];
		};
		$opt_debug && print STDERR "metapackage: $meta_package, Depends: @{$TeXLive{'mbinary'}{$meta_package}{'depends'}}\n";
		$TeXLive{'mbinary'}{$meta_package}{'suggests'}    = [ @{$Config{'suggests'}{$meta_package}} ];
		$TeXLive{'mbinary'}{$meta_package}{'recommends'}  = [ @{$Config{'recommends'}{$meta_package}} ];
		$TeXLive{'mbinary'}{$meta_package}{'replaces'}    = [ @{$Config{'replaces'}{$meta_package}} ];
		$TeXLive{'mbinary'}{$meta_package}{'breaks'}    = [ @{$Config{'breaks'}{$meta_package}} ];
		if ($meta_package eq "texlive-common") {
			$TeXLive{'mbinary'}{$meta_package}{'conflicts'}   = [ @{$Config{'conflicts'}{$meta_package}}, @conflictpkgs ];
		} else {
			$TeXLive{'mbinary'}{$meta_package}{'conflicts'}   = [ @{$Config{'conflicts'}{$meta_package}} ];
		}
	  
		# Short and long description
		$TeXLive{'mbinary'}{$meta_package}{'title'}       = $Config{'title'}{$meta_package};
		$TeXLive{'mbinary'}{$meta_package}{'description'} = $Config{'description'}{$meta_package};
		if (defined($Config{'bin-section'}{$meta_package})) {
			$TeXLive{'mbinary'}{$meta_package}{'section'} = $Config{'bin-section'}{$meta_package};
		}
		if (defined($Config{'bin-priority'}{$meta_package})) {
			$TeXLive{'mbinary'}{$meta_package}{'priority'} = $Config{'bin-priority'}{$meta_package};
		}
	}
}

sub check_consistency {
	my %UsedPackages;
	my $raiseerror = 0;
	my @allincludedpkgs = ();
	my %PackageToCollection;

	print "Checking consistency ... \n";
	#
	# first go through all the collections and collect all included
	# packages and check on double inclusion of packages
	#
	my %TLB = %{$TeXLive{'binary'}};
	foreach my $collection (keys %TLB) {
		if ($TLB{$collection}{'type'} ne "Collection") { next; }
		foreach my $tpm (@{$TLB{$collection}{'includedpackages'}}) {
			$PackageToCollection{$tpm} = $collection;
			push @allincludedpkgs, $tpm;
			push (@{$UsedPackages{$tpm}},$collection);
		}
	}
	my @badpacks = ();
	foreach my $k (keys %UsedPackages) {
		if (@{$UsedPackages{$k}} > 1) {
			print STDERR "Double inclusion of $k:\n";
			print STDERR "@{$UsedPackages{$k}}:\n";
			push @badpacks, $k;
		}
	}
	if ($#badpacks >= 0) {
		#
		# ujjeeee, a package is included more than once, STOP!!!
		#
		print ("ERROR double-inclusion @badpacks\n");
		$raiseerror = 1;
	}
	#
	# check for not included packages
	#
	my $binary_package;
	foreach $binary_package (keys %TLB) {
		if ($TLB{$binary_package}{'type'} ne "Package") { next; }
		if (grep(/$binary_package/,@allincludedpkgs) == 0) {
			print STDERR "ERROR not-covered $binary_package\n";
			$raiseerror = 1;
		}
	}
	#
	# check for packages in packages which are not included or depended on
	#
	foreach my $c (keys %TLB) {
		if ($TLB{$binary_package}{'type'} ne "TLCore") { next; }
		foreach $binary_package (@{$TLB{$c}{'includedpackages'}}) {
			foreach my $subdep (@{$TLB{$binary_package}{'includedpackages'}}) {
				# $binary_package depends on $subdep, we have to check wether
				# either $subdep is also included in $c
				# or $c depends on the collection $subdep is in
				my $subdepcoll = $PackageToCollection{$subdep};
				# the package is included in the parent collection, ok
				if ($subdepcoll eq $c) { next ; }
				# the parent collection depends on the collection
				# including the subdep by tpm depends
				if ($subdepcoll eq "texlive-base-bin") { next; }
				if (ismember($subdepcoll,@{$TLB{$c}{'depends'}})) { next; }
				# ujeee, cross wise inclusion does not work
				print STDERR "ERROR diamond\n";
				print STDERR "There is a problem here:\n";
				print STDERR "$c includes $binary_package\n";
				print STDERR "\t$binary_package depends on $subdep\n";
				print STDERR "\t$subdep is NOT included in $c\n";
				print STDERR "$subdep is included in $subdepcoll\n";
				print STDERR "$c does NOT depend on $subdepcoll\n";
				print STDERR "Please fix this problem!\n";
				$raiseerror = 1;
			}
		}
	}
	if ($raiseerror) { exit 1; }
	print " ... tlpdb and config file are consistent, good!\n";
}


sub initialize_config_file_data {
	my ($cfgfile) = @_;
	my ($dn, $fn);
	if ($cfgfile =~ /(.*)\/(.*)/) {
		($dn, $fn) = ($1, $2);
	} else {
		die "What the hell, cannot get dirname/filename? $cfgfile\n";
	}
	use_global_vars();
	read_one_config_file($dn, $fn);
	my $spf = "";
	if (-d "./all") {
		$spf = "all/";
	}
	print "Reading ${spf}debian/scripts.lst ...\n";
	for (`bash ${spf}debian/create-linked-scripts ${spf}debian/scripts.lst`) {
		chomp;
		my ($type, $a, @rest) = split ";";
		if ($type eq "linkedscript") {
			my ($b) = @rest;
			$TeXLive{'all'}{'linkedscript'}{$a} = $b;
			next;
		} else {
			die "Unknown output of created-linked-scripts: $!";
		}
	}
}

sub read_one_config_file {
	my ($dn, $fn) = @_;
	my $cfgfile = "$dn/$fn";
	use_global_vars();
	print "Start loading config file $cfgfile ...\n";
	my $foo;
	open($foo,"<$cfgfile") or die "Cannot open $cfgfile\n";
	while (<$foo>) {
		if (m/^#/) { 
			next ; 
		}
		if (m/^\s*$/) {
			next ;
		}
		chomp;
		my @foo;
		my ($type, $a, @rest) = split ";";
		$opt_debug && print STDERR  "type=$type, a=$a, ";
		if ($type eq "depends") {
			my ($b) = @rest;
			$opt_debug && print STDERR  "b=$b.\n";
			$Config{'depends'}{$a} = [ @{$Config{'depends'}{$a}}, split(/[ \t]*,[ \t]*/,$b) ];
			next;
		}
		if ($type eq "recommends") {
			my ($b) = @rest;
			$opt_debug && print STDERR  "b=$b.\n";
			$Config{'recommends'}{$a} = [ @{$Config{'recommends'}{$a}}, split(/[ \t]*,[ \t]*/,$b) ];
			next;
		}
		if ($type eq "provides") {
			my ($b) = @rest;
			$opt_debug && print STDERR  "b=$b.\n";
			$Config{'provides'}{$a} = [ @{$Config{'provides'}{$a}}, split(/[ \t]*,[ \t]*/,$b) ];
			next;
		}
		if ($type eq "suggests") {
			my ($b) = @rest;
			$opt_debug && print STDERR  "b=$b.\n";
			$Config{'suggests'}{$a} = [ @{$Config{'suggests'}{$a}}, split(/[ \t]*,[ \t]*/,$b) ];
			next;
		}
		if ($type eq "conflicts") {
			my ($b) = @rest;
			$opt_debug && print STDERR  "b=$b.\n";
			$Config{'conflicts'}{$a} = [ @{$Config{'conflicts'}{$a}}, split(/[ \t]*,[ \t]*/,$b) ];
			next;
		}
		if ($type eq "replaces") {
			my ($b) = @rest;
			$opt_debug && print STDERR  "b=$b.\n";
			$Config{'replaces'}{$a} = [ @{$Config{'replaces'}{$a}}, split(/[ \t]*,[ \t]*/,$b) ];
			next;
		}
		if ($type eq "breaks") {
			my ($b) = @rest;
			$opt_debug && print STDERR  "b=$b.\n";
			$Config{'breaks'}{$a} = [ @{$Config{'breaks'}{$a}}, split(/[ \t]*,[ \t]*/,$b) ];
			next;
		}
		if ($type eq "execute") {
			my ($b) = @rest;
			$opt_debug && print STDERR  "b=$b.\n";
			$Config{'add_execute'}{$a} = [ @{$Config{'add_execute'}{$a}}, "$b" ];
			next;
		}
		if ($type eq "disable_format") {
			my ($b) = @rest;
			$opt_debug && print STDERR "b=$b.\n";
			$Config{'disabled_formats'}{$a} = [ @{$Config{'disabled_formats'}{$a}}, "$b" ];
			next;
		}
		if ($type eq "ignore") {
			push @{$TeXLive{'all'}{'ignore'}}, $a;
			next;
		}
		if ($type eq "kill") {
			push @{$TeXLive{'all'}{'kill'}}, $a;
			next;
		}
		if ($type eq "blacklist") {
			if ($a eq "file") {
				my ($b) = @rest;
				$opt_debug && print STDERR  "b=$b.\n";
				push @{$TeXLive{'all'}{'file_blacklist'}}, $b;
			} elsif ($a eq "tpm") {
				my ($b,$c) = @rest;
				$opt_debug && print STDERR  "b=$b, c=$c.\n";
				$Config{'package_blacklist'}{$b} = "$c";
			} else {
				print STDERR "tpm2deb.cfg: Unknown blacklist directive: $_. Exiting!\n";
				exit 1;
			}
			next;
		}
		if ($type eq "source") {
			push @{$TeXLive{'all'}{'sources'}}, $a;
			next;
		}
		if ($type eq "move") {
			my ($b) = @rest;
			$opt_debug && print STDERR  "b=$b.\n";
			# move tpm $a to binary package $b
			$Config{'moved_to'}{$a} = $b;
			$Config{'extra_packages'}{$b} = [ @{$Config{'extra_packages'}{$b}}, $a ];
			next;
		}
		if ($type eq "extra") {
			my ($b,$c,$d) = @rest;
			if ($a ne "format") {
				print STDERR "tpm2deb.cfg: Unknown extra directive: $_. Exiting!\n";
				exit 1;
			}
			$opt_debug && print STDERR  "b=$b, c=$c, d=$d.\n";
			$TeXLive{'binary'}{$c}{'extra_format'}{$b} = $d;
			next;
		}
		if ($type eq "arch") {
			my ($b) = @rest;
			$opt_debug && print STDERR  "b=$b.\n";
			$TeXLive{'source'}{$a}{'architecture'} = $b;
			next;
		}
		if ($type eq "name") {
			my ($b,$c) = @rest;
			$opt_debug && print STDERR  "b=$b, c=$c.\n";
			$Config{'name_mapping'}{$a} = $b;
			$TeXLive{'binary'}{$b}{'source_package'} = $c;
			$TeXLive{'source'}{$c}{'binary_packages'} = [ @{$TeXLive{'source'}{$c}{'binary_packages'}}, $b ];
			next;
		}
		if ($type eq "special") {
			my ($b) = @rest;
			$opt_debug && print STDERR  "b=$b.\n";
			push @{$TeXLive{'all'}{'special_actions_config'}}, "$a:$b";
			next;
		}
		if ($type eq "description") {
			my ($b) = @rest;
			$opt_debug && print STDERR  "b=$b.\n";
			# push the next line into the description string, but
			# avoid to get a "use of uninitialized value in concat..."
			$Config{'description'}{$a} = 
				($Config{'description'}{$a} ? $Config{'description'}{$a} : "") 
				. "$b\n";
			next;
		}
		if ($type eq "shortdesc") {
			my ($b) = @rest;
			$opt_debug && print STDERR  "b=$b.\n";
			$Config{'shortdesc'}{$a} = "$b";
			next;
		}
		if ($type eq "title") {
			my ($b) = @rest;
			$opt_debug && print STDERR  "b=$b.\n";
			$Config{'title'}{$a} = "$b";
			next;
		}
		if ($type eq "docsplitting") {
			$opt_debug && print STDERR  "\n";
			$Config{'docsplitting'}{$a} = 1;
			next;
		}
		if ($type eq "build-dep") {
			my ($b) = @rest;
			$opt_debug && print STDERR  "b=$b.\n";
			$Config{'build_dep'}{$a} = "$b";
			next;
		}
		if ($type eq "build-dep-indep") {
			my ($b) = @rest;
			$opt_debug && print STDERR  "b=$b.\n";
			$Config{'build_dep_indep'}{$a} = "$b";
			next;
		}
		if ($type eq "texlive-common-version") {
			$TeXLive{'all'}{'tl_common_version'} = "$a";
			next;
		}
		if ($type eq "old-version") {
			my ($b) = @rest;
			$opt_debug && print STDERR  "b=$b.\n";
			$TeXLive{'source'}{$a}{'old_version'} = "$b";
			next;
		}
		if ($type eq "latest-version") {
			my ($b) = @rest;
			$opt_debug && print STDERR  "b=$b.\n";
			$TeXLive{'source'}{$a}{'latest_version'} = "$b";
			next;
		}
		if ($type eq "maintainer") {
			my ($b) = @rest;
			$opt_debug && print STDERR  "b=$b.\n";
			if ("$a" eq "*") {
				$TeXLive{'all'}{'maintainer'} = "$b";
			} else {
				$Config{'maintainer'}{$a} = "$b";
			}
			next;
		}
		if ($type eq "priority") {
			my ($b) = @rest;
			$opt_debug && print STDERR  "b=$b.\n";
			if ("$a" eq "*") {
				$TeXLive{'all'}{'priority'} = "$b";
			} else {
				$Config{'priority'}{$a} = "$b";
			}
			next;
		}
		if ($type eq "bin-priority") {
			my ($b) = @rest;
			$opt_debug && print STDERR  "b=$b.\n";
			$Config{'bin-priority'}{$a} = "$b";;
			next;
		}
		if ($type eq "uploaders") {
			my ($b) = @rest;
			$opt_debug && print STDERR  "b=$b.\n";
			if ("$a" eq "*") {
				$TeXLive{'all'}{'uploaders'} = "$b";
			} else {
				$Config{'uploaders'}{$a} = "$b";
			}
			next;
		}
		if ($type eq "section") {
			my ($b) = @rest;
			$opt_debug && print STDERR  "b=$b.\n";
			if ("$a" eq "*") {
				$TeXLive{'all'}{'section'} = "$b";
			} else {
				$Config{'section'}{$a} = "$b";
			}
			next;
		}
		if ($type eq "bin-section") {
			my ($b) = @rest;
			$opt_debug && print STDERR  "b=$b.\n";
			$Config{'bin-section'}{$a} = "$b";;
			next;
		}
		if ($type eq "standards") {
			my ($b) = @rest;
			$opt_debug && print STDERR  "b=$b.\n";
			if ("$a" eq "*") {
				$TeXLive{'all'}{'standards'} = "$b";
			} else {
				$Config{'standards'}{$a} = "$b";
			}
			next;
		}
		if ($type eq "addpackages") {
			@{$Config{'add_packages'}} = @rest;
			$opt_debug && print STDERR "\nAdditional packages: @{$Config{'add_packages'}}\n";
			next;
		}
		if ($type eq "dependsnot") {
			@{$Config{'depends_not'}{$a}} = @rest;
			$opt_debug && print STDERR "Dropped depends of $a on @{$Config{'depends_not'}{$a}}\n";
			next;
		}
		if ($type eq "linkedscript") {
			my ($b) = @rest;
			$TeXLive{'all'}{'linkedscript'}{$a} = $b;
			next;
		}
		# removeconffile;texlive-lang-french;etc/texmf/language.d/10texlive-lang-french.cnf
		if ($type eq "removeconffile") {
			my ($b) = @rest;
			push @{$TeXLive{'binary'}{$a}{'remove_conffile'}}, $b;
			next;
		}
		if ($type eq "include-config") {
			read_one_config_file($dn,$a);
			next;
		}
		print STDERR "tpm2deb.cfg: Unknown directive: $type. Maybe an empty line?\n Exiting!\n"; 
		exit 1;
	}
	close($foo);
	print " ... done $cfgfile\n";
}


sub is_blacklisted {
	my ($tpm,$coll) = @_;
	# blacklist check
	# either there is no blacklist entry, then it is ok
	# if there is a blacklist entry and it is *, blacklist it
	# if it is not *, and the $coll is not "", then match it
	if (defined($Config{'package_blacklist'}{$tpm}) &&
			(($Config{'package_blacklist'}{$tpm} eq "*") ||
			 (($coll ne "") && ($Config{'package_blacklist'}{$tpm} =~ m/$coll/)))) {
		return(1);
	} else {
		return(0);
	}
}

#
# this function is called for debian package names
# we have to make sure that dummy transitional meta packages
# with the *SAME* name as a TeX Live package do not pull in 
# files from the TeX Live package.
sub get_all_files {
	my ($pkg, $rl) = @_;
	if (defined($TeXLive{'mbinary'}{$pkg})) {
		my %files;
		return(\%files);
	}
	return(get_all_files_real($pkg, $rl));
}

sub get_all_files_real {
	my ($entry,$reclevel) = @_;
	my @requires = @{$TeXLive{'binary'}{$entry}{'includedpackages'}};
	my %files;
	#
	$files{'BinFiles'} = \@{$TeXLive{'binary'}{$entry}{'binfiles'}};
	$files{'DocFiles'} = \@{$TeXLive{'binary'}{$entry}{'docfiles'}};
	$files{'RunFiles'} = \@{$TeXLive{'binary'}{$entry}{'runfiles'}};
	$files{'SourceFiles'} = \@{$TeXLive{'binary'}{$entry}{'sourcefiles'}};
	if ($reclevel > 0) {
		foreach my $r (@requires) {
			$opt_debug && print STDERR  "  package " . $r . "\n";
			my %foo = %{&get_all_files_real($r,$reclevel-1)};
			push @{$files{'BinFiles'}}, @{$foo{'BinFiles'}};
			push @{$files{'DocFiles'}}, @{$foo{'DocFiles'}};
			push @{$files{'RunFiles'}}, @{$foo{'RunFiles'}};
			push @{$files{'SourceFiles'}}, @{$foo{'SourceFiles'}};
		}
	}
	return(\%files);
}

sub get_all_executes {
	my ($entry,$reclevel) = @_;
	my @requires = @{$TeXLive{'binary'}{$entry}{'includedpackages'}};
	my %bar;
	my @executes;
	#
	@executes = @{$TeXLive{'binary'}{$entry}{'executes'}};
	if ($reclevel > 0) {
		foreach my $r (@requires) {
			$opt_debug && print STDERR  "  package " . $r . "\n";
			my @foo = get_all_executes($r,$reclevel-1);
			push @executes, @foo;
		}
	}
	foreach (@executes) {
		$bar{$_} = 1;
	}
	return(keys %bar);
}
 
sub myopen {
	my ($a, $fn) = @_;
	open($a, $fn) or die "Cannot open $fn: $!\n";
}

sub mycopy {
	my ($src,$dest) = @_;
	&mkpath(dirname($dest));
# 	system("/bin/cp -a $src $dest 2>/dev/null") == 0
	my $cp_cmdline = "/bin/cp -a $src $dest" . ( $opt_debug ? "" : " 2>/dev/null");
	if ($opt_onlyscripts == 0) {
		system($cp_cmdline) == 0
			or do {
				print STDERR "$cp_cmdline\n";
				die ("missing: $src\n");
		};
	};
}

sub ismember {
	my ($a, @rest) = @_;
	foreach my $i (@rest) {
		if ($a eq $i) { return 1; }
	}
	return 0;
}


1;

### Local Variables:
### perl-indent-level: 4
### tab-width: 4
### indent-tabs-mode: t
### End:
# vim:set tabstop=4: #
