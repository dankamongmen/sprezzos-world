#!/usr/bin/perl
#
# tpm2deb-bin.pl
# machinery to create debian packages from TeX Live depot
# (c) 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012 Norbert Preining
#
# configuration is done via the file tpm2deb.cfg
#

BEGIN {   # get our other local perl modules.
	unshift (@INC, "./debian");
	unshift (@INC, "./tlpkg");
}

use strict "vars";
# use strict "refs"; # not possible with merge_into
use warnings;
no warnings 'once';
no warnings 'uninitialized';

#use Strict;
use Getopt::Long;
use File::Basename;
use File::Copy;
use File::Path;
use File::Temp qw/ tempfile tempdir /;
use Cwd;

use TeXLive::TLPDB;
use TeXLive::TLPOBJ;

# use Data::Dumper;


my $basedir;
my $debdest = "./debian";


my $opt_nosource=0;
our $opt_onlyscripts=0;
my $opt_onlycopy=0;

our $opt_debug; #global variable
our $Master;
my $globalreclevel=1;

my $result = GetOptions ("debug!" => \$opt_debug, 	# debug mode
	"nosource!" => \$opt_nosource,			# don't include source files
	"onlyscripts!" => \$opt_onlyscripts, # only create maintainer scripts
	"onlycopy!" => \$opt_onlycopy # no maintscripts, only copy files
	);

$Master = `pwd`;
chomp($Master);

my $startdir=getcwd();
chdir($startdir);
File::Basename::fileparse_set_fstype('unix');

use tpm2debcommon;

&main(@ARGV);

1;


sub main {
	my (@packages) = @_;
	my $arch = "all";
	# the following variable is used in the Tpm.pm module,
	# and should always be set to i386-linux, no matter what 
	# the real Debian architecture is
	$::tlpdb = TeXLive::TLPDB->new(root => "$Master");
	die "Cannot load tlpdb!" unless defined($::tlpdb);
	initialize_config_file_data("debian/tpm2deb.cfg");
	build_data_hash();
	#    use Data::Dumper;
	#    $Data::Dumper::Indent = 1;
	#    print Dumper(\%TeXLive);
    #    exit(1);
	check_consistency();
	foreach my $package (@packages) {
		# 
		# various variables have to be set
		#
		#$arch = get_arch($package);
		#print "Working on $package, arch=$arch\n";
		print "Working on $package\n";
		# determine variables used in all subsequent functions
		$opt_debug && print STDERR "Setting global vars\n";
		$basedir = "./debian/$package";
		#
		# copy files etc.
		# 
		make_deb($package); #unless ($opt_onlyscripts);
		#
		# create the maintainer scripts
		#
		make_maintainer($package) unless ($opt_onlycopy);
	}
}

#
# tl_is_blacklisted <filename>
#
sub tl_is_blacklisted {
	my ($file) = @_;
	my $blacklisted = 0;
	foreach my $pat (@{$TeXLive{'all'}{'file_blacklist'}}) { 
		$blacklisted = 1 if ($file =~ m|^${pat}$|);
	}
	foreach my $pat (@{$TeXLive{'all'}{'kill'}}) { 
		$blacklisted = 1 if ($file =~ m|^${pat}$|);
	}
	$opt_debug && $blacklisted && print STDERR "$file is blacklisted/killed\n";
	return $blacklisted;
}
sub tl_is_ignored {
	my ($file) = @_;
	my $ignored = 0;
	foreach my $pat (@{$TeXLive{'all'}{'ignore'}}) { 
		$ignored = 1 if ($file =~ m|^${pat}$|);
	}
	$opt_debug && $ignored && print STDERR "$file is ignored\n";
	return $ignored;
}


#
# make_deb_copy_to_righplace
#
sub make_deb_copy_to_rightplace {
	my ($package,$listref) = @_;
	my %lists = %$listref;
	my @all_files;
	my @SpecialActions;
	push @all_files, @{$lists{'RunFiles'}};
	push @all_files, @{$lists{'DocFiles'}};
	push @all_files, @{$lists{'SourceFiles'}} if (!$opt_nosource);
	foreach my $file (@all_files) {
		# all files are now in texmf-dist
		my $lfile = $file;
		# SWITCH to move all files to /usr/share/texmf
		#$lfile =~ s:^texmf-dist/::;
		next if tl_is_blacklisted($file);
		if (!tl_is_ignored($file)) {
			my $finaldest = "$basedir/usr/share/texlive/$lfile";
			if ($lfile =~ m!^texmf-dist/doc/(.*)$!) {
				$finaldest = "$basedir/usr/share/doc/texlive-doc/$1";
			}
			# SWITCH my $finaldest = "$basedir/usr/share/texmf/$lfile";
			$opt_debug && print STDERR "NORMAL COPY: $finaldest\n";
			&mkpath(dirname($finaldest));
			mycopy("$Master/$file", $finaldest);
		}
		#
		# if a file name matches a linked script, also create the
		# actual link
		if (defined($TeXLive{'all'}{'linkedscript'}{$file})) {
			unless ($opt_onlyscripts == 1) {
				my $bindest = "$basedir/usr/bin";
				&mkpath($bindest);
				my @foo = split ",", $TeXLive{'all'}{'linkedscript'}{$file};
				for my $i (@foo) {
					# SWITCH symlink("../share/texmf/$lfile", "$bindest/$i") or
					symlink("../share/texlive/$lfile", "$bindest/$i") or
						die "Cannot symlink $bindest/$i -> ../share/texlive/$lfile: $!\n"
				}
			};
		}
		SPECIALS: foreach my $special (@{$TeXLive{'all'}{'special_actions_config'}}) {
			my ($pat, $act) = ($special =~ m/(.*):(.*)/);
			if ($file =~ m|$pat$|) {
				if ($act eq "install-info") {
					push @SpecialActions, "install-info:$file";
				} elsif ( $act eq "install-man") {
					push @SpecialActions, "install-man:$file";
				} else {
					print STDERR "Unknown special action $act, terminating!\n";
					exit 1;
				}
			}
		}
	}
	return(@SpecialActions);
}

#
# make_deb_execute_actions
#
# depends on global variable $globalreclevel
# FIXXME: could be divided in get_execute_actions and
# do_execute_actions, probably needs pass-by-reference if we don't
# want to use global vars.
sub make_deb_execute_actions {
	my ($package) = @_;
    my @Executes = get_all_executes($package,$globalreclevel);
	my @maplines = ();
	my @formatlines = ();
	my @languagelines = ();
	my $gotmapfiles = 0;
	my $firstlang =1;
	my %langhash = ();
	my %formathash = ();
	$opt_debug && print STDERR "Executes= @Executes\n";
	my %Job;
	for my $e (@Executes) {
		my ($what, $first, @rest) = split ' ', $e;
		my $instcmd;
		my $rmcmd;
		if ($what eq 'addMap') {
			push @maplines, "Map $first\n";
		} elsif ($what eq 'addMixedMap') {
			push @maplines, "MixedMap $first\n";
		} elsif ($what eq 'addKanjiMap') {
			push @maplines, "KanjiMap $first\n";
		} elsif ($what eq 'AddFormat') {
			my %r = TeXLive::TLUtils::parse_AddFormat_line(join(" ", $first, @rest));
			if (defined($r{"error"})) {
				die "$r{'error'}, package $package, execute $e";
			}
			my $mode = ($r{"mode"} ? "" : "#! ");
			if (defined($Config{'disabled_formats'}{$package})) {
				next if (ismember($r{'name'}, @{$Config{'disabled_formats'}{$package}}));
			}
			push @formatlines, "$mode$r{'name'} $r{'engine'} $r{'patterns'} $r{'options'}\n";
		} elsif ($what eq 'AddHyphen') {
			my %r = TeXLive::TLUtils::parse_AddHyphen_line(join(" ", $first, @rest));
			my $lline = "name=$r{'name'} file=$r{'file'} patterns=$r{'file_patterns'} lefthyphenmin=$r{'lefthyphenmin'} righthyphenmin=$r{'righthyphenmin'}";
			if (defined($r{'file_exceptions'})) {
				$lline .= " exceptions=$r{'file_exceptions'}";
			}
			my @syns;
			@syns = @{$r{"synonyms"}} if (defined($r{"synonyms"}));
			if ($#syns >= 0) {
				$lline .= " synonyms=" . join(",",@syns);
			}
			push @languagelines, "$lline\n";
		}
	}
	if ($#maplines >= 0) {
		open(OUTFILE, ">$debdest/$package.maps")
			or die("Cannot open $debdest/$package.maps");
		foreach (@maplines) { print OUTFILE; }
		close(OUTFILE);
	}
	if ($#formatlines >= 0) {
		open(OUTFILE, ">$debdest/$package.formats")
			or die("Cannot open $debdest/$package.formats");
		foreach (@formatlines) { print OUTFILE; }
		close(OUTFILE);
	}
	if ($#languagelines >= 0) {
		open(OUTFILE, ">$debdest/$package.hyphens")
			or die("Cannot open $debdest/$package.hyphens");
		foreach (@languagelines) { print OUTFILE; }
		close(OUTFILE);
	}
}

#
# make_deb
#
sub make_deb {
	# my function
	#
	# do_special ($originalfilename, $finaldestinationfilename)
	#
	# Do special actions as specified in the config file, like install info
	# etc
	# real start
	my ($package) = @_;
	my $type_of_package = 'binary';
	if (defined($TeXLive{'mbinary'}{$package})) {
		# this is a meta package!
		$type_of_package = 'mbinary';
	}
	my %lists = %{&get_all_files($package, $globalreclevel)};
	my $title = $TeXLive{$type_of_package}{$package}{'title'};
	my $description = $TeXLive{$type_of_package}{$package}{'description'};
	#eval { mkpath($rundest) };
	if ($@) {
		die "Couldn't create dir: $@";
	}  
	if ($opt_debug) {
		print STDERR "SOURCEFILES: ", @{$lists{'SourceFiles'}}, "\n";
		print STDERR "RUNFILES: ", @{$lists{'RunFiles'}}, "\n";
		print STDERR "DOCFILES: ", @{$lists{'DocFiles'}}, "\n";
		print STDERR "BINFILES: ", @{$lists{'BinFiles'}}, "\n";
	}
	#&mkpath($docdest);
	#
	# DO REMAPPINGS and COPY FILES TO DEST
	#
	my @SpecialActions = make_deb_copy_to_rightplace($package,\%lists);
	#
	# EXECUTE ACTIONS
	#
	make_deb_execute_actions($package);
	#
	# Work on @SpecialActions
	#
	my @infofiles = ();
	my @manfiles = ();
	foreach my $l (@SpecialActions) {
		my ($act, $fname) = ($l =~ m/(.*):(.*)/);
		if ($act eq "install-info") {
			push @infofiles, "$fname";
		} elsif ($act eq "install-man") {
			push @manfiles, $fname;
		} else {
			print STDERR "Unknown action, huuu, where does this come from: $act, exit!\n";
			exit 1;
		}
	}
	if ($#infofiles >=0) {
		open(INFOLIST, ">$debdest/$package.info")
		    or die("Cannot open $debdest/$package.info");
		foreach my $f (@infofiles) {
			print INFOLIST "$f\n";
		}
		close(INFOLIST);
	}
	if ($#manfiles >=0) {
		# that would be nice, but dh_installman is completely 
		# broken, so we have to install the man pages manually
		#open(MANLIST, ">$debdest/$package.manpages")
		#    or die("Cannot open $debdest/$package.manpages");
		#foreach my $f (@manfiles) {
		#	print MANLIST "$f\n";
		#}
		#close(MANLIST);
		for my $f (@manfiles) {
			if ($f =~ m!texmf-dist/doc/man/man(.*)/(.*)$!) {
				mycopy($f, "$debdest/$package/usr/share/man/man$1/$2");
			} else {
				printf STDERR "Unhandled man page: $f\n";
			}
		}
	}
}

#
# make_maintainer
#
# create maintainer scripts. 
# This function uses global vars: $debdest
#
sub make_maintainer {
	sub merge_into {
		my ($source_fname, $target_fhandle) = @_;
		if (-e "$source_fname") {
			open(SOURCE,"<$source_fname")
			    or die("Cannot open $source_fname");
			while (<SOURCE>) { print $target_fhandle $_; }
			close(SOURCE);
		}
	}
	my ($package) = @_;
	print "Making maintainer scripts for $package in $debdest...\n";
	&mkpath($debdest);
	# create debian/maintscript
	if ((-r "$debdest/$package.maintscript.dist") ||
	    ($#{$TeXLive{'binary'}{$package}{'remove_conffile'}} >= 0)) {
		open(MAINTHELP, ">$debdest/$package.maintscript")
			or die("Cannot open $debdest/$package.maintscript for writing");
		merge_into("$debdest/$package.maintscript.dist", MAINTSCRIPT);
		#
		# handling of conffile moves
		if ($#{$TeXLive{'binary'}{$package}{'remove_conffile'}} >= 0) {
			for my $conffile (@{$TeXLive{'binary'}{$package}{'remove_conffile'}}) {
				my $srcpkg = $TeXLive{'binary'}{$package}{'source_package'};
				my $oldversion = $TeXLive{'source'}{$srcpkg}{'old_version'};
				print MAINTHELP "rm_conffile $conffile $oldversion\n";
			}
		}
		close(MAINTHELP);
	}
	for my $type (qw/postinst preinst postrm prerm/) {
		$opt_debug && print STDERR "Handling $type ";
		if ((-r "$debdest/$type.pre") ||
			(-r "$debdest/$type.post") ||
			(-r "$debdest/$package.$type.pre") || 
			(-r "$debdest/$package.$type.post"))
		{
			open(MAINTSCRIPT, ">$debdest/$package.$type")
				or die("Cannot open $debdest/$package.$type for writing");
			print MAINTSCRIPT "#!/bin/sh -e\n";
			merge_into("$debdest/common.functions", MAINTSCRIPT);
			merge_into("$debdest/common.functions.$type", MAINTSCRIPT);
			merge_into("$debdest/$type.pre", MAINTSCRIPT);
			merge_into("$debdest/$package.$type.pre", MAINTSCRIPT);
			#
			# add debhelper stuff and post-parts.
			print MAINTSCRIPT "\n#DEBHELPER#\n";
			merge_into("$debdest/$package.$type.post", MAINTSCRIPT);
			merge_into("$debdest/$type.post", MAINTSCRIPT);
			print MAINTSCRIPT "exit 0\n";
			close MAINTSCRIPT;
		}
		$opt_debug && print STDERR " done.\n";
	}
}


### Local Variables:
### perl-indent-level: 4
### tab-width: 4
### indent-tabs-mode: t
### End:
# vim:set tabstop=4 autoindent: #
