#!/usr/bin/perl
# $Id: check_versions.pl 191 2010-06-30 08:42:54Z tg $

use Getopt::Std;
%options=();
getopts("vus",\%options);

sub main::HELP_MESSAGE {
  print STDERR << "EOF";
This script tests latest file version from given svn url
Use: check_version.pl -v -u -s /path/to/svn-urls
  Option -u forces update of each file
  Option -v turns verbose mode on
  Option -s skips svn revision check and other files check
EOF
  exit ;
}

my @new;
my @added;
my @refused;
my @exttops;
my $file = shift;
my $redirect = ">/dev/null 2>&1" unless defined $options{v} ;
my %allfiles;
my %listdirs;

open (F, $file);
while (<F>) {
  next if (/^\s*(#.*)*$/);
  /^(.+)\s-\s(.+)\s-\s(.+)\s-\s(.+)\s-\s(.+)\s-\s(-?\d+)/;
  $allfiles{$1} = 1;
}
close(F);
open (F, $file);

sub latest_revision {
  my $url = shift ;
  my $latest_revision = `LC_ALL=C svn info $url | LC_ALL=C sed -n '/^Last Changed Rev: /s///p'`;
  $latest_revision =~ s/[\r\n\/]+$//;
  return $latest_revision ;
}

my $latest_package = "base" ;
my $latest_basedir = "usr/share" ;
my $latest_link    = "no" ;
my $latest_extname = "";

sub add_other {
  my ($base,$files) = @_;
  my @lines = split("\n",$files);
  my %is_refused;
  for (@refused) { $is_refused{$_} = 1; }
  my %is_added;
  my @blacklist = qw/COPYING/;
  for (@added) { $is_added{$_} = 1; }
  print "Checking other files in the same directory...\n" ;
  foreach $new_file (@lines) {
    my $exists = $allfiles{"$base/$new_file"};
    my @filtered = grep(/$new_file/,@blacklist);
    print "Checking: $new_file\n";
    my $size = @filtered;
    print "Filtered\n" unless $size == 0;
    print "Already refused\n" unless $is_refused{"$base/$new_file"} != 1;
    print "Already present\n" unless "$exists" eq "";
    if ("$exists" eq "" && $is_refused{"$base/$new_file"} != 1 
          && $is_added{"$base/$new_file"} != 1
	  && $size == 0) {
      print "Add $new_file ? (y/n) (default: y)\n";
      my $add = <> ;
      $add =~ s/[\r\n]+$//;
      if ($add eq "") {
        print "Using default (y)\n";
        $add = "y" ;
      }
      if ("$add" eq "y") {
        print "Name ? ($latest_extname)\n" ;
        my $extname = <> ;
        $extname =~ s/[\r\n]+$//;
        if ($extname eq "") {
          print "Using default ($latest_extname)\n";
          $extname = $latest_extname ;
        } else {
          $latest_extname = $extname ;
        }
        print "Package ? ($latest_package)\n" ;
        my $package = <> ;
        $package =~ s/[\r\n]+$//;
        if ($package eq "") {
          print "Using default ($latest_package)\n";
          $package = $latest_package ;
        } else {
          $latest_package = $package ;
        }
        print "Basedir ? ($latest_basedir)\n" ;
        my $basedir = <> ;
	$basedir =~ s/[\r\n]+$//;
        if ($basedir eq "") {
          print "Using default ($latest_basedir)\n";
          $basedir = $latest_basedir ;
        } else {
          $latest_basedir = $basedir ;
        }
        print "Link ? (yes|no) ($latest_link)\n" ;
        my $link = <> ;
        $link =~ s/[\r\n]+$//;
        if ($link eq "") {
          print "Using default ($latest_link)\n";
          $link = $latest_link ;
        } else {
          $latest_link = $link ;
        }
	my $revision = latest_revision("$base/$new_file");
	push @new,"$base/$new_file - $extname - $package - $basedir - $link - $revision\n" ;
	push @added,"$base/$new_file" ;
	download_latest ("$base/$new_file",$extname,$basedir,$package,$link,$revision) ;
      } else {
	push @new,"$base/$new_file - none - none - none - no - -1\n" ;
        push @refused,"$base/$new_file";
	print "Not adding file..\n";
      }
   }
 }
}

sub get_extdir  {
  my ($package,$basedir,$extname) = @_ ;
  $basedir = "$basedir/mediawiki-extensions/$package" ;
  if ("$package" eq "base") {
    if ("$extname" ne "extensions") {
      $basedir = "$basedir/$extname" ;
    }
  }
  $basedir
}

sub download_latest {
  my ($url,$extname,$basedir,$package,$link,$old) = @_;
  print "Downloading file\n\n";
  $url =~ /^http:\/\/.*\/(.*)$/;
  my $file = $1;
  $basedir = get_extdir($package,$basedir,$extname) ;
  my $extdir = "dist/mediawiki-extensions-$package/$basedir" ;
  system("mkdir -p $extdir $redirect");
  system("cd $extdir && LC_ALL=C svn export --force $url $redirect");
}

my $tmpdir = `mktemp -d` ;
$tmpdir =~ s/[\r\n]+$//;

while (<F>) {
  print "Processing line: $_" if defined $options{v};
  next if (/^\s*(#.*)*$/);
  /^(.+)\s-\s(.+)\s-\s(.+)\s-\s(.+)\s-\s(.+)\s-\s(-?\d+)/;
  my ($url,$extname,$package,$basedir,$link,$revision) = ($1,$2,$3,$4,$5,$6);
  my $latest_revision = -1;
  if ($revision != -1) {
    $revision = 0 if defined $options{u};
    $latest_revision = (defined $options{s}) ? $revision : latest_revision($url) ;
  }
  if ($latest_revision eq "") {
    print "Warning: cannot get revision for $url\n";
    $latest_revision = $revision;
  }
  my $dirname = `dirname $url` ;
  $dirname =~ s/[\n\r]+$//;
  if ((!defined $options{s}) && (!defined $listdirs{$dirname})) {
    # Avoid listing files in extensions/
    my $dir = `basename $dirname` ;
    $dir =~ s/[\n\r\/]+$//;
    if ("$dir" ne "extensions") {
      my $list = `LC_ALL=C svn list $dirname`;
      add_other($dirname,$list);
    }
    $listdirs{$dirname} = 1;
  }
  push @new,"$url - $extname - $package - $basedir - $link - $latest_revision\n" ;
  print "$url:\noutdated: local revison = $revision, newest = $latest_revision\n" and
  download_latest($url,$extname,$basedir,$package,$link,$revision) 
    unless (($latest_revision == $revision));
  print "$url:\nup to date at revision $latest_revision\n\n" ;
  if ("$link" eq "yes") {
    my $extdir = get_extdir($package,$basedir,$extname) ;
    system("touch $tmpdir/mediawiki-extensions-$package.links") ;
    if ($extname ne "extensions") {
      if (grep(/$extname/,@exttops)) {
        print "link to $extname in /var/lib/mediawiki/extensions already set\n" if defined $options{v} ;
      } else {
        system("echo \"$extdir var/lib/mediawiki/extensions/$extname\" >> $tmpdir/mediawiki-extensions-$package.links") ;
        push @exttops,$extname ;
      }
    }
    my $file = `basename $url` ;
    $file =~ s/[\n\r\/]+$// ;
    system("echo \"$extdir/$file etc/mediawiki-extensions/extensions-available/$file\" >> $tmpdir/mediawiki-extensions-$package.links") ;
  }
}

close(F);
open (F,">$file");
print F "# svn-revisions, generated by check_version.pl\n";
print F "# Format is: URL - name - package - basedir - link - revision\n";
foreach (@new) {
  print F $_;
}
close(F);

system("for extrafile in extra/*; do cat \$extrafile >>$tmpdir/\$(basename \$extrafile); done");
system("cp $tmpdir/* debian $redirect") ;
system("rm -rf $tmpdir \$(cat killfile) $redirect");
