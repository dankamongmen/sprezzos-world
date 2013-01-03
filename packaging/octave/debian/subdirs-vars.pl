#!/usr/bin/perl -w

### Manipulates the SUBDIRS and INSTALL_SUBDIRS variables. Run like this
### subdirs-vars.pl no-doc
### subdirs-vars.pl only-doc

my $line = "";
my $action = shift @ARGV;

while (<>) {
  $line .= $_;

  ## Line continuation character ("\")
  next if /\\$/;

  if ($line =~ /^((INSTALL_|)SUBDIRS =)/) {
    if ($action eq "no-doc") {
      $line =~ s/doc//;
    } else {
      $line = "$1 doc\n";
    }
  }

  print $line;

  $line = "";
}


