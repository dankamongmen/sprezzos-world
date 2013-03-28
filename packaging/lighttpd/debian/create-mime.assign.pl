#!/usr/bin/perl -w
use strict;
open MIMETYPES, "/etc/mime.types" or exit;
print "mimetype.assign = (\n";
my %extensions;
while(<MIMETYPES>) {
  chomp;
  s/\#.*//;
  next if /^\w*$/;
  if(/^([a-z0-9\/+-.]+)\s+((?:[a-z0-9.+-]+[ ]?)+)$/) {
    foreach(split / /, $2) {
      # mime.types can have same extension for different
      # mime types
      next if $extensions{$_};
      $extensions{$_} = 1;
      print "\".$_\" => \"$1\",\n";
    }
  }
}
print ")\n";
