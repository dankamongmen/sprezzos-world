#!/usr/bin/perl -w

my $flag = 0;

while (<>) {
  s {"/.*/}{"} if /^\.TH/;
  if (/^\.SH NAME/) {
    $flag = 1;
  } elsif ($flag) {
    m {^(.*) \\-} if not m {^/.*/([^/\\]+) };
    $_ = "libmtp \\- $1\n";
    $flag = 0;
  }
  ## Escape minus signs before "-1" as return value
  s/-1 /\\-1 / if not /-1c/ and /-1/;
  print;
}
