#!/usr/bin/perl

($fontdir, $linkdir, $scale, $hints) = @ARGV;

open (HINTS, "< $hints");
undef $/;
@dir = ();
map {
  m{/([^/]+)\.pfb};
  $file = $1;
  if (/X-FontName = (.*)/) {
    $xfnt = $1;
    push @dir, "$file.pfb\t$xfnt";
    $xfnt =~ s/silipa-1/adobe-fontspecific/;
    push @dir, "$file.pfb\t$xfnt";
  }
  foreach $ext ("pfb", "afm") {
    system ("ln -fs $fontdir/$file.$ext $linkdir/$file.$ext\n");
  }
} split ("end\nbegin", <HINTS>);
close HINTS;

open (SCALE, ">$scale");
print SCALE scalar @dir, "\n", join ("\n", @dir);
close SCALE;
