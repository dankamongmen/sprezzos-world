#!/usr/bin/perl -w

die "Usage: ".$0." <fontname>\n" unless @ARGV;
$setname=$ARGV[0];
$fontname=$ARGV[1];
$ttfdir="/usr/share/fonts/truetype/$setname/";

use SWF qw(:ALL);

$m = new SWF::Movie();

print STDERR "Adding $ttfdir/$fontname\n";
$t = new SWF::Text(2);
$f = new SWF::Font($ttfdir."/".$fontname.".ttf");
$t->setFont($f);
$t->addString($fontname);
$m->add($t);
$m->nextFrame();
print STDERR "Writing $fontname.fft\n";
$m->save($fontname.".fft");
