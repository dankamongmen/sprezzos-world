#!/usr/bin/perl

# These corrections makes index.html from the libgd 2.0.11 source
# pass the weblint check.

my @lines=<>;
my $text = join "", @lines;

# <BODY> must contain all or none of the parameters
$text =~ s|<(body)[^>]*>|<$1>|i;

# <Hn> is of higher order than <A>
$text =~ s|(<a\s[^<]*)(<h3(\s[^>]*)?>)|$2$1|gis;
$text =~ s|(</h3>)([^<]*</a>)|$2$1|gi;

# <A> requires closing </A>
$text =~ s|(<h3[\s>][^<]*<(a)\s[^<]*)(</h3>)|$1</$2>$3|gis;
$text =~ s|(<dt><(a)\sname=[^<]+?(</?code[^<]+)*?)(\n?(</?strong[^<]+)+)(</a>)?([^<]*<dd)|$1</$2>$4$7|gis;

# <A> cannot be nested
$text =~ s|(<a\s[^<]+)(\([^<]*<a\s[^<]+</a>[^<]*\)[^<]*)(</a>)|$1$3$2|gis;

# HREF parameter of <A> must be quoted
$text =~ s|href=([^"][^\s">]*)|href="$1"|gi;

# <> not defining a tag must be escaped
$text =~ s|<([^\s"@<>]+\@[^\s"@<>]+)>|&lt;$1&gt;|g;
$text =~ s|(\s)<(\s)|$1&lt;$2|g;
$text =~ s|([^-]-)>|$1&gt;|g;
$text =~ s|<xxx>|&lt;xxx&gt;|;

# Correct typos...
$text =~ s|(void gdImageJpegCtx[^<]*</A>)|<A NAME="gdImageJpegCtx">$1|;
$text =~ s|</DL>\n(<DT><A NAME="gdFTStringExtra">)|$1|;
$text =~ s|<DL>\n(<DT><A NAME=")gdImageBlue(">)|$1gdImageAlpha$2|;
$text =~ s|(int gdImageBlue[^<]*</A>)|<DT><A NAME="gdImageBlue">$1|;
$text =~ s|VALIGN="TOP >|VALIGN="TOP">|g;
$text =~ s|(<p>\n<H3><A NAME="whatsnew2.0.2">)|</ul>\n$1|gis;

print $text;
