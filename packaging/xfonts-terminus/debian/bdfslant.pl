#!/usr/bin/perl -w
#
# Convert bdf fonts to slanted style.
# Copyright (C) 1994-95 Cronyx Ltd.
# Author: Serge Vakulenko, <vak@cronyx.ru>
# Changes Copyright (C) 1995-1997 by Andrey A. Chernov, Moscow, Russia.
#
# This software may be used, modified, copied, distributed, and sold,
# in both source and binary form provided that the above copyright
# and these terms are retained. Under no circumstances is the author
# responsible for the proper functioning of this software, nor does
# the author assume any responsibility for damages incurred with its use.
#
# Changes (C) 2000 by Serge Winitzki (version 1.1). Summary of changes:
# 1. operation speeded up by about 2x
# 2. implemented an algorithm to prevent broken lines in slanted chars
# (on by default, "-nofill" to switch it off, -morefill to enable filling broken diagonal blocks)
# 3. introduced variable slant factor (default to 4, "-slant=" option).
# Larger numbers correspond to *smaller* slant angle. Numbers can be non-integer (e.g. "-slant=3.7"). Admissible values are between 1 and 5 (values larger than 5 are quite ugly while slant=1 means an unreadable 45 degrees).
# 4. experimental slanting algorithm to produce smoother shapes ("-unjag" option)
# Warning: it may not give good results on smaller font sizes, so it's off by default.
# 5. improved bounding box generation algorithm (with assistance from A. Chernov)
#

$default_factor = 3.7;

$factor = ("@ARGV" =~ /-slant=([0-9.]+)/) ? $1 : $default_factor;	# Slant factor. Default=3, weakly recommended to be an integer or at least a value not less than 3. Larger values mean smaller slant
$factor = $default_factor if ($factor < 1 or $factor > 5);

$fill = ("@ARGV" =~ /-nofill/) ? 0 : 1;	# Default is to fill holes
$morefill = ("@ARGV" =~ /-morefill/) ? 1 : 0;
$unjag = ("@ARGV" =~ /-unjag/) ? 1 : 0;	# Default is not to correct jag
$need_edit = ($fill or $unjag) ? 1 : 0;

$pattern{"0"} = "0000";
$pattern{"1"} = "0001";
$pattern{"2"} = "0010";
$pattern{"3"} = "0011";
$pattern{"4"} = "0100";
$pattern{"5"} = "0101";
$pattern{"6"} = "0110";
$pattern{"7"} = "0111";
$pattern{"8"} = "1000";
$pattern{"9"} = "1001";
$pattern{"a"} = "1010";         $pattern{"A"} = "1010";
$pattern{"b"} = "1011";         $pattern{"B"} = "1011";
$pattern{"c"} = "1100";         $pattern{"C"} = "1100";
$pattern{"d"} = "1101";         $pattern{"D"} = "1101";
$pattern{"e"} = "1110";         $pattern{"E"} = "1110";
$pattern{"f"} = "1111";         $pattern{"F"} = "1111";
$pattern{"\n"} = "";            $pattern{"\r"} = "";

$hexdig{"0000"} = "0";
$hexdig{"0001"} = "1";
$hexdig{"0010"} = "2";
$hexdig{"0011"} = "3";
$hexdig{"0100"} = "4";
$hexdig{"0101"} = "5";
$hexdig{"0110"} = "6";
$hexdig{"0111"} = "7";
$hexdig{"1000"} = "8";
$hexdig{"1001"} = "9";
$hexdig{"1010"} = "A";
$hexdig{"1011"} = "B";
$hexdig{"1100"} = "C";
$hexdig{"1101"} = "D";
$hexdig{"1110"} = "E";
$hexdig{"1111"} = "F";

$times_font = 0;

while (<STDIN>) {
	if (/^SLANT\s/) {
		if ($times_font) {
			s/"R"/"I"/;
		} else {
			s/"R"/"O"/;
		}
		print;
	} elsif (/^FONT\s/) {
		if (/Times/) {
			s/-R-/-I-/;
			$times_font = 1;
		} else {
			s/-R-/-O-/;
		}
		print;
	} elsif (/^FONTBOUNDINGBOX\s/) {
		($w, $h, $bbx, $bby) = &BBX(split);
		printf "FONTBOUNDINGBOX %d %d %d %d\n", $w, $h, $bbx, $bby;
	} elsif (/^CHARS\s/) {
		print;
		last;
	} else {
		print;
	}
}

while (<STDIN>) {
	if (/^STARTCHAR\s/) {
		print;
#		print STDERR;	# Debugging
	} elsif (/^ENDCHAR/) {
		print;
	} elsif (/^ENCODING\s/) {
		print;
	} elsif (/^SWIDTH\s/) {
		print;
	} elsif (/^DWIDTH\s/) {
		print;
	} elsif (/^BBX\s/) {
		($w, $h, $bbx, $bby) = &BBX(split);
		printf "BBX %d %d %d %d\n", $w, $h, $bbx, $bby;
	} elsif (/^BITMAP/) {
		print "BITMAP\n";
		&makechar;
	}
}
print "ENDFONT\n";

sub BBX {	# Recalculate bounding box
	my ($dummy, $w, $h, $bbx, $bby) = (@_);
	$w += int($h/$factor);
	$bbx += int($bby * 1.5 / $factor - 1);
	($w, $h, $bbx, $bby);
}

sub makechar {
	# This procedure will be called once to read the whole bitmap from STDIN
	# Make a slanted version of the character
	# Use $factor = tangent of the slant angle (measured from the horizontal direction, so infinite $factor is no slant. Old script had $factor=3)
	# Store three consecutive lines but output the first line ($a)
	$a = $b = $c = "";	#"0" x $w;
	# Check for jagging and for line breaks
	# Fill line breaks by putting an extra pixel on the lower line at those locations
	$a_shift_amount =
	$b_shift_amount =
	$c_shift_amount = int($h/$factor);	# This is for the upper line
	for ($i = 0; $i<$h; ++$i) {	# $i is the line number counted from top
	# Use a string instead of an array to represent pixels
	# Currently reading new line $c
	# Precondition: all arrays for previously read lines are filled, modified version of line $a is printed
	# Determine if the stepping changes now. If it does, we may have to do filling
		# Push stack and clear the $c related values
		&rotate_stack;
		if ($b_shift_amount != int(($h-$i)/$factor)) {
			$c_shift_amount= $b_shift_amount - 1;
		}
#			: $b_shift_amount;
		# Now read the new line into $c
		$c = "0" x $c_shift_amount;
		foreach $ch (split(//,<STDIN>)) {
			$c .= $pattern{$ch};
		}
		$c .= "0" x (int($h/$factor)+7);	# Now $c is the current line, already shifted
		# Now we need to perform editing on the stack
		&perform_editing if ($need_edit);
	
	# Now we are ready with the draft of line $a
	&print_line($a) if ($i > 1);	# Avoid printing first 2 dummy lines
	}	# End of the loop over input lines
	# Now need to finish processing lines $b and $c
	# Input a dummy empty line
	&rotate_stack;
	$c = "0" x length($b);
	&perform_editing if ($need_edit);
	&print_line($a) if ($a ne "");
	&print_line($b) if ($b ne "");
}

sub rotate_stack {
	# Shift all stack-related variables
	$a = $b;
	$b = $c;
	$a_shift_amount = $b_shift_amount;
	$b_shift_amount = $c_shift_amount;
}

sub perform_editing {
	# Modify pixels in consecutive lines $a, $b, $c
	# Figure out where the splitting point is
	if ($a_shift_amount != $b_shift_amount) {	# Split between $b and $a
		$loc = 0;	# Loop over locations of "01" in $a
		while ($loc+1 < length($a) and ($loc = index($a, "01",  $loc)) != -1) {
			if ($fill and $loc > 0 and substr($b, $loc-1, 2) eq "10") {	# Situation 1: broken diagonal line
				if (substr($c, $loc-1, 2) eq "00") {
					substr($b, $loc, 1) = "1";
				} elsif ($loc == 1 or substr($b, $loc-2, 1) eq "0") {
					substr($b, $loc-1, 2) = "01";
				} else {
					substr($b, $loc, 1) = "1";
				}
			} elsif ($fill and $morefill and $loc > 1 and substr($b, $loc-2, 3) eq "011") { # Situation 1a: jagged diagonal blocks, a bit ugly
			# Quick fix: don't care about $c
#				print STDERR "1a\n";
				substr($a, $loc, 1) = "1";
			} elsif ($unjag and substr($b, $loc, 1) eq "1" and substr($c, $loc, 2) eq "01" and  ($loc == 0 or substr($b, $loc-1, 1) eq "0" and substr($a, $loc-1, 1) eq "0" and substr($c, $loc-1, 1) eq "0")) {	# Situation 2: left-directed jag
				substr($b, $loc, 2) = "01";
			}	# No more situations
			$loc += 2;
		}	# No more locations of "01" in $a
	} elsif ($c_shift_amount != $b_shift_amount) {	# Split between $b and $c
		$loc = 0;	# Loop over locations of "10" in $c
		while ($loc+1 < length($c) and ($loc = index($c, "10",  $loc)) != -1) {
			if ($unjag and (length($c) == $loc+2 or substr($c, $loc+2, 1) eq "0") and length($a) >= $loc+1 and substr($a, $loc, 1) eq "1" and (length($a) == $loc+2 or substr($a, $loc+2, 1) eq "0") and length($b) >= $loc+2 and substr($b, $loc+1, 1) eq "1" and (length($b) == $loc+2 or substr($b, $loc+2, 1) eq "0")) { # Situation 3: right-directed jag
				substr($b, $loc, 2) = "10";
			} elsif ($fill and (length($a) >= $loc+2 and substr($a, $loc+1, 1) ne "0" or length($a) >= $loc+3 and substr($a, $loc+2, 1) ne "0") and length($b) >= $loc+3 and substr($b, $loc+1, 2) eq "01") {	# Situation 4: broken diagonal line
				if (length($b) > $loc+3 and substr($b, $loc+3, 1) eq "1") {
					substr($b, $loc+1, 1) = "1";
				} else {
					substr($b, $loc+1, 2) = "10";
				}
			} elsif ($fill and $morefill and length($b) >= $loc+3 and substr($b, $loc, 3) eq "011" and (length($c) < $loc+2 or substr($c, $loc+2, 1) eq "0")) {	# Situation 4a: jagged diagonal blocks, a bit ugly
			# Quick fix
				substr($c, $loc+1, 1) = "1";
#				print STDERR "4a\n";
			} # No more situations
			$loc += 2;
		}	# No more locations of "10" in $c
	}	# No splitting points, no editing
}

sub print_line {
	my ($line) = (@_);
	my ($n);
	for ($n = 0; $n<2*int(($w+7)/8); ++$n) {
		print $hexdig{substr($line, $n*4, 4)};
	}
	print "\n";
}
