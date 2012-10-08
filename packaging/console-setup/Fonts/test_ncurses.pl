#!/usr/bin/perl

use strict;
use warnings 'all';
use Curses;

initscr;

my @syms = (ACS_BLOCK, ACS_BOARD, ACS_BTEE, ACS_BULLET, ACS_CKBOARD,
            ACS_DARROW, ACS_DEGREE, ACS_DIAMOND, ACS_HLINE,
            ACS_LANTERN, ACS_LARROW, ACS_LLCORNER, ACS_LRCORNER,
            ACS_LTEE, ACS_PLMINUS, ACS_PLUS, ACS_RARROW, ACS_RTEE,
            ACS_S1, ACS_S9, ACS_TTEE, ACS_UARROW, ACS_ULCORNER,
            ACS_URCORNER, ACS_VLINE);

my @symnames = ('ACS_BLOCK', 'ACS_BOARD', 'ACS_BTEE', 'ACS_BULLET',
                'ACS_CKBOARD', 'ACS_DARROW', 'ACS_DEGREE',
                'ACS_DIAMOND', 'ACS_HLINE', 'ACS_LANTERN',
                'ACS_LARROW', 'ACS_LLCORNER', 'ACS_LRCORNER',
                'ACS_LTEE', 'ACS_PLMINUS', 'ACS_PLUS', 'ACS_RARROW',
                'ACS_RTEE', 'ACS_S1', 'ACS_S9', 'ACS_TTEE',
                'ACS_UARROW', 'ACS_ULCORNER', 'ACS_URCORNER',
                'ACS_VLINE');

for my $i (0..$#syms) {
    addstr(sprintf("\n%-15s ", $symnames[$i]));
    echochar($syms[$i]);
}
my $c = getch();

endwin;
