#!/usr/bin/perl -w
#
# Description: generating .install files for ocaml binary packages
#
# Copyright © 2009 Stéphane Glondu <steph@glondu.net>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301 USA.
#

#
# This script takes on its standard input a .install file with
# additional lines starting with "STD: ", which denote files that are
# installed in OCaml library directory, and outputs the same file with
# the special lines replaced. Special care is taken with native files:
#
#  - they are not in the output on bytecode-only architectures
#  - for each .cmx, .{o,p.cmx,p.o} is added if it exists
#  - for each .cmxa, .{a,p.cmxa,p.a} is added if it exists
#

my $opt_arch = $ENV{OCAML_OPT_ARCH};
my $ocaml_stdlib_dir = $ENV{OCAML_STDLIB_DIR};
my $destdir = $ENV{DESTDIR};

sub print_if_existing {
    my $name = shift;
    if (-f "${destdir}/${name}") {
        print "${name}\n";
    }
}

while (<>) {
    if (s/^STD: //) {
        my $base;
        my $ext;
        s/\n$//;
        ($base, $ext) = /^(.*)\.([^.]+)$/;
        $prefix = "${ocaml_stdlib_dir}/${base}";
        if ($ext eq "cmx") {
            if ($opt_arch) {
                print "${prefix}.cmx\n";
                print_if_existing("${prefix}.o");
                print_if_existing("${prefix}.p.cmx");
                print_if_existing("${prefix}.p.o");
            }
        } elsif ($ext eq "cmxa") {
            if ($opt_arch) {
                print "${prefix}.cmxa\n";
                print_if_existing("${prefix}.a");
                print_if_existing("${prefix}.p.cmxa");
                print_if_existing("${prefix}.p.a");
            }
        } else {
            print "${prefix}.${ext}\n";
        }
    } else {
        print;
    }
}
