#!/bin/sh

rm -f mpfr.texi fdl.texi
grep -v texi Makefile.am > Makefile.am.free && mv Makefile.am.free Makefile.am
autoreconf
