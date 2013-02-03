#! /bin/sh -e
#
# Configuration build script: ggz-*
# build-depends: automake1.7, gettext, libtool, autoconf2.50
#
autogen () {
    aclocal
    grep -q ^AM_GNU_GETTEXT   configure.ac && gettextize -c -f
    grep -q ^AM_PROG_LIBTOOL  configure.ac && libtoolize -c -f --automake
    grep -q ^A._CONFIG_HEADER configure.ac && autoheader -f
    automake -acf --foreign
    autoconf -f
}

autogen

