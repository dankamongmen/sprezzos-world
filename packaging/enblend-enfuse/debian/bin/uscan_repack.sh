#! /bin/sh
#
# This script is called as:
# $0 --upstream-version <version> <filename>

echo
set -ex

test $# -eq 3 || exit 255

version="$2"
filename="$3"

toplevel_sourcedir=`tar ztf ${filename} | head -n 1 | sed -e 's,/.*,,g'`

tar zxf ${filename}
rm -f ${filename}

mv $toplevel_sourcedir $toplevel_sourcedir+dfsg

rm -rf $toplevel_sourcedir+dfsg/src/win32helpers
find $toplevel_sourcedir+dfsg -name '*.vcproj' -o -name '*.sln' | xargs rm -f

for patch in debian/patches-repack/*.patch; do
	( cd $toplevel_sourcedir+dfsg; patch -p1 ) < $patch
done

(
	cd $toplevel_sourcedir+dfsg
	aclocal -I m4
	autoheader -f
	automake -acf
	autoconf
	rm -rf autom4te.cache/ config.h.in~
)

tar cf - $toplevel_sourcedir+dfsg | gzip -9 \
	> ../enblend-enfuse_$version+dfsg.orig.tar.gz
rm -rf $toplevel_sourcedir+dfsg

