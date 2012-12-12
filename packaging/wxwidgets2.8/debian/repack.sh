#!/bin/sh
# Repackage upstream source to exclude non-distributable files
# should be called as "repack.sh --upstream-source <ver> <downloaded file>
# (for example, via uscan)

set -e
set -u

VER="$2"
FILE="$3"
PKG=`dpkg-parsechangelog|grep ^Source:|sed 's/^Source: //'`

REPACK_DIR="$PKG-$VER.orig" # DevRef § 6.7.8.2

echo -e "\nRepackaging $FILE\n"

DIR=`mktemp -d ./tmpRepackXXXXXX`
trap "rm -rf \"$DIR\"" QUIT INT EXIT

# Create an extra directory to cope with rootless tarballs
UP_BASE="$DIR/unpack"
mkdir "$UP_BASE"
tar xjf "$FILE" -C "$UP_BASE"

if [ `ls -1 "$UP_BASE" | wc -l` -eq 1 ]; then
	# Tarball does contain a root directory
	UP_BASE="$UP_BASE/`ls -1 "$UP_BASE"`"
fi

## Remove stuff
mv $UP_BASE/debian $UP_BASE/debian-upstream
# There are some non-free DLLs under wxPython.  DLLs aren't useful for us
# so just nuke any regardless which protects us from any new DLLs which get
# added by upstream.
find "$UP_BASE" -iname '*.dll' -delete
# We don't use the built-in copy of expat and it contains an ancient copy
# of libtool which lintian warns about, so just delete it.  This also ensures
# that we don't accidentally start building against it in future.  By similar
# logic, remove other embedded copies of libraries we don't want to use, and
# which lintian might warn about in future.
rm -rf "$UP_BASE"/src/expat
rm -rf "$UP_BASE"/src/iodbc
rm -rf "$UP_BASE"/src/jpeg
rm -rf "$UP_BASE"/src/png
rm -rf "$UP_BASE"/src/tiff
rm -rf "$UP_BASE"/src/zlib
## End

mv "$UP_BASE" "$DIR/$REPACK_DIR"

# Using a pipe hides tar errors!
tar cfC "$DIR/repacked.tar" "$DIR" "$REPACK_DIR"
gzip -9 < "$DIR/repacked.tar" > "$DIR/repacked.tar.gz"

FILE=$(echo $FILE | sed 's/bz2/gz/')
mv "$DIR/repacked.tar.gz" "$FILE"

echo "*** $FILE repackaged"
