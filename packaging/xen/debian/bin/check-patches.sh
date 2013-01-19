#!/bin/sh -e

TMPDIR=$(mktemp -d)
trap "rm -rf $TMPDIR" EXIT
grep -v "^#"  debian/patches/series | awk '{if (NF == 1) print "debian/patches/" $1}' | sort -u > $TMPDIR/used
find debian/patches -type f -name "*.diff" -printf "%p\n" | sort > $TMPDIR/avail
echo "Used patches"
echo "=============="
cat $TMPDIR/used
echo
echo "Unused patches"
echo "=============="
fgrep -v -f $TMPDIR/used $TMPDIR/avail
