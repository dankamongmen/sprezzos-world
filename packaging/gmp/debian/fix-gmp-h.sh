#! /bin/sh

# $1 is directory

set -e

test -d "$1" || exit 1

oldfn="$1/gmp.h"
test -f "$oldfn" || exit 2

fixname=`sed -e 's/#include//' debian/gmp.h | $CC -E - | sed -e '/^#/d' -e 's/ "//' -e 's/"//'`
mv $oldfn $1/$fixname
cp debian/gmp.h $oldfn

