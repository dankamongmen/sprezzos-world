#!/bin/sh

# called by uscan with '--upstream-version' <version> <file>
DIR=clang-*.src
DIRIN=clang-$2
TAR=../clang_$2.orig.tar.bz2

# clean up the upstream tarball
tar zxvf $3
mkdir -p $DIRIN/tools/clang
mv $DIR/* $DIRIN/tools/clang/
tar -c -j -f $TAR $DIRIN
rm -rf $DIRIN $DIR

# move to directory 'tarballs'
if [ -r .svn/deb-layout ]; then
    . .svn/deb-layout
    mv $TAR $origDir
    echo "moved $TAR to $origDir"
fi

exit 0

