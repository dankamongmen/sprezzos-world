#!/bin/sh -e

# called by uscan with '--upstream-version' <version> <file>
TAR=openjpeg_$2.orig.tar.gz
DIR=openjpeg-$2

# clean up the upstream tarball
tar -zxf $3
rm -rf $DIR/thirdparty/*
GZIP="--best --no-name" tar -czf $TAR $DIR
rm -rf $DIR $3

# move to directory 'tarballs'
if [ -r .svn/deb-layout ]; then
    . .svn/deb-layout
    mv $TAR $origDir
    echo "moved $TAR to $origDir"
fi
