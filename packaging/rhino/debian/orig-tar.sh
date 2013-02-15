#!/bin/sh -e

# called by uscan with '--upstream-version' <version> <file>
DIR=rhino-$2
TAR=rhino_$2.orig.tar.gz

# Repack upstream source to tar.gz
unzip $3
mv rhino* $DIR
GZIP=--best tar czf $TAR -X debian/orig-tar.exclude $DIR
rm -rf $DIR

