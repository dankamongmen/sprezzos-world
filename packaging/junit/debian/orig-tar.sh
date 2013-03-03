#!/bin/sh -e

# called by uscan with '--upstream-version' <version> <file>
DIR=junit-$2.orig
TAR=../junit_$2.orig.tar.gz

# clean up the upstream tarball
unzip $3
rm -f $3
mv junit$2 $DIR
GZIP=--best tar czf $TAR --exclude docs/api --exclude junit.jar --exclude *.class $DIR
rm -rf $DIR

exit 0
