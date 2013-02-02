#!/bin/bash -e

VERSION=$2
TAR=../zookeeper_$VERSION.orig.tar.gz
NEWTAR=../zookeeper_$VERSION+dfsg.orig.tar.bz2
DIR=zookeeper-$VERSION
mkdir -p $DIR

# Unpack ready fo re-packing
tar -xzf $TAR -C $DIR --strip-components=1

# Repack excluding stuff we don't need
GZIP=--best tar -cjf $NEWTAR --exclude '*.jar' \
         --exclude "Makefile.in" \
         --exclude "aclocal.m4" \
         --exclude "autom4te.cache" \
         --exclude "compile" \
         --exclude "config.guess" \
         --exclude "config.sub" \
         --exclude "configure" \
         --exclude "ltmain.sh" \
         --exclude "zookeeper-${VERSION}/contrib" \
         --exclude "zookeeper-${VERSION}/docs" \
         --exclude "zookeeper-${VERSION}/recipes" \
         --exclude "zookeeper-${VERSION}/dist-maven" \
         --exclude "zookeeper-${VERSION}/src/contrib/fatjar" \
         --exclude "zookeeper-${VERSION}/src/c/generated" \
         --exclude "zookeeper-${VERSION}/src/java/main/org/apache/jute/compiler/generated/*.java" \
         --exclude "zookeeper-${VERSION}/src/java/generated" $DIR

rm -rf $DIR $TAR

