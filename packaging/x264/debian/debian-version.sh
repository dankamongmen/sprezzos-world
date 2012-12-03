#!/bin/sh -e

# mimics upstream's version.h but works inside the debian package
VENDOR=`dpkg-vendor --query vendor`
PACKAGE_VERSION=`dpkg-parsechangelog  | awk '/^Version:/ {print $$2}'`
BUILD=`grep '#define X264_BUILD' < x264.h | sed -e 's/.* \([1-9][0-9]*\).*/\1/'`
POINTVER=`echo $PACKAGE_VERSION | sed -e 's/[0-9]\:\(0\.[0-9]*\.[0-9]*\).*/\1/'`

echo "#define X264_VERSION \" ${VENDOR}_${PACKAGE_VERSION}\"" >> config.h
echo "#define X264_POINTVER \"$POINTVER ${VENDOR}_${PACKAGE_VERSION}\"" >> config.h

