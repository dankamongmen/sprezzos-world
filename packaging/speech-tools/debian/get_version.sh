#!/bin/sh
# Written by: Kumar Appaiah <akumar@ee.iitm.ac.in>
# This scripts extracts the version of the package for the soname to
# be fixed.

FULLVER=`dpkg-parsechangelog |awk '/^Version/ { sub(/^1:/, "", $2); sub(/~.*$/, "", $2);print $2}'`
MAJORVER=`echo $FULLVER|sed 's/^\([^\.]\+\.[^\.]\)\.[^\.]\+$/\1/'`
echo $FULLVER $MAJORVER
