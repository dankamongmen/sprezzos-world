#!/bin/sh

set -e

#VERSION=$2
VERSION=$(dpkg-parsechangelog | grep ^Version: | sed -r 's/.*:\ (.*)-.*$$/\1/')
REVNO=$(echo $VERSION | sed -r 's/.*bzr([0-9]+).*/\1/')
PKG=euca2ools
#TAR=../${PKG}_{$VERSION}.orig.tar.gz
TAR=${PKG}_${VERSION}.orig.tar.gz
DIR=$PKG-$VERSION
REPO=$DIR.bzr

bzr branch lp:$PKG $REPO
cd $REPO && bzr export --revision=$REVNO ../$DIR && cd -
tar -c -z -f $TAR --exclude 'man/*' $DIR
rm -rf $DIR $REPO

