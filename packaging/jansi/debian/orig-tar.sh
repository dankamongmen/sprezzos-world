#!/bin/sh 

set -e

# called by uscan with '--upstream-version' <version> <file>
echo "version $2"
package=`dpkg-parsechangelog | sed -n 's/^Source: //p'`
version=$2
tarball=$3
REPO="git://forge.fusesource.com/jansi.git"
TAR=${package}_${version}.orig.tar.gz
DIR=${package}-${version}.orig

git clone ${REPO} ${DIR}
cd ${DIR} && git checkout ${package}-${version} && cd ..
GZIP=--best tar --exclude=.git --numeric --group 0 --owner 0 -cvzf ${TAR} ${DIR}
rm -rf $tarball $DIR
