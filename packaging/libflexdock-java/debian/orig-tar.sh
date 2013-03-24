#!/bin/sh -e

# called by uscan with '--upstream-version' <version> <file>

VERSION=$(dpkg-parsechangelog | sed -ne 's,^Version: \(.*\)-.*,\1,p')
SOURCE=$(dpkg-parsechangelog | sed -ne 's,Source: \(.*\),\1,p')

DIR=flexdock-${VERSION}
TAR=../${SOURCE}_${VERSION}.orig.tar.gz

# clean up the upstream tarball
# Export to the $DIR directory since there is no directory in
# the archive
tar zxvf $3
tar -c -z -f $TAR -X debian/orig-tar.exclude $DIR
rm -rf $DIR

# move to directory 'tarballs'
if [ -r .svn/deb-layout ]; then
    . .svn/deb-layout
    mv $TAR $origDir
    echo "moved $TAR to $origDir"
fi

exit 0
