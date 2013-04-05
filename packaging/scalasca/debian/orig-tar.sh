#!/bin/sh -e

# called by uscan with '--upstream-version' <version> <file>
DIR=scalasca-$2
TAR=../scalasca_$2.orig.tar.bz2

# clean up the upstream tarball
tar zxvf $3
rm $DIR/doc/*.pdf
tar -c -j -f $TAR $DIR
rm -rf $DIR $3

# move to directory 'tarballs'
if [ -r .svn/deb-layout ]; then
    . .svn/deb-layout
    mv $TAR $origDir
    echo "moved $TAR to $origDir"
fi

exit 0
