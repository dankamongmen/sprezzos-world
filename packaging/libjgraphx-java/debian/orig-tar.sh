#!/bin/sh -e

# called by uscan with '--upstream-version' <version> <file>
DIR=jgraphx
DIRTARGET=libjgraphx-java-$2
TAR=../libjgraphx-java_$2.orig.tar.gz

# clean up the upstream tarball
unzip $3 
mv $DIR $DIRTARGET
tar -c -z -f $TAR -X debian/orig-tar.exclude $DIRTARGET
rm -rf $DIRTARGET $3

# move to directory 'tarballs'
if [ -r .svn/deb-layout ]; then
    . .svn/deb-layout
    mv $TAR $origDir
    echo "moved $TAR to $origDir"
fi

exit 0
