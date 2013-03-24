#!/bin/sh -e

# called by uscan with '--upstream-version' <version> <file>
DIR=libjlatexmath-java-$2
TAR=../libjlatexmath-java_$2.orig.tar.gz

# clean up the upstream tarball
unzip $3 
mv jlatexmath-$2 $DIR
tar -c -z -f $TAR $DIR
rm -rf $DIR $3

# move to directory 'tarballs'
if [ -r .svn/deb-layout ]; then
    . .svn/deb-layout
    mv $TAR $origDir
    echo "moved $TAR to $origDir"
fi

exit 0
