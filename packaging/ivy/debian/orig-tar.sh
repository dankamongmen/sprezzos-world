#!/bin/sh -e

# called by uscan with '--upstream-version' <version> <file>
DIR=ivy-$2.orig

# clean up the upstream tarball
tar -z -x -f $3
mv apache-ivy* $DIR
tar -c -z -f $3 --exclude '*/test*' --exclude '*/doc*' $DIR
rm -rf $DIR

# move to directory 'tarballs'
if [ -r .svn/deb-layout ]; then
  . .svn/deb-layout
  mv $3 $origDir
  echo "moved $3 to $origDir"
fi
