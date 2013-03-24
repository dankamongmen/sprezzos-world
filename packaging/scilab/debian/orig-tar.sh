#!/bin/sh -e

# called by uscan with '--upstream-version' <version> <file>

# move to directory 'tarballs'
if [ -r .svn/deb-layout ]; then
  . .svn/deb-layout
  mv $3 $origDir
  echo "moved $3 to $origDir"
fi
