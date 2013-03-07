#!/bin/sh

set -e

SEQANDIR=seqan-$2
SEQANFILE=seqan_${2}.orig.tar.xz

# called by uscan with '--upstream-version' <version> <file>
mkdir $SEQANDIR
echo $3

unzip -q $3 # -d $SEQANDIR
rm -rf $SEQANDIR/lib/samtools
tar cJf $SEQANFILE $SEQANDIR
rm -rf $SEQANDIR
rm -f $3

# move to directory 'tarballs'
if [ -r .svn/deb-layout ]; then
  . .svn/deb-layout
else
  origDir=../tarballs
fi
mkdir -p $origDir
mv $SEQANFILE $origDir
echo "moved $SEQANFILE to $origDir"

exit 0
