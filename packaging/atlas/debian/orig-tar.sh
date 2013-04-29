#!/bin/sh -e

# called by uscan with '--upstream-version' <version> <file>
DIR=ATLAS
DIRTARGET=atlas-$2
TAR=../atlas_$2.orig.tar.gz

# clean up the upstream tarball
tar zxvf $3
mv $DIR $DIRTARGET
# Before
#  cvs -d:pserver:anonymous@math-atlas.cvs.sourceforge.net:/cvsroot/math-atlas login 
cvs -z3 -d:pserver:anonymous@math-atlas.cvs.sourceforge.net:/cvsroot/math-atlas co -P AtlasBase
mv AtlasBase/TexDoc/ $DIRTARGET
rm -rf AtlasBase
tar -c -z -f $TAR -X debian/orig-tar.exclude $DIRTARGET
rm -rf $DIRTARGET

# move to directory 'tarballs'
if [ -r .svn/deb-layout ]; then
    . .svn/deb-layout
    mv $TAR $origDir
    echo "moved $TAR to $origDir"
fi

exit 0
