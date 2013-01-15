#!/bin/sh -e

# called by uscan with '--upstream-version' <version> <file>
DIR=lapack-$2
TAR=../lapack_$2.orig.tar.bz2

# clean up the upstream tarball
tar -z -x -f $3
rm $3
(cd $DIR
mv TESTING testing; ln -s testing TESTING
mv SRC src; ln -s src SRC
mv INSTALL install; ln -s install INSTALL
mv BLAS blas; ln -s blas BLAS
)
echo tar -j -c -f $TAR $DIR
tar -j -c -f $TAR $DIR
rm -rf $DIR

# move to directory 'tarballs'
if [ -r .svn/deb-layout ]; then
    . .svn/deb-layout
    mv $TAR $origDir
    echo "moved $TAR to $origDir"
fi

exit 0
