#!/bin/sh

version=1.1.4
base=icedtea-web
pkgdir=$base-$version
origtar=${base}_${version}.orig.tar.gz
icedteaweb_checkout=icedtea-web
debian_checkout=debian-icedtea-web

if [ -d $pkgdir ]; then
    echo directory $pkgdir already exists
    exit 1
fi

if [ -d $pkgdir.orig ]; then
    echo directory $pkgdir.orig already exists
    exit 1
fi

if [ -f $origtar ]; then
    echo "Using existing $origtar"
    tar xf $origtar
    if [ -d $pkgdir.orig ]; then
       mv $pkgdir.orig $pkgdir
    fi
    rm -rf $pkgdir/.hg
else
    echo "Creating new $pkgdir.orig/"
    rm -rf $pkgdir.orig
    mkdir -p $pkgdir.orig
    tar -c -f - -C $icedteaweb_checkout . | tar -x -f - -C $pkgdir.orig
    (
      cd $pkgdir.orig
      #sh autogen.sh
      #rm -rf autom4te.cache
    )
    rm -rf $pkgdir.orig/.hg
    cp -a $pkgdir.orig $pkgdir
fi

echo "Build debian diff in $pkgdir/"
cp -a $debian_checkout $pkgdir/debian
rm -rf $pkgdir/debian/.bzr
(
  cd $pkgdir
  #sh autogen.sh
  #rm -rf autom4te.cache
)
