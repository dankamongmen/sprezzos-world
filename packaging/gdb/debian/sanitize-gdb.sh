#!/bin/sh

set -e

tarball=$1
if ! test -f "$tarball"; then
  echo "Could not open $tarball"
  exit 1
fi

tardir=$(dirname "$tarball")
tardir=$(cd "$tardir" && pwd)
version=$(basename "$tarball" | sed "s/^gdb-//; s/\.tar\.bz2\$//")
debversion=${version}
tarball="$tardir"/gdb-$version.tar.bz2
dfsg="$tardir/gdb_$debversion.orig.tar.gz"
doc="$tardir/gdb-doc_$version.orig.tar.gz"

dir=`cd $(dirname "$0") && pwd`

temp=$(mktemp -d)
olddir=`pwd`

cd "$temp"
mkdir src
cd src
tar xjf "$tarball"
cd ..

src=src/gdb-$version
dest=gdb-$debversion
dest2=gdb-doc-$version

if ! test -d "$src"; then
  echo "Could not find source directory $src"
  exit 1
fi

if test -z "$dest" || test -e "$dest"; then
  echo "Could not create dest directory $dest"
  exit 1
fi

if test -z "$dest2" || test -e "$dest2"; then
  echo "Could not create dest directory $dest2"
  exit 1
fi

src=`cd "$src" && pwd`

cp -a "$src" "$dest"
pushd "$dest" > /dev/null

# The GDB manual pages are not covered by the GFDL, but the simulator's
# is.
echo > sim/common/run.1

# Almost all of the texinfo documentation is GFDL.  PSIM's is not, but
# we don't need that manual especially anyway.  Special care must be taken
# with observer.texi, which is necessary for the build process.  Remove
# all pregenerated info files, then replace all texinfo files with dummy
# versions.

rm -f $(find . \( -name \*.info -o -name \*.info-\* \))

for f in $(find . \( -name \*.texinfo -o -name \*.texi \)); do
  if test $(basename $f) = observer.texi; then
    sed -ne '/@c This/,/@c any later/p; /@deftype/p' "$src/$f" > $f
    continue
  fi

  echo > "$f"
done

popd > /dev/null
mkdir "$dest2"
mkdir "$dest2"/readline
cp -a "$src"/readline/doc "$dest2"/readline/doc
mkdir "$dest2"/gdb
cp -a "$src"/gdb/doc "$dest2"/gdb/doc

# Supporting files.
cp -a "$src"/config.guess "$dest2/"
cp -a "$src"/config.sub "$dest2/"
cp -a "$src"/install-sh "$dest2/"
cp -a "$src"/mkinstalldirs "$dest2/"
cp -a "$src"/gdb/version.in "$dest2"/gdb/

tar czf "$dfsg" gdb-$debversion
tar czf "$doc" gdb-doc-$version

cd "$olddir"
rm -rf $temp
