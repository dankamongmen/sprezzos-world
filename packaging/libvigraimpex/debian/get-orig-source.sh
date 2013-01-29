#!/bin/sh
set -e
export GZIP="-9 -n"
export TAR_OPTIONS="--owner root --group root --mode a+rX"
pwd=`pwd`
dfsgversion="$1"
version="${1%+dfsg*}"
if [ -z "$version" ]
then
    printf 'Usage: %s <version>\n' "$0"
    exit 1
fi
cd "`dirname "$0"`/../"
tmpdir=`mktemp -d get-orig-source.XXXXXX`
uscan --noconf --force-download --rename --download-version="$version" --destdir="$tmpdir"
cd "$tmpdir"
tar -xzf libvigraimpex_*.orig.tar.gz
rm *.tar.gz
# Remove documentation without source:
find vigra-* -name '*.ps' -delete
# Remove not-for-us stuff:
rm -v vigra-*/*win32*.zip
# Remove documentation that we'll rebuild anyway:
rm -v vigra-*/doc/vigra/*.html
rm -v vigra-*/doc/vigra/*.png
rm -rv vigra-*/doc/vigranumpy/
rm -rv vigra-*/vigranumpy/docsrc/_static/
# Remove Mercurial artifacts:
rm -v vigra-*/.hg*
mv vigra-*/ "vigra-${version%+dfsg*}.orig"
tar -czf "$pwd/libvigraimpex_$dfsgversion.orig.tar.gz" vigra-*.orig/
cd ..
rm -Rf "$tmpdir"

# vim:ts=4 sw=4 et
