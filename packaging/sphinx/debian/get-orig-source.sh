#!/bin/sh
set -e
export TAR_OPTIONS='--owner root --group root --mode a+rX'
export GZIP_OPTIONS='-9n'
pwd=$(pwd)
dfsg_version="$1"
if [ -z "$dfsg_version" ]
then
    printf 'Usage: %s <version>\n' "$0"
    exit 1
fi
upstream_version="${dfsg_version%+dfsg*}"
cd "$(dirname "$0")/../"
tmpdir=$(mktemp -t -d get-orig-source.XXXXXX)
uscan --noconf --force-download --rename --download-version="$upstream_version" --destdir="$tmpdir"
cd "$tmpdir"
tar -xzf sphinx_*.orig.tar.gz
rm *.tar.gz
# Remove JavaScript code without source:
rm Sphinx-*/sphinx/themes/basic/static/jquery.js
rm Sphinx-*/sphinx/themes/basic/static/underscore.js
mv Sphinx-*/ "sphinx-${dfsg_version}.orig"
tar -czf "$pwd/sphinx_${dfsg_version}.orig.tar.gz" sphinx-*.orig/
cd ..
rm -Rf "$tmpdir"

# vim:ts=4 sw=4 et
