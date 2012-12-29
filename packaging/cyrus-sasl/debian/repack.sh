#!/bin/sh
#
# Repackage upstream source to exclude non-distributable files.
# Should be called as "repack sh --upstream-source <version> <file>
# (for example, via uscan).

set -e
set -u

if [ $# -ne 3 ]; then
        echo "Usage: $0 --upstream-source <version> <file>"
        exit 1
fi

OPT_VERSION=$2
OPT_FILE=$3
TMPDIR=`mktemp -d`
trap "rm -rf $TMPDIR" QUIT INT EXIT

echo "Repackaging $OPT_FILE"

orig_file_path=$(readlink --canonicalize $OPT_FILE)
package_name=$(dpkg-parsechangelog | sed -n 's/^Source: //p')
dfsg_directory=${package_name}_${OPT_VERSION}.dfsg1
dfsg_file_path=$(dirname ${orig_file_path})/${dfsg_directory}.orig.tar.gz

zcat "${orig_file_path}" | \
tar --wildcards \
    --delete '*/dlcompat-20010505/*' \
    --delete '*rfc*.txt' \
    --delete '*draft-*.txt' \
    --delete '*~' | \
gzip -c > $dfsg_file_path

echo "File $OPT_FILE repackaged successfully to $dfsg_file_path"
