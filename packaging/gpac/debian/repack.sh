#!/bin/sh

# see the repack.stub for how to use

# TODO: provide example watch files and repack.locals
# TODO: test suite. problems fixed that need to be tested:
# * globbing
# * whitespace and then comments in the MANIFEST
# TODO: does / in weird places work? test suite too.
# TODO: I actually broke stuff with the MANIFEST change not thinking..
# TODO: allow for a sepearate (and multiple) MANIFEST files, then
# de-uglify libsyntax-highlight-engine-kate-perl.
# TODO: have each mv and rm check that something actually changed, and
# if not, die

set -e
set -u

usage() {
    echo "Usage: repack.sh --upstream-version <ver> <downloaded file>"
    exit 1
}

if [ "$#" != "3" ]; then
    usage
fi
if [ "$1" != "--upstream-version" ]; then
    usage
fi
if [ ! -f "$3" ]; then
    if [ -n "$3" ]; then
        echo "$3 doesn't exist"
    fi
    usage
fi
VER="$2"
FILE="$3"
PKG=`dpkg-parsechangelog|grep ^Source:|sed 's/^Source: //'`

SUFFIX="+dfsg"

echo
echo "Repackaging $FILE"
echo

DIR=`mktemp -d ./tmpRepackXXXXXX`
DIR=$(readlink -f "$DIR")
trap "/bin/rm -rf \"$DIR\"" QUIT INT EXIT

# Create an extra directory to cope with rootless tarballs
UP_BASE="$DIR/unpack"
mkdir "$UP_BASE"
tar xf "$FILE" -C "$UP_BASE" || unzip "$FILE" -d "$UP_BASE"

if [ `ls -1 "$UP_BASE" | wc -l` -eq 1 ]; then
	# Tarball does contain a root directory
	UP_BASE="$UP_BASE/`ls -1 "$UP_BASE"`"
fi

RM_OPTS="-vrf"

real_rm(){
    /bin/rm "$@"
}

real_mv(){
    /bin/mv "$@"
}

rm(){
    set +f
    MYOLDPWD=$(pwd)
    cd "$UP_BASE"
    if [ "$MANIFEST" = "1" ]; then
        PERM=$(stat --format=%a "MANIFEST")
        chmod u+w "MANIFEST"
    fi
    for i in $@; do
        if [ "$MANIFEST" = "1" ]; then
            PATTERN="^$i"
            if [ -d "$i" ]; then
                if ! { echo "$PATTERN" | grep -q "/$" ; }; then
                    PATTERN="${PATTERN}/"
                fi
            else
                PATTERN="${PATTERN}\s?"
            fi
            grep -Ev "$PATTERN" "MANIFEST" > "$DIR/MANIFEST"
            real_mv "$DIR/MANIFEST" "MANIFEST"
        fi
        real_rm "$RM_OPTS" "$i"
    done
    if [ "$MANIFEST" = "1" ]; then
        chmod $PERM "MANIFEST"
    fi
    cd $MYOLDPWD
    set -f
}

mv(){
    set +f
    OLD=$(pwd)
    cd $UP_BASE
    real_mv "$@"
    cd $OLD
    if [ "$MANIFEST" = "1" ]; then
        echo "MANIFEST cannot be manipulated with mv yet, patches welcome"
        exit 1
    fi
    set -f
}

# bump with incompatible changes
REPACK_VERSION=3

requires_version(){
    if [ $REPACK_VERSION -lt $1 ]; then
        echo "repack.sh is not up to date enough for this package. you need at least version $1, while this script is only version $REPACK_VERSION"
        exit 1
    fi
}

MANIFEST=0
## Remove stuff
set -f
MYORIGPWD=$(pwd)
cd "$UP_BASE"
. "$MYORIGPWD/debian/repack.local"
cd $MYORIGPWD
set +f
## End

REPACK_DIR="$PKG-${VER}${SUFFIX}.orig" # DevRef § 6.7.8.2
DFSG_TAR="$(dirname $FILE)/${PKG}_${VER}${SUFFIX}.orig.tar"

real_mv "$UP_BASE" "$DIR/$REPACK_DIR"

# .gz or .bz2?
FILETYPE=$(file --brief --mime-type --dereference "$FILE")
case "$FILETYPE" in
    application/x-gzip|application/zip)
        C_PROGRAM="gzip"
        C_SUFFIX="gz"
        ;;
    application/x-bzip2)
        C_PROGRAM="bzip2"
        C_SUFFIX="bz2"
        ;;
    *)
        echo "E: Unknown filetye $FILETYPE"
        exit 1
        ;;
esac
    
# Using a pipe hides tar errors!
tar cfC "$DIR/repacked.tar" "$DIR" "$REPACK_DIR"
$C_PROGRAM -9 < "$DIR/repacked.tar" > "$DIR/repacked.tar.$C_SUFFIX"

real_mv "$DIR/repacked.tar.$C_SUFFIX" "$DFSG_TAR.$C_SUFFIX"

echo "*** $DFSG_TAR.$C_SUFFIX ready"
