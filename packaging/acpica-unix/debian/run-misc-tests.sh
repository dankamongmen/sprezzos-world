#!/bin/bash
#
#       run the misc tests: we need to do this in a script since
#       these are expected to fail which would normally cause %check
#       to stop.  however, this is expected behavior.  we are running
#       iasl precisely because we expect it to stop when presented with
#       faulty ASL.
#
#       this script assumes it is in the source 'tests' directory at
#       start.
#

set -x

CURDIR="$1"
VERSION="$2"
DEBDIR=$CURDIR/debian
TSTDIR=$CURDIR/tests/misc

m=`uname -m`
case $m in
    *64) BITS=64
         ;;
    *)   BITS=32
         ;;
esac

BINDIR=$CURDIR/generate/unix/bin${BITS}

# create files to compare against
$BINDIR/iasl --help
WHEN=`date +"%b %_d %Y"`
sed -e "s/XXXXXXXXXXX/$WHEN/" \
    -e "s/YYYY/$BITS/" \
    -e "s/VVVVVVVV/$VERSION/" \
    $DEBDIR/badcode.asl.result > $TSTDIR/badcode.asl.result
sed -e "s/XXXXXXXXXXX/$WHEN/" \
    -e "s/YYYY/$BITS/" \
    -e "s/VVVVVVVV/$VERSION/" \
    $DEBDIR/grammar.asl.result > $TSTDIR/grammar.asl.result

# run the tests
cd $TSTDIR

# see if badcode.asl failed as expected
$BINDIR/iasl badcode.asl > badcode 2>&1
diff badcode badcode.asl.result >/dev/null 2>&1
[ $? -eq 0 ] || exit 1

# see if grammar.asl failed as expected
$BINDIR/iasl -f -of grammar.asl > grammar 2>&1
diff grammar grammar.asl.result >/dev/null 2>&1
[ $? -eq 0 ] || exit 1

exit 0
