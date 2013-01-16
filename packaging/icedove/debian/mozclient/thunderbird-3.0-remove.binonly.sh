#!/bin/sh

# We have a mozilla dir containing mozilla-central, so we must call the main
# remove.binonly.sh in it
SCRIPT=`echo $0 | sed -e 's,/[^/]*-\(remove.binonly.sh\),/\1,'`

echo "\$ cd mozilla" >&2
cd mozilla

echo "\$ sh $SCRIPT" >&2
sh $SCRIPT >> ../REMOVED+nobinonly.txt 2>&1
