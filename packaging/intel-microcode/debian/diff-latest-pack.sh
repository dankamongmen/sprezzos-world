#!/bin/sh
#
# Generate an unified diff of the list of microcodes included in the
# latest two Intel-supplied Microcode packs (microcode-<date>.dat)
#
# Use this to locate updated/removed/added microcodes.  Further manual
# analysis is necessary to identify the deletion of recently-added
# or recently-updated microcode, and the addition of previously-deleted
# microcode (often with a version downgrade).
#
# Run this from the directory with the Intel microcode packs.

export LC_COLLATE=C
PATH=${PATH}:/sbin:/usr/sbin

FILES=$(ls -1 microcode-*.dat | tail -n 2 | xargs)

DFILES=
for i in $FILES ; do
	fn=$(basename $i)
	iucode_tool -q -l $i | sed -e 's/^[^:]\+: //' | sort | grep sig > /tmp/$fn.list
	DFILES="${DFILES} /tmp/$fn.list"
done

diff --unified=1 ${DFILES} | grep sig | sort -k 2
