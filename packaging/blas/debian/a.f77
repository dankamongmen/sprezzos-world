#!/bin/bash

set -e

OPTS=""
FILE=""
while [ $# -gt 0 ] ; do
	if echo $1 | grep '\.f$' >/dev/null ; then
		FILE=$1
	else
		OPTS="$OPTS $1"
	fi
	shift
done

if [ "$FILE" != "" ] && grep -i abs1 $FILE >/dev/null ; then

	echo f2c $FILE
	f2c $FILE
	echo cc $OPTS $(echo $FILE | sed 's,\.f,.c,1')
	cc $OPTS $(echo $FILE | sed 's,\.f,.c,1')
	rm $(echo $FILE | sed 's,\.f,.c,1')

else

	echo f77 $OPTS $FILE
	f77 $OPTS $FILE

fi

exit 0
