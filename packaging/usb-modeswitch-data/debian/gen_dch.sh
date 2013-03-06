#!/bin/bash

set -e

# Commit identifiant
cid=$1

attrs_line=0

echo "    + New devices"
git show $cid *-usb_modeswitch.rules | egrep -B1 -e '^\+ATTRS{idVendor}' | while read line
do
	
	if [ $attrs_line -eq 0 ]
	then
		attrs_line=1
		# Line 0 is the Description
		DESC=`echo $line | sed -e 's/^.*# \(.*\)$/\1/'`
	elif [ $attrs_line -eq 1 ]
	then
		attrs_line=2
		# Line 1 is the Attributes
		ATTRS=`echo $line | sed -e 's/^.*ATTRS{idVendor}=="\(.*\)".*ATTRS{idProduct}=="\(.*\)",.*$/\[\1:\2\]/'`
		echo "     $ATTRS $DESC"
	else
		attrs_line=0
		# Line 2 is the context
	fi
done

