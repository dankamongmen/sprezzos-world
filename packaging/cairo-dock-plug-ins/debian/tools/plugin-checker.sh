#!/bin/sh

for f in `ls $1` ; do

	if [ -d $1/$f ] ; then
		bn=$(basename $f)
		if [ $bn != "po" -a $bn != "debian" ] ; then
			grep $bn $1/debian/tools/plugins-list  > /dev/null
			if [ $? -ne 0 ] ; then
				echo $bn 
			fi
		fi
	fi
done
