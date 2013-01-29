#!/bin/sh

libs=`grep libebook.*\.so connectivity/source/drivers/evoab2/EApi.cxx | perl -pe 's/\s+\"(.*)\".*/$1/'`

for l in $libs; do
	if [ -e "/usr/lib/$l" ]; then
		# sanity check: do the libs match with what we would get
		# for our libebook version if we followed the .so symlink?
		l1=`readlink /usr/lib/$l`
		l2_tmp=`echo $l | perl -pe 's/(.*)\.\d+$/$1/'`
		l2=`readlink /usr/lib/$l2_tmp`
		if [ "$l1" = "$l2" ]; then
			dep=`dpkg -S /usr/lib/$l | cut -d: -f1`
		fi
	fi
done

if [ -n "$dep" ]; then
	echo $dep
else
	echo "Cannot find libebook dependency. None of the following libs found:"
	echo $libs
	exit 1
fi

