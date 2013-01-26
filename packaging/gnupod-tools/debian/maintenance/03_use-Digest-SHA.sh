#!/bin/sh

dir="$1"
list="configure configure.ac src/ext/Hash58.pm"

for file in $list; do
	if [ ! -e $dir/$file ]; then
		echo "E: File $dir/$file does not exist!"
		exit 1
	fi
	echo "I: Patching File $dir/$file."
	perl -pi -e 's{Digest::SHA1}{Digest::SHA}g;' $dir/$file
done
