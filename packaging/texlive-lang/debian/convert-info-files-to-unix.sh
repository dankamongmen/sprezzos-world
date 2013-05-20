#!/bin/bash
#
# convert-info-files-to-unix.sh
#
# info files in texlive are with DOS lineendings. Fix this here
#
# Norbert Preining, 2005
# GPL
set -e
shopt -s nullglob

for f in debian/*.info ; do
    for i in `cat $f` ; do
        sed -e 's/\r$//' $i > $i.new
	mv $i.new $i
    done
done

