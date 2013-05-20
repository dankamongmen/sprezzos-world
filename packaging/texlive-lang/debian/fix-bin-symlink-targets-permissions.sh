#!/bin/bash
#
# TeX Live 2008 ships many "binaries" as symlinks to ../../texmf-*/...
# we make sure that the link targets have executable bit set
#
# Norbert Preining, 2008
# GPL

set -e

for i in `find debian/ -wholename 'debian/texlive-*/usr/bin/*' -type l` ; do
	ln=`readlink $i`
	case "$ln" in 
	../share/texlive/*)
	  # set the executable bit on the target
	  dn=`dirname $i`
	  target="$dn/$ln"
	  chmod ugo+x "$target"
	  ;;
	esac
done


