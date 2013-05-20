#!/bin/bash
#
# update-link-files.sh
#
# This script serves two purposes:
# 1) it copies the content of $package.links.dist to $package.links
# 2) it adjusts the links in $package.links.generated to files which
#    have been compressed by dh_compress
#
# Norbert Preining, 2005-2006
# GPL
set -e
shopt -s nullglob

#
# first copy the dist link files to the normal
for i in debian/*.links.dist ; do
    cp $i debian/`basename $i .dist`
done

for i in debian/*.links.generated ; do
    bn=`basename $i .links.generated`
    while read a b ; do 
        case "$a" in
	    usr/share/doc/$bn/* )
		if [ -r debian/$bn/$a ] ; then 
	    	    echo "$a $b"
		else 
	    	    if [ -r debian/$bn/$a.gz ] ; then #
	                echo "$a.gz $b.gz"
	    	    else 
		        echo "Missing $a" >&2
	    	    fi
        	fi
		;;
	     * )
	        echo "$a $b"
		;;
	esac
    done < $i >> debian/$bn.links
done

