#!/bin/sh
dest=`dirname $0`
case "$dest" in
/*)
     ;;
*)
     dest=`pwd`/$dest
     ;;
esac

pdebuild --debbuildopts -i --auto-debsign --buildresult $dest/../.. -- --basetgz /var/cache/pbuilder/base-i386.tgz 
