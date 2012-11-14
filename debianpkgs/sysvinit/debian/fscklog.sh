#!/bin/sh
# fscklog.sh	See if we need to rotate the fsck logs
#

MAXSIZE=51200
NUMFILES=5
export BOOTLOGFILE=/var/log/fsck/boot.log
export ROOTLOGFILE=/var/log/fsck/root.log

if [ -f "$BOOTLOGFILE" ] && [ "$(stat -c %s $BOOTLOGFILE)" -gt "$MAXSIZE" ]
then
	savelog -g adm -m 640 -u root -c $NUMFILES $BOOTLOGFILE
fi

if [ -f "$ROOTLOGFILE" ] && [ "$(stat -c %s $ROOTLOGFILE)" -gt "$MAXSIZE" ]
then
	savelog -g adm -m 640 -u root -c $NUMFILES $ROOTLOGFILE
fi
