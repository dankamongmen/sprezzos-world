#!/bin/bash -e

# Script to build images for one or more architectures and/or source

if [ -z "$CF" ] ; then
    CF=./CONF.sh
fi
. $CF

START=`date -u`
echo "$START: Using CONF from $CF."

if [ -z "$COMPLETE" ] ; then
    export COMPLETE=1
fi

if [ $# -gt 1 ] ; then
    echo "ERROR: too many arguments." >&2
    exit 1
elif [ -n "$1" ] ; then
    export ARCHES="$1"
fi

PATH=$BASEDIR/tools:$PATH
export PATH

if [ "$TASK"x = ""x ] ; then
	case "$INSTALLER_CD"x in
		"1"x)
			TASK=debian-installer
			unset COMPLETE
			;;
		"2"x|"C"x)
			TASK=debian-installer+kernel
			unset COMPLETE
			;;
		*)
			COMPLETE=1
			;;
	esac
fi

export TASK COMPLETE

make distclean
make ${CODENAME}_status
echo " ... checking your mirror"
RET=""
make mirrorcheck || RET=$?
if [ "$RET" ]; then
	echo "ERROR: Your mirror has a problem, please correct it." >&2
	exit 1
fi

if [ -z "$IMAGETARGET" ] ; then
    IMAGETARGET="official_images"
fi
echo " ... building the images; using target(s) \"$IMAGETARGET\""

if [ "$MAXISOS"x = ""x ] ; then
    export MAXISOS="ALL"
fi
if [ "$MAXJIGDOS"x = ""x ] ; then
    export MAXJIGDOS="ALL"
fi

if [ "$MAXISOS" = "all" ] || [ "$MAXISOS" = "ALL" ] ; then
    NUMISOS="all available"
elif [ "$MAXISOS" -eq 0 ] ; then
    NUMISOS="no"
else
    NUMISOS="up to $MAXISOS"
fi
if [ "$MAXJIGDOS" = "all" ] || [ "$MAXJIGDOS" = "ALL" ] ; then
    NUMJIGDOS="all available"
elif [ "$MAXJIGDOS" -eq 0 ] ; then
    NUMJIGDOS="no"
else
    NUMJIGDOS="up to $MAXJIGDOS"
fi
echo "Building $NUMJIGDOS jigdos and $NUMISOS isos for $ARCHES $DISKTYPE"

make $IMAGETARGET

if [ "$IMAGESUMS"x = 1x ]; then
	make imagesums
fi

END=`date -u`
echo "$END: Finished."
