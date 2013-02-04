#!/bin/sh

# Script to build everything possible : sources and binaries for all archs

if [ -z "$CF" ] ; then
    CF=./CONF.sh
fi
. $CF

echo "Using CONF from $CF"

if [ -z "$COMPLETE" ] ; then
    export COMPLETE=1
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

TMP_OUT=$OUT

if [ -z "$IMAGETARGET" ] ; then
	IMAGETARGET="official_images"
fi

for ARCHES in i386 amd64 armel armhf ia64 mips mipsel powerpc s390 s390x sparc kfreebsd-amd64 kfreebsd-i386 source
do
	export ARCHES
	echo "Now we're going to build CD for $ARCHES !"
	echo " ... cleaning"

	make distclean
	make ${CODENAME}_status
	echo " ... checking your mirror"
	RET=""
	make mirrorcheck || RET=$?
	if [ "$RET" ]; then
		echo "ERROR: Your mirror has a problem, please correct it." >&2
		exit 1
	fi

	OUT="$TMP_OUT/$ARCHES"
	export OUT
	mkdir -p $OUT
	echo " ... building the images; using target(s) \"$IMAGETARGET\""
	make $IMAGETARGET

	if [ "$IMAGESUMS"x = 1x ]; then
		make imagesums
	fi

	echo "--------------- `date` ---------------"
done
