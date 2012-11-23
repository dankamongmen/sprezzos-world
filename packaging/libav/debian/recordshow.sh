#!/bin/bash

# Copyright 2008, Daniel Dickinson <cshore@wightman.ca>
#
# This script script (which depends on xawtv for the v4lctl command to
# select channel) and crontab show how one can record tv shows using
# ffmpeg.

STATION="$1"
TODAY=$(date +"%A %B %d %Y")
SHOWLENGTH="$2"
SHOWDIR="$3"
SHOWNAME="$4"

function err_exit {
	EXITCODE=$1
	shift
	echo $* 1>&2
	exit $EXITCODE
}

BADPARAM=FALSE

if [ -z "STATION" ]; then
	BADPARAM=TRUE
fi

if [ -z "$SHOWDIR" ]; then
	BADPARAM=TRUE
fi

if [ -z "$SHOWLENGTH" ]; then
	BADPARAM=TRUE
fi

if [ "$BADPARAM" != "FALSE" ]; then
	err_exit 2 "Usage: recordshow.sh station show-length show-dir [show-name]"
fi

if [ -z "$SHOWNAME" ]; then
	BASEFILENAME="$SHOWDIR/$TODAY"
else
	BASEFILENAME="$SHOWDIR/$SHOWNAME-$TODAY"
fi

SECONDS=$(echo $SHOWLENGTH | cut -f3 -d:)
MINUTES=$(echo $SHOWLENGTH | cut -f2 -d:)
HOURS=$(echo $SHOWLENGTH | cut -f1 -d:)

TOTALSECONDS=0

TOTALSECONDS=$(expr $(expr $(expr $HOURS '*' 3600) + $(expr $MINUTES '*' 60)) + $SECONDS) 

/usr/bin/v4lctl setstation $1 >/dev/null || err_exit 1 "Unable to set station (channel) $STATION"
/usr/bin/v4lctl volume mute off >/dev/null || err_exit 4 "Unable to unmute audio"
/usr/bin/ffmpeg -y -tvstd ntsc -t "$TOTALSECONDS" -s 480x352 -re -deinterlace -f video4linux2 -i /dev/video0 -f audio_device -i /dev/dsp -ac 2 -s 768x576 -f mpegts -acodec mp2 -vcodec mpeg1video "$BASEFILENAME.mpegts" >/dev/null 2>&1 || err_exit 3 "Error recording show $BASEFILENAME to mpeg2 transport stream"
/usr/bin/v4lctl volume mute on >/dev/null || err_exit 5 "Unable to mute audio"

