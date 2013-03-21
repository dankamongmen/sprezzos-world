#! /bin/sh

if [ ! -d mpeg2enc ]; then
    echo >&2 "Cannot find directory mpeg2enc: is this the simage source tree?"
    exit 1
fi


rm -rf mpeg2enc
mkdir mpeg2enc
touch mpeg2enc/Makefile.in
