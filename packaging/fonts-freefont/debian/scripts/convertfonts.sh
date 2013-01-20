#!/bin/sh

CONVERTFONT=$(pwd)/debian/scripts/ConvertFont.ff
CONVERTOTFONT=$(pwd)/debian/scripts/ConvertOTFont.ff
SRC="./"
TARGET="TTF"
OTFTARGET="OTF"
test ! -d ${TARGET} && mkdir ${TARGET}
test ! -d ${OTFTARGET} && mkdir ${OTFTARGET}

FONTS=`cd ${SRC} && find . -maxdepth 1 -name "*.sfd"`
for font in ${FONTS}; do
	echo $font
	(cd ${TARGET} && ${CONVERTFONT} ../${SRC}/${font} )
        (cd ${OTFTARGET} && ${CONVERTOTFONT} ../${SRC}/${font} )
done
