#!/bin/sh

CONVERTFONT=$(pwd)/debian/scripts/ConvertFont.ff
STRIPFONT=$(pwd)/debian/scripts/FreeSans-strip
SRC="./sfd"
TARGET="TTF-stripped"
FONTPAT="FreeSans.sfd FreeSansOblique.sfd FreeSansBold.sfd"

chmod u+x ${STRIPFONT}
chmod u+x ${CONVERTFONT}

test ! -d ${TARGET} && mkdir ${TARGET}
test ! -d udeb-sfd && mkdir udeb-sfd

#FONTS=`cd ${SRC} && find . -name "${FONTPAT}"`
for font in ${FONTPAT}; do
	echo $font
	(cd udeb-sfd && ${STRIPFONT} ../${SRC}/${font} $font)
	(cd ${TARGET} && ${CONVERTFONT} ../udeb-sfd/${font})
done
