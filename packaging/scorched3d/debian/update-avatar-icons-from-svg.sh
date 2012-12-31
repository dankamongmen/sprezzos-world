#!/bin/sh

set -x
set -e

for i in avatars-svg/*.svg ; do \
	inkscape --export-png=`echo $i | sed -e 's/\.svg$/.png/g'` \
	--export-area-page --export-width=32 --export-height=32 \
	$i ; done
mkdir -p avatars
mv avatars-svg/*.png avatars/
