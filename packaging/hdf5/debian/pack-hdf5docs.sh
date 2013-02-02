#!/bin/sh

# requires svn and php

BRANCH_VERSION=$(dpkg-parsechangelog | sed -rne 's,^Version: (\w)\.(\w)\.(\w)(.+),\1_\2_\3,p')
BRANCH_VERSION_FORMAT=$(dpkg-parsechangelog | sed -rne 's,^Version: (\w)\.(\w)\.(\w)(.+),\1.\2.\3,p')

svn export https://svn.hdfgroup.uiuc.edu/hdf5doc/branches/hdf5_$BRANCH_VERSION/html

find html -name 'Makefile.*' -delete
rm -rf html/PSandPDF html/Specifications
rm -f html/HL/Examples/* html/Specs.html
#rm -f html/RM/Tools/h5check.htm html/RM/Tools/h5fix_swapped_ids.htm html/RM/CollectiveCalls.html

HTML_FILES=$(find html/ -iname '*.html')
for f in $HTML_FILES; do
	echo "processing $f...";\
	php -f $f > $f.new;\
	rm $f;\
	mv $f.new $f;\
done

#rm -rf html/RM/Tools html/RM/H5 html/RM/H5?

tar czvf ../hdf5_$BRANCH_VERSION_FORMAT.orig-doc.tar.gz html
rm -rf html

