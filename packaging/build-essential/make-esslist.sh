#!/bin/sh

esslist=essential-packages-list
mirror=http://ftp.debian.org/debian
mirror=http://ftp2.de.debian.org/debian
dist=sid

set -e

arches=`awk '($1 != "" && $1 !~ /^#/) {print $2}' /usr/share/dpkg/archtable \
	| sort | uniq`

for arch in $arches
do
	if [ ! -f Packages-$arch ]
	then
		if wget -O Packages-$arch.bz2 $mirror/dists/$dist/main/binary-$arch/Packages.bz2
		then
			bunzip2 -f Packages-$arch.bz2
		else
			rm -f Packages-$arch
		fi
	fi
	if [ -f Packages-$arch ]
	then
		printf > $esslist-$arch \
			'This list was generated on %s for %s\n' \
			"`LANG=C date`" "$arch"
		echo >> $esslist-$arch \
			'It contains a list of essential packages' \
			'(which are also build-essential).'
		echo >> $esslist-$arch

		grep-status -FEssential -sPackage -ni yes Packages-$arch \
			>> $esslist-$arch
	else
		echo "No essential packages list is available" \
			"for $arch in $dist" > $esslist-$arch
	fi
done
rm -f Packages-*
