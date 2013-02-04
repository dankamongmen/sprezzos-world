#!/bin/bash
# Install files in /install and some in /doc

set -e

if [ "$RELEASE_NOTES_LOCATION"x = ""x ] ; then
	export RELEASE_NOTES_LOCATION="http://www.debian.org/releases/$CODENAME"
fi

# The location of the tree for CD#1, passed in
DIR=$1
ARCHES="$2"
NUM_ARCHES=`echo $ARCHES | wc -w`

if [ "$OMIT_MANUAL" != 1 ]; then
	DOCDIR=doc
	MANTDIR=$TDIR/installguide

	for ARCH in $ARCHES
	do
		if [ $ARCH != source ] ; then
			if [ $NUM_ARCHES = 1 ] ; then
				INSTALLDIR=$DIR/$DOCDIR/manual
			else
				INSTALLDIR=$DIR/$DOCDIR/manual/$ARCH
			fi

            INSTALLGUIDE=$(zcat $MIRROR/dists/$CODENAME/main/binary-$ARCH//Packages.gz | \
                sed -n "s/Filename: \(pool\/main\/i\/installation-guide\/installation-guide-$ARCH.*deb\)$/\1/p")

            if [ -f "$MIRROR/$INSTALLGUIDE" ]; then
                rm -rf $MANTDIR
                # Extract documentation from package
                dpkg -x $MIRROR/$INSTALLGUIDE $MANTDIR || true
                if [ -d $MANTDIR/usr/share/doc/installation-guide-$ARCH/ ]; then
                    cd $MANTDIR/usr/share/doc/installation-guide-$ARCH
                    rm -f changelog* copyright
                    find . -name '*.gz' | xargs gunzip || true

                    mkdir -p $INSTALLDIR
                    if ! cp -a * $INSTALLDIR; then
                        echo "ERROR: Unable to copy installer documentation to CD."
                    fi
                else
                    echo "ERROR: installation-guide package not unpacked correctly."
               fi
            else
                echo "ERROR: package installation-guide-$ARCH not found."
            fi
		fi
	done
fi

if [ "$OMIT_RELEASE_NOTES" != 1 ]; then
	for ARCH in $ARCHES
	do
		if [ $ARCH != source ] ; then
			RN=$DIR/doc/release-notes
			mkdir -p $RN
			cd $RN
			echo "Downloading most recent release notes for $ARCH"
			wget $RELEASE_NOTES_LOCATION/release-notes-$ARCH.tar.gz
			if [ -e release-notes-$ARCH.tar.gz ] ; then
				tar xzvf release-notes-$ARCH.tar.gz
				rm -f release-notes-$ARCH.tar.gz
				rm -f */*.ps
			else
				echo "No release notes found at $RELEASE_NOTES_LOCATION/release-notes-$ARCH.tar.gz"
			fi
		fi
	done
fi
