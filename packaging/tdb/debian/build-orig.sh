#!/bin/bash -e

if [ -z "$SAMBA_GIT_URL" ]; then
	SAMBA_GIT_URL=git://git.samba.org/samba.git
fi

TDBTMP=`mktemp -d`
version=$( dpkg-parsechangelog -l`dirname $0`/changelog | sed -n 's/^Version: \(.*:\|\)//p' | sed 's/-[0-9.]\+$//' )
if [ -d $SAMBA_GIT_URL/.bzr ]; then
	bzr co --lightweight $SAMBA_GIT_URL $TDBTMP
else
	git clone --depth 1 $SAMBA_GIT_URL $TDBTMP
fi

pushd $TDBTMP/lib/tdb
./configure
make dist
popd
version=$( dpkg-parsechangelog -l`dirname $0`/changelog | sed -n 's/^Version: \(.*:\|\)//p' | sed 's/-[0-9.]\+$//' )
mv $TDBTMP/lib/tdb/tdb-*.tar.gz tdb_$version.orig.tar.gz
rm -rf $TALLOCTMP
