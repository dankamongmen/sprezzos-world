#!/bin/bash -e

version=$( dpkg-parsechangelog -l`dirname $0`/changelog | sed -n 's/^Version: \(.*:\|\)//p' | sed 's/-[0-9.]\+$//' )
if echo $version | grep -v git >/dev/null; then
	# Not a snapshot, use watch
	exit 1
fi

if [ -z "$SAMBA_GIT_URL" ]; then
	SAMBA_GIT_URL=git://git.samba.org/samba.git
fi

LDBTMP=`mktemp -d`
if [ -d $SAMBA_GIT_URL/.bzr ]; then
	bzr co --lightweight $SAMBA_GIT_URL $LDBTMP
else
	git clone --depth 1 $SAMBA_GIT_URL $LDBTMP
fi
pushd $LDBTMP/lib/ldb
./configure
make dist
popd
mv $LDBTMP/lib/ldb/ldb-*.tar.gz ldb_$version.orig.tar.gz
rm -rf $LDBTMP
