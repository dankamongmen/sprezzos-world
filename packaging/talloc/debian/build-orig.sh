#!/bin/bash -e

if [ -z "$SAMBA_GIT_URL" ]; then
	SAMBA_GIT_URL=git://git.samba.org/samba.git
fi

TALLOCTMP=`mktemp -d`
if [ -d $SAMBA_GIT_URL/.bzr ]; then
	bzr co --lightweight $SAMBA_GIT_URL $TALLOCTMP
else
	git clone --depth 1 $SAMBA_GIT_URL $TALLOCTMP
fi
pushd $TALLOCTMP/lib/talloc
./autogen-waf.sh
./configure
make dist
popd
version=$( dpkg-parsechangelog -l`dirname $0`/changelog | sed -n 's/^Version: \(.*:\|\)//p' | sed 's/-[0-9.]\+$//' )
mv $TALLOCTMP/lib/talloc/talloc-*.tar.gz talloc_$version.orig.tar.gz
rm -rf $TALLOCTMP
