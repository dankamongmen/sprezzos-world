#!/bin/bash

# Examples of using:
# ./elmerfem_get-orig-source.sh
# ./elmerfem_get-orig-source.sh 5.5.0.svn.4499.dfsg
# ./elmerfem_get-orig-source.sh 6.1.0.svn.5396.dfsg2

PACKAGE=elmerfem
SRC_VERSION="${1}"
SVN_REPO="https://elmerfem.svn.sourceforge.net/svnroot/elmerfem/trunk"

if [ -z "${SRC_VERSION}" ]; then
	echo "Package version is not specified, last revision from SVN repo will be used."
	SVN_REVISION=$(svn log "${SVN_REPO}" | head -n2 |grep "r[0-9]\+" | sed -e "s/^r\([0-9]\+\).*$/\1/")
	if [ -z "${SVN_REVISION}" ]; then
	    echo "Failed to find last SVN revision."
	    exit 1
	fi
    SRC_VERSION="6.1.0.svn.${SVN_REVISION}.dfsg"
	echo "SVN_REVISION = ${SVN_REVISION}"
	echo "SRC_VERSION  = ${SRC_VERSION}"
else
    SVN_REVISION=$(echo ${SRC_VERSION} | sed -e "s/^.*.svn.\([0-9]\+\).dfsg.*$/\1/")
	if [ -z "${SVN_REVISION}" ]; then
	    echo "Failed to get SVN revision from package version."
	    exit 1
	fi
	echo "SVN_REVISION = ${SVN_REVISION}"
	echo "SRC_VERSION  = ${SRC_VERSION}"
fi

TARBALL="${PACKAGE}_${SRC_VERSION}.orig.tar.gz"

rm -rf "${PACKAGE}-${SRC_VERSION}" "${TARBALL}"
svn export -r ${SVN_REVISION} "${SVN_REPO}" "${PACKAGE}-${SRC_VERSION}" || exit 1

cd "${PACKAGE}-${SRC_VERSION}"
rm -rf mathlibs umfpack elmergrid/src/metis post/src/fonts elmergrid/acx_metis.m4
rm -rf */*.cache post/src/*/*.cache
rm -rf ElmerGUI/Application/plugins/tetgen.h misc/tetgen_plugin/*
cd ..

GZIP='--best -n' tar -czf ${TARBALL} "${PACKAGE}-${SRC_VERSION}" || exit 1
rm -rf "${PACKAGE}-${SRC_VERSION}"

echo "${TARBALL} was created."
