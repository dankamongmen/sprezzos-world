#!/bin/sh

PACKAGE="$(dpkg-parsechangelog | sed -n 's/^Source: //p')"
VERSION="$(dpkg-parsechangelog | sed -ne 's/^Version: \(.*\)-.*/\1/p')"

tar \
    --exclude-vcs \
    --exclude debian \
    --transform s/^\./${PACKAGE}-${VERSION}/ \
    --bzip2 \
    --create \
    --verbose \
    --file ../${PACKAGE}_${VERSION}.orig.tar.bz2 \
    .
