#!/bin/sh

autoreconf -i -v

# not needed in tarball
rm -r autom4te.cache
