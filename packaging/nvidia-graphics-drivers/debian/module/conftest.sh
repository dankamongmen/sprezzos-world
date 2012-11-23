#!/bin/sh
# This is a replacement for nvidia's conftest.sh
#
# We have a new conftest.h that works with kbuild that we don't want
# overwritten or generated or anything else, so we want any call to
# conftest.sh to do absolutely nothing.
exit 0
