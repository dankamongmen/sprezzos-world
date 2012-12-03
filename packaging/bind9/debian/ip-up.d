#!/bin/sh -e
# Called when a new interface comes up
# Written by LaMont Jones <lamont@debian.org>

# kick named as needed

# If /usr isn't mounted yet, silently bail.
if [ ! -d /usr/sbin ]; then
	exit 0
fi

# if named is running, reconfig it.
rndc reconfig >/dev/null 2>&1 || true

exit 0
