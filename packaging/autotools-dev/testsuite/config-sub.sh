#!/bin/sh
#
# Copyright 2004, 2005, 2009 Free Software Foundation, Inc.
# Contributed by Ben Elliston <bje@gnu.org>.
#
# This test reads pairs from config-sub.data: an alias and its
# canonical triplet.  The config.sub scripts is invoked and the test
# checks that the alias expands to the expected canonical triplet.

verbose=false

run_config_sub ()
{
    rc=0
    while read alias canonical ; do
	output=`sh ../config.sub $alias`
	if test $output != $canonical ; then
	    echo "FAIL: $alias -> $output, but expected $canonical"
	    rc=1
	else
	    $verbose && echo "PASS: $alias"
	fi
    done < config-sub.data
    return $rc
}

if run_config_sub ; then
    $verbose || echo "PASS: config.sub checks"
else
    echo "Unexpected failures."
    exit 1
fi

exit 0
