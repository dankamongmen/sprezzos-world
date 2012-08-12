#!/bin/bash

if [ $# -ne '2' ]; then
  echo -e "\nUsage: Compare a test-expected-* file and a test-results-* file."
  echo -e "$0 : < Expected testsuite results > < Testsuite results >\n";
  exit 1
fi;

expected=$(tempfile)
results=$(tempfile)
grep -v '^#.*' $1 | sort > $expected
grep -v '^#.*' $2 | sort > $results

REGRESSIONS=$(diff -wB $expected $results | sed -e '/^>/!d;s/^> //g')
rm -f $expected $results
if [ -n "$REGRESSIONS" ] ; then
  echo "Encountered regressions that don't match expected failures:"
  echo "$REGRESSIONS"
  exit 1
else
  echo "Passed regression testing. No new failures, no changed error values."
  exit 0
fi

