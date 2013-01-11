#!/bin/bash
set -e

MAXFAIL=30
MAXERR=3

echo "*******************************************************************"
echo "Running 'make test'"
echo
make OPTS=-v test
echo
echo "*******************************************************************"
echo "Running 'make test-all'"
echo
make TESTS=-v test-all 2>&1 |tee make-test-all.log || true
echo
echo "'make test-all' finished. checking number of failures and errors."
echo "max number allowed: failures:$MAXFAIL errors:$MAXERR"
if ! grep -q tests, make-test-all.log; then
	echo "Could not find number of failures. Interpreter crash? Failing build."
	exit 1
fi
f=$(grep tests, make-test-all.log)
fail=$(echo $f | cut -d ' ' -f 5)
err=$(echo $f | cut -d ' ' -f 7)
if [ $err -gt $MAXERR ];then
	echo "Too many errors. Failing build."
	exit 1
fi
if [ $fail -gt $MAXFAIL ];then
	echo "Too many failures. Failing build."
	exit 1
fi
exit 0
