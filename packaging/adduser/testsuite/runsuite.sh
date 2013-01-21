#!/bin/bash

FAILED=""

PASSWD_BAK="./passwd.backup"


if [ "$(id -u)" != "0" ]; then
  echo "root needed"
  exit 1
fi

cp /etc/passwd $PASSWD_BAK

for a in off on; do
  for i in ./test*.pl ; do
    if ! shadowconfig $a > /dev/null; then
      echo "shadowconfig $a failed"
      exit 1
    fi
    echo
    echo "Starting $i (shadow $a)"
    /usr/bin/perl $i
    if [ "$?" != "0" ]; then
      FAILED="$FAILED $i($a)"
    fi
  done
done

if [ -z "$FAILED" ]; then
  echo "All tests passed successfully"
  rm $PASSWD_BAK
  exit 0
else
  echo "tests $FAILED failed"
  echo "see $PASSWD_BAK for a copy of /etc/passwd before starting"
  exit 1
fi

