#!/bin/sh

# seascope workaround for binary

test -d /usr/share/pyshared/seascope || exit 1;
cd /usr/share/pyshared/seascope;
python Seascope.py

