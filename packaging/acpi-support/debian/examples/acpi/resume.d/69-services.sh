#!/bin/sh

# we need to restart our services after the network is back
for x in $STOP_SERVICES; do
        invoke-rc.d --quiet $x start
done

