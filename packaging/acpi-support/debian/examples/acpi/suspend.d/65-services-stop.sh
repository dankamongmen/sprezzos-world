#!/bin/sh

# Shut down services known to misbehave
for x in $STOP_SERVICES; do
        invoke-rc.d --quiet $x stop
done

