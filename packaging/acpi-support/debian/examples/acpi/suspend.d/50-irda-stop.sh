#!/bin/sh

# Stop IRDA if it's running
if [ -f /var/run/irattach.pid ]; then
    if [ -x "`which invoke-rc.d 2>/dev/null`" ]; then
	invoke-rc.d irda-utils stop
    else
	/etc/init.d/irda-utils stop
    fi
    killall -9 irattach
fi

