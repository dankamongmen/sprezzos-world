#!/bin/sh

# Get sound back
if [ -x /etc/init.d/alsa-utils ]; then
        if [ -x "`which invoke-rc.d 2>/dev/null`" ]; then
                invoke-rc.d alsa-utils start
        else
		/etc/init.d/alsa-utils start
        fi
fi

