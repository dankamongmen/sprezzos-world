#!/bin/sh

# Save the ALSA state
if [ -x /etc/init.d/alsa-utils ]; then
        if [ -x "`which invoke-rc.d 2>/dev/null`" ]; then
                invoke-rc.d alsa-utils stop
        else
		/etc/init.d/alsa-utils stop
        fi
fi

