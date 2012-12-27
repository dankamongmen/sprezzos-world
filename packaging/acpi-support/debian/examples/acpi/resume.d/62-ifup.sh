#!/bin/sh

# Bring up the interfaces. This should probably be left up to some policy
# manager, but at the moment we just bring back all the ifupdown-managed
# interfaces that were up at suspend time.
for x in $IFUP_INTERFACES; do
    if must_control_interface $x ; then
        ifup $x &
    fi
done

# Wake up NetworkManager
dbus-send --system --dest=org.freedesktop.NetworkManager /org/freedesktop/NetworkManager org.freedesktop.NetworkManager.wake

# Kick ifplugd to check its interfaces. (Not sure if this is necessary, but it
# can't hurt either.)
invoke-rc.d --quiet ifplugd restart

