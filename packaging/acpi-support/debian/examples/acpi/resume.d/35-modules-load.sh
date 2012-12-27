#!/bin/sh

# Increase the firmware loading timeout while we're doing this
# Otherwise, swap thrash tends to lead to failure to start
if [ -f /sys/class/firmware/timeout ]; then
    timeout=`cat /sys/class/firmware/timeout`
    echo 100 >/sys/class/firmware/timeout
fi

# Load any drivers that we removed
for x in $MODULES; do
    modprobe $x;
done

# And reset the firmware timeout
if [ -f /sys/class/firmware/timeout ]; then
    echo $timeout >/sys/class/firmware/timeout
fi

# And bring back PCMCIA code
[ -x /sbin/pccardctl ] && pccardctl insert
