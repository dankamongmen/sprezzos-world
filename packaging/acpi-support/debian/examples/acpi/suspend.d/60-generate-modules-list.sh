#!/bin/sh

for x in /sys/module/*_ircc /sys/module/*_ircc2; do
    x=`basename $x`
    # nsc_ircc is safe over suspend/resume
    if [ $x != "nsc_ircc" ]; then
	modprobe -r $x 2>/dev/null;
	# These don't get added to the reload list, since irda startup will
 	# do that
    fi
done

if [ -d /sys/module/ndiswrapper ]; then
        modprobe -r ndiswrapper
        MODULES="$MODULES ndiswrapper"
fi

# This is not guaranteed to work - several drivers appear to use names that
# are not the same as their module name
for x in /sys/class/net/*; do
    if [ -e $x/device/driver ]
        then
        MODULES="$MODULES $(basename $(readlink $x/device/driver) | tr [:upper:\] [:lower:])"
    fi
done

if [ -d /sys/module/netconsole ]; then
    rmmod netconsole
    MODULES="$MODULES netconsole"
fi
