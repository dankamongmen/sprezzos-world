#!/bin/sh
# Paul Sladen 2007-03-22
# Save the state of various things that the kernel does not, or cannot
# maintain over the course of a suspend/resume or suspend/hibernate
# cycle.  Lines are written into '/var/run/proc-sysfs-save-state' in
# the format:
# /sys/foo/bar/moo = XYZ

if [ -d /var/run -a -w /var/run ] ; then
    ( 
	for i in /sys/class/net/*/device/rf_kill /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor ; do
	    if [ -r "$i" ] ; then
		echo -n "$i = " ; cat "$i"
	    fi
	done

	i=/proc/acpi/ibm/bluetooth
	if [ -r $i ] ; then
	    echo -n "$i = "
	    grep -q disabled /proc/acpi/ibm/bluetooth && echo disable || echo enable
	fi
    ) > /var/run/proc-sysfs-save-state
fi

