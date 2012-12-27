#!/bin/sh
# Paul Sladen 2007-03-22
# Restore saved /sys and /proc states following resume.
# See suspend.d/??-proc-sysfs-restore-state.sh for details.

if [ -r /var/run/proc-sysfs-save-state ] ; then
    while read WHERE foo WHAT ; do
	if [ "x$foo" = "x=" -a -w "$WHERE" ] ; then
	    echo -n "$WHAT" > "$WHERE"
	fi
    done < /var/run/proc-sysfs-save-state
fi
