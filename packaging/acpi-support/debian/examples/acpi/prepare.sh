#!/bin/sh

test -f /usr/share/acpi-support/key-constants || exit 0

for SCRIPT in /etc/acpi/suspend.d/*.sh; do
    if [ -x $SCRIPT ] ; then
	. $SCRIPT
    fi
done
