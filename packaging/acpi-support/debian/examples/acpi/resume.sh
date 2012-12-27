#!/bin/sh

test -f /usr/share/acpi-support/key-constants || exit 0

# Source from /etc/acpi/resume.d/
for SCRIPT in /etc/acpi/resume.d/*.sh; do
    if [ -x $SCRIPT ] ; then
        . $SCRIPT
    fi
done
