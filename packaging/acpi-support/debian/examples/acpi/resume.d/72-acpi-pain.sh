#!/bin/sh

# Some hardware gets unhappy about button events unless we do this
modprobe -r button
modprobe button

# Kick the fans
modprobe -r fan
modprobe -r thermal
modprobe fan
modprobe thermal

if [ "`grep thinkpad_acpi /proc/modules`" ]; then
    # No, I don't know why
    modprobe -r thinkpad_acpi
    modprobe thinkpad_acpi
fi

# NNGH FAN HATE
for x in /proc/acpi/fan/*; do
    if [ -f "$x/state" ] && [ "`grep on $x/state`" ]; then
        echo -n 3 > $x/state;
        echo -n 0 > $x/state;
    fi
done

# Make sure that the drive power state is set correctly again
modprobe -r sbs
modprobe -r ac
modprobe ac
modprobe -r battery
modprobe battery
modprobe sbs
/etc/acpi/power.sh

# Force monitoring processes to reopen their /proc/acpi files after modules
# have been replaced.
killall -s HUP ksysguardd
