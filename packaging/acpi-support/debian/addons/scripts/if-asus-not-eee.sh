#! /bin/sh

# The hotkeys for the Asus Eee PC conflict with brightness keys on other Asus
# laptops. This script invokes another acpi script (passed as an argument), but
# only if the system is not an Eee PC.

test -d /sys/bus/platform/devices/eeepc && exit 0 

/etc/acpi/$1
