#!/bin/sh

test -d /usr/share/acpi-support/state-funcs || exit 0

. /usr/share/acpi-support/state-funcs

if isAnyWirelessPoweredOn ; then
    setLEDAsusWireless 1
else
    setLEDAsusWireless 0
fi

  
