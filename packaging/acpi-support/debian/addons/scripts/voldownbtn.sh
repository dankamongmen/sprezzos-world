#!/bin/sh

test -f /usr/share/acpi-support/key-constants || exit 0

. /usr/share/acpi-support/key-constants
acpi_fakekey $KEY_VOLUMEDOWN
