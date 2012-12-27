#!/bin/sh
# on ThinkPads, the volume keys always performs mute,
# regardless of the previous state;  for the moment
# we fake this by "always" unmuting and then remuting.
# Kludge/behaviour copied over from 'thinkpad-keys'
#  -Paul Sladen 2007-09-13
[ -f /usr/share/acpi-support/key-constants ] || exit 0
. /usr/share/acpi-support/key-constants
#acpi_fakekey $KEY_VOLUMEUP
acpi_fakekey $KEY_MUTE
