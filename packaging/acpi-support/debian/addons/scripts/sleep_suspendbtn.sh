#!/bin/sh

command=$1
if [ $command = "sleep" ]; then
	key=$KEY_SLEEP
elif [ $command = "suspend" ]; then
	key=$KEY_SUSPEND
else
	logger -t${0##*/} -perr -- "Error: Cannot recognize command $1"
fi

test -f /usr/share/acpi-support/key-constants || exit 0

. /usr/share/acpi-support/policy-funcs

if CheckPolicy; then
  # If gnome-power-manager or klaptopdaemon are running, generate the X
  # "sleep/suspend" key. The daemons will handle that keypress according to
  # their settings.

  # (With this script being called only when a key is pressed that is *not*
  # seen as a suspend key by the rest of the system, we still need to do this
  # translation here.)
  
  . /usr/share/acpi-support/key-constants
  acpi_fakekey $key 
else
  # No power management daemons are running. Divert to our own implementation.

  # Note that sleep.sh assumes that the pressed key is also seen by the rest
  # of the system. However, it will choose the right path (our own
  # implementation) if CheckPolicy says so. And this way we have a single
  # user-configurable point for how we do suspend. (Not that that's nice, 
  # but until we have pluggable suspend methods this is the way to go.)
  
  /etc/acpi/sleep_suspend.sh $command
fi

