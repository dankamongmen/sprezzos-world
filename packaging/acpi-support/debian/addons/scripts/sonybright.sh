#!/bin/bash
# TODO: Change above to /bin/sh

test -f /usr/share/acpi-support/key-constants || exit 0

BRIGHTNESS=$(cat /sys/class/backlight/sony/brightness)

if [ "$BRIGHTNESS" -gt 7 ]; then
	BRIGHTNESS=0
fi

if [ "x$1" = "xdown" ]; then
   if [ "x$BRIGHTNESS" != "x0" ]; then
      BRIGHTNESS=$(( $BRIGHTNESS - 1 ))
      echo $BRIGHTNESS > /sys/class/backlight/sony/brightness
   else
      [ -x /usr/bin/spicctrl ] && /usr/bin/spicctrl -b 0   
   fi
   # Recent nvidia Sonys have ACPI methods that do nothing. Thanks, Sony.
   [ -x /usr/bin/smartdimmer ] && smartdimmer -d 2>/dev/null
elif [ "x$1" = "xup" ]; then
   if [ "x$BRIGHTNESS" != "x7" ]; then
      BRIGHTNESS=$(( $BRIGHTNESS + 1 ))
      echo $BRIGHTNESS > /sys/class/backlight/sony/brightness
   fi
   [ -x /usr/bin/smartdimmer ] && smartdimmer -i 2>/dev/null
else
   echo >&2 Unknown argument $1
fi
