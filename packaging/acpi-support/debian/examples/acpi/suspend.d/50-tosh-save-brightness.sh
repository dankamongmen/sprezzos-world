#!/bin/sh

TOSH_LCD=/proc/acpi/toshiba/lcd

if [ -e $TOSH_LCD ]; then
  TOSH_BRIGHTNESS=$(( `grep brightness: $TOSH_LCD | cut -d: -f2` + 0 ))
  TOSH_MAXBRIGHT=$(( `grep brightness_levels: $TOSH_LCD | cut -d: -f2` - 1))
  # Turn brightness to max to make bios password prompt easier to see
  echo 'brightness : '$TOSH_MAXBRIGHT > $TOSH_LCD
fi

