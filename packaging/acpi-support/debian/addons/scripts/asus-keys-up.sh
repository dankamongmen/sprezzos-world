#!/bin/sh

KEYS_DIR=/sys/class/leds/asus\:\:kbd_backlight/brightness
test -f $KEYS_DIR || exit 0
CURRENT=`cat $KEYS_DIR`
NEXT=$((CURRENT>=3 ? 3 : CURRENT+1))
echo $NEXT > $KEYS_DIR
