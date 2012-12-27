#!/bin/sh

# Make sure the backlight goes off
if [ x$USE_DPMS = "xtrue" ]; then
  vbetool dpms off
fi

# SHUT UP FRAMEBUFFER
for x in /sys/class/graphics/*; do
    if [ -f $x/state ]; then
        echo -n 1 >$x/state;
    fi
done

