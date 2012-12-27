#!/bin/sh

# And turn the framebuffer back on
for x in /sys/class/graphics/*; do
    if [ -f $x/state ]; then
        echo -n 0 >$x/state;
    fi
done

