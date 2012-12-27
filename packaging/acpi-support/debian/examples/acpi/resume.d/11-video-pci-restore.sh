#!/bin/sh

# Restore video PCI state
if [ x$SAVE_VIDEO_PCI_STATE = xtrue ]; then
  for x in /sys/bus/pci/devices/*; do
    if [ -f /var/run/vga-pci-`basename $x` ]; then
        cat /var/run/vga-pci-`basename $x` >$x/config;
    fi
  done
fi

