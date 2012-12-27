#!/bin/sh

# Save video PCI state?
if [ x$SAVE_VIDEO_PCI_STATE = xtrue ]; then
  for x in /sys/bus/pci/devices/*; do
    if [ `cat $x/class` = "0x030000" ]; then
        cat $x/config >/var/run/vga-pci-`basename $x`;
    fi
  done
fi

