#! /bin/sh
#
# This script adjusts hard drive APM settings using hdparm. The hardware
# defaults (usually hdparm -B 128) cause excessive head load/unload cycles
# on many modern hard drives. We therefore set hdparm -B 254 while on AC
# power. On battery we set hdparm -B 128, because the head parking is
# very useful for shock protection.
#

. /usr/share/acpi-support/power-funcs

DO_HDPARM=y
if [ -e /usr/sbin/laptop_mode ] ; then 
  LMT_CONTROL_HD_POWERMGMT=$(. /etc/laptop-mode/laptop-mode.conf && echo "$CONTROL_HD_POWERMGMT")
  if [ "$LMT_CONTROL_HD_POWERMGMT" != 0 ] \
     && [ -e /var/run/laptop-mode-tools/enabled ]
  then
    # Laptop mode controls hdparm -B settings, we don't.
    DO_HDPARM=n
  fi
fi

if [ "$DO_HDPARM" = y ] ; then
  # Get the power state into STATE
  getState;
  
  for dev in /dev/sd? /dev/hd? ; do
    if [ -b $dev ] ; then
      # Check for APM support; discard errors since not all drives
      # support HDIO_GET_IDENTITY (-i).    
      if hdparm -i $dev 2> /dev/null | grep -q 'AdvancedPM=yes' ; then
	if [ "$STATE" = "BATTERY" ] ; then
	  hdparm -B 128 $dev
	else
	  hdparm -B 254 $dev
	fi
      fi
    fi
  done
fi

