#!/bin/sh

# Fix for dark password prompt after a resume on some ASUS laptops
# from https://bugs.launchpad.net/system76/+bug/114675/

if [ -r /proc/acpi/asus/brn ] ; then
	brtNum=`cat /proc/acpi/asus/brn`
	echo $brtNum > /proc/acpi/asus/brn
fi
