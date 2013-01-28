#! /bin/bash
#
# Johannes Meixner <jsmeix@suse.de>, 2004, 2005, 2006

#set -x

export PATH="/sbin:/usr/sbin:/usr/bin:/bin"
export LC_ALL="POSIX"
export LANG="POSIX"
umask 022

MY_NAME=${0##*/}

# Input:

# Create temporary files:
TMP_DATA=$(mktemp -u /tmp/$MY_NAME.XXXXXX)

touch $TMP_DATA
for CLASS in `cat data/rules/55-hpmud.rules | grep 'SYSFS{idVendor}=="03f0"' | cut -d ' ' -f 2 | cut -d '"' -f 2 | cut -d '?' -f3`; do
    IDS=''
    for A in 0 1 2 3 4 5 6 7 8 9 a b c d e f; do
	for B in 0 1 2 3 4 5 6 7 8 9 a b c d e f; do
	    IDS="${IDS}0x$A$B$CLASS;"
	done
    done
    IDS=`echo $IDS | sed -e 's/;$//'`
    echo $IDS >> $TMP_DATA
done

# Output:

# Output header:
echo '<?xml version="1.0" encoding="ISO-8859-1"?>'
echo '<deviceinfo version="0.2">'
echo '  <device>'

# Output model specific HP USB device entries:
exec <$TMP_DATA
while read PRODUCTS
do echo
   echo '    <match key="info.subsystem" string="usb_device">'
   echo '      <match key="usb_device.vendor_id" int="0x03f0">'
   echo -n '        <match key="usb_device.product_id" int_outof="'
   echo -n "$PRODUCTS"
   echo '">'
   echo '          <append key="info.capabilities" type="strlist">scanner</append>'
   echo '        </match>'
   echo '      </match>'
   echo '    </match>'
done

# Output footer:
echo
echo '  </device>'
echo '</deviceinfo>'

# Remove the temporary file
rm $TMP_DATA

exit 0

