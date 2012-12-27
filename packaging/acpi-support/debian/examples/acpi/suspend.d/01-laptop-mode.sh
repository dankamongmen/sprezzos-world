#! /bin/sh

# There have been reports of non-working suspend if laptop mode was enabled
# during suspend. See for example Debian BTS #458437.
if [ -e /etc/init.d/laptop-mode ] ; then
	invoke-rc.d laptop-mode stop 
fi
