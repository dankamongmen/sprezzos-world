#! /bin/sh

if [ -e /etc/init.d/laptop-mode ] ; then
	invoke-rc.d laptop-mode restart
fi
