#
# Regular cron jobs for the xbmc package
#
0 4	* * *	root	[ -x /usr/bin/xbmc_maintenance ] && /usr/bin/xbmc_maintenance
