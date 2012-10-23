#
# Regular cron jobs for the sick-beard package
#
0 4	* * *	root	[ -x /usr/bin/sick-beard_maintenance ] && /usr/bin/sick-beard_maintenance
