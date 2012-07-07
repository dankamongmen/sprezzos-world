#
# Regular cron jobs for the fbv package
#
0 4	* * *	root	[ -x /usr/bin/fbv_maintenance ] && /usr/bin/fbv_maintenance
