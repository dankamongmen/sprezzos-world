#
# Regular cron jobs for the libeina package
#
0 4	* * *	root	[ -x /usr/bin/libeina_maintenance ] && /usr/bin/libeina_maintenance
