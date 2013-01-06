#
# Regular cron jobs for the libgeis package
#
0 4	* * *	root	[ -x /usr/bin/libgeis_maintenance ] && /usr/bin/libgeis_maintenance
