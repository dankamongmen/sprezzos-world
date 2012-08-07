#
# Regular cron jobs for the pcsx2 package
#
0 4	* * *	root	[ -x /usr/bin/pcsx2_maintenance ] && /usr/bin/pcsx2_maintenance
