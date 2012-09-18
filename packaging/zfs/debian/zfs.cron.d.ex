#
# Regular cron jobs for the zfs package
#
0 4	* * *	root	[ -x /usr/bin/zfs_maintenance ] && /usr/bin/zfs_maintenance
