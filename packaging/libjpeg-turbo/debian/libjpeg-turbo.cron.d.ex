#
# Regular cron jobs for the libjpeg-turbo package
#
0 4	* * *	root	[ -x /usr/bin/libjpeg-turbo_maintenance ] && /usr/bin/libjpeg-turbo_maintenance
