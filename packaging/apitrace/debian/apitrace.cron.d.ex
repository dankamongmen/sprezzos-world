#
# Regular cron jobs for the apitrace package
#
0 4	* * *	root	[ -x /usr/bin/apitrace_maintenance ] && /usr/bin/apitrace_maintenance
