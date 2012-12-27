#! /bin/bash
#
# rrdcached - start and stop the RRDtool data caching daemon
# http://oss.oetiker.ch/rrdtool/
#
# Based on the collectd init script.
#
# Copyright (C) 2005-2006 Florian Forster <octo@verplant.org>
# Copyright (C) 2006-2009 Sebastian Harl <tokkee@debian.org>
#

### BEGIN INIT INFO
# Provides:          rrdcached
# Required-Start:    $local_fs $remote_fs
# Required-Stop:     $local_fs $remote_fs
# Should-Start:      $network
# Should-Stop:       $network
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: start the RRDtool data caching daemon
### END INIT INFO

set -e

PATH=/sbin:/bin:/usr/sbin:/usr/bin

DISABLE=0

DESC="RRDtool data caching daemon"
NAME=rrdcached
DAEMON=/usr/bin/rrdcached

OPTS="-l unix:/var/run/rrdcached.sock"
OPTS="$OPTS -j /var/lib/rrdcached/journal/ -F"
OPTS="$OPTS -b /var/lib/rrdcached/db/ -B"

PIDFILE=/var/run/rrdcached.pid

MAXWAIT=30

# Gracefully exit if the package has been removed.
test -x $DAEMON || exit 0

if [ -r /etc/default/$NAME ]; then
	. /etc/default/$NAME
fi

if test "$DISABLE" != 0 -a "$1" == "start"; then
	echo "$NAME has been disabled - see /etc/default/$NAME."
	exit 0
fi

if test "$ENABLE_COREFILES" == 1; then
	ulimit -c unlimited
fi

d_start() {
	if test "$DISABLE" != 0; then
		# we get here during restart
		echo -n " - disabled by /etc/default/$NAME"
		return 0
	fi

	# make sure we have the needed directories
	mkdir -p /var/lib/rrdcached/journal/
	mkdir -p /var/lib/rrdcached/db/

	start-stop-daemon --start --quiet --oknodo --pidfile "$PIDFILE" \
		--exec $DAEMON -- $OPTS -p "$PIDFILE"
}

still_running_warning="
WARNING: $NAME might still be running.
In large setups it might take some time to write all pending data to
the disk. You can adjust the waiting time in /etc/default/$NAME."

d_stop() {
	PID=$( cat "$PIDFILE" 2> /dev/null ) || true

	start-stop-daemon --stop --quiet --oknodo --pidfile "$PIDFILE"

	sleep 1
	if test -n "$PID" && kill -0 $PID 2> /dev/null; then
		i=0
		while kill -0 $PID 2> /dev/null; do
			i=$(( $i + 2 ))
			echo -n " ."

			if test $i -gt $MAXWAIT; then
				echo "$still_running_warning" >&2
				return 1
			fi

			sleep 2
		done
		return 0
	fi
}

d_status() {
	PID=$( cat "$PIDFILE" 2> /dev/null ) || true

	if test -n "$PID" && kill -0 $PID 2> /dev/null; then
		echo "$NAME ($PID) is running."
		exit 0
	else
		PID=$( pidof $NAME ) || true

		if test -n "$PID"; then
			echo "$NAME ($PID) is running."
			exit 0
		else
			echo "$NAME is stopped."
		fi
	fi
	exit 1
}

case "$1" in
	start)
		echo -n "Starting $DESC: $NAME"
		d_start
		echo "."
		;;
	stop)
		echo -n "Stopping $DESC: $NAME"
		d_stop
		echo "."
		;;
	status)
		d_status
		;;
	restart|force-reload)
		echo -n "Restarting $DESC: $NAME"
		d_stop
		sleep 1
		d_start
		echo "."
		;;
	*)
		echo "Usage: $0 {start|stop|restart|force-reload|status}" >&2
		exit 1
		;;
esac

exit 0

# vim: syntax=sh noexpandtab sw=4 ts=4 :

