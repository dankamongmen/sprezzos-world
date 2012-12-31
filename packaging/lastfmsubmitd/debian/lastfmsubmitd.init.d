#!/bin/sh -e

### BEGIN INIT INFO
# Provides:          lastfmsubmitd
# Required-Start:    $remote_fs $syslog
# Required-Stop:     $remote_fs $syslog
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: Start and stop lastfmsubmit daemon
### END INIT INFO

PATH=/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin
DAEMON=/usr/bin/lastfmsubmitd
NAME=lastfmsubmitd
DESC="Last.fm submission daemon"
RUNDIR=/var/run/lastfm
USER=lastfm
GROUP=lastfm

if ! [ -x $DAEMON ]; then
    exit 0
fi

if [ ! -d ${RUNDIR} ] ; then
	mkdir -p ${RUNDIR} || true
	if [ -d ${RUNDIR} ] ; then
		chown ${USER}:${GROUP} ${RUNDIR}
		chmod 2775 ${RUNDIR}
	fi
fi

case "$1" in
    start)
        echo -n "Starting $DESC: "
        start-stop-daemon --quiet --start -c $USER:$GROUP \
            --pidfile $RUNDIR/$NAME.pid --exec $DAEMON -- $DAEMON_OPTS
        echo "$NAME."
        ;;
    stop)
        echo -n "Stopping $DESC: "
        start-stop-daemon --quiet --oknodo --stop -u $USER \
            --pidfile $RUNDIR/$NAME.pid
        echo "$NAME."
        ;;
    restart|force-reload)
        echo -n "Restarting $DESC: "
        start-stop-daemon --quiet --oknodo --stop -u $USER \
            --pidfile $RUNDIR/$NAME.pid
        sleep 1
        start-stop-daemon --quiet --start -c $USER:$GROUP \
            --pidfile $RUNDIR/$NAME.pid --exec $DAEMON -- $DAEMON_OPTS
        echo "$NAME."
        ;;
    *)
        N=/etc/init.d/$NAME
        echo "Usage: $N {start|stop|restart|force-reload}" >&2
        exit 1
        ;;
esac

exit 0
