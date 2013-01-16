#! /bin/sh
#

### BEGIN INIT INFO
# Provides:          c-icap
# Required-Start:    $network $remote_fs $syslog
# Required-Stop:     $network $remote_fs $syslog
# Should-Start:      $named
# Should-Stop:       $named
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: C-ICAP Server Version 0.1.3
### END INIT INFO

PATH=/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin
DAEMON=/usr/bin/c-icap
NAME=c-icap
DESC=c-icap

test -x $DAEMON || exit 0

LOGDIR=/var/log/c-icap
PIDFILE=/var/run/c-icap/$NAME.pid
DODTIME=3                   # Time to wait for the server to die, in seconds
                            # If this value is set too low you might not
                            # let some servers to die gracefully and
                            # 'restart' will not work
STARTUPTIME=1		    # Time to wait to decide if daemon is up and running

# Include c-icap defaults if available
if [ -f /etc/default/c-icap ] ; then
        . /etc/default/c-icap
fi

check_ctl_dir() {
    # Create the ctl empty dir if necessary
    if [ ! -d /var/run/c-icap ]; then
        mkdir /var/run/c-icap
	chown c-icap /var/run/c-icap
        chmod 0755 /var/run/c-icap
    fi
}

# If the daemon is not enabled, give the user a warning and stop.
# Check to create /var/run directory if someone wants to run c-icap
# in debug mode / foreground to test some functions without start it from init.d
if [ "$START" != "yes" ]; then
    check_ctl_dir
    echo "To enable $NAME, edit /etc/default/c-icap and set START=yes"
    exit 0
fi

set -e

running_pid()
{
    # Check if a given process pid's cmdline matches a given name
    pid=$1
    name=$2
    [ -z "$pid" ] && return 1
    [ ! -d /proc/$pid ] &&  return 1
    cmd=`cat /proc/$pid/cmdline | tr "\000" "\n"|head -n 1 |cut -d : -f 1`
    # Is this the expected child?
    [ "$cmd" != "$name" ] &&  return 1
    return 0
}


running()
{
# Check if the process is running looking at /proc
# (works for all users)

    # No pidfile, probably no daemon present
    [ ! -f "$PIDFILE" ] && return 1
    # Obtain the pid and check it against the binary name
    pid=`cat $PIDFILE`
    running_pid $pid $DAEMON || return 1
    return 0
}

force_stop() {
# Forcefully kill the process
    [ ! -f "$PIDFILE" ] && return
    if running ; then
        kill -15 $pid
        # Is it really dead?
        [ -n "$DODTIME" ] && sleep "$DODTIME"s
        if running ; then
            kill -9 $pid
            [ -n "$DODTIME" ] && sleep "$DODTIME"s
            if running ; then
                echo "Cannot kill $LABEL (pid=$pid)!"
                exit 1
            fi
        fi
    fi
    rm -f $PIDFILE
    return 0
}

case "$1" in
  start)
	check_ctl_dir
        echo -n "Starting $DESC: "
        start-stop-daemon --start --quiet --pidfile $PIDFILE \
                --exec $DAEMON -- $DAEMON_OPTS
	[ -n "$STARTUPTIME" ] && sleep "$STARTUPTIME"s
        if running ; then
            echo "$NAME."
        else
            echo " ERROR."
        fi
        ;;
  stop)
        echo -n "Stopping $DESC: "
        start-stop-daemon --stop --quiet --pidfile $PIDFILE \
                --exec $DAEMON
	[ -n "$DODTIME" ] && sleep "$DODTIME"s
        echo "$NAME."
        ;;
  force-stop)
        echo -n "Forcefully stopping $DESC: "
        force_stop
        if ! running ; then
            echo "$NAME."
        else
            echo " ERROR."
        fi
        ;;
  #reload)
        #
        #       If the daemon can reload its config files on the fly
        #       for example by sending it SIGHUP, do it here.
        #
        #       If the daemon responds to changes in its config file
        #       directly anyway, make this a do-nothing entry.
        #
        # echo "Reloading $DESC configuration files."
        # start-stop-daemon --stop --signal 1 --quiet --pidfile \
        #       /var/run/$NAME.pid --exec $DAEMON
  #;;
  force-reload)
        #
        #       If the "reload" option is implemented, move the "force-reload"
        #       option to the "reload" entry above. If not, "force-reload" is
        #       just the same as "restart" except that it does nothing if the
        #   daemon isn't already running.
        # check wether $DAEMON is running. If so, restart
        start-stop-daemon --stop --test --quiet --pidfile \
                /var/run/$NAME.pid --exec $DAEMON \
        && $0 restart \
        || exit 0
        ;;
  restart)
	check_ctl_dir
    	echo -n "Restarting $DESC: "
        if running ; then
		start-stop-daemon --stop --quiet --pidfile \
                	$PIDFILE --exec $DAEMON
	fi
        [ -n "$DODTIME" ] && sleep $DODTIME
        start-stop-daemon --start --quiet --pidfile \
                $PIDFILE --exec $DAEMON -- $DAEMON_OPTS
        echo "$NAME."
        ;;
  status)
    echo -n "$NAME is "
    if running ;  then
        echo "running"
    else
        echo " not running."
        exit 1
    fi
    ;;
  *)
        N=/etc/init.d/$NAME
        # echo "Usage: $N {start|stop|restart|reload|force-reload}" >&2
        echo "Usage: $N {start|stop|restart|force-reload|status|force-stop}" >&2
        exit 1
        ;;
esac

exit 0

