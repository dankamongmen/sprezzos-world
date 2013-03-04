#!/bin/sh
#
### BEGIN INIT INFO
# Provides:          qpidd
# Required-Start:       $remote_fs $syslog $network
# Required-Stop:        $remote_fs $syslog $network
# Should-Start:
# Should-Stop:
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: start or stop qpidd
# Description:       Qpidd is an AMQP broker. It receives, stores, routes and
#                    forwards messages using the AMQP protcol.
### END INIT INFO

PATH=/sbin:/bin:/usr/sbin:/usr/bin
export SASL_CONF_PATH=/etc/sasl2

DAEMON=/usr/sbin/qpidd
NAME=qpidd
DESC="AMQP broker"
LOGDIR=/var/log/qpid
STARTTIME=2
PIDFILE=/var/run/qpid/$NAME.pid

test -x $DAEMON || exit 0

. /lib/lsb/init-functions

# Default options, these can be overriden by the information
# at /etc/default/$NAME
DAEMON_OPTS="--config /etc/qpid/qpidd.conf"
DIETIME=10
                        
LOGFILE=$LOGDIR/$NAME.log
DAEMONUSER=qpidd

# Include defaults if available
if [ -f /etc/default/qpidd ] ; then
	. /etc/default/qpidd
fi

# Check that the user exists (if we set a user)
# Does the user exist?
if [ -n "$DAEMONUSER" ] ; then
    if getent passwd | grep -q "^$DAEMONUSER:"; then
        # Obtain the uid and gid
        DAEMONUID=`getent passwd |grep "^$DAEMONUSER:" | awk -F : '{print $3}'`
        DAEMONGID=`getent passwd |grep "^$DAEMONUSER:" | awk -F : '{print $4}'`
    else
        log_failure_msg "The user $DAEMONUSER, required to run $NAME does not exist."
        exit 1
    fi
fi

# Create /var/run/qpid if it does not exist
if [ ! -d /var/run/qpid ]; then
	install -d -o$DAEMONUSER -m750 /var/run/qpid
fi

# Create /var/spool/qpidd if it does not exist
if [ ! -d /var/spool/qpidd ]; then
	install -d -o$DAEMONUSER -m750 /var/spool/qpidd
fi


set -e

running_pid() {
# Check if a given process pid's cmdline matches a given name
    pid=$1
    name=$2
    [ -z "$pid" ] && return 1
    [ ! -d /proc/$pid ] &&  return 1
    cmd=`cat /proc/$pid/cmdline | tr "\000" "\n"|head -n 1 |cut -d : -f 1`
    # Is this the expected server
    [ "$cmd" != "$name" ] &&  return 1
    return 0
}

running() {
# Check if the process is running looking at /proc
# (works for all users)

    # No pidfile, probably no daemon present
    [ ! -f "$PIDFILE" ] && return 1
    pid=`cat $PIDFILE`
    running_pid $pid $DAEMON || return 1
    return 0
}

start_server() {
# Start the process using the wrapper
        if [ -z "$DAEMONUSER" ] ; then
            start_daemon -p $PIDFILE $DAEMON -- $DAEMON_OPTS
            errcode=$?
        else
# if we are using a daemonuser then change the user id
            start-stop-daemon --start --quiet -b -m --pidfile $PIDFILE \
                        --chuid $DAEMONUSER \
                        --exec $DAEMON -- $DAEMON_OPTS
            errcode=$?
        fi
	return $errcode
}

stop_server() {
# Stop the process using the wrapper
        if [ -z "$DAEMONUSER" ] ; then
            killproc -p $PIDFILE $DAEMON
            errcode=$?
        else
# if we are using a daemonuser then look for process that match
            start-stop-daemon --stop --quiet --pidfile $PIDFILE \
                        --user $DAEMONUSER \
                        --exec $DAEMON
            errcode=$?
        fi

	return $errcode
}

reload_server() {
    [ ! -f "$PIDFILE" ] && return 1
    pid=pidofproc $PIDFILE # This is the daemon's pid
    # Send a SIGHUP
    kill -1 $pid
    return $?
}

force_stop() {
# Force the process to die killing it manually
	[ ! -e "$PIDFILE" ] && return
	if running ; then
		kill -15 $pid
	# Is it really dead?
		sleep "$DIETIME"s
		if running ; then
			kill -9 $pid
			sleep "$DIETIME"s
			if running ; then
				echo "Cannot kill $NAME (pid=$pid)!"
				exit 1
			fi
		fi
	fi
	rm -f $PIDFILE
}


case "$1" in
  start)
	log_daemon_msg "Starting $DESC" "$NAME"
        # Check if it's running first
        if running ;  then
            log_progress_msg "apparently already running"
            log_end_msg 0
            exit 0
        fi
        if start_server ; then
            # NOTE: Some servers might die some time after they start,
            # this code will detect this issue if STARTTIME is set
            # to a reasonable value
            [ -n "$STARTTIME" ] && sleep $STARTTIME # Wait some time 
            if  running ;  then
                # It's ok, the server started and is running
                log_end_msg 0
            else
                # It is not running after we did start
                log_end_msg 1
            fi
        else
            # Either we could not start it
            log_end_msg 1
        fi
	;;
  stop)
        log_daemon_msg "Stopping $DESC" "$NAME"
        if running ; then
            # Only stop the server if we see it running
			errcode=0
            stop_server || errcode=$?
            log_end_msg $errcode
        else
            # If it's not running don't do anything
            log_progress_msg "apparently not running"
            log_end_msg 0
            exit 0
        fi
        ;;
  force-stop)
        # First try to stop gracefully the program
        $0 stop
        if running; then
            # If it's still running try to kill it more forcefully
            log_daemon_msg "Stopping (force) $DESC" "$NAME"
			errcode=0
            force_stop || errcode=$?
            log_end_msg $errcode
        fi
	;;
  restart|force-reload)
        log_daemon_msg "Restarting $DESC" "$NAME"
		errcode=0
        stop_server || errcode=$?
        # Wait some sensible amount, some server need this
        [ -n "$DIETIME" ] && sleep $DIETIME
        start_server || errcode=$?
        [ -n "$STARTTIME" ] && sleep $STARTTIME
        running || errcode=$?
        log_end_msg $errcode
	;;
  status)

        log_daemon_msg "Checking status of $DESC" "$NAME"
        if running ;  then
            log_progress_msg "running"
            log_end_msg 0
        else
            log_progress_msg "apparently not running"
            log_end_msg 1
            exit 1
        fi
        ;;
  # Use this if the daemon cannot reload
  reload)
        log_warning_msg "Reloading $NAME daemon: not implemented, as the daemon"
        log_warning_msg "cannot re-read the config file (use restart)."
        ;;

  *)
	N=/etc/init.d/$NAME
	echo "Usage: $N {start|stop|force-stop|restart|force-reload|status}" >&2
	exit 1
	;;
esac

exit 0
