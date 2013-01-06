#! /bin/sh
### BEGIN INIT INFO
# Provides:          nasd
# Required-Start:    $remote_fs $syslog
# Required-Stop:     $remote_fs $syslog
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: start or stop the Network Audio System.
### END INIT INFO

test -f /usr/bin/nasd || exit 0

if test -f /etc/default/nas; then
    . /etc/default/nas
fi

case "$1" in
  start)
    echo Starting the Network Audio System
    start-stop-daemon --start --quiet --startas /usr/bin/start-nas \
						--exec /usr/bin/nasd -- $NASD_OPTS
    ;;
  stop)
    echo Stopping the Network Audio System
    start-stop-daemon --stop --quiet --exec /usr/bin/nasd
    ;;
  restart|force-reload)
    echo Restarting the Network Audio System
    start-stop-daemon --stop --quiet --exec /usr/bin/nasd
    start-stop-daemon --start --quiet --startas /usr/bin/start-nas \
						--exec /usr/bin/nasd -- $NASD_OPTS
    ;;
  *)
    echo "Usage: /etc/init.d/nas {start|stop|restart|force-reload}"
    exit 1
esac

exit 0
