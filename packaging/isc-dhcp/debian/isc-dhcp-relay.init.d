#!/bin/sh
#
#

### BEGIN INIT INFO
# Provides:          isc-dhcp-relay
# Required-Start:    $remote_fs $network
# Required-Stop:     $remote_fs $network
# Should-Start:      $local_fs
# Should-Stop:       $local_fs
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: DHCP relay
# Description:       Dynamic Host Configuration Protocol Relay
### END INIT INFO

# It is not safe to start if we don't have a default configuration...
if [ ! -f /etc/default/isc-dhcp-relay ]; then
	echo "/etc/default/isc-dhcp-relay does not exist! - Aborting..."
	echo "Run 'dpkg-reconfigure isc-dhcp-relay' to fix the problem."
	exit 1
fi

# Read init script configuration (interfaces the daemon should listen on
# and the DHCP server we should forward requests to.)
[ -f /etc/default/isc-dhcp-relay ] && . /etc/default/isc-dhcp-relay

# Build command line for interfaces (will be passed to dhrelay below.)
IFCMD=""
if test "$INTERFACES" != ""; then
	for I in $INTERFACES; do
		IFCMD=${IFCMD}"-i "${I}" "
	done
fi

DHCRELAYPID=/var/run/dhcrelay.pid

case "$1" in
	start)
		start-stop-daemon --start --quiet --pidfile $DHCRELAYPID \
			--exec /usr/sbin/dhcrelay -- -q $OPTIONS $IFCMD $SERVERS
		;;
	stop)
		start-stop-daemon --stop --quiet --pidfile $DHCRELAYPID
		;;
	restart | force-reload)
		$0 stop
		sleep 2
		$0 start
		;;
	*)
		echo "Usage: /etc/init.d/isc-dhcp-relay {start|stop|restart|force-reload}"
		exit 1 
esac

exit 0
