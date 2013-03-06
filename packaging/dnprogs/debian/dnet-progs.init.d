#!/bin/sh
#
# dnet-progs.sh
#
# Starts/stops DECnet processes
#
# --------------------------------------------------------------------------
#
### BEGIN INIT INFO
# Provides:          dnet-progs
# Required-Start:    $network $local_fs $remote_fs
# Required-Stop:     $network $local_fs $remote_fs
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: Starts DECnet daemons
# Description:       Starts dnetd (the DECnet superserver) and other
#                    optional daemons
### END INIT INFO
#
# Daemons to start are defined in /etc/default/decnet
#

[ -f /etc/default/decnet ] && . /etc/default/decnet

ADDR="`grep executor /etc/decnet.conf 2> /dev/null | cut -f2`"

# Don't issue any messages if DECnet is not configured as
# dnet-common will have taken care of those.
if [ ! -f /etc/decnet.conf -o ! -f /proc/net/decnet -o ! -n "$ADDR" ]
then
  exit 0
fi

case $1 in
   start)

     echo -n "Starting DECnet daemons:"

     for i in $DNET_DAEMONS
     do
       if [ -f /usr/sbin/$i ]
       then
         echo -n " $i"
         eval "flags=\$${i}_FLAGS"
         start-stop-daemon --start --quiet --exec /usr/sbin/$i -- $flags
       fi
     done
     echo "."
     ;;

   stop)
     echo -n "Stopping DECnet daemons:"
     for i in $DNET_DAEMONS
     do
       echo -n " $i"
       start-stop-daemon --stop --quiet --exec /usr/sbin/$i
     done
     echo "."
     ;;

   # DECnet daemons all automatically reconfigure.
   reload)
     ;;

   restart|force-reload)
     echo -n "Restarting DECnet daemons:"
     for i in $DNET_DAEMONS
     do
       echo -n " $i"
       eval "flags=\$${i}_FLAGS"
       start-stop-daemon --stop --quiet --exec /usr/sbin/$i
       start-stop-daemon --start --quiet --exec /usr/sbin/$i  $flags
     done
     echo "."
     ;;

   *)
     echo "Usage $0 {start|stop|restart|reload|force-reload}"
     ;;
esac

exit 0
