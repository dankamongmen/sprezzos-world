#!/bin/sh
#
# decnet.sh 
#
# Sets up the ethernet interface(s).
#
# This script MUST be run before TCP/IP is started.
#
### BEGIN INIT INFO
# Provides:          dnet-common decnet
# Required-Start:    $network
# Required-Stop:     $network
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: Set up ethernet interface(s) for DECnet
# Description:       Sets the MAC address of the ethernet card(s) for DECnet
#                    operation, and enables routing if requested.
### END INIT INFO
# ---------------------------------------------------------------------------
#
FLAGS="start 39 S .  stop 11 1 ."

#
# Interfaces to set the MAC address of are specified in /etc/default/decnet
# The variable DNET_INTERFACES should be either set to a list of interfaces
# or "all". If it is empty then no interfaces will be modified.
#
# The MAC address *must* be set for DECnet to work so if you do not use this
# program you must do it some other way.
#
# ROUTING specifies whether we should be a endnode (0), level 1 router (1)
# or aread router (2)
#
# PRIORITY specifies the routing priority. Defaults to 32. Note VMS defaults
# to 64, max is 127.

[ ! -f /sbin/setether ] && exit 0


[ -f /etc/default/decnet ] && . /etc/default/decnet

interfaces="$DNET_INTERFACES"

ADDR="`grep executor /etc/decnet.conf 2>/dev/null | cut -f2`"

setether="/sbin/setether $ADDR $interfaces"


set_routing()
{

# Enable routing if required
if [ -n "$ROUTING" ]
then

# Set a default priority lower than VMS
    if [ -z "$PRIORITY" ]
    then
	PRIORITY=32
    fi

    for i in /proc/sys/net/decnet/conf/eth[0-9]*
    do
      echo "$1"        > $i/forwarding
      echo "$PRIORITY" > $i/priority
    done 
fi

}



case $1 in
   start)
     if [ ! \( -f /etc/decnet.conf -a -n "$ADDR" \) ]
     then
       echo "DECnet not started as it is not configured."
       exit 0
     fi

     # If there is no DECnet in the kernel then try to load it.
     if [ ! -f /proc/net/decnet ]
     then
       modprobe decnet
       if [ ! -f /proc/net/decnet ]
       then
         echo "DECnet not started as it is not in the kernel."
	 exit 0
       fi
     fi

     echo -n "Starting DECnet..."
     $setether
     set_routing $ROUTING
     echo "done."
     ;;

   stop)
     set_routing 0
     ;;

   restart|reload|force-reload)
     ;;

   *)
     echo "Usage $0 {start|stop|restart|reload|force-reload}"
     ;;
esac

exit 0
