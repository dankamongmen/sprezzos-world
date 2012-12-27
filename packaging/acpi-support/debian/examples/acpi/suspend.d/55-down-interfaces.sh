#!/bin/sh

pccardctl eject

# Get rid of any currently running dhclients
killall dhclient dhclient3 2>/dev/null

# First do all network interfaces that were brought up by ifupdown. These are
# the only interfaces that we bring up on resume (the other ones are probably
# managed by other tools, such as NetworkManager, and will be brought up
# automatically). Due to logical interfaces, the interfaces that we pass to
# ifdown are DIFFERENT than those that we pass to ifup, see Debian BTS #475002.
IFDOWN_INTERFACES="`cat /var/run/network/ifstate | sed 's/=.*//'`"
IFUP_INTERFACES="`cat /var/run/network/ifstate`"

must_control_interface()
{
  # Always skip lo
  test "$1" = "lo" && return 1
  for i in $SKIP_INTERFACES; do
      # Skip if listed in $SKIP_INTERFACES
      echo "$1" | grep -q "^$i" && return 1
  done
  return 0
}


# Shut down the interfaces (except lo, which can and should be kept up)
for x in $IFDOWN_INTERFACES; do
    if must_control_interface $x ; then
 	if [ -x /sbin/wpa_action ] && wpa_action $x check; then
	 	# Ugly workaround for the fact that wpasupplicant does not hook
	 	# into ifdown itself. See Debian BTS #473184, #390884.
 		wpa_action $x stop
 	else
 		ifdown $x
 	fi
    fi
done

# Tell NetworkManager to shut down networking
dbus-send --system --dest=org.freedesktop.NetworkManager /org/freedesktop/NetworkManager org.freedesktop.NetworkManager.sleep

# Find the remaining running network interfaces...
INTERFACES=`/sbin/ifconfig | awk '/^[^ ]+/ {print $1}'`

# And shut them down (except lo, which can and should be kept up)
for x in $INTERFACES; do
    if must_control_interface $x ; then
      ifconfig $x down
    fi
done

