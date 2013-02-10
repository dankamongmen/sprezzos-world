#!/bin/sh
### BEGIN INIT INFO
# Provides:          console-screen
# Required-Start:    $local_fs $remote_fs
# Required-Stop:     
# Default-Start:     S
# Default-Stop:      
# Description: Set console screen modes and fonts
# Short-Description:	Prepare console
### END INIT INFO

# Don't set 'set -e' for production use. -- amck, 2010-01-11
# set -e

#
# This is the boot script for the `console-tools' package.
#
# It loads parameters from /etc/console-tools/config, maybe loads
# default screen-font, screen font-map, and application charset-map,
# and maybe start "vcstime"
#
# (c) 1997 Yann Dirson

if [ -r /etc/console-tools/config ] ; then
    . /etc/console-tools/config
fi

if [ -d /etc/console-tools/config.d ]; then
    for i in `run-parts --list /etc/console-tools/config.d `; do
       . $i
    done
fi

. /lib/lsb/init-functions

PATH=/sbin:/bin:/usr/sbin:/usr/bin
SETFONT="/usr/bin/consolechars"
SETFONT_OPT=""
CHARSET="/usr/bin/charset"
VCSTIME="/usr/sbin/vcstime"

# Different device name for 2.6 kernels and devfs
if [ `uname -r | cut -f 2 -d .` = 6 ] && [ -e /dev/.devfsd ]; then
    VCSTIME_OPT="-2 /dev/vcsa0"
else
    VCSTIME_OPT=""
fi



# set DEVICE_PREFIX depending on devfs/udev
if [ -d /dev/vc ]; then
    DEVICE_PREFIX="/dev/vc/"
else
    DEVICE_PREFIX="/dev/tty"
fi

ENV_FILE=''
[ -r /etc/environment ] && ENV_FILE="/etc/environment"
[ -r /etc/default/locale ] && ENV_FILE="/etc/default/locale"
[ "$ENV_FILE" ] && CHARMAP=$(set -a && . "$ENV_FILE" && locale charmap)
if test "$CHARMAP" = "UTF-8" ; then
	UNICODE_MODE=yes
fi

unicode_start_stop ()
{
    vc=$1
    if [ -n "$UNICODE_MODE" -a -z "`eval echo \\$CONSOLE_MAP_vc$vc`" ]; then
        action=unicode_start
    else
        action=unicode_stop
    fi
    if [ "${CONSOLE_FONT}" ]; then
        $action "${CONSOLE_FONT}" < ${DEVICE_PREFIX}$vc > ${DEVICE_PREFIX}$vc 2> /dev/null || true
    else
        $action < ${DEVICE_PREFIX}$vc > ${DEVICE_PREFIX}$vc 2> /dev/null || true
    fi
}

reset_vga_palette ()
{
	if [ -f /proc/fb ]; then
           # They have a framebuffer device.
           # That means we have work to do...
	    echo -n "]R"
	fi
}

setup_fonts ()
{
    # be sure the main program is installed
    [ -x "${SETFONT}" ] || return 0

    VT="no"
    # If we can't access the console, quit
    CONSOLE_TYPE=`fgconsole 2>/dev/null` || return 0

    if [ ! $CONSOLE_TYPE = "serial" ]  ; then
	if [ "x" != "x`readlink /proc/self/fd/0 | grep -e /dev/vc \
            -e '/dev/tty[^p]' -e /dev/console`" ] ; then
	    VT="yes"
	    reset_vga_palette
	fi
    fi

    [ $VT = "no" ] && return 0

    # start vcstime
    if [ "${DO_VCSTIME}" = "yes" -a -x ${VCSTIME} ] ; then
	[ "$VERBOSE" != "no" ] && log_action_begin_msg "Starting clock on text console"
	${VCSTIME} ${VCSTIME_OPT} &
        [ "$VERBOSE" != "no" ] && log_action_end_msg 0
    fi

    # Default to Unicode mode for new VTs?
    if [ -f /sys/module/vt/parameters/default_utf8 ]; then
        if [ -n "$UNICODE_MODE" ]; then
            echo 1
        else
            echo 0
        fi > /sys/module/vt/parameters/default_utf8
    fi

    # Try to be cleverer and run for all consoles, but this is run
    # _before_ getty and so only one console running. So,
    # Set for the first 6 VCs (as they are allocated in /etc/inittab)
    NUM_CONSOLES=`fgconsole --next-available`
    NUM_CONSOLES=$(($NUM_CONSOLES - 1))
    [ ${NUM_CONSOLES} -eq 1 ] && NUM_CONSOLES=6

    # Global default font+sfm
    if [ "${SCREEN_FONT}" ]
	then
	[ "$VERBOSE" != "no" ] && log_action_begin_msg "Setting up general console font"
	SCREEN_FONT="-f ${SCREEN_FONT}"

	# maybe use an external SFM
	[ "${SCREEN_FONT_MAP}" ] && SCREEN_FONT_MAP="-u ${SCREEN_FONT_MAP}"

	i=1
	while [ $i -le $NUM_CONSOLES ]
	    do
            unicode_start_stop $i
	    if ! ${SETFONT} --tty=${DEVICE_PREFIX}$i ${SETFONT_OPT} ${SCREEN_FONT} ${SCREEN_FONT_MAP} ; then
	      [ "$VERBOSE" != "no" ] && log_action_end_msg 1
	      break
	    elif [ "$i" -eq "$NUM_CONSOLES" ]; then
	      [ "$VERBOSE" != "no" ] && log_action_end_msg 0
	    fi
	    i=$(($i + 1))
	done
    fi


    # Per-VC font+sfm
    VCS="`set | grep '^SCREEN_FONT_vc[0-9]*=' | sed -e 's/^SCREEN_FONT_vc//' -e 's/=.*//'`"
    if [ "${VCS}" ]
	then
	[ "$VERBOSE" != "no" ] && log_action_begin_msg "Setting up per-VC fonts"
	for vc in ${VCS}
	  do
	    # extract FONTNAME info from variable setting
	  eval font=\$SCREEN_FONT_vc$vc
	  # eventually find an associated SFM
	  eval sfm=\${SCREEN_FONT_MAP_vc${vc}}
	  [ "$sfm" ] && sfm="-u $sfm"

	  ${SETFONT} --tty=${DEVICE_PREFIX}$vc ${SETFONT_OPT} -f $font $sfm
	done
	[ "$VERBOSE" != "no" ] && log_action_end_msg 0
    fi

}

setup_keymaps() {

    # Global ACM
    [ "${APP_CHARSET_MAP}" ] && ${CHARSET} G0 ${APP_CHARSET_MAP}


    # Per-VC ACMs
    VCS="`set | grep '^APP_CHARSET_MAP_vc[0-9]*=' | sed -e 's/^APP_CHARSET_MAP_vc//' -e 's/=.*//'`"
    if [ "${VCS}" ]
	then
	[ "$VERBOSE" != "no" ] && log_action_begin_msg "Setting up per-VC ACMs"
	for vc in ${VCS}
	  do
	    # extract FONTNAME info from variable setting
	  eval acm=\$APP_CHARSET_MAP_vc$vc
	  ${CHARSET} --tty="${DEVICE_PREFIX}$vc" G0 "$acm"
	done
	[ "$VERBOSE" != "no" ] && log_action_end_msg 0
    fi


}

setup_screensaver() {
    # screensaver stuff
    setterm_args=""
    if [ "$BLANK_TIME" ]; then
        setterm_args="$setterm_args -blank $BLANK_TIME"
    fi
    if [ "$BLANK_DPMS" ]; then
        setterm_args="$setterm_args -powersave $BLANK_DPMS"
    fi
    if [ "$POWERDOWN_TIME" ]; then
        setterm_args="$setterm_args -powerdown $POWERDOWN_TIME"
    fi
    if [ "$setterm_args" ]; then
        setterm $setterm_args 
    fi

}

setup_keyboard() {
    # Keyboard rate and delay
    KBDRATE_ARGS=""
    if [ -n "$KEYBOARD_RATE" ]; then
        KBDRATE_ARGS="-r $KEYBOARD_RATE"
    fi
    if [ -n "$KEYBOARD_DELAY" ]; then
        KBDRATE_ARGS="$KBDRATE_ARGS -d $KEYBOARD_DELAY"
    fi
    if [ -n "$KBDRATE_ARGS" ]; then
	[ "$VERBOSE" != "no" ] && log_action_begin_msg "Setting keyboard rate and delay"
        kbdrate -s $KBDRATE_ARGS
	[ "$VERBOSE" != "no" ] && log_action_end_msg 0
    fi

    # Inform gpm if present, of potential changes.
    if [ -f /var/run/gpm.pid ]; then
	kill -WINCH `cat /var/run/gpm.pid` 2> /dev/null
    fi

    # Allow user to remap keys on the console
    if [ -r /etc/console-tools/remap ]
	then
	dumpkeys < ${DEVICE_PREFIX}1 | sed -f /etc/console-tools/remap | loadkeys --quiet
    fi
    # Set LEDS here
    if [ "$LEDS" != "" ]
	then
	i=1
	while [ $i -le $NUM_CONSOLES ]
	  do
          setleds -D $LEDS < $DEVICE_PREFIX$i
	  i=$(($i + 1))
	done
    fi
}

case "$1" in
    start|reload|restart|force-reload|*)
       # Changing the fonts make usplash exit.  Avoid this.
       if pidof usplash ; then
           log_action_msg "Setting console screen modes"
            setup_keymaps
            setup_screensaver
            setup_keyboard
       else
           log_action_msg "Setting console screen modes and fonts"
            setup_fonts
            setup_keymaps
            setup_screensaver
            setup_keyboard
       fi
	;;
    stop)
	;;
esac
