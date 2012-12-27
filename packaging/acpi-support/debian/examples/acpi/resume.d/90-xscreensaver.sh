#!/bin/sh

# now, we should poke xscreensaver so you get a dialog
if pidof xscreensaver > /dev/null; then 
    d=/tmp/.X11-unix
    for x in $d/*; do
	displaynum=${x#$d/X}
	getXuser;
	if [ x"$XAUTHORITY" != x"" ]; then
	    export DISPLAY=":$displaynum"
	    su $user -c "(xscreensaver-command -deactivate)"
	fi
    done
fi
