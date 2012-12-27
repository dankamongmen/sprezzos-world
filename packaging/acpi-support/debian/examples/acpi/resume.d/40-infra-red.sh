#!/bin/sh

# Restart IR if necessary
if [ -f /var/run/irdadev ] && [ x$RESTART_IRDA = xtrue ]; then
    rm /var/run/irdadev;
    /etc/init.d/irda-setup start;
    /etc/init.d/irda-utils start;
fi;

