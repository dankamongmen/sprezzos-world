#!/bin/bash

if [ -x /usr/sbin/855resolution ]; then
    . /etc/default/855resolution
    if [ "$MODE" != "" ] && [ "$XRESO" != "" ] && [ "$YRESO" != "" ]; then
	/etc/init.d/855resolution start;
    fi
fi
