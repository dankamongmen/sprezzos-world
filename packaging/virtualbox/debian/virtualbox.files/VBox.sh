#!/bin/sh
#
# written by Patrick Winnertz <patrick.winnertz@skolelinux.org> and
# Michael Meskes <meskes@debian.org>
# and placed under GPLv2
#
# this is based on a script by
# InnoTek VirtualBox
#
# Copyright (C) 2006 InnoTek Systemberatung GmbH
#
# This file is part of VirtualBox Open Source Edition (OSE), as
# available from http://www.virtualbox.org. This file is free software;
# you can redistribute it and/or modify it under the terms of the GNU
# General Public License as published by the Free Software Foundation,
# in version 2 as it comes in the "COPYING" file of the VirtualBox OSE
# distribution. VirtualBox OSE is distributed in the hope that it will
# be useful, but WITHOUT ANY WARRANTY of any kind.

PATH="/usr/bin:/bin:/usr/sbin:/sbin"

# VirtualBox installation directory
INSTALL_DIR="/usr/lib/virtualbox"

# We don't distribute this file anymore. However, if it is still there we use
# it, just to be sure we stay compatible with older versions.
[ -f /etc/vbox/vbox.cfg ] && . /etc/vbox/vbox.cfg

if [ "$VBOX_USER_HOME" = "" ]; then
    if [ ! -d "$HOME/.VirtualBox" ]; then
        mkdir -p "$HOME/.VirtualBox"
    fi
    LOG="$HOME/.VirtualBox/VBoxSVC.log"
else
    if [ ! -d "$VBOX_USER_HOME" ]; then
        mkdir -p "$VBOX_USER_HOME"
    fi
    LOG="$VBOX_USER_HOME/VBoxSVC.log"
fi

# Note: This script must not fail if the module was not successfully installed
#       because the user might not want to run a VM but only change VM params!

if [ ! -c /dev/vboxdrv ]; then
    cat << EOF
WARNING: The character device /dev/vboxdrv does not exist.
	 Please install the virtualbox-ose-dkms package and the appropriate
	 headers, most likely linux-headers-$(uname -r | cut -d- -f3).

	 You will not be able to start VMs until this problem is fixed.
EOF
fi

APP=`which $0`
APP=${APP##/*/}
case "$APP" in
  VirtualBox|virtualbox)
    exec "$INSTALL_DIR/VirtualBox" "$@"
  ;;
  VBoxHeadless|vboxheadless)
    exec "$INSTALL_DIR/VBoxHeadless" "$@"
  ;;
  VBoxManage|vboxmanage)
    exec "$INSTALL_DIR/VBoxManage" "$@"
  ;;
  VBoxSDL|vboxsdl)
    exec "$INSTALL_DIR/VBoxSDL" "$@"
  ;;
  vditool)
    exec "$INSTALL_DIR/vditool" "$@"
  ;;
  vboxwebsrv)
    exec "$INSTALL_DIR/vboxwebsrv" "$@"
  ;;
  *)
    echo "Unknown application - $APP"
  ;;
esac
