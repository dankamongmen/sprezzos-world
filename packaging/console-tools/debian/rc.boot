#!/bin/sh

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

SETFONT="/usr/bin/consolechars"
SETFONT_OPT=""
VCSTIME="/usr/sbin/vcstime"

# start vcstime
if [ "${DO_VCSTIME}" = "yes" -a -x ${VCSTIME} ] ; then
  echo -n Starting clock on text console: `basename ${VCSTIME}`
  ${VCSTIME} &
  echo .
fi

# handle font stuff if anything was asked for
if [ -x "${SETFONT}" -a "${SCREEN_FONT}${SCREEN_FONT_MAP}${APP_CHARSET_MAP}" != "" ] ; then
    # load font-map if asked for
    if [ "${SCREEN_FONT}" != "" ] ; then
        SCREEN_FONT="-f ${SCREEN_FONT}"
    fi

    # load font-map if asked for
    if [ "${SCREEN_FONT_MAP}" != "" ] ; then
        SCREEN_FONT_MAP="-u ${SCREEN_FONT_MAP}"
    fi

    # load map if asked for
    if [ "${APP_CHARSET_MAP}" != "" ] ; then
        APP_CHARSET_MAP="-m ${APP_CHARSET_MAP}"
    fi
    
    echo -n "Setting up console font and friends..."
    ${SETFONT} ${SETFONT_OPT} ${SCREEN_FONT} ${SCREEN_FONT_MAP} ${APP_CHARSET_MAP} && echo done.
fi
