#!/bin/sh

# And make sure that the screen is on
if [ x$USE_DPMS = xtrue ]; then
  vbetool dpms on
fi

