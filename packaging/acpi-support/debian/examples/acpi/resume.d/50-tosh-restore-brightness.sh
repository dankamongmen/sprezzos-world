#!/bin/sh

if [ "x$TOSH_BRIGHTNESS" != "x" ]; then
  # To reset the brightness...
  echo 'brightness : '$TOSH_MAXBRIGHT > $TOSH_LCD
  echo 'brightness : 0' > $TOSH_LCD
  # And then restore it...
  echo 'brightness : '$TOSH_BRIGHTNESS > $TOSH_LCD
  unset TOSH_BRIGHTNESS TOSH_MAXBRIGHT
fi

unset TOSH_LCD
