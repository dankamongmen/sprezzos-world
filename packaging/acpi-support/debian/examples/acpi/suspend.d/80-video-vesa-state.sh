#!/bin/sh

if [ x$SAVE_VBE_STATE = "xtrue" ]; then
  # Check if we're in a VESA mode - if so, we need to do things more
  # awkwardly. Otherwise, just use the state from boot.
  VBEMODE=$(vbetool vbemode get || echo FAILED);
  if ( echo "$VBEMODE" | grep -q FAILED ); then
 	echo "vbetool returned an error while requesting the vbe mode."
  else
    if [ "$VBEMODE" != "3" ]; then
      vbetool vbemode set 3;
      vbetool vbestate save >$VBESTATE;
    fi
  fi
fi

