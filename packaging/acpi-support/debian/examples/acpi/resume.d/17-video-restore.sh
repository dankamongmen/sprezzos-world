#!/bin/sh

# Attempt to restore some video card state
if [ x$SAVE_VBE_STATE = xtrue ]; then
  vbetool vbestate restore <$VBESTATE
  if [ $VBEMODE != "3" ]; then
    vbetool vbemode set $VBEMODE;
  else
    vbetool vgamode set 3;
  fi
fi


