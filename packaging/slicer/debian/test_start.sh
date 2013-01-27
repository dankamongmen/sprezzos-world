#!/bin/sh

export LD_LIBRARY_PATH=/usr/lib/Slicer3/Modules/:/usr/lib/Slicer3/:$LD_LIBRARY_PATH
export TK_LIBRARY=/usr/share/tcltk/tk8.5/

exec /usr/bin/Slicer3-real
