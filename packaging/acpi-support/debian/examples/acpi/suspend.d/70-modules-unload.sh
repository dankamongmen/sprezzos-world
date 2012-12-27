#!/bin/sh

# Some modules survive better if they're left loaded
for x in $MODULES_WHITELIST; do
        MODULES=`echo $MODULES | sed s/$x//g`;
done

# Now remove various modules that might misbehave while suspending
for x in $MODULES; do
    modprobe -r $x 2>/dev/null;
done

