#!/bin/sh
ORIG_VERSION=3.1
TARGET_VERSION=3.2
ORIG_VERSION_2=3_1
TARGET_VERSION_2=3_2

LIST=`ls debian/*$ORIG_VERSION*`
for F in $LIST; do
    TARGET=`echo $F|sed -e "s|$ORIG_VERSION|$TARGET_VERSION|g"`
    mv $F $TARGET
done
LIST=`ls debian/*$TARGET_VERSION* debian/control`
for F in $LIST; do
    sed -i -e "s|$ORIG_VERSION_2|$TARGET_VERSION_2|g" $F
    sed -i -e "s|$ORIG_VERSION|$TARGET_VERSION|g" $F
done

