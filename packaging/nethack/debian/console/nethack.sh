#!/bin/sh

set -e

HACKSTYLE=console

if [ -z "$NETHACKOPTIONS" ]; then
  if [ -e $HOME/.nethackrc ]; then
    NETHACKOPTIONS=$HOME/.nethackrc
    export NETHACKOPTIONS
  elif [ -e $HOME/.nethackrc.$HACKSTYLE ]; then
    NETHACKOPTIONS=$HOME/.nethackrc.$HACKSTYLE
    export NETHACKOPTIONS
  elif [ -e $HOME/.nethackrc.tty ]; then
    NETHACKOPTIONS=$HOME/.nethackrc.tty
    export NETHACKOPTIONS
  else
    NETHACKOPTIONS=/etc/nethack/nethackrc.$HACKSTYLE
    export NETHACKOPTIONS
  fi
fi

HACKDIR=/usr/lib/games/nethack
export HACKDIR
HACK=$HACKDIR/nethack-$HACKSTYLE

cd $HACKDIR
exec $HACK "$@"
