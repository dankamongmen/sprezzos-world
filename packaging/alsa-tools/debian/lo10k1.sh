#!/bin/bash

PIPE_NAME=/var/run/ld10k1/ld10k1.socket

if [ -r /etc/default/ld10k1 ]; then
    . /etc/default/ld10k1
fi

# FIXME: exec -a is a bashism
exec -a lo10k1 lo10k1.bin --pipe_name $PIPE_NAME "$@"
