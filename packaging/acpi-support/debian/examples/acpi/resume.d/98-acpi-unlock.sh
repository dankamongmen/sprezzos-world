#!/bin/sh

#Let acpid process events again
(sleep 10 && rm /var/lock/acpisleep)&

