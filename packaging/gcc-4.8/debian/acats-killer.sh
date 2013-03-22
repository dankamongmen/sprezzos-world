#! /bin/sh

# on ia64 systems, the acats hangs in unaligned memory accesses.
# kill these testcases.

pidfile=acats-killer.pid

usage()
{
    echo >&2 "usage: `basename $0` [-p <pidfile>] <ada logfile> <next logfile>"
    exit 1
}

while [ $# -gt 0 ]; do
    case $1 in
    -p)
	pidfile=$2
	shift
	shift
	;;
    -*)
	usage
	;;
    *)
	break
    esac
done

[ $# -eq 2 ] || usage

logfile=$1
stopfile=$2
interval=30

echo $$ > $pidfile

while true; do
    if [ -f "$stopfile" ]; then
	echo "`basename $0`: finished."
	rm -f $pidfile
	exit 0
    fi
    sleep $interval
    if [ ! -f "$logfile" ]; then
	continue
    fi
    pids=$(ps aux | awk '/testsuite\/ada\/acats\/tests/ { print $2 }')
    if [ -n "$pids" ]; then
	sleep $interval
        pids2=$(ps aux | awk '/testsuite\/ada\/acats\/tests/ { print $2 }')
	if [ "$pids" = "$pids2" ]; then
	    #echo kill: $pids
	    kill $pids
	    sleep 1
            pids2=$(ps aux | awk '/testsuite\/ada\/acats\/tests/ { print $2 }')
	    if [ "$pids" = "$pids2" ]; then
	        #echo kill -9: $pids
	        kill -9 $pids
	    fi
	fi
    fi
done
