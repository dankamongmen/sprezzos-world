#! /bin/sh

# script to trick the build daemons and output something, if there is
# still test/build activity

# $1: primary file to watch. if there is activity on this file, we do nothing
# $2+: files to watch to look for activity despite no output in $1
#      if the files are modified or are newly created, then the message
#      is printed on stdout.
#      if nothing is modified, don't output anything (so the buildd timeout
#      hits).

pidfile=logwatch.pid
timeout=3600
message='\nlogwatch still running\n'

usage()
{
    echo >&2 "usage: `basename $0` [-p <pidfile>] [-t <timeout>] [-m <message>]"
    echo >&2 "           <logfile> [<logfile> ...]"
    exit 1
}

while [ $# -gt 0 ]; do
    case $1 in
    -p)
	pidfile=$2
	shift
	shift
	;;
    -t)
	timeout=$2
	shift
	shift
	;;
    -m)
	message="$2"
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

[ $# -gt 0 ] || usage

logfile="$1"
shift
otherlogs="$@"

cleanup()
{
    rm -f $pidfile
    exit 0
}

#trap cleanup 0 1 3 15

echo $$ > $pidfile

update()
{
    _logvar=$1
    _othervar=$2

    # logfile may not exist yet
    if [ -r $logfile ]; then
	_logtail="`tail -10 $logfile | md5sum` $f"
    else
	_logtail="does not exist: $logfile"
    fi
    eval $_logvar="'$_logtail'"

    _othertails=''
    for f in $otherlogs; do
	if [ -r $f ]; then
	    _othertails="$_othertails `tail -10 $f | md5sum` $f"
	else
	    _othertails="$_othertails does not exist: $f"
	fi
    done
    eval $_othervar="'$_othertails'"
}

update logtail othertails
while true; do
    sleep $timeout
    update newlogtail newothertails
    if [ "$logtail" != "$newlogtail" ]; then
	# there is still action in the primary logfile. do nothing.
	logtail="$newlogtail"
    elif [ "$othertails" != "$newothertails" ]; then
	# there is still action in the other log files, so print the message
	/bin/echo -e $message
	othertails="$newothertails"
    else
	# nothing changed in the other log files. maybe a timeout ...
	:
    fi
done
