# Common handy shell script functions

l=/var/run/reboot-lock

reboot_lock () {
    exec 3<$l
    if ! flock --shared -w 0 3; then
	echo 2>&1 "Cannot acquire reboot lock."
	#exit 1
    fi
}

reboot_unlock () {
    flock --shared -u 3
}

now () {
    date -u +%F:%H:%M:%S
}

build_description () {
    case $1 in
        CD)
	    DESC="Full CD";;
        DVD)
            DESC="Full DVD";;
        BD)
            DESC="Blu-ray";;
        DLBD)
            DESC="Dual-layer Blu-ray";;
        KDE)
	    DESC="KDE CD";;
        LIGHTCD)
	    DESC="XFCE/lxde CD";;
        XFCECD)
	    DESC="XFCE CD";;
        LXDECD)
	    DESC="lxde CD";;
	*)
	    DESC="UNKNOWN";;
    esac
    echo "$DESC"
}    

calc_time () {
    echo $1 $2 | awk '
    {
        split($1, start, ":")
        start_time = (3600*start[2]) + (60*start[3]) + start[4]
        split($2, end, ":")
        end_time = (3600*end[2]) + (60*end[3]) + end[4]
        # Cope with going to a new day; do not worry about more than 1 day!
        if (start[1] != end[1]) { end_time += 86400 }
        time_taken = end_time - start_time
        hours = int(time_taken / 3600)
        time_taken -= (hours * 3600)
        minutes = int(time_taken / 60)
        time_taken -= (minutes * 60)
        seconds = time_taken
        printf("%dh%2.2dm%2.2ds\n", hours, minutes, seconds)
    }'
}

build_started () {
    export BUILDNAME=$1
    BUILDS_RUNNING="$BUILDS_RUNNING $BUILDNAME"
    export ${BUILDNAME}START=`now`
}

build_finished () {
    ARCH="$1"
    BUILDNAME="$2"
    BUILDNAMESTART="${BUILDNAME}START"
    start=${!BUILDNAMESTART}

    . $PUBDIRJIG/$ARCH/$BUILDNAME-trace

    time_spent=`calc_time $start $end`
    echo "  $ARCH $BUILDNAME build started at $start, ended at $end (took $time_spent), error $error"
    if [ $error -ne 0 ] ; then
        arch_error="$arch_error "$BUILDNAME"FAIL/$error/$end/$logfile"
    fi    
}

catch_parallel_builds () {
    # Catch parallel builds here                                                                                               
    while [ "$BUILDS_RUNNING"x != ""x  ] ; do
	BUILDS_STILL_RUNNING=""
	for BUILDNAME in $BUILDS_RUNNING; do
            if [ -e $PUBDIRJIG/$arch/$BUILDNAME-trace ] ; then
		build_finished $arch $BUILDNAME
            else
		BUILDS_STILL_RUNNING="$BUILDS_STILL_RUNNING $BUILDNAME"
            fi
	done
	BUILDS_RUNNING=$BUILDS_STILL_RUNNING
	if [ "$BUILDS_RUNNING"x != ""x  ] ; then
            sleep 1
	fi
    done
    if [ "$arch_error"x = ""x ] ; then
	arch_error="none"
    fi
    arch_end=`now`
    arch_time=`calc_time $arch_start $arch_end`
    echo "$ARCH build started at $arch_start, ended at $arch_end (took $arch_time), error(s) $arch_error"
}
