#! /bin/sh
### BEGIN INIT INFO
# Provides:          umountiscsi.sh
# Required-Start:
# Required-Stop:     $network $remote_fs sendsigs open-iscsi
# Default-Start:
# Default-Stop:      0 1 6
# Short-Description: Unmounts iSCSI filesystems
# Description:       Unmounts iSCSI filesystems
### END INIT INFO

. /lib/init/vars.sh

. /lib/lsb/init-functions


# Include defaults if available
if [ -f /etc/default/open-iscsi ]; then
	. /etc/default/open-iscsi
fi


do_stop () {
    log_daemon_msg "Unmounting iscsi-backed filesystems"

    umount_fail=0

    if [ $HANDLE_NETDEV -eq 1 ]; then
	    log_progress_msg "Unmounting all devices marked _netdev";
	    umount -a -O _netdev >/dev/null 2>&1
    fi


    # Now handle iSCSI LVM Volumes
    if [ -n "$LVMGROUPS" ]; then
    	log_daemon_msg "Deactivating iSCSI volume groups"
    	for vg in "$LVMGROUPS"; do
    		log_progress_msg $vg
    		vgchange --available=n $vg
		if [ $? -ne 0 ]; then
			log_warning_msg "Cannot deactivate Volume Group $vg"
			umount_fail=1
		fi
    	done
    	log_end_msg 0
    fi

    for HOST_DIR in /sys/devices/platform/host*; do
	if ! [ -d $HOST_DIR/iscsi_host* ]; then
	    continue
	fi
        for SESSION_DIR in $HOST_DIR/session*; do
		if ! [ -d $SESSION_DIR/target* ]; then
			continue
		fi
		for BLOCK_FILE in $SESSION_DIR/target*/*\:*/block/*; do
			BLOCK_DEV=`echo "$BLOCK_FILE" | sed 's/.*block\///'`
			DOS_PARTITIONS="`awk "/^\/dev\/$BLOCK_DEV/ { print \\$2; }" < /proc/mounts`"
			for DEVICE in $DOS_PARTITIONS; do
				#log_progress_msg $DEVICE
				#echo $DEVICE
				umount $DEVICE
				exit_status=$?
				if ! [ $exit_status -eq 0 ]; then
					umount_fail=1
					log_warning_msg "Could not unmount $DEVICE"
				fi
			done
		done
	done
    done

    if [ $umount_fail -ne 0 ]; then
	    log_end_msg 1
	    exit 1
    else
	    log_end_msg 0
	    exit 0
    fi
}

case "$1" in
    start)
	# No-op
        ;;
    restart|reload|force-reload)
	echo "Error: argument '$1' not supported" >&2
	exit 3
	;;
    stop|"")
        do_stop
	;;
    *)
        echo "Usage: umountiscsi.sh [start|stop]" >&2
	exit 3
	;;
esac
