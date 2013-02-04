# This file provides some common code that is intented to be called
# by the various boot-<arch> scripts.


# Expand %ARCH% variable in envvars for location of D-I images
DI_WWW_HOME="$(echo "$DI_WWW_HOME" | sed -e "s|%ARCH%|$ARCH|g")"
DI_DIR="$(echo "$DI_DIR" | sed -e "s|%ARCH%|$ARCH|g")"

# Only i386 and amd64 support desktop selection with the 'light' and 'all'
# desktops; make sure other arches get a working config
if [ "$ARCH" != i386 ] && [ "$ARCH" != amd64 ]; then
    case $DESKTOP in
        all|gnome)
            DESKTOP=
            KERNEL_PARAMS="$(echo "$KERNEL_PARAMS" | \
                sed -r "s/desktop=[^ ]* ?//")"
            ;;
        light)
            DESKTOP=xfce
            KERNEL_PARAMS="$(echo "$KERNEL_PARAMS" | \
                sed -r "s/(desktop=)light/\1xfce/")"
            ;;
    esac
fi


# install_languages decompacts the language packs, you should give the path
# to the CD temporary tree.
# This function should be called for all bootable images.
install_languages() {
    # Param $1 is the CD directory
    if [ -f "$MIRROR/dists/$DI_CODENAME/main/disks-$ARCH/current/xlp.tgz" ]
    then
	mkdir $1/.xlp
	(cd $1/.xlp; \
	tar zxf $MIRROR/dists/$DI_CODENAME/main/disks-$ARCH/current/xlp.tgz )
    fi
}

# Add an option to the mkisofs options for this CD _only_ if it's not
# already set. $1 is the opts file location, "$2" is the new
# option. Call this with _logical groupings_ of options
add_mkisofs_opt() {
   OPTS_FILE=$1
   NEW_OPT="$2"

   if ! ( grep -q -- "$NEW_OPT" $OPTS_FILE 2>/dev/null) ; then
       echo -n "$NEW_OPT " >> $OPTS_FILE
   fi
}

variant_enabled() {
    VARIANT=$1

    echo "$VARIANTS" | grep -qw "$VARIANT"
    return $?
}

# Wrapper around which_deb which looks in all relevant distributions
find_pkg_file() {
    local pkgfile
    for dist in $DI_CODENAME $CODENAME; do
        pkgfile=$($BASEDIR/tools/which_deb $MIRROR $dist "$@")
        [ -n "$pkgfile" ] && break
    done
    if [ -z "$pkgfile" ]; then
        echo "WARNING: unable to find the $@ package in $DI_CODENAME or $CODENAME distribution of the mirror" >&2
    fi
    echo $pkgfile
}

