    # Handle upgrades when libc-opt package has been installed.
    # When a /etc/ld.so.nohwcap file exists, ld.so only use libraries
    # from /lib, and ignore all optimised libraries. This file is
    # inconditionaly created in the preinst script of libc.
 
    # Get the list of optimized packages for a given architecture
    # Before removing a package from this list, make sure it appears
    # in the Conflicts: line of libc.
    case $(dpkg --print-architecture) in
        alpha)
            hwcappkgs="libc6-alphaev67"
            ;;
        i386)
            hwcappkgs="libc6-i686 libc6-xen"
            ;;
        kfreebsd-i386)
            hwcappkgs="libc0.1-i686"
            ;;
        mipsel)
	    hwcappkgs="libc6-loongson2f"
	    ;;
        sparc)
            hwcappkgs="libc6-sparcv9 libc6-sparcv9b"
            ;;
    esac
 
    # We check the version between the current installed libc and
    # all optimized packages (on architectures where such packages
    # exists).
    all_upgraded=yes
    if [ -n "$hwcappkgs" ]; then
        for pkg in $hwcappkgs ; do
            ver=$(dpkg-query -l $pkg 2>/dev/null | sed -e '/^[a-z][a-z]\s/!d;/^.[nc]/d;' -e "s/^..\s\+$pkg\s\+//;s/\s.*//g")
            if [ -n "$ver" ] && [ "$ver" != "CURRENT_VER" ]; then
                all_upgraded=no
            fi
        done
    fi

    # If the versions of all optimized packages are the same as the libc
    # one, we could remove /etc/ld.so.nohwcap. Otherwise, it will be removed
    # when all optimized packages are upgraded or removed.
    if [ "$all_upgraded" = yes ] ; then
        rm -f /etc/ld.so.nohwcap
    fi
