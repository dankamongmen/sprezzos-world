# Functions to convert isolinux config to allow selection of desktop
# environment for certain images.

di_syslinux_version() {
	local version
	version=$(sed -nr "s/^# D-I config version ([0-9.])/\1/p" \
		boot$N/isolinux/isolinux.cfg)

	[ -n "Sversion" ] || return 1
	echo "$version"
}

# Workaround for #505243
# Syslinux does not correctly handle a default64 option in combination
# with vesamenu. Instead, add special default label to automatically
# select i386/amd64 if user hits enter from help screens.
multiarch_workaround() {
	cp -f $CDDIR/../syslinux/usr/lib/syslinux/ifcpu64.c32 boot$N/isolinux/
	sed -i "/^default install/ s/^/#/" \
		boot$N/isolinux/txt.cfg || true
	sed -i "/^default64 amd64-install/ s/^/#/" \
		boot$N/isolinux/amdtxt.cfg || true
	sed -i "/^include menu.cfg/ a\include instsel.cfg" \
		boot$N/isolinux/prompt.cfg
	sed -i "/^default install/ a\include instsel.cfg" \
		boot$N/isolinux/desktop/prompt.cfg
	cat >boot$N/isolinux/instsel.cfg <<EOF
default install-select
label install-select
    kernel ifcpu64.c32
    append amd64-install -- install
EOF
}

create_desktop_dir() {
	local desktop=$1 title

	case $desktop in
		kde)	title=KDE ;;
		xfce)	title=Xfce ;;
		lxde)	title=LXDE ;;
	esac

	cp -r boot$N/isolinux/desktop boot$N/isolinux/$desktop
	sed -i "s:%desktop%:$desktop:
		s:%dt-name%:$title:" boot$N/isolinux/$desktop/*.cfg
}

modify_for_single_desktop() {
	# Cleanup
	rm boot$N/isolinux/dtmenu.cfg
	rm -r boot$N/isolinux/desktop

	# Set default desktop, or remove if not applicable
	if [ "$DESKTOP" ]; then
		sed -i "s:%desktop%:$DESKTOP:g" boot$N/isolinux/*.cfg
	else
		sed -i "s/desktop=%desktop% //" boot$N/isolinux/*.cfg
	fi
}

modify_for_light_desktop() {
	local desktop

	for file in boot$N/isolinux/{,amd}{,ad}{txt,gtk}.cfg; do
		if [ -e $file ]; then
			mv $file boot$N/isolinux/desktop
		fi
	done
	sed -i "s/desktop=%desktop% //" boot$N/isolinux/*.cfg

	for desktop in xfce lxde; do
		create_desktop_dir $desktop
	done

	# Cleanup
	rm -r boot$N/isolinux/desktop
	rm boot$N/isolinux/prompt.cfg boot$N/isolinux/dtmenu.cfg

	# Create new "top level" menu file
	cat >boot$N/isolinux/menu.cfg <<EOF
menu hshift 13
menu width 49

include stdmenu.cfg
menu title Desktop environment menu
menu begin lxde-desktop
    include stdmenu.cfg
    menu label ^LXDE
    menu title LXDE desktop boot menu
    text help
   Select the 'Lightweight X11 Desktop Environment' for the Desktop task
    endtext
    label mainmenu-lxde
        menu label ^Back..
        menu exit
    include lxde/menu.cfg
menu end
menu begin xfce-desktop
    include stdmenu.cfg
    menu label ^Xfce
    menu title Xfce desktop boot menu
    text help
   Select the 'Xfce lightweight desktop environment' for the Desktop task
    endtext
    label mainmenu-xfce
        menu label ^Back..
        menu exit
    include xfce/menu.cfg
menu end
menu begin rescue
    include stdmenu.cfg
    menu label ^System rescue
    menu title System rescue boot menu
    label mainmenu-rescue
        menu label ^Back..
        menu exit
    include rqtxt.cfg
    include amdrqtxt.cfg
    include rqgtk.cfg
    include amdrqgtk.cfg
menu end
EOF
}

modify_for_all_desktop() {
	local desktop

	for file in boot$N/isolinux/{,amd}{,ad}{txt,gtk}.cfg; do
		if [ -e $file ]; then
			cp $file boot$N/isolinux/desktop
		fi
	done
	sed -i "s/desktop=%desktop% //" boot$N/isolinux/*.cfg

	for desktop in kde xfce lxde; do
		create_desktop_dir $desktop
	done

	# Cleanup
	rm -r boot$N/isolinux/desktop
}
