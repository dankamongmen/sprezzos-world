.DELETE_ON_ERROR:
.PHONY: world all clean
.DEFAULT_GOAL:=all

all: world

ARCH:=amd64

DEBKEY:=9978711C
DEBFULLNAME:='nick black'
DEBEMAIL:=nick.black@sprezzatech.com

# These spur generation of definition files in sprezzos-world from
# debian/changelog files in packaging/*.
PACKAGES:=growlight fwts util-linux linux-latest libpng libjpeg8-turbo lvm2 gdm3 \
	omphalos sudo systemd librsvg grub2 xmlstarlet openssh hfsutils fbi udisks \
	conpalette strace splitvt xbmc sprezzos-grub2theme apitrace cairo wpa git \
	fbv fonts-adobe-sourcesanspro mplayer nethorologist fbterm base-files gtk2 \
	netbase firmware-all gtk3 libdrm mesa pulseaudio socat wireless-tools vim \
	nfs-utils eglibc hwloc freetype pango fontconfig gdk-pixbuf glib ibus lsb \
	harfbuzz curl libxml libxslt console-setup f2fs-tools linux-tools vte \
	lightdm opencv gsettings-desktop-schemas gnome-desktop less spl zfs gvfs \
	gnome-control-center nautilus eog atk aptitude atk-bridge cheese yelp \
	gnome-settings-daemon clutter-gtk clutter-gst brasero aptitude clutter \
	installation-report gnome-shell gnome-shell-extensions gnome-contacts icu \
	gnome-power-manager evince poppler gnome-media compiz9 fbset meta-gnome \
	gobject-introspection gnomecatalog kismet wireshark gnome-sushi gnutls \
	freeglut libwnck d-conf gnome-user-docs abcde pidgin libdebian-installer \
	libatasmart gcrypt gcovr dri2proto x11proto-gl x11proto-randr GLU anna \
	libx86 Sick-Beard gnome-font-viewer gnome-screenshot gnome-search-tool netcf \
	gnome-themes-standard usbview mcelog libjpeg compiz openldap boost screenlets \
	gnome-orca at-spi banshee inkscape shotwell webkit libsoup enchant frei0r \
	gmake packagekit gnome-dictionary gnome-color-manager mash yelp-xsl dbus \
	pixman gnome-disk-utility gnome-doc-utils libvirt reportbug gphoto2 razorqt \
	libgphoto2 nvidia-cuda-toolkit pcre zerofree gstreamer zenity autokey eio exo \
	metacity grilo lcms2 colord colord-gtk telepathy-glib enlightenment eet gdl \
	eina evas ecore exactimage edje efreet embryo edbus eeze itstool virtualbox \
	emotion elementary ethumb cogl mpd mutter lftp ncmpcpp evas-generic-loaders \
	gcstar gperf evolution evolution-data-server calibre baobab ghex syncevolution \
	libsynthesis simple-scan eog-plugins vinagre libxcb xcb-proto xinput gcr \
	gnome-keyring reaver wifite aacplusenc faac handbrake gnome-themes bluez w3m \
	gnome-session gnome-bluetooth nautilus-sendto libgnome-keyring mp4v2 rtmpdump \
	gnome-terminal xfce4-terminal libxfce4ui libxfce4util xfconf gtkhtml iproute \
	gnome-online-accounts pygobject yelp-tools gnome-icon-theme-extras gnome-menus \
	gnome-icon-theme gnome-icon-theme-symbolic audit mdadm anjuta gimp zsh bash \
	ratpoison ghostscript jbig2dec cups libgd2 xorg-xserver libexif numactl \
	libcap2 devhelp libsecret cinnamon file-roller alacarte x11proto-fonts x11proto-xext \
	x11proto-core x11proto-damage x11proto-fixes x11proto-input x11proto-kb bison procps \
	x11proto-record x11proto-render x11proto-resource x11proto-video x11proto-scrnsaver \
	x11proto-xinerama wayland libxrender x11proto-xf86dga x11proto-xf86dri mutt \
	x11proto-xf86vidmode libtasn

SPREZZ:=packaging

include $(addprefix sprezzos-world/,$(PACKAGES))

sprezzos-world/%: $(SPREZZ)/%/debian/changelog
	[ -d $(@D) ] || mkdir -p $(@D)
	( echo "# Automatically generated from $<" && \
	 echo -n "$(@F)_VERSION:=" && \
	 dpkg-parsechangelog -l$< | grep-dctrl -ensVersion -FSource . | cut -d: -f2- && \
	 echo -n "$(shell echo $(@F) | tr [:lower:] [:upper:] | tr -d -):=$(@F)_" &&\
	 dpkg-parsechangelog -l$< | grep-dctrl -ensVersion -FSource . | cut -d: -f2- && \
	 echo -n "$(@F)_UPVER:=" && \
	 dpkg-parsechangelog -l$< | grep-dctrl -ensVersion -FSource . | cut -d: -f2- | sed -e 's/[+-]SprezzOS[0-9]*//' \
	 ) > $@

APITRACEORIG:=apitrace_$(shell echo $(apitrace_VERSION) | cut -d- -f1).orig.tar.xz
GROWLIGHTORIG:=growlight_$(shell echo $(growlight_VERSION) | cut -d- -f1).orig.tar.xz
OMPHALOSORIG:=omphalos_$(shell echo $(omphalos_VERSION) | cut -d- -f1).orig.tar.xz
SICKBEARDORIG:=sick-beard_$(shell echo $(Sick-Beard_VERSION) | cut -d- -f1).orig.tar.xz

APTITUDEORIG:=aptitude_$(shell echo $(aptitude_VERSION) | cut -d- -f1).orig.tar.bz2
ATKUP:=atk-$(shell echo $(atk_VERSION) | cut -d- -f1)
ATKORIG:=atk1.0_$(shell echo $(atk_VERSION) | cut -d- -f1).orig.tar.xz
BRASEROUP:=brasero-$(shell echo $(brasero_VERSION) | cut -d: -f2- | cut -d- -f1)
BRASEROORIG:=brasero_$(shell echo $(brasero_VERSION) | cut -d: -f2- | cut -d- -f1).orig.tar.xz
CHEESEUP:=cheese-$(shell echo $(cheese_VERSION) | cut -d: -f2- | cut -d- -f1)
CHEESEORIG:=cheese_$(shell echo $(cheese_VERSION) | cut -d: -f2- | cut -d- -f1).orig.tar.xz
CLUTTERUP:=clutter-$(shell echo $(clutter_VERSION) | cut -d: -f2- | cut -d- -f1)
CLUTTERORIG:=clutter-1.0_$(shell echo $(clutter_VERSION) | cut -d: -f2- | cut -d- -f1).orig.tar.xz
CLUTTERGSTUP:=clutter-gst-$(shell echo $(clutter-gst_VERSION) | cut -d: -f2- | cut -d- -f1)
CLUTTERGSTORIG:=clutter-gst_$(shell echo $(clutter-gst_VERSION) | cut -d: -f2- | cut -d- -f1).orig.tar.xz
CLUTTERGTKUP:=clutter-gtk-$(shell echo $(clutter-gtk_VERSION) | cut -d: -f2- | cut -d- -f1)
CLUTTERGTKORIG:=clutter-gtk_$(shell echo $(clutter-gtk_VERSION) | cut -d: -f2- | cut -d- -f1).orig.tar.xz
CURLUP:=curl-$(shell echo $(curl_VERSION) | cut -d= -f2- | cut -d- -f1)
CURLORIG:=curl_$(shell echo $(curl_VERSION) | cut -d- -f1).orig.tar.bz2
EGLIBCUP:=glibc-$(shell echo $(eglibc_VERSION) | cut -d- -f1)
EGLIBCORIG:=eglibc_$(shell echo $(eglibc_VERSION) | cut -d- -f1).orig.tar.gz
EVINCEUP:=evince-$(shell echo $(evince_VERSION) | cut -d: -f2- | cut -d- -f1)
EVINCEORIG:=evince_$(shell echo $(evince_VERSION) | cut -d: -f2- | cut -d- -f1).orig.tar.xz
FBIUP:=fbida-$(shell echo $(fbi_VERSION) | cut -d= -f2- | cut -d- -f1)
FBTERMUP:=nfbterm-$(shell echo $(fbterm_VERSION) | cut -d= -f2 | cut -d- -f1)
FBTERMORIG:=fbterm_$(shell echo $(fbterm_VERSION) | cut -d- -f1).orig.tar.gz
FBVORIG:=fbv_$(shell echo $(fbv_VERSION) | cut -d- -f1).orig.tar.gz
FONTCONFIGUP:=fontconfig-$(shell echo $(fontconfig_VERSION) | cut -d- -f1)
FONTCONFIGORIG:=fontconfig_$(shell echo $(fontconfig_VERSION) | cut -d- -f1).orig.tar.gz
FREETYPEUP:=freetype-$(shell echo $(freetype_VERSION) | cut -d- -f1) \
	freetype-doc-$(shell echo $(freetype_VERSION) | cut -d- -f1) \
	ft2demos-$(shell echo $(freetype_VERSION) | cut -d- -f1)
FREETYPEORIG:=freetype_$(shell echo $(freetype_VERSION) | cut -d- -f1).orig.tar.gz
GDKPIXBUFUP:=gdk-pixbuf-$(shell echo $(gdk-pixbuf_VERSION) | cut -d- -f1)
GDKPIXBUFORIG:=gdk-pixbuf_$(shell echo $(gdk-pixbuf_VERSION) | cut -d- -f1).orig.tar.xz
GNOMECONTACTSUP:=gnome-contacts-$(shell echo $(gnome-contacts_VERSION) | cut -d- -f1)
GNOMECONTACTSORIG:=gnome-contacts_$(shell echo $(gnome-contacts_VERSION) | cut -d- -f1).orig.tar.xz
GNOMECONTROLCENTERUP:=gnome-control-center-$(shell echo $(gnome-control-center_VERSION) | cut -d: -f2- | cut -d- -f1)
GNOMECONTROLCENTERORIG:=gnome-control-center_$(shell echo $(gnome-control-center_VERSION) | cut -d: -f2- | cut -d- -f1).orig.tar.xz
GNOMEDESKTOPUP:=gnome-desktop-$(shell echo $(gnome-desktop_VERSION) | cut -d- -f1)
GNOMEDESKTOPORIG:=gnome-desktop_$(shell echo $(gnome-desktop_VERSION) | cut -d- -f1).orig.tar.xz
GNOMEMEDIAUP:=gnome-media-$(shell echo $(gnome-media_VERSION) | cut -d- -f1)
GNOMEMEDIAORIG:=gnome-media_$(shell echo $(gnome-media_VERSION) | cut -d- -f1).orig.tar.xz
GNOMEPOWERMANAGERUP:=gnome-power-manager-$(shell echo $(gnome-power-manager_VERSION) | cut -d: -f2- | cut -d- -f1)
GNOMEPOWERMANAGERORIG:=gnome-power-manager_$(shell echo $(gnome-power-manager_VERSION) | cut -d: -f2- | cut -d- -f1).orig.tar.xz
GNOMESHELLUP:=gnome-shell-$(shell echo $(gnome-shell_VERSION) | cut -d: -f2- | cut -d- -f1)
GNOMESHELLORIG:=gnome-shell_$(shell echo $(gnome-shell_VERSION) | cut -d: -f2- | cut -d- -f1).orig.tar.xz
GNOMESHELLEXTENSIONSUP:=gnome-shell-extensions-$(shell echo $(gnome-shell-extensions_VERSION) | cut -d: -f2- | cut -d- -f1)
GNOMESHELLEXTENSIONSORIG:=gnome-shell-extensions_$(shell echo $(gnome-shell-extensions_VERSION) | cut -d: -f2- | cut -d- -f1).orig.tar.xz
GSETSCHEMASUP:=gsettings-desktop-schemas-$(shell echo $(gsettings-desktop-schemas_VERSION) | cut -d- -f1)
GSETSCHEMASORIG:=gsettings-desktop-schemas_$(shell echo $(gsettings-desktop-schemas_VERSION) | cut -d- -f1).orig.tar.xz
SPLORIG:=spl_$(shell echo $(spl_VERSION) | cut -d- -f1).orig.tar.xz
ZFSORIG:=zfs_$(shell echo $(zfs_VERSION) | cut -d- -f1).orig.tar.xz
GRUBUP:=grub-$(shell echo $(grub2_VERSION) | cut -d- -f1 | cut -d= -f2- | tr : -)
HARFBUZZUP:=harfbuzz-$(shell echo $(harfbuzz_VERSION) | cut -d- -f1 | cut -d= -f2- | cut -d: -f2)
HARFBUZZORIG:=harfbuzz_$(shell echo $(harfbuzz_VERSION) | cut -d- -f1).orig.tar.gz
HFSUTILSUP:=hfsutils-$(shell echo $(hfsutils_VERSION) | cut -d- -f1 | cut -d= -f2- | cut -d: -f2)
HFSUTILSORIG:=hfsutils_$(shell echo $(hfsutils_VERSION) | cut -d- -f1).orig.tar.gz
HWLOCUP:=hwloc-$(shell echo $(hwloc_VERSION) | cut -d- -f1)
HWLOCORIG:=hwloc_$(shell echo $(hwloc_VERSION) | cut -d- -f1).orig.tar.bz2
IBUSUP:=ibus-$(shell echo $(ibus_VERSION) | cut -d: -f2- | cut -d- -f1)
IBUSORIG:=ibus_$(shell echo $(ibus_VERSION) | cut -d: -f2- | cut -d- -f1).orig.tar.gz
LESSUP:=less-$(shell echo $(less_VERSION) | cut -d- -f1 | cut -d= -f2- | cut -d: -f2)
LESSORIG:=$(shell echo $(LESSUP) | tr - _).orig.tar.gz
LIBDRMUP:=libdrm-$(shell echo $(libdrm_VERSION) | cut -d- -f1 | cut -d= -f2- | cut -d: -f2)
LIBDRMORIG:=$(shell echo $(LIBDRMUP) | tr - _).orig.tar.bz2
LIBPNGUP:=libpng-$(shell echo $(libpng_VERSION) | cut -d- -f1 | cut -d= -f2- | cut -d: -f2)
LIBPNGORIG:=$(shell echo $(LIBPNGUP) | tr - _).orig.tar.bz2
LIBRSVGUP:=librsvg-$(shell echo $(librsvg_VERSION) | cut -d- -f1 | cut -d= -f2- | cut -d: -f2)
LIBRSVGORIG:=$(shell echo $(LIBRSVGUP) | tr - _).orig.tar.xz
LIBXMLUP:=libxml2-$(shell echo $(libxml_VERSION) | cut -d- -f1 | cut -d. -f-3 | cut -d= -f2- | cut -d: -f2)
LIBXMLORIG:=$(shell echo $(LIBXMLUP) | tr - _).orig.tar.gz
LIBXSLTUP:=libxslt-$(shell echo $(libxslt_VERSION) | cut -d- -f1 | cut -d= -f2- | cut -d: -f2)
LIBXSLTORIG:=$(shell echo $(LIBXSLTUP) | tr - _).orig.tar.gz
LIGHTDMUP:=lightdm_$(shell echo $(lightdm_VERSION) | cut -d- -f1 | cut -d= -f2- | cut -d: -f2)
LIGHTDMORIG:=$(shell echo $(LIGHTDMUP) | tr - _).orig.tar.gz
LINUXTOOLSORIG:=linux-tools_$(shell echo $(linux-tools_VERSION) | cut -d- -f1).orig.tar.bz2
LVM2:=lvm2_$(shell echo $(lvm2_VERSION) | tr : .)
LVM2UP:=LVM2.$(shell echo $(lvm2_VERSION) | cut -d- -f1 | cut -d= -f2- | cut -d: -f2)
MCELOGORIG:=mcelog_$(shell echo $(mcelog_VERSION) | cut -d- -f1).orig.tar.xz
MPLAYER:=mplayer_$(shell echo $(mplayer_VERSION) | tr : .)
NETHOROLOGISTORIG:=nethorologist_$(shell echo $(nethorologist_VERSION) | cut -d- -f1).orig.tar.xz
FREI0RORIG:=frei0r_$(shell echo $(frei0r_VERSION) | cut -d- -f1).orig.tar.xz
XMLSTARLETORIG:=xmlstarlet_$(shell echo $(xmlstarlet_VERSION) | cut -d- -f1).orig.tar.bz2
OPENSSH:=openssh_$(shell echo $(openssh_VERSION) | tr : .)
OPENSSHUP:=openssh-$(shell echo $(openssh_VERSION) | cut -d- -f1 | cut -d= -f2- | cut -d: -f2)
PANGOUP:=pango-$(shell echo $(pango_VERSION) | cut -d- -f1)
PANGOORIG:=pango1.0_$(shell echo $(PANGOUP) | cut -d- -f2 | tr - _).orig.tar.xz
POPPLERUP:=poppler-$(shell echo $(poppler_VERSION) | cut -d- -f1)
POPPLERORIG:=poppler_$(shell echo $(POPPLERUP) | cut -d- -f2 | tr - _).orig.tar.gz
PULSEAUDIOUP:=pulseaudio-$(shell echo $(pulseaudio_VERSION) | cut -d- -f1)
PULSEAUDIOORIG:=$(shell echo $(PULSEAUDIOUP) | tr - _).orig.tar.xz
SOCATUP:=socat-$(shell echo $(socat_VERSION) | cut -d- -f1 | tr \~ -)
SOCATORIG:=socat_$(shell echo $(socat_VERSION) | cut -d- -f1).orig.tar.bz2
USBVIEWUP:=usbview-$(shell echo $(usbview_VERSION) | cut -d= -f2- | cut -d- -f1)
USBVIEWORIG:=usbview_$(shell echo $(usbview_VERSION) | cut -d- -f1).orig.tar.gz

DEBS:=$(GROWLIGHT) $(LIBRSVG) $(GRUB2) $(LVM2) $(OPENSSH) $(LIBPNG) $(FWTS) $(ICU) \
	$(UTILLINUX) $(LINUXLATEST) $(LIBJPEG8TURBO) $(OMPHALOS) $(SUDO) $(VTE) \
	$(GRUBTHEME) $(ADOBE) $(STRACE) $(SPLITVT) $(HFSUTILS) $(APTITUDE) $(DCONF) \
	$(NETHOROLOGIST) $(XBMC) $(MPLAYER) $(CONPALETTE) $(APITRACE) $(YELP) $(DBUS) \
	$(SYSTEMD) $(BASEFILES) $(NETBASE) $(FBI) $(CAIRO) $(XMLSTARLET) $(WPA) $(LSB) \
	$(GTK3) $(LIBDRM) $(PULSEAUDIO) $(SOCAT) $(NFSUTILS) $(EGLIBC) $(FBV) $(GIT) \
	$(FREETYPE) $(PANGO) $(GDKPIXBUF) $(GLIB) $(HARFBUZZ) $(CURL) $(IBUS) $(GMAKE) \
	$(LIBXSLT) $(LIBXML) $(F2FSTOOLS) $(LINUXTOOLS) $(LIGHTDM) $(OPENCV) $(GVFS) \
	$(GSETTINGSDESKTOPSCHEMAS) $(LESS) $(ZFS) $(SPL) $(EOG) $(ATK) $(KISMET) \
	$(GNOMECONTROLCENTER) $(NAUTILUS) $(GNOMESETTINGSDAEMON) $(CHEESE) $(UDISKS) \
	$(CLUTTERGST) $(CLUTTERGTK) $(BRASERO) $(INSTALLATIONREPORT) $(CLUTTER) \
	$(APTITUDE) $(GNOMESHELL) $(GNOMESHELLEXTENSIONS) $(GNOMECONTACTS) $(GDM3) \
	$(EVINCE) $(POPPLER) $(COMPIZ9) $(FBSET) $(GOBJECTINTROSPECTION) $(GCOVR) \
	$(METAGNOME) $(GNOMECATALOG) $(WIRELESSTOOLS) $(WIRESHARK) $(GNOMESUSHI) \
	$(LIBATASMART) $(GCRYPT) $(GNUTLS) $(DRI2PROTO) $(X11PROTOGL) $(X11PROTORANDR) \
	$(GLU) $(FREEGLUT) $(LIBWNCK) $(GNOMEUSERDOCS) $(ABCDE) $(PIDGIN) $(LIBX86) \
	$(SICKBEARD) $(GNOMEFONTVIEWER) $(GNOMESCREENSHOT) $(GNOMESEARCHTOOL) $(NETCF) \
	$(GNOMETHEMESSTANDARD) $(USBVIEW) $(MCELOG) $(LIBJPEG) $(COMPIZ) $(BOOST) \
	$(OPENLDAP) $(SCREENLETS) $(GNOMEORCA) $(ATSPI) $(BANSHEE) $(INKSCAPE) \
	$(SHOTWELL) $(WEBKIT) $(LIBSOUP) $(ENCHANT) $(FREI0R) $(PACKAGEKIT) $(MASH) \
	$(GNOMEDICTIONARY) $(GNOMECOLORMANAGER) $(YELPXSL) $(PIXMAN) $(LIBVIRT) \
	$(GNOMEDISKUTILITY) $(GNOMEDOCUTILS) $(REPORTBUG) $(GPHOTO2) $(LIBGPHOTO2) \
	$(NVIDIACUDATOOLKIT) $(RAZORQT) $(GTK2) $(VIM) $(PCRE) $(ZEROFREE) $(LCMS) \
	$(GSTREAMER) $(ZENITY) $(AUTOKEY) $(METACITY) $(COLORD) $(COLORDGTK) $(EVAS) \
	$(TELEPATHYGLIB) $(ENLIGHTENMENT) $(EINA) $(EET) $(ECORE) $(EXACTIMAGE) $(GDL) \
	$(EIO) $(EDJE) $(EFREET) $(EMBRYO) $(EDBUS) $(EEZE) $(ITSTOOL) $(VIRTUALBOX) \
	$(EMOTION) $(ELEMENTARY) $(ETHUMB) $(COGL) $(MPD) $(MUTTER) $(LFTP) $(NCMPCPP) \
	$(EVASGENERICLOADERS) $(GCSTAR) $(GPERF) $(CALIBRE) $(BAOBAB) $(GHEX) $(BLUEZ) \
	$(SYNCEVOLUTION) $(LIBSYNTHESIS) $(SIMPLESCAN) $(EOGPLUGINS) $(LIBXCB) $(GIMP) \
	$(XCBPROTO) $(XINPUT) $(GNOMEKEYRING) $(GCR) $(REAVER) $(WIFITE) $(AACPLUSENC) \
	$(FAAC) $(HANDBRAKE) $(GNOMETHEMES) $(GNOMESESSION) $(GNOMEBLUETOOTH) $(EXO) \
	$(NAUTILUSSENDTO) $(LIBGNOMEKEYRING) $(MP4V2) $(GNOMETERMINAL) $(XFCE4TERMINAL) \
	$(LIBXFCE4UI) $(LIBXFCE4UTIL) $(XFCONF) $(GTKHTML) $(GNOMEONLINEACCOUNTS) \
	$(PYGOBJECT) $(YELPTOOL) $(RTMPDUMP) $(GNOMEMENUS) $(GNOMEICONTHEMESYMBOLIC) \
	$(GNOMEICONTHEME) $(AUDIT) $(MDADM) $(IPROUTE) $(ANJUTA) $(ZSH) $(BASH) \
	$(RATPOISON) $(GHOSTSCRIPT) $(JBIG2DEC) $(CUPS) $(LIBGD2) $(XORGXSERVER) \
	$(LIBEXIF) $(NUMACTL) $(LIBCAP2) $(DEVHELP) $(LIBSECRET) $(CINNAMON) $(W3M) \
	$(FILEROLLER) $(ALACARTE) $(X11PROTOXEXT) $(X11PROTOFONTS) $(X11PROTOCORE) \
	$(BISON) $(X11PROTODAMAGE) $(X11PROTOFIXES) $(X11PROTOINPUT) $(X11PROTOKB) \
	$(X11PROTORECORD) $(X11PROTORENDER) $(X11PROTORESOURCE) $(X11PROTOVIDEO) \
	$(X11PROTOSCRNSAVER) $(X11PROTOXINERAMA) $(WAYLAND) $(LIBXRENDER) $(PROCPS) \
	$(X11PROTOXF86DGA) $(X11PROTOXF86DRI) $(X11PROTOXF86VIDMODE) $(LIBTASN) $(MUTT)
UDEBS:=$(FIRMWAREALL) $(ANNA) $(LIBDEBIANINSTALLER)
DUPUDEBS:=$(GROWLIGHT) $(FBTERM) $(CONPALETTE) $(STRACE) $(SPLITVT) $(FBV) \
	$(NETHOROLOGIST) $(FWTS) $(UTILLINUX) $(HFSUTILS) $(LIBPNG) $(EGLIBC) \
	$(FREETYPE) $(CURL) $(LIBXSLT) $(LIBXML) $(F2FSTOOLS) $(ZFS) $(SPL) \
	$(WPA) $(LIBATASMART) $(LIBX86) $(LIBJPEG) $(PCRE)

DEBS:=$(subst :,.,$(DEBS))
UDEBS:=$(subst :,.,$(UDEBS))

DSCS:=$(addsuffix .dsc,$(DEBS) $(UDEBS))
CHANGES:=$(addsuffix .changes,$(DEBS) $(UDEBS))

DEBS:=$(addsuffix _$(ARCH).deb,$(DEBS))
UDEBS:=$(addsuffix _$(ARCH).udeb,$(UDEBS))

world: $(DEBS) $(UDEBS)

#cd $< && apt-get -y build-dep $(shell echo $@ | cut -d_ -f1) || true # source package might not exist
%_$(ARCH).udeb %_$(ARCH).deb: %
	cd $< && debuild -j8 -k$(DEBKEY)

# Packages which we take from upstream source repositories rather than a
# release tarball. We must make our own *.orig.tar.* files for these.
.PHONY: apitrace
apitrace:$(APITRACE)_$(ARCH).deb
$(APITRACE): $(SPREZZ)/apitrace/debian/changelog
	git clone git://github.com/apitrace/apitrace.git $@
	tar cJf $(APITRACEORIG) $@ --exclude-vcs
	cp -r $(<D) $@/

.PHONY: growlight
growlight: $(GROWLIGHT)_$(ARCH).deb
$(GROWLIGHT): $(SPREZZ)/growlight/debian/changelog
	git clone git://github.com/dankamongmen/growlight.git $@
	tar cJf $(GROWLIGHTORIG) $@ --exclude-vcs
	cp -r $(<D) $@/

.PHONY: mcelog
mcelog:$(MCELOG)_$(ARCH).deb
$(MCELOG): $(SPREZZ)/mcelog/debian/changelog
	git clone git://git.kernel.org/pub/scm/utils/cpu/mce/mcelog.git $@
	tar cJf $(MCELOGORIG) $@ --exclude-vcs
	cp -r $(<D) $@/

.PHONY: omphalos
omphalos:$(OMPHALOS)_$(ARCH).deb
$(OMPHALOS): $(SPREZZ)/omphalos/debian/changelog
	git clone git://github.com/dankamongmen/omphalos.git $@
	tar cJf $(OMPHALOSORIG) $@ --exclude-vcs
	cp -r $(<D) $@/

.PHONY: frei0r
frei0r:$(FREI0R)_$(ARCH).deb
$(FREI0R): $(SPREZZ)/frei0r/debian/changelog
	git clone git://code.dyne.org/frei0r.git $@
	tar cJf $(FREI0RORIG) $@ --exclude-vcs
	cp -r $(<D) $@/

.PHONY: aptitude
aptitude: $(APTITUDE)_$(ARCH).deb
$(APTITUDE): $(SPREZZ)/aptitude/debian/changelog
	git clone git://git.debian.org/git/aptitude/aptitude.git $@
	rm -rfv $@/debian
	tar cjf $(APTITUDEORIG) $@ --exclude-vcs
	cp -rv $(<D) $@/

.PHONY: Sick-Beard
Sick-Beard: $(SICKBEARD)_$(ARCH).deb
$(SICKBEARD): $(SPREZZ)/Sick-Beard/debian/changelog
	git clone git://github.com/midgetspy/Sick-Beard.git $@
	tar cJf $(SICKBEARDORIG) $@ --exclude-vcs
	cp -r $(<D) $@/

.PHONY: xmlstarlet
xmlstarlet:$(XMLSTARLET)_$(ARCH).deb
$(XMLSTARLET): $(SPREZZ)/xmlstarlet/debian/changelog
	git clone git://github.com/dankamongmen/xmlstarlet.git $@
	tar cjf $(XMLSTARLETORIG) $@ --exclude-vcs --exclude=debian
	cp -r $(<D) $@/

.PHONY: spl
spl: $(SPL)_$(ARCH).deb
$(SPL): $(SPREZZ)/spl/debian/changelog
	git clone git://github.com/zfsonlinux/spl.git $@
	cd $@ && ./autogen.sh
	tar cJf $(SPLORIG) $@ --exclude-vcs
	cp -r $(<D) $@/

.PHONY: zfs
zfs: $(ZFS)_$(ARCH).deb
$(ZFS): $(SPREZZ)/zfs/debian/changelog
	git clone git://github.com/zfsonlinux/zfs.git $@
	cd $@ && ./autogen.sh
	tar cJf $(ZFSORIG) $@ --exclude-vcs
	cp -r $(<D) $@/

.PHONY: nethorologist
nethorologist: $(NETHOROLOGIST)_$(ARCH).deb
$(NETHOROLOGIST): $(SPREZZ)/nethorologist/debian/changelog
	git clone git://github.com/Sprezzatech/nethorologist.git $@
	tar cJf $(NETHOROLOGISTORIG) $@ --exclude-vcs --exclude=debian
	cp -r $(<D) $@/

.PHONY: strace
strace: $(STRACE)_$(ARCH).deb
$(STRACE): $(SPREZZ)/strace/debian/changelog
	git clone git://strace.git.sourceforge.net/gitroot/strace/strace $@
	cd $@ && autoreconf -sif
	tar cjf $(shell echo $< | sed -e 's/\(.*\)-.*/\1/' | sed -e 's/\(.*\)-/\1_/').orig.tar.bz2 $< --exclude-vcs
	cp -r $(<D) $@/

.PHONY: linux-tools
linux-tools:$(LINUXTOOLS)_$(ARCH).deb
$(LINUXTOOLS): $(SPREZZ)/linux-tools/debian/changelog
	cp -r $(<D)/.. $@
	tar cjf $(LINUXTOOLSORIG) $@ --exclude-vcs --exclude=debian

.PHONY: fbv
fbv:$(FBV)_$(ARCH).deb
$(FBV): $(SPREZZ)/fbv/debian/changelog
	git clone git://repo.or.cz/fbv.git $@
	tar czf $(FBVORIG) $@
	cp -r $(<D) $@/

.PHONY: xbmc
xbmc:$(XBMC)_$(ARCH).deb
$(XBMC): $(SPREZZ)/xbmc/debian/changelog
	git clone git://github.com/xbmc/xbmc.git $@
	cp -r $(<D) $@/

.PHONY: mplayer
mplayer:$(MPLAYER)_$(ARCH).deb
$(MPLAYER): $(SPREZZ)/mplayer/debian/changelog
	svn co svn://svn.mplayerhq.hu/mplayer/trunk $@
	rm -rf $@/debian
	cp -r $(<D) $@/

# Ubuntu native packages ship their own debian/
.PHONY: fwts
fwts:$(FWTS)_$(ARCH).deb
$(FWTS): $(SPREZZ)/fwts/debian/changelog
	git clone git://kernel.ubuntu.com/hwe/fwts $@
	rm -rf $@/debian
	cd $@ && autoreconf -sif
	tar cjf $(shell echo $< | sed -e 's/\(.*\)-.*/\1/' | sed -e 's/\(.*\)-/\1_/').orig.tar.bz2 $< --exclude-vcs
	cp -r $(<D) $@/

.PHONY: installation-report
installation-report:$(INSTALLATIONREPORT)_$(ARCH).deb
$(INSTALLATIONREPORT): $(SPREZZ)/installation-report/debian/changelog
	[ ! -e $@ ] || { echo "$@ already exists; remove it" >&2 ; false ; }
	cp -r $(<D)/.. $@

.PHONY: linux-latest
linux-latest:$(LINUXLATEST)_$(ARCH).deb
$(LINUXLATEST): $(SPREZZ)/linux-latest/debian/changelog
	[ ! -e $@ ] || { echo "$@ already exists; remove it" >&2 ; false ; }
	cp -r $(<D)/.. $@

.PHONY: lsb
lsb:$(LSB)_$(ARCH).deb
$(LSB): $(SPREZZ)/lsb/debian/changelog
	[ ! -e $@ ] || { echo "$@ already exists; remove it" >&2 ; false ; }
	cp -r $(<D)/.. $@

FETCHED:=$(FETCHED) $(USBVIEWUP).tar.gz
$(USBVIEWUP).tar.gz:
	wget -nc -O$@ http://www.kroah.com/linux/usb/$@

$(USBVIEWORIG): $(USBVIEWUP).tar.gz
	ln -s $< $@

.PHONY: usbview
usbview:$(USBVIEW)_$(ARCH).deb
$(USBVIEW): $(SPREZZ)/usbview/debian/changelog $(USBVIEWORIG)
	mkdir $@
	tar xzvf $(USBVIEWUP).tar.gz --strip-components=1 -C $@
	cp -r $(<D) $@/

.PHONY: abcde
abcde:$(ABCDE)_$(ARCH).deb
$(ABCDE): $(SPREZZ)/abcde/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf abcde-$(abcde_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: aacplusenc
aacplusenc:$(AACPLUSENC)_$(ARCH).deb
$(AACPLUSENC): $(SPREZZ)/aacplusenc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf aacplusenc_$(aacplusenc_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: alacarte
alacarte:$(ALACARTE)_$(ARCH).deb
$(ALACARTE): $(SPREZZ)/alacarte/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf alacarte-$(alacarte_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: anjuta
anjuta:$(ANJUTA)_$(ARCH).deb
$(ANJUTA): $(SPREZZ)/anjuta/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf anjuta-$(anjuta_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: audit
audit:$(AUDIT)_$(ARCH).deb
$(AUDIT): $(SPREZZ)/audit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf audit-$(audit_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: at-spi
at-spi:$(ATSPI)_$(ARCH).deb
$(ATSPI): $(SPREZZ)/at-spi/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf at-spi2-core-$(at-spi_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: autokey
autokey:$(AUTOKEY)_$(ARCH).deb
$(AUTOKEY): $(SPREZZ)/autokey/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf autokey-$(autokey_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: banshee
banshee:$(BANSHEE)_$(ARCH).deb
$(BANSHEE): $(SPREZZ)/banshee/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf banshee-$(banshee_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: baobab
baobab:$(BAOBAB)_$(ARCH).deb
$(BAOBAB): $(SPREZZ)/baobab/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf baobab-$(baobab_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: bash
bash:$(BASH)_$(ARCH).deb
$(BASH): $(SPREZZ)/bash/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf bash-$(bash_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: bison
bison:$(BISON)_$(ARCH).deb
$(BISON): $(SPREZZ)/bison/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf bison-$(bison_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: bluez
bluez:$(BLUEZ)_$(ARCH).deb
$(BLUEZ): $(SPREZZ)/bluez/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf bluez-$(bluez_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: boost
boost:$(BOOST)_$(ARCH).deb
$(BOOST): $(SPREZZ)/boost/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	{ cd $@ && TARBALL=`uscan --no-symlink --force-download --dehs | xmlstarlet sel -t -v //target` && \
	  cd - && ln -sf $$TARBALL boost-build_2.0.m10.orig.tar.bz2 && tar xjvf $$TARBALL --strip-components=1 -C $@ ; }

.PHONY: cairo
cairo:$(CAIRO)_$(ARCH).deb
$(CAIRO): $(SPREZZ)/cairo/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf cairo-$(cairo_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: calibre
calibre:$(CALIBRE)_$(ARCH).deb
$(CALIBRE): $(SPREZZ)/calibre/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf calibre-$(calibre_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: cinnamon
cinnamon:$(CINNAMON)_$(ARCH).deb
$(CINNAMON): $(SPREZZ)/cinnamon/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf cinnamon-$(cinnamon_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: cogl
cogl:$(COGL)_$(ARCH).deb
$(COGL): $(SPREZZ)/cogl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf cogl-$(cogl_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: colord
colord:$(COLORD)_$(ARCH).deb
$(COLORD): $(SPREZZ)/colord/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf colord-$(colord_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: colord-gtk
colord-gtk:$(COLORDGTK)_$(ARCH).deb
$(COLORDGTK): $(SPREZZ)/colord-gtk/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf colord-gtk-$(colord-gtk_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: compiz
compiz:$(COMPIZ)_$(ARCH).deb
$(COMPIZ): $(SPREZZ)/compiz/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf compiz-$(compiz_UPVER).tar.bz2 --strip-components=1 -C $@

.PHONY: compiz9
compiz9:$(COMPIZ9)_$(ARCH).deb
$(COMPIZ9): $(SPREZZ)/compiz9/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf compiz-$(compiz9_UPVER).tar.bz2 --strip-components=1 -C $@

.PHONY: cups
cups:$(CUPS)_$(ARCH).deb
$(CUPS): $(SPREZZ)/cups/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf cups-$(cups_UPVER)-source.tar.bz2 --strip-components=1 -C $@

.PHONY: d-conf
d-conf:$(DCONF)_$(ARCH).deb
$(DCONF): $(SPREZZ)/d-conf/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf dconf-$(d-conf_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: dbus
dbus:$(DBUS)_$(ARCH).deb
$(DBUS): $(SPREZZ)/dbus/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf dbus-$(dbus_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: dri2proto
dri2proto:$(DRI2PROTO)_$(ARCH).deb
$(DRI2PROTO): $(SPREZZ)/dri2proto/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf dri2proto-$(dri2proto_UPVER).tar.bz2 --strip-components=1 -C $@

.PHONY: enchant
enchant:$(ENCHANT)_$(ARCH).deb
$(ENCHANT): $(SPREZZ)/enchant/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf enchant-$(enchant_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: enlightenment
enlightenment:$(ENLIGHTENMENT)_$(ARCH).deb
$(ENLIGHTENMENT): $(SPREZZ)/enlightenment/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf enlightenment-$(enlightenment_UPVER).tar.bz2 --strip-components=1 -C $@

.PHONY: devhelp
devhelp:$(DEVHELP)_$(ARCH).deb
$(DEVHELP): $(SPREZZ)/devhelp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf devhelp-$(devhelp_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: eog
eog:$(EOG)_$(ARCH).deb
$(EOG): $(SPREZZ)/eog/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf eog_$(eog_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: eog-plugins
eog-plugins:$(EOGPLUGINS)_$(ARCH).deb
$(EOGPLUGINS): $(SPREZZ)/eog-plugins/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf eog-plugins_$(eog-plugins_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: evolution
evolution:$(EVOLUTION)_$(ARCH).deb
$(EVOLUTION): $(SPREZZ)/evolution/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf evolution-$(evolution_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: evolution-data-server
evolution-data-server:$(EVOLUTIONDATASERVER)_$(ARCH).deb
$(EVOLUTIONDATASERVER): $(SPREZZ)/evolution-data-server/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf evolution-data-server-$(evolution-data-server_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: f2fs-tools
f2fs-tools:$(F2FSTOOLS)_$(ARCH).deb
$(F2FSTOOLS): $(SPREZZ)/f2fs-tools/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf f2fs-tools-$(f2fs-tools_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: faac
faac:$(FAAC)_$(ARCH).deb
$(FAAC): $(SPREZZ)/faac/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf faac-$(faac_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: fbset
fbset:$(FBSET)_$(ARCH).deb
$(FBSET): $(SPREZZ)/fbset/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf fbset-$(fbset_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: file-roller
file-roller:$(FILEROLLER)_$(ARCH).deb
$(FILEROLLER): $(SPREZZ)/file-roller/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf file-roller-$(file-roller_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: freeglut
freeglut:$(FREEGLUT)_$(ARCH).deb
$(FREEGLUT): $(SPREZZ)/freeglut/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf freeglut_$(freeglut_UPVER).orig.tar.gz --strip-components=1 -C $@

.PHONY: gcr
gcr:$(GCR)_$(ARCH).deb
$(GCR): $(SPREZZ)/gcr/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gcr_$(gcr_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: gcrypt
gcrypt:$(GCRYPT)_$(ARCH).deb
$(GCRYPT): $(SPREZZ)/gcrypt/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf libgcrypt11_$(gcrypt_UPVER).orig.tar.bz2 --strip-components=1 -C $@

.PHONY: gcstar
gcstar:$(GCSTAR)_$(ARCH).deb
$(GCSTAR): $(SPREZZ)/gcstar/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf gcstar-$(gcstar_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: gdl
gdl:$(GDL)_$(ARCH).deb
$(GDL): $(SPREZZ)/gdl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gdl-$(gdl_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: gdm3
gdm3:$(GDM3)_$(ARCH).deb
$(GDM3): $(SPREZZ)/gdm3/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gdm-$(gdm3_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: ghex
ghex:$(GHEX)_$(ARCH).deb
$(GHEX): $(SPREZZ)/ghex/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf ghex-$(ghex_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: ghostscript
ghostscript:$(GHOSTSCRIPT)_$(ARCH).deb
$(GHOSTSCRIPT): $(SPREZZ)/ghostscript/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf ghostscript-$(ghostscript_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: gimp
gimp:$(GIMP)_$(ARCH).deb
$(GIMP): $(SPREZZ)/gimp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf gimp-$(gimp_UPVER).tar.bz2 --strip-components=1 -C $@

.PHONY: git
git:$(GIT)_$(ARCH).deb
$(GIT): $(SPREZZ)/git/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf git-$(git_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: glib
glib:$(GLIB)_$(ARCH).deb
$(GLIB): $(SPREZZ)/glib/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf glib-$(glib_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: GLU
GLU:$(GLU)_$(ARCH).deb
$(GLU): $(SPREZZ)/GLU/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf glu_$(GLU_UPVER).orig.tar.bz2 --strip-components=1 -C $@

.PHONY: gnome-bluetooth
gnome-bluetooth:$(GNOMEBLUETOOTH)_$(ARCH).deb
$(GNOMEBLUETOOTH): $(SPREZZ)/gnome-bluetooth/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gnome-bluetooth_$(gnome-bluetooth_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: gnome-color-manager
gnome-color-manager:$(GNOMECOLORMANAGER)_$(ARCH).deb
$(GNOMECOLORMANAGER): $(SPREZZ)/gnome-color-manager/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gnome-color-manager_$(gnome-color-manager_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: gnome-dictionary
gnome-dictionary:$(GNOMEDICTIONARY)_$(ARCH).deb
$(GNOMEDICTIONARY): $(SPREZZ)/gnome-dictionary/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gnome-dictionary_$(gnome-dictionary_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: gnome-disk-utility
gnome-disk-utility:$(GNOMEDISKUTILITY)_$(ARCH).deb
$(GNOMEDISKUTILITY): $(SPREZZ)/gnome-disk-utility/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gnome-disk-utility_$(gnome-disk-utility_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: gnome-doc-utils
gnome-doc-utils:$(GNOMEDOCUTILS)_$(ARCH).deb
$(GNOMEDOCUTILS): $(SPREZZ)/gnome-doc-utils/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gnome-doc-utils_$(gnome-doc-utils_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: gnome-font-viewer
gnome-font-viewer:$(GNOMEFONTVIEWER)_$(ARCH).deb
$(GNOMEFONTVIEWER): $(SPREZZ)/gnome-font-viewer/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gnome-font-viewer_$(gnome-font-viewer_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: gnome-icon-theme
gnome-icon-theme:$(GNOMEICONTHEME)_$(ARCH).deb
$(GNOMEICONTHEME): $(SPREZZ)/gnome-icon-theme/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gnome-icon-theme_$(gnome-icon-theme_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: gnome-icon-theme-extras
gnome-icon-theme-extras:$(GNOMEICONTHEMEEXTRAS)_$(ARCH).deb
$(GNOMEICONTHEMEEXTRAS): $(SPREZZ)/gnome-icon-theme-extras/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gnome-icon-theme-extras_$(gnome-icon-theme-extras_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: gnome-icon-theme-symbolic
gnome-icon-theme-symbolic:$(GNOMEICONTHEMESYMBOLIC)_$(ARCH).deb
$(GNOMEICONTHEMESYMBOLIC): $(SPREZZ)/gnome-icon-theme-symbolic/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gnome-icon-theme-symbolic_$(gnome-icon-theme-symbolic_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: gnome-keyring
gnome-keyring:$(GNOMEKEYRING)_$(ARCH).deb
$(GNOMEKEYRING): $(SPREZZ)/gnome-keyring/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gnome-keyring_$(gnome-keyring_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: libgnome-keyring
libgnome-keyring:$(LIBGNOMEKEYRING)_$(ARCH).deb
$(LIBGNOMEKEYRING): $(SPREZZ)/libgnome-keyring/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf libgnome-keyring_$(libgnome-keyring_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: gnome-media
gnome-media:$(GNOMEMEDIA)_$(ARCH).deb
$(GNOMEMEDIA): $(SPREZZ)/gnome-media/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gnome-media_$(gnome-media_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: gnome-menus
gnome-menus:$(GNOMEMENUS)_$(ARCH).deb
$(GNOMEMENUS): $(SPREZZ)/gnome-menus/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gnome-menus_$(gnome-menus_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: gnome-online-accounts
gnome-online-accounts:$(GNOMEONLINEACCOUNTS)_$(ARCH).deb
$(GNOMEONLINEACCOUNTS): $(SPREZZ)/gnome-online-accounts/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gnome-online-accounts_$(gnome-online-accounts_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: gnome-orca
gnome-orca:$(GNOMEORCA)_$(ARCH).deb
$(GNOMEORCA): $(SPREZZ)/gnome-orca/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gnome-orca_$(gnome-orca_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: gnome-screenshot
gnome-screenshot:$(GNOMESCREENSHOT)_$(ARCH).deb
$(GNOMESCREENSHOT): $(SPREZZ)/gnome-screenshot/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gnome-screenshot_$(gnome-screenshot_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: gnome-search-tool
gnome-search-tool:$(GNOMESEARCHTOOL)_$(ARCH).deb
$(GNOMESEARCHTOOL): $(SPREZZ)/gnome-search-tool/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gnome-search-tool_$(gnome-search-tool_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: gnome-session
gnome-session:$(GNOMESESSION)_$(ARCH).deb
$(GNOMESESSION): $(SPREZZ)/gnome-session/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gnome-session_$(gnome-session_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: gnome-settings-daemon
gnome-settings-daemon:$(GNOMESETTINGSDAEMON)_$(ARCH).deb
$(GNOMESETTINGSDAEMON): $(SPREZZ)/gnome-settings-daemon/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gnome-settings-daemon_$(gnome-settings-daemon_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: gnome-sushi
gnome-sushi:$(GNOMESUSHI)_$(ARCH).deb
$(GNOMESUSHI): $(SPREZZ)/gnome-sushi/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gnome-sushi_$(gnome-sushi_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: gnome-terminal
gnome-terminal:$(GNOMETERMINAL)_$(ARCH).deb
$(GNOMETERMINAL): $(SPREZZ)/gnome-terminal/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gnome-terminal_$(gnome-terminal_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: gnome-themes
gnome-themes:$(GNOMETHEMES)_$(ARCH).deb
$(GNOMETHEMES): $(SPREZZ)/gnome-themes/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf gnome-themes_$(gnome-themes_UPVER).orig.tar.bz2 --strip-components=1 -C $@

.PHONY: gnome-themes-standard
gnome-themes-standard:$(GNOMETHEMESSTANDARD)_$(ARCH).deb
$(GNOMETHEMESSTANDARD): $(SPREZZ)/gnome-themes-standard/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gnome-themes-standard_$(gnome-themes-standard_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: gnome-user-docs
gnome-user-docs:$(GNOMEUSERDOCS)_$(ARCH).deb
$(GNOMEUSERDOCS): $(SPREZZ)/gnome-user-docs/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gnome-user-docs_$(gnome-user-docs_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: gnomecatalog
gnomecatalog:$(GNOMECATALOG)_$(ARCH).deb
$(GNOMECATALOG): $(SPREZZ)/gnomecatalog/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf gnomecatalog_$(gnomecatalog_UPVER).orig.tar.bz2 --strip-components=1 -C $@

.PHONY: gnutls
gnutls:$(GNUTLS)_$(ARCH).deb
$(GNUTLS): $(SPREZZ)/gnutls/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gnutls-$(gnutls_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: gobject-introspection
gobject-introspection:$(GOBJECTINTROSPECTION)_$(ARCH).deb
$(GOBJECTINTROSPECTION): $(SPREZZ)/gobject-introspection/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gobject-introspection-$(gobject-introspection_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: gperf
gperf:$(GPERF)_$(ARCH).deb
$(GPERF): $(SPREZZ)/gperf/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf gperf-$(gperf_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: gphoto2
gphoto2:$(GPHOTO2)_$(ARCH).deb
$(GPHOTO2): $(SPREZZ)/gphoto2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf gphoto2-$(gphoto2_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: grilo
grilo:$(GRILO)_$(ARCH).deb
$(GRILO): $(SPREZZ)/grilo/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf grilo-$(grilo_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: gstreamer
gstreamer:$(GSTREAMER)_$(ARCH).deb
$(GSTREAMER): $(SPREZZ)/gstreamer/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gstreamer-$(gstreamer_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: gtkhtml
gtkhtml:$(GTKHTML)_$(ARCH).deb
$(GTKHTML): $(SPREZZ)/gtkhtml/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gtkhtml4.0-$(gtkhtml_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: gtk2
gtk2:$(GTK2)_$(ARCH).deb
$(GTK2): $(SPREZZ)/gtk2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gtk+-$(gtk2_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: gtk3
gtk3:$(GTK3)_$(ARCH).deb
$(GTK3): $(SPREZZ)/gtk3/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gtk+-$(gtk3_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: gvfs
gvfs:$(GVFS)_$(ARCH).deb
$(GVFS): $(SPREZZ)/gvfs/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gvfs-$(gvfs_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: handbrake
handbrake:$(HANDBRAKE)_$(ARCH).deb
$(HANDBRAKE): $(SPREZZ)/handbrake/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf HandBrake-$(handbrake_UPVER).tar.bz2 --strip-components=1 -C $@

.PHONY: icu
icu:$(ICU)_$(ARCH).deb
$(ICU): $(SPREZZ)/icu/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf icu-$(icu_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: iproute
iproute:$(IPROUTE)_$(ARCH).deb
$(IPROUTE): $(SPREZZ)/iproute/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf iproute2-$(iproute_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: itstool
itstool:$(ITSTOOL)_$(ARCH).deb
$(ITSTOOL): $(SPREZZ)/itstool/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf itstool-$(itstool_UPVER).tar.bz2 --strip-components=1 -C $@

.PHONY: inkscape
inkscape:$(INKSCAPE)_$(ARCH).deb
$(INKSCAPE): $(SPREZZ)/inkscape/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf inkscape-$(inkscape_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: jbig2dec
jbig2dec:$(JBIG2DEC)_$(ARCH).deb
$(JBIG2DEC): $(SPREZZ)/jbig2dec/debian/changelog
	git clone git://github.com/dankamongmen/jbig2dec.git $@
	tar cJf jbig2dec-$(jbig2dec_UPVER).tar.xz $@ --exclude-vcs
	ln -s jbig2dec-$(jbig2dec_UPVER).tar.xz jbig2dec_$(jbig2dec_UPVER).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: kismet
kismet:$(KISMET)_$(ARCH).deb
$(KISMET): $(SPREZZ)/kismet/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	{ cd $@ && TARBALL=`uscan --force-download --dehs | xmlstarlet sel -t -v //target` && \
	  cd - && tar xzvf $$TARBALL --strip-components=1 -C $@ ; }

.PHONY: lcms2
lcms2:$(LCMS2)_$(ARCH).deb
$(LCMS2): $(SPREZZ)/lcms2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf lcms2-$(lcms2_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: libatasmart
libatasmart:$(LIBATASMART)_$(ARCH).deb
$(LIBATASMART): $(SPREZZ)/libatasmart/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf libatasmart_$(libatasmart_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: edbus
edbus:$(EDBUS)_$(ARCH).deb
$(EDBUS): $(SPREZZ)/edbus/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf edbus_$(edbus_UPVER).orig.tar.bz2 --strip-components=1 -C $@

.PHONY: embryo
embryo:$(EMBRYO)_$(ARCH).deb
$(EMBRYO): $(SPREZZ)/embryo/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf embryo_$(embryo_UPVER).orig.tar.bz2 --strip-components=1 -C $@

.PHONY: emotion
emotion:$(EMOTION)_$(ARCH).deb
$(EMOTION): $(SPREZZ)/emotion/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf emotion_$(emotion_UPVER).orig.tar.bz2 --strip-components=1 -C $@

.PHONY: ecore
ecore:$(ECORE)_$(ARCH).deb
$(ECORE): $(SPREZZ)/ecore/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf ecore_$(ecore_UPVER).orig.tar.bz2 --strip-components=1 -C $@

.PHONY: edje
edje:$(EDJE)_$(ARCH).deb
$(EDJE): $(SPREZZ)/edje/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf edje_$(edje_UPVER).orig.tar.bz2 --strip-components=1 -C $@

.PHONY: eet
eet:$(EET)_$(ARCH).deb
$(EET): $(SPREZZ)/eet/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf eet_$(eet_UPVER).orig.tar.bz2 --strip-components=1 -C $@

.PHONY: eeze
eeze:$(EEZE)_$(ARCH).deb
$(EEZE): $(SPREZZ)/eeze/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf eeze_$(eeze_UPVER).orig.tar.bz2 --strip-components=1 -C $@

.PHONY: efreet
efreet:$(EFREET)_$(ARCH).deb
$(EFREET): $(SPREZZ)/efreet/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf efreet_$(efreet_UPVER).orig.tar.bz2 --strip-components=1 -C $@

.PHONY: eina
eina:$(EINA)_$(ARCH).deb
$(EINA): $(SPREZZ)/eina/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf eina_$(eina_UPVER).orig.tar.bz2 --strip-components=1 -C $@

.PHONY: eio
eio:$(EIO)_$(ARCH).deb
$(EIO): $(SPREZZ)/eio/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf eio_$(eio_UPVER).orig.tar.bz2 --strip-components=1 -C $@

.PHONY: elementary
elementary:$(ELEMENTARY)_$(ARCH).deb
$(ELEMENTARY): $(SPREZZ)/elementary/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf elementary_$(elementary_UPVER).orig.tar.bz2 --strip-components=1 -C $@

.PHONY: ethumb
ethumb:$(ETHUMB)_$(ARCH).deb
$(ETHUMB): $(SPREZZ)/ethumb/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf ethumb_$(ethumb_UPVER).orig.tar.bz2 --strip-components=1 -C $@

.PHONY: evas
evas:$(EVAS)_$(ARCH).deb
$(EVAS): $(SPREZZ)/evas/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf evas_$(evas_UPVER).orig.tar.bz2 --strip-components=1 -C $@

.PHONY: evas-generic-loaders
evas-generic-loaders:$(EVASGENERICLOADERS)_$(ARCH).deb
$(EVASGENERICLOADERS): $(SPREZZ)/evas-generic-loaders/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf evas-generic-loaders_$(evas-generic-loaders_UPVER).orig.tar.bz2 --strip-components=1 -C $@

.PHONY: exactimage
exactimage:$(EXACTIMAGE)_$(ARCH).deb
$(EXACTIMAGE): $(SPREZZ)/exactimage/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf exactimage_$(exactimage_UPVER).orig.tar.bz2 --strip-components=1 -C $@

.PHONY: exo
exo:$(EXO)_$(ARCH).deb
$(EXO): $(SPREZZ)/exo/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf exo-$(exo_UPVER).tar.bz2 --strip-components=1 -C $@

.PHONY: libcap2
libcap2:$(LIBCAP2)_$(ARCH).deb
$(LIBCAP2): $(SPREZZ)/libcap2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf libcap-$(libcap2_UPVER).tar.bz2 --strip-components=1 -C $@

.PHONY: libexif
libexif:$(LIBEXIF)_$(ARCH).deb
$(LIBEXIF): $(SPREZZ)/libexif/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf libexif-$(libexif_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: libgd2
libgd2:$(LIBGD2)_$(ARCH).deb
$(LIBGD2): $(SPREZZ)/libgd2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf libgd2-$(libgd2_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: libgphoto2
libgphoto2:$(LIBGPHOTO2)_$(ARCH).deb
$(LIBGPHOTO2): $(SPREZZ)/libgphoto2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf libgphoto2-$(libgphoto2_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: libjpeg
libjpeg:$(LIBJPEG)_$(ARCH).deb
$(LIBJPEG): $(SPREZZ)/libjpeg/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf libjpeg8_$(libjpeg_UPVER).orig.tar.gz --strip-components=1 -C $@

.PHONY: libsecret
libsecret:$(LIBSECRET)_$(ARCH).deb
$(LIBSECRET): $(SPREZZ)/libsecret/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf libsecret-$(libsecret_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: libsoup
libsoup:$(LIBSOUP)_$(ARCH).deb
$(LIBSOUP): $(SPREZZ)/libsoup/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf libsoup2.4_$(libsoup_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: libsynthesis
libsynthesis:$(LIBSYNTHESIS)_$(ARCH).deb
$(LIBSYNTHESIS): $(SPREZZ)/libsynthesis/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf libsynthesis_$(synthesis_UPVER).orig.tar.gz --strip-components=1 -C $@

.PHONY: libtasn
libtasn:$(LIBTASN)_$(ARCH).deb
$(LIBTASN): $(SPREZZ)/libtasn/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf libtasn1-$(libtasn_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: libvirt
libvirt:$(LIBVIRT)_$(ARCH).deb
$(LIBVIRT): $(SPREZZ)/libvirt/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf libvirt-$(libvirt_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: libwnck
libwnck:$(LIBWNCK)_$(ARCH).deb
$(LIBWNCK): $(SPREZZ)/libwnck/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf libwnck3-$(libwnck_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: libx86
libx86:$(LIBX86)_$(ARCH).deb
$(LIBX86): $(SPREZZ)/libx86/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf libx86_$(libx86_UPVER).orig.tar.gz --strip-components=1 -C $@

.PHONY: libxcb
libxcb:$(LIBXCB)_$(ARCH).deb
$(LIBXCB): $(SPREZZ)/libxcb/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf libxcb_$(libxcb_UPVER).orig.tar.gz --strip-components=1 -C $@

.PHONY: gmake
gmake:$(GMAKE)_$(ARCH).deb
$(GMAKE): $(SPREZZ)/gmake/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf make-$(gmake_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: lftp
lftp:$(LFTP)_$(ARCH).deb
$(LFTP): $(SPREZZ)/lftp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf lftp_$(lftp_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: mash
mash:$(MASH)_$(ARCH).deb
$(MASH): $(SPREZZ)/mash/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf mash_$(mash_UPVER).orig.tar.gz --strip-components=1 -C $@

.PHONY: mdadm
mdadm:$(MDADM)_$(ARCH).deb
$(MDADM): $(SPREZZ)/mdadm/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf mdadm_$(mdadm_UPVER).orig.tar.gz --strip-components=1 -C $@

.PHONY: metacity
metacity:$(METACITY)_$(ARCH).deb
$(METACITY): $(SPREZZ)/metacity/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf metacity_$(metacity_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: mesa
mesa:$(MESA)_$(ARCH).deb
$(MESA): $(SPREZZ)/mesa/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf mesa_$(mesa_UPVER).orig.tar.gz --strip-components=1 -C $@

.PHONY: mp4v2
mp4v2:$(MP4V2)_$(ARCH).deb
$(MP4V2): $(SPREZZ)/mp4v2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf mp4v2_$(mp4v2_UPVER).orig.tar.bz2 --strip-components=1 -C $@

.PHONY: mpd
mpd:$(MPD)_$(ARCH).deb
$(MPD): $(SPREZZ)/mpd/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf mpd_$(mpd_UPVER).orig.tar.gz --strip-components=1 -C $@

.PHONY: mutt
mutt:$(MUTT)_$(ARCH).deb
$(MUTT): $(SPREZZ)/mutt/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf mutt-$(mutt_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: mutter
mutter:$(MUTTER)_$(ARCH).deb
$(MUTTER): $(SPREZZ)/mutter/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf mutter_$(mutter_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: nautilus
nautilus:$(NAUTILUS)_$(ARCH).deb
$(NAUTILUS): $(SPREZZ)/nautilus/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf nautilus_$(nautilus_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: nautilus-sendto
nautilus-sendto:$(NAUTILUSSENDTO)_$(ARCH).deb
$(NAUTILUSSENDTO): $(SPREZZ)/nautilus-sendto/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf nautilus-sendto_$(nautilus-sendto_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: ncmpcpp
ncmpcpp:$(NCMPCPP)_$(ARCH).deb
$(NCMPCPP): $(SPREZZ)/ncmpcpp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf ncmpcpp_$(ncmpcpp_UPVER).orig.tar.bz2 --strip-components=1 -C $@

.PHONY: netcf
netcf:$(NETCF)_$(ARCH).deb
$(NETCF): $(SPREZZ)/netcf/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf netcf_$(netcf_UPVER).orig.tar.gz --strip-components=1 -C $@

.PHONY: nfs-utils
nfs-utils:$(NFSUTILS)_$(ARCH).deb
$(NFSUTILS): $(SPREZZ)/nfs-utils/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf nfs-utils_$(nfs-utils_UPVER).orig.tar.bz2 --strip-components=1 -C $@

.PHONY: numactl
numactl:$(NUMACTL)_$(ARCH).deb
$(NUMACTL): $(SPREZZ)/numactl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf numactl-$(numactl_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: nvidia-cuda-toolkit
nvidia-cuda-toolkit:$(NVIDIACUDATOOLKIT)_$(ARCH).deb
$(NVIDIACUDATOOLKIT): $(SPREZZ)/nvidia-cuda-toolkit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf nvidia-cuda-toolkit-$(nvidia-cuda-toolkit_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: opencv
opencv:$(OPENCV)_$(ARCH).deb
$(OPENCV): $(SPREZZ)/opencv/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf opencv-$(opencv_UPVER).tar.bz2 --strip-components=1 -C $@

.PHONY: openldap
openldap:$(OPENLDAP)_$(ARCH).deb
$(OPENLDAP): $(SPREZZ)/openldap/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf openldap_$(openldap_UPVER).orig.tar.gz --strip-components=1 -C $@

.PHONY: pcre
pcre:$(PCRE)_$(ARCH).deb
$(PCRE): $(SPREZZ)/pcre/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	{ cd $@ && TARBALL=`uscan --no-symlink --force-download --dehs | xmlstarlet sel -t -v //upstream-url` && \
	  cd - && ln -sf `basename $$TARBALL` pcre-$(pcre_UPVER).orig.zip && unzip `basename $$TARBALL` -d $@ ; }

.PHONY: packagekit
packagekit:$(PACKAGEKIT)_$(ARCH).deb
$(PACKAGEKIT): $(SPREZZ)/packagekit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf packagekit_$(packagekit_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: pidgin
pidgin:$(PIDGIN)_$(ARCH).deb
$(PIDGIN): $(SPREZZ)/pidgin/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf pidgin-$(pidgin_UPVER).tar.bz2 --strip-components=1 -C $@

.PHONY: pixman
pixman:$(PIXMAN)_$(ARCH).deb
$(PIXMAN): $(SPREZZ)/pixman/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf pixman-$(pixman_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: procps
procps:$(PROCPS)_$(ARCH).deb
$(PROCPS): $(SPREZZ)/procps/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf procps-$(procps_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: pygobject
pygobject:$(PYGOBJECT)_$(ARCH).deb
$(PYGOBJECT): $(SPREZZ)/pygobject/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf pygobject-$(pygobject_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: ratpoison
ratpoison:$(RATPOISON)_$(ARCH).deb
$(RATPOISON): $(SPREZZ)/ratpoison/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf ratpoison-$(ratpoison_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: razorqt
razorqt:$(RAZORQT)_$(ARCH).deb
$(RAZORQT): $(SPREZZ)/razorqt/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf razorqt-$(razorqt_UPVER).tar.bz2 --strip-components=1 -C $@

.PHONY: reaver
reaver:$(REAVER)_$(ARCH).deb
$(REAVER): $(SPREZZ)/reaver/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf reaver-$(reaver_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: reportbug
reportbug:$(REPORTBUG)_$(ARCH).deb
$(REPORTBUG): $(SPREZZ)/reportbug/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf reportbug-$(reportbug_UPVER).tar.bz2 --strip-components=1 -C $@

.PHONY: rtmpdump
rtmpdump:$(RTMPDUMP)_$(ARCH).deb
$(RTMPDUMP): $(SPREZZ)/rtmpdump/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf rtmpdump-$(rtmpdump_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: screenlets
screenlets:$(SCREENLETS)_$(ARCH).deb
$(SCREENLETS): $(SPREZZ)/screenlets/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf screenlets-$(screenlets_UPVER).tar.bz2 --strip-components=1 -C $@

.PHONY: shotwell
shotwell:$(SHOTWELL)_$(ARCH).deb
$(SHOTWELL): $(SPREZZ)/shotwell/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf shotwell_$(shotwell_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: simple-scan
simple-scan:$(SIMPLESCAN)_$(ARCH).deb
$(SIMPLESCAN): $(SPREZZ)/simple-scan/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf simple-scan_$(simple-scan_UPVER).orig.tar.gz --strip-components=1 -C $@

.PHONY: syncevolution
syncevolution:$(SYNCEVOLUTION)_$(ARCH).deb
$(SYNCEVOLUTION): $(SPREZZ)/syncevolution/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf syncevolution_$(syncevolution_UPVER).orig.tar.gz --strip-components=1 -C $@

.PHONY: systemd
systemd:$(SYSTEMD)_$(ARCH).deb
$(SYSTEMD): $(SPREZZ)/systemd/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf systemd_$(systemd_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: util-linux
util-linux:$(UTILLINUX)_$(ARCH).deb
$(UTILLINUX): $(SPREZZ)/util-linux/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf util-linux-$(util-linux_UPVER).tar.bz2 --strip-components=1 -C $@

.PHONY: telepathy-glib
telepathy-glib:$(TELEPATHYGLIB)_$(ARCH).deb
$(TELEPATHYGLIB): $(SPREZZ)/telepathy-glib/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf telepathy-glib-$(telepathy-glib_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: vim
vim:$(VIM)_$(ARCH).deb
$(VIM): $(SPREZZ)/vim/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	{ cd $@ && TARBALL=`uscan --force-download --dehs | xmlstarlet sel -t -v //target` && \
	  cd - && tar xjvf $$TARBALL --strip-components=1 -C $@ ; }

.PHONY: vinagre
vinagre:$(VINAGRE)_$(ARCH).deb
$(VINAGRE): $(SPREZZ)/vinagre/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf vinagre_$(vinagre_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: virtualbox
virtualbox:$(VIRTUALBOX)_$(ARCH).deb
$(VIRTUALBOX): $(SPREZZ)/virtualbox/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf virtualbox_$(virtualbox_UPVER).orig.tar.bz2 --strip-components=1 -C $@

.PHONY: vte
vte:$(VTE)_$(ARCH).deb
$(VTE): $(SPREZZ)/vte/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf vte_$(vte_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: wayland
wayland:$(WAYLAND)_$(ARCH).deb
$(WAYLAND): $(SPREZZ)/wayland/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf wayland-$(wayland_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: w3m
w3m:$(W3M)_$(ARCH).deb
$(W3M): $(SPREZZ)/w3m/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf w3m-$(w3m_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: webkit
webkit:$(WEBKIT)_$(ARCH).deb
$(WEBKIT): $(SPREZZ)/webkit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf webkit_$(webkit_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: wifite
wifite:$(WIFITE)_$(ARCH).deb
$(WIFITE): $(SPREZZ)/wifite/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf wifite-$(wifite_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: wireless-tools
wireless-tools:$(WIRELESSTOOLS)_$(ARCH).deb
$(WIRELESSTOOLS): $(SPREZZ)/wireless-tools/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	wget http://www.hpl.hp.com/personal/Jean_Tourrilhes/Linux/wireless_tools.30.pre9.tar.gz
	tar xzvf wireless-tools-$(wireless-tools_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: wireshark
wireshark:$(WIRESHARK)_$(ARCH).deb
$(WIRESHARK): $(SPREZZ)/wireshark/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf wireshark_$(wireshark_UPVER).orig.tar.bz2 --strip-components=1 -C $@
	rm -rf $@/debian
	cp -r $(<D) $@/

.PHONY: wpa
wpa:$(WPA)_$(ARCH).deb
$(WPA): $(SPREZZ)/wpa/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf wpa_$(wpa_UPVER).orig.tar.gz --strip-components=1 -C $@

.PHONY: x11proto-core
x11proto-core:$(X11PROTOCORE)_$(ARCH).deb
$(X11PROTOCORE): $(SPREZZ)/x11proto-core/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf xproto-$(x11proto-core_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: x11proto-damage
x11proto-damage:$(X11PROTODAMAGE)_$(ARCH).deb
$(X11PROTODAMAGE): $(SPREZZ)/x11proto-damage/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf damageproto-$(x11proto-damage_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: x11proto-fixes
x11proto-fixes:$(X11PROTOFIXES)_$(ARCH).deb
$(X11PROTOFIXES): $(SPREZZ)/x11proto-fixes/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf fixesproto-$(x11proto-fixes_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: x11proto-fonts
x11proto-fonts:$(X11PROTOFONTS)_$(ARCH).deb
$(X11PROTOFONTS): $(SPREZZ)/x11proto-fonts/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf fontsproto-$(x11proto-fonts_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: x11proto-gl
x11proto-gl:$(X11PROTOGL)_$(ARCH).deb
$(X11PROTOGL): $(SPREZZ)/x11proto-gl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf glproto-$(x11proto-gl_UPVER).tar.bz2 --strip-components=1 -C $@

.PHONY: x11proto-input
x11proto-input:$(X11PROTOINPUT)_$(ARCH).deb
$(X11PROTOINPUT): $(SPREZZ)/x11proto-input/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf inputproto-$(x11proto-input_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: x11proto-kb
x11proto-kb:$(X11PROTOKB)_$(ARCH).deb
$(X11PROTOKB): $(SPREZZ)/x11proto-kb/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf kbproto-$(x11proto-kb_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: x11proto-randr
x11proto-randr:$(X11PROTORANDR)_$(ARCH).deb
$(X11PROTORANDR): $(SPREZZ)/x11proto-randr/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf randrproto-$(x11proto-randr_UPVER).tar.bz2 --strip-components=1 -C $@

.PHONY: x11proto-record
x11proto-record:$(X11PROTORECORD)_$(ARCH).deb
$(X11PROTORECORD): $(SPREZZ)/x11proto-record/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf recordproto-$(x11proto-record_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: x11proto-render
x11proto-render:$(X11PROTORENDER)_$(ARCH).deb
$(X11PROTORENDER): $(SPREZZ)/x11proto-render/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf renderproto-$(x11proto-render_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: x11proto-resource
x11proto-resource:$(X11PROTORESOURCE)_$(ARCH).deb
$(X11PROTORESOURCE): $(SPREZZ)/x11proto-resource/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf resourceproto-$(x11proto-resource_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: x11proto-scrnsaver
x11proto-scrnsaver:$(X11PROTOSCRNSAVER)_$(ARCH).deb
$(X11PROTOSCRNSAVER): $(SPREZZ)/x11proto-scrnsaver/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf scrnsaverproto-$(x11proto-scrnsaver_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: x11proto-video
x11proto-video:$(X11PROTOVIDEO)_$(ARCH).deb
$(X11PROTOVIDEO): $(SPREZZ)/x11proto-video/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf videoproto-$(x11proto-video_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: x11proto-xext
x11proto-xext:$(X11PROTOXEXT)_$(ARCH).deb
$(X11PROTOXEXT): $(SPREZZ)/x11proto-xext/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf xextproto-$(x11proto-xext_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: x11proto-xf86dga
x11proto-xf86dga:$(X11PROTOXF86DGA)_$(ARCH).deb
$(X11PROTOXF86DGA): $(SPREZZ)/x11proto-xf86dga/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf xf86dgaproto-$(x11proto-xf86dga_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: x11proto-xf86dri
x11proto-xf86dri:$(X11PROTOXF86DRI)_$(ARCH).deb
$(X11PROTOXF86DRI): $(SPREZZ)/x11proto-xf86dri/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf xf86driproto-$(x11proto-xf86dri_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: x11proto-xf86vidmode
x11proto-xf86vidmode:$(X11PROTOXF86VIDMODE)_$(ARCH).deb
$(X11PROTOXF86VIDMODE): $(SPREZZ)/x11proto-xf86vidmode/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf xf86vidmodeproto-$(x11proto-xf86vidmode_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: x11proto-xinerama
x11proto-xinerama:$(X11PROTOXINERAMA)_$(ARCH).deb
$(X11PROTOXINERAMA): $(SPREZZ)/x11proto-xinerama/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf xineramaproto-$(x11proto-xinerama_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: xcb-proto
xcb-proto:$(XCBPROTO)_$(ARCH).deb
$(XCBPROTO): $(SPREZZ)/xcb-proto/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf xcb-proto_$(xcb-proto_UPVER).orig.tar.gz --strip-components=1 -C $@

.PHONY: xfconf
xfconf:$(XFCONF)_$(ARCH).deb
$(XFCONF): $(SPREZZ)/xfconf/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf xfconf-$(xfconf_UPVER).tar.bz2 --strip-components=1 -C $@

.PHONY: libxfce4ui
libxfce4ui:$(LIBXFCE4UI)_$(ARCH).deb
$(LIBXFCE4UI): $(SPREZZ)/libxfce4ui/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf libxfce4ui-$(libxfce4ui_UPVER).tar.bz2 --strip-components=1 -C $@

.PHONY: libxfce4util
libxfce4util:$(LIBXFCE4UTIL)_$(ARCH).deb
$(LIBXFCE4UTIL): $(SPREZZ)/libxfce4util/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf libxfce4util-$(libxfce4util_UPVER).tar.bz2 --strip-components=1 -C $@

.PHONY: libxrender
libxrender:$(LIBXRENDER)_$(ARCH).deb
$(LIBXRENDER): $(SPREZZ)/libxrender/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf libXrender-$(libxrender_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: xfce4-terminal
xfce4-terminal:$(XFCE4TERMINAL)_$(ARCH).deb
$(XFCE4TERMINAL): $(SPREZZ)/xfce4-terminal/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf Terminal-$(xfce4-terminal_UPVER).tar.bz2 --strip-components=1 -C $@

.PHONY: xinput
xinput:$(XINPUT)_$(ARCH).deb
$(XINPUT): $(SPREZZ)/xinput/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf xinput-$(xinput_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: xorg-xserver
xorg-xserver:$(XORGXSERVER)_$(ARCH).deb
$(XORGXSERVER): $(SPREZZ)/xorg-xserver/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf xorg-server-$(xorg-xserver_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: udisks
udisks:$(UDISKS)_$(ARCH).deb
$(UDISKS): $(SPREZZ)/udisks/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf udisks_$(udisks_UPVER).orig.tar.bz2 --strip-components=1 -C $@

.PHONY: yelp
yelp:$(YELP)_$(ARCH).deb
$(YELP): $(SPREZZ)/yelp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf yelp-$(yelp_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: yelp-tools
yelp-tools:$(YELPTOOLS)_$(ARCH).deb
$(YELPTOOLS): $(SPREZZ)/yelp-tools/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf yelp-tools-$(yelp-tools_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: yelp-xsl
yelp-xsl:$(YELPXSL)_$(ARCH).deb
$(YELPXSL): $(SPREZZ)/yelp-xsl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf yelp-xsl-$(yelp-xsl_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: zerofree
zerofree:$(ZEROFREE)_$(ARCH).deb
$(ZEROFREE): $(SPREZZ)/zerofree/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf zerofree-$(zerofree_UPVER).tgz --strip-components=1 -C $@

.PHONY: zenity
zenity:$(ZENITY)_$(ARCH).deb
$(ZENITY): $(SPREZZ)/zenity/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf zenity-$(zenity_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: zsh
zsh:$(ZSH)_$(ARCH).deb
$(ZSH): $(SPREZZ)/zsh/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf zsh-$(zsh_UPVER).tar.bz2 --strip-components=1 -C $@

FETCHED:=$(FETCHED) $(CURLUP).tar.bz2
$(CURLUP).tar.bz2:
	wget -nc -O$@ http://curl.haxx.se/download/$@

$(CURLORIG): $(CURLUP).tar.bz2
	ln -s $< $@

.PHONY: curl
curl:$(CURL)_$(ARCH).deb
$(CURL): $(SPREZZ)/curl/debian/changelog $(CURLORIG)
	mkdir $@
	tar xjvf $(CURLUP).tar.bz2 --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(EGLIBCUP).tar.gz
$(EGLIBCUP).tar.gz:
	wget -nc -O$@ ftp://ftp.gnu.org/gnu/glibc/$@

$(EGLIBCORIG): $(EGLIBCUP).tar.gz
	ln -s $< $@

.PHONY: eglibc
eglibc:$(EGLIBC)_$(ARCH).deb
$(EGLIBC): $(SPREZZ)/eglibc/debian/changelog $(EGLIBCORIG)
	mkdir $@
	tar xzvf $(EGLIBCORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(FBIUP).tar.gz
$(FBIUP).tar.gz:
	wget -nc -O$@ http://www.kraxel.org/releases/fbida/$@

.PHONY: fbi
fbi:$(FBI)_$(ARCH).deb
$(FBI): $(SPREZZ)/fbi/debian/changelog $(FBIUP).tar.gz
	mkdir $@
	tar xzvf $(FBIUP).tar.gz --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) libjpeg-turbo-1.2.1.tar.gz
libjpeg-turbo-1.2.1.tar.gz:
	wget -nc -O$@ http://sourceforge.net/projects/libjpeg-turbo/files/1.2.1/libjpeg-turbo-1.2.1.tar.gz/download

.PHONY: libjpeg-turbo
libjpeg-turbo:$(LIBJPEG8TURBO)_$(ARCH).deb
$(LIBJPEG8TURBO): $(SPREZZ)/libjpeg-turbo/debian/changelog libjpeg-turbo-1.2.1.tar.gz
	mkdir $@
	tar xzvf libjpeg-turbo-1.2.1.tar.gz --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(FBTERMUP).tar.gz
$(FBTERMUP).tar.gz:
	wget -nc -O$@ http://nick-black.com/pub/$@

$(FBTERMORIG): $(FBTERMUP).tar.gz
	ln -s $< $@

.PHONY: fbterm
fbterm:$(FBTERM)_$(ARCH).deb
$(FBTERM): $(SPREZZ)/fbterm/debian/changelog $(FBTERMORIG)
	mkdir $@
	tar xzvf $(FBTERMORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(FONTCONFIGUP).tar.gz
$(FONTCONFIGUP).tar.gz:
	wget -nc -O$@ http://www.freedesktop.org/software/fontconfig/release/$@

$(FONTCONFIGORIG): $(FONTCONFIGUP).tar.gz
	ln -s $< $@

.PHONY: fontconfig
fontconfig:$(FONTCONFIG)_$(ARCH).deb
$(FONTCONFIG): $(SPREZZ)/fontconfig/debian/changelog $(FONTCONFIGORIG)
	mkdir $@
	tar xzvf $(FONTCONFIGORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(FREETYPEUP).tar.gz
$(FREETYPEUP).tar.gz:
	wget -nc -O$@ http://download.savannah.gnu.org/releases/freetype/$@

$(FREETYPEORIG): $(FREETYPEUP).tar.gz
	ln -s $< $@

.PHONY: freetype
freetype:$(FREETYPE)_$(ARCH).deb
$(FREETYPE): $(SPREZZ)/freetype/debian/changelog $(FREETYPEORIG)
	mkdir $@
	tar xzvf $(FREETYPEORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(GDKPIXBUFUP).tar.xz
$(GDKPIXBUFUP).tar.xz:
	wget -nc -O$@ http://ftp.acc.umu.se/pub/gnome/sources/gdk-pixbuf/2.26/$@

$(GDKPIXBUFORIG): $(GDKPIXBUFUP).tar.xz
	ln -s $< $@

.PHONY: gdk-pixbuf
gdk-pixbuf:$(GDKPIXBUF)_$(ARCH).deb
$(GDKPIXBUF): $(SPREZZ)/gdk-pixbuf/debian/changelog $(GDKPIXBUFORIG)
	mkdir $@
	tar xJvf $(GDKPIXBUFORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(HARFBUZZUP).tar.gz
$(HARFBUZZUP).tar.gz:
	wget -nc -O$@ http://cgit.freedesktop.org/harfbuzz/snapshot/$@

$(HARFBUZZORIG): $(HARFBUZZUP).tar.gz
	ln -s $< $@

.PHONY: harfbuzz
harfbuzz:$(HARFBUZZ)_$(ARCH).deb
$(HARFBUZZ): $(SPREZZ)/harfbuzz/debian/changelog $(HARFBUZZORIG)
	mkdir $@
	tar xzvf $(HARFBUZZORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(HFSUTILSUP).tar.gz
$(HFSUTILSUP).tar.gz:
	wget -nc -O$@ ftp://ftp.mars.org/pub/hfs/$@

$(HFSUTILSORIG): $(HFSUTILSUP).tar.gz
	ln -s $< $@

.PHONY: hfsutils
hfsutils:$(HFSUTILS)_$(ARCH).deb
$(HFSUTILS): $(SPREZZ)/hfsutils/debian/changelog $(HFSUTILSORIG)
	mkdir $@
	tar xzvf $(HFSUTILSORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(HWLOCUP).tar.bz2
$(HWLOCUP).tar.bz2:
	wget -nc -O$@ http://www.open-mpi.org/software/hwloc/v1.5/downloads/$@

$(HWLOCORIG): $(HWLOCUP).tar.bz2
	ln -s $< $@

.PHONY: hwloc
hwloc:$(HWLOC)_$(ARCH).deb
$(HWLOC): $(SPREZZ)/hwloc/debian/changelog $(HWLOCORIG)
	mkdir $@
	tar xjvf $(HWLOCORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(LESSUP).tar.gz
$(LESSUP).tar.gz:
	wget -nc -O$@ http://www.greenwoodsoftware.com/less/$@

$(LESSORIG): $(LESSUP).tar.gz
	ln -s $< $@

.PHONY: less
less:$(LESS)_$(ARCH).deb
$(LESS): $(SPREZZ)/less/debian/changelog $(LESSORIG)
	mkdir $@
	tar xzvf $(LESSORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(LIBDRMUP).tar.bz2
$(LIBDRMUP).tar.bz2:
	wget -nc -O$@ http://dri.freedesktop.org/libdrm/$(LIBDRMUP).tar.bz2

$(LIBDRMORIG): $(LIBDRMUP).tar.bz2
	ln -s $< $@

.PHONY: libdrm
libdrm:$(LIBDRM)_$(ARCH).deb
$(LIBDRM): $(SPREZZ)/libdrm/debian/changelog $(LIBDRMORIG)
	mkdir $@
	tar xjvf $(LIBDRMORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(LIBPNGUP).tar.bz2
$(LIBPNGUP).tar.bz2:
	wget -nc -O$@ ftp://ftp.simplesystems.org/pub/libpng/png/src/$(LIBPNGUP).tar.bz2

$(LIBPNGORIG): $(LIBPNGUP).tar.bz2
	ln -s $< $@

.PHONY: libpng
libpng:$(LIBPNG)_$(ARCH).deb
$(LIBPNG): $(SPREZZ)/libpng/debian/changelog $(LIBPNGORIG)
	mkdir $@
	tar xjvf $(LIBPNGORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) sudo-1.8.5p3.tar.gz
sudo-1.8.5p3.tar.gz:
	wget -nc -O$@ http://www.gratisoft.us/sudo/dist/sudo-1.8.5p3.tar.gz

FETCHED:=$(FETCHED) $(ATKUP).tar.xz
$(ATKUP).tar.xz:
	wget -nc -O$@ http://ftp.gnome.org/pub/GNOME/sources/atk/2.6/$@

$(ATKORIG): $(ATKUP).tar.xz
	ln -sf $< $@

.PHONY: atk
atk:$(ATK)_$(ARCH).deb
$(ATK): $(SPREZZ)/atk/debian/changelog $(ATKORIG)
	mkdir -p $@
	tar xJvf $(ATKORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(EVINCEUP).tar.xz
$(EVINCEUP).tar.xz:
	wget -nc -O$@ http://ftp.gnome.org/pub/GNOME/sources/evince/3.6/$@

$(EVINCEORIG): $(EVINCEUP).tar.xz
	ln -sf $< $@

.PHONY: evince
evince:$(EVINCE)_$(ARCH).deb
$(EVINCE): $(SPREZZ)/evince/debian/changelog $(EVINCEORIG)
	mkdir -p $@
	tar xJvf $(EVINCEORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(PANGOUP).tar.xz
$(PANGOUP).tar.xz:
	wget -nc -O$@ http://ftp.gnome.org/pub/GNOME/sources/pango/1.32/$@

$(PANGOORIG): $(PANGOUP).tar.xz
	ln -s $< $@

.PHONY: pango
pango:$(PANGO)_$(ARCH).deb
$(PANGO): $(SPREZZ)/pango/debian/changelog $(PANGOORIG)
	mkdir $@
	tar xJvf $(PANGOORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

.PHONY: poppler
poppler:$(POPPLER)_$(ARCH).deb
$(POPPLER): $(SPREZZ)/poppler/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf $(POPPLERORIG) --strip-components=1 -C $@

FETCHED:=$(FETCHED) $(PULSEAUDIOUP).tar.xz
$(PULSEAUDIOUP).tar.xz:
	wget -nc -O$@ http://freedesktop.org/software/pulseaudio/releases/$(@F)

$(PULSEAUDIOORIG): $(PULSEAUDIOUP).tar.xz
	ln -s $< $@

.PHONY: pulseaudio
pulseaudio:$(PULSEAUDIO)_$(ARCH).deb
$(PULSEAUDIO): $(SPREZZ)/pulseaudio/debian/changelog $(PULSEAUDIOORIG)
	mkdir $@
	tar xJvf $(PULSEAUDIOORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(SOCATUP).tar.bz2
$(SOCATUP).tar.bz2:
	wget -nc -O$@ http://www.dest-unreach.org/socat/download/$(@F)

$(SOCATORIG): $(SOCATUP).tar.bz2
	ln -s $< $@

.PHONY: socat
socat:$(SOCAT)_$(ARCH).deb
$(SOCAT): $(SPREZZ)/socat/debian/changelog $(SOCATORIG)
	mkdir $@
	tar xjvf $(SOCATORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

.PHONY: sudo
sudo:$(SUDO)_$(ARCH).deb
$(SUDO): $(SPREZZ)/sudo/debian/changelog sudo-1.8.5p3.tar.gz
	mkdir $@
	tar xzvf sudo-1.8.5p3.tar.gz --strip-components=1 -C $@
	cp -r $(<D) $@/

.PHONY: grubtheme
sprezzos-grub2theme:$(GRUBTHEME)_$(ARCH).deb
$(GRUBTHEME): $(SPREZZ)/sprezzos-grub2theme/debian/changelog
	mkdir -p $@
	cp -r $(SPREZZ)/sprezzos-grub2theme/images $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(IBUSUP).tar.gz
$(IBUSUP).tar.gz:
	wget -nc -O$@ http://ibus.googlecode.com/files/$@

$(IBUSORIG): $(IBUSUP).tar.gz
	ln -sf $< $@

.PHONY: ibus
ibus:$(IBUS)_$(ARCH).deb
$(IBUS): $(SPREZZ)/ibus/debian/changelog $(IBUSORIG)
	mkdir -p $@
	tar xzvf $(IBUSORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) App-ConPalette-0.1.5.tar.gz
App-ConPalette-0.1.5.tar.gz:
	wget -nc -O$@ http://search.cpan.org/CPAN/authors/id/H/HI/HINRIK/App-ConPalette-0.1.5.tar.gz

FETCHED:=$(FETCHED) $(LVM2UP).tgz
$(LVM2UP).tgz:
	wget -nc -O$@ ftp://sources.redhat.com/pub/lvm2/$@

.PHONY: lvm2
lvm2:$(LVM2)_$(ARCH).deb
$(LVM2): $(SPREZZ)/lvm2/debian/changelog $(LVM2UP).tgz
	mkdir -p $@
	tar xzvf $(LVM2UP).tgz --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(OPENSSHUP).tar.gz
$(OPENSSHUP).tar.gz:
	wget -nc -O$@ ftp://ftp.openbsd.org/pub/OpenBSD/OpenSSH/portable/$(OPENSSHUP).tar.gz

.PHONY: openssh
openssh:$(OPENSSH)_$(ARCH).deb
$(OPENSSH): $(SPREZZ)/openssh/debian/changelog $(OPENSSHUP).tar.gz
	mkdir -p $@
	tar xzvf $(OPENSSHUP).tar.gz --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(GRUBUP).tar.xz
$(GRUBUP).tar.xz:
	wget -nc -O$@ http://ftp.gnu.org/gnu/grub/$(GRUBUP).tar.xz

.PHONY: grub2
grub2:$(GRUB2)_$(ARCH).deb
$(GRUB2): $(SPREZZ)/grub2/debian/changelog $(GRUBUP).tar.xz
	mkdir -p $@
	tar xJvf $(GRUBUP).tar.xz --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(LIBXMLUP).tar.gz
$(LIBXMLUP).tar.gz:
	wget -nc -O$@ ftp://xmlsoft.org/libxml/$@

$(LIBXMLORIG): $(LIBXMLUP).tar.gz
	ln -sf $< $@

.PHONY: libxml
libxml:$(LIBXML)_$(ARCH).deb
$(LIBXML): $(SPREZZ)/libxml/debian/changelog $(LIBXMLORIG)
	mkdir -p $@
	tar xzvf $(LIBXMLORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(LIBXSLTUP).tar.gz
$(LIBXSLTUP).tar.gz:
	wget -nc -O$@ ftp://xmlsoft.org/libxslt/$@

$(LIBXSLTORIG): $(LIBXSLTUP).tar.gz
	ln -sf $< $@

.PHONY: libxslt
libxslt:$(LIBXSLT)_$(ARCH).deb
$(LIBXSLT): $(SPREZZ)/libxslt/debian/changelog $(LIBXSLTORIG)
	mkdir -p $@
	tar xzvf $(LIBXSLTORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(BRASEROUP).tar.xz
$(BRASEROUP).tar.xz:
	wget -nc -O$@ http://ftp.gnome.org/pub/GNOME/sources/brasero/3.6/$@

$(BRASEROORIG): $(BRASEROUP).tar.xz
	ln -sf $< $@

.PHONY: brasero
brasero:$(BRASERO)_$(ARCH).deb
$(BRASERO): $(SPREZZ)/brasero/debian/changelog $(BRASEROORIG)
	mkdir -p $@
	tar xJvf $(BRASEROORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(CHEESEUP).tar.xz
$(CHEESEUP).tar.xz:
	wget -nc -O$@ http://ftp.gnome.org/pub/GNOME/sources/cheese/3.6/$@

$(CHEESEORIG): $(CHEESEUP).tar.xz
	ln -sf $< $@

.PHONY: cheese
cheese:$(CHEESE)_$(ARCH).deb
$(CHEESE): $(SPREZZ)/cheese/debian/changelog $(CHEESEORIG)
	mkdir -p $@
	tar xJvf $(CHEESEORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(CLUTTERUP).tar.xz
$(CLUTTERUP).tar.xz:
	wget -nc -O$@ http://ftp.gnome.org/pub/GNOME/sources/clutter/1.12/$@

$(CLUTTERORIG): $(CLUTTERUP).tar.xz
	ln -sf $< $@

.PHONY: clutter
clutter:$(CLUTTER)_$(ARCH).deb
$(CLUTTER): $(SPREZZ)/clutter/debian/changelog $(CLUTTERORIG)
	mkdir -p $@
	tar xJvf $(CLUTTERORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(CLUTTERGSTUP).tar.xz
$(CLUTTERGSTUP).tar.xz:
	wget -nc -O$@ http://ftp.gnome.org/pub/GNOME/sources/clutter-gst/1.9/$@

$(CLUTTERGSTORIG): $(CLUTTERGSTUP).tar.xz
	ln -sf $< $@

.PHONY: clutter-gst
clutter-gst:$(CLUTTERGST)_$(ARCH).deb
$(CLUTTERGST): $(SPREZZ)/clutter-gst/debian/changelog $(CLUTTERGSTORIG)
	mkdir -p $@
	tar xJvf $(CLUTTERGSTORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(CLUTTERGTKUP).tar.xz
$(CLUTTERGTKUP).tar.xz:
	wget -nc -O$@ http://ftp.gnome.org/pub/GNOME/sources/clutter-gtk/1.4/$@

$(CLUTTERGTKORIG): $(CLUTTERGTKUP).tar.xz
	ln -sf $< $@

.PHONY: clutter-gtk
clutter-gtk:$(CLUTTERGTK)_$(ARCH).deb
$(CLUTTERGTK): $(SPREZZ)/clutter-gtk/debian/changelog $(CLUTTERGTKORIG)
	mkdir -p $@
	tar xJvf $(CLUTTERGTKORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(GNOMECONTACTSUP).tar.xz
$(GNOMECONTACTSUP).tar.xz:
	wget -nc -O$@ http://ftp.gnome.org/pub/GNOME/sources/gnome-contacts/3.6/$@

$(GNOMECONTACTSORIG): $(GNOMECONTACTSUP).tar.xz
	ln -sf $< $@

.PHONY: gnome-contacts
gnome-contacts:$(GNOMECONTACTS)_$(ARCH).deb
$(GNOMECONTACTS): $(SPREZZ)/gnome-contacts/debian/changelog $(GNOMECONTACTSORIG)
	mkdir -p $@
	tar xJvf $(GNOMECONTACTSORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(GNOMECONTROLCENTERUP).tar.xz
$(GNOMECONTROLCENTERUP).tar.xz:
	wget -nc -O$@ http://ftp.gnome.org/pub/GNOME/sources/gnome-control-center/3.6/$@

$(GNOMECONTROLCENTERORIG): $(GNOMECONTROLCENTERUP).tar.xz
	ln -sf $< $@

.PHONY: gnome-control-center
gnome-control-center:$(GNOMECONTROLCENTER)_$(ARCH).deb
$(GNOMECONTROLCENTER): $(SPREZZ)/gnome-control-center/debian/changelog $(GNOMECONTROLCENTERORIG)
	mkdir -p $@
	tar xJvf $(GNOMECONTROLCENTERORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(GNOMEDESKTOPUP).tar.xz
$(GNOMEDESKTOPUP).tar.xz:
	wget -nc -O$@ http://ftp.gnome.org/pub/GNOME/sources/gnome-desktop/3.6/$@

$(GNOMEDESKTOPORIG): $(GNOMEDESKTOPUP).tar.xz
	ln -sf $< $@

.PHONY: gnome-desktop
gnome-desktop:$(GNOMEDESKTOP)_$(ARCH).deb
$(GNOMEDESKTOP): $(SPREZZ)/gnome-desktop/debian/changelog $(GNOMEDESKTOPORIG)
	mkdir -p $@
	tar xJvf $(GNOMEDESKTOPORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(GNOMEPOWERMANAGERUP).tar.xz
$(GNOMEPOWERMANAGERUP).tar.xz:
	wget -nc -O$@ http://ftp.gnome.org/pub/gnome/sources/gnome-power-manager/3.6/$@

$(GNOMEPOWERMANAGERORIG): $(GNOMEPOWERMANAGERUP).tar.xz
	ln -sf $< $@

.PHONY: gnome-power-manager
gnome-power-manager:$(GNOMEPOWERMANAGER)_$(ARCH).deb
$(GNOMEPOWERMANAGER): $(SPREZZ)/gnome-power-manager/debian/changelog $(GNOMEPOWERMANAGERORIG)
	mkdir -p $@
	tar xJvf $(GNOMEPOWERMANAGERORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(GSETSCHEMASUP).tar.xz
$(GSETSCHEMASUP).tar.xz:
	wget -nc -O$@ http://ftp.gnome.org/pub/gnome/sources/gsettings-desktop-schemas/3.6/$@

$(GSETSCHEMASORIG): $(GSETSCHEMASUP).tar.xz
	ln -sf $< $@

.PHONY: gsettings-desktop-schemas
gsettings-desktop-schemas:$(GSETTINGSDESKTOPSCHEMAS)_$(ARCH).deb
$(GSETTINGSDESKTOPSCHEMAS): $(SPREZZ)/gsettings-desktop-schemas/debian/changelog $(GSETSCHEMASORIG)
	mkdir -p $@
	tar xJvf $(GSETSCHEMASORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(GNOMESHELLUP).tar.xz
$(GNOMESHELLUP).tar.xz:
	wget -nc -O$@ http://ftp.gnome.org/pub/GNOME/sources/gnome-shell/3.6/$@

$(GNOMESHELLORIG): $(GNOMESHELLUP).tar.xz
	ln -sf $< $@

.PHONY: gnome-shell
gnome-shell:$(GNOMESHELL)_$(ARCH).deb
$(GNOMESHELL): $(SPREZZ)/gnome-shell/debian/changelog $(GNOMESHELLORIG)
	mkdir -p $@
	tar xJvf $(GNOMESHELLORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(GNOMESHELLEXTENSIONSUP).tar.xz
$(GNOMESHELLEXTENSIONSUP).tar.xz:
	wget -nc -O$@ http://ftp.gnome.org/pub/GNOME/sources/gnome-shell-extensions/3.2/$@

$(GNOMESHELLEXTENSIONSORIG): $(GNOMESHELLEXTENSIONSUP).tar.xz
	ln -sf $< $@

.PHONY: gnome-shell-extensions
gnome-shell-extensions:$(GNOMESHELLEXTENSIONS)_$(ARCH).deb
$(GNOMESHELLEXTENSIONS): $(SPREZZ)/gnome-shell-extensions/debian/changelog $(GNOMESHELLEXTENSIONSORIG)
	mkdir -p $@
	tar xJvf $(GNOMESHELLEXTENSIONSORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(LIGHTDMUP).orig.tar.gz
$(LIGHTDMUP).orig.tar.gz:
	wget -nc -O$@ https://launchpad.net/ubuntu/quantal/+source/lightdm/1.4.0-0ubuntu1/+files/$@

.PHONY: lightdm
lightdm:$(LIGHTDM)_$(ARCH).deb
$(LIGHTDM): $(SPREZZ)/lightdm/debian/changelog $(LIGHTDMORIG)
	mkdir -p $@
	tar xzvf $(LIGHTDMORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(LIBRSVGORIG)
$(LIBRSVGORIG):
	wget -nc -O$@ http://ftp.gnome.org/pub/gnome/sources/librsvg/2.36/$(LIBRSVGUP).tar.xz

.PHONY: librsvg
librsvg:$(LIBRSVG)_$(ARCH).deb
$(LIBRSVG): $(SPREZZ)/librsvg/debian/changelog $(LIBRSVGORIG)
	mkdir -p $@
	tar xJvf $(LIBRSVGORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

CONPAL:=App-ConPalette-0.1.5
.PHONY: conpalette
conpalette:$(CONPALETTE)_$(ARCH).deb
$(CONPALETTE): $(SPREZZ)/conpalette/debian/changelog $(CONPAL).tar.gz
	tar xzvf $(CONPAL).tar.gz
	mv $(CONPAL) $@
	cp -r $(<D) $@/

.PHONY: adobe
adobe:$(FONTSADOBESOURCESANSPRO)_$(ARCH).deb
$(FONTSADOBESOURCESANSPRO): $(SPREZZ)/fonts-adobe-sourcesanspro/debian/changelog
	mkdir -p $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	unzip SourceSansPro_FontsOnly-$(fonts-adobe-sourcesanspro_UPVER).zip -d $@

# Native packages (those containing their own source)
.PHONY: anna
anna:$(ANNA)_$(ARCH).udeb
$(ANNA): $(SPREZZ)/anna/debian/changelog
	cp -r $(<D)/.. $@

.PHONY: base-files
base-files:$(BASEFILES)_$(ARCH).deb
$(BASEFILES): $(SPREZZ)/base-files/debian/changelog
	cp -r $(<D)/.. $@

.PHONY: meta-gnome
meta-gnome:$(METAGNOME)_$(ARCH).deb
$(METAGNOME): $(SPREZZ)/meta-gnome/debian/changelog
	cp -r $(<D)/.. $@
	tar cJf meta-gnome_$(meta-gnome_UPVER).orig.tar.xz $@ --exclude-vcs --exclude=debian

.PHONY: console-setup
console-setup:$(CONSOLESETUP)_$(ARCH).deb
$(CONSOLESETUP): $(SPREZZ)/console-setup/debian/changelog
	cp -r $(<D)/.. $@

.PHONY: libdebian-installer
libdebian-installer:$(LIBDEBIANINSTALLER)_$(ARCH).udeb
$(LIBDEBIANINSTALLER): $(SPREZZ)/libdebian-installer/debian/changelog
	cp -r $(<D)/.. $@

.PHONY: netbase
netbase:$(NETBASE)_$(ARCH).deb
$(NETBASE): $(SPREZZ)/netbase/debian/changelog
	cp -r $(<D)/.. $@

.PHONY: firmware-all
firmware-all:$(FIRMWAREALL)_$(ARCH).deb
$(FIRMWAREALL): $(SPREZZ)/firmware-all/debian/changelog
	cp -r $(<D)/.. $@

clean:
	rm -rf sprezzos-world $(DEBS) $(UDEBS) $(DSCS) $(CHANGES)
	rm -rf $(GRUBTHEME) $(OMPHALOS) $(GROWLIGHT) $(FBV) $(LVM2) $(CAIRO) $(ICU)
	rm -rf $(ADOBE) $(FBTERM) $(CONPALETTE) $(APITRACE) $(SUDO) $(LIBPNG) $(MASH)
	rm -rf $(DEBS) $(UDEBS) $(LIBJPEGTURBO) $(STRACE) $(SPLITVT) $(GTK+3) $(GIT)
	rm -rf $(LINUXLATEST) $(NETHOROLOGIST) $(FWTS) $(UTILLINUX) $(SYSTEMD) $(DBUS)
	rm -rf $(LIBRSVG) $(GRUB2) $(XMLSTARLET) $(OPENSSH) $(HFSUTILS) $(IBUS) $(EIO)
	rm -rf $(BASEFILES) $(NETBASE) $(FIRMWAREALL) $(FBI) $(OPENLDAP) $(BOOST)
	rm -rf $(LIBDRM) $(MESA) $(PULSEAUDIO) $(SOCAT) $(EGLIBC) $(FREETYPE) $(GMAKE)
	rm -rf $(PANGO) $(GDKPIXBUF) $(FONTCONFIG) $(GLIB) $(HARFBUZZ) $(CURL) $(GTK2)
	rm -rf $(LIBXSLT) $(LIBXML) $(CONSOLESETUP) $(F2FSTOOLS) $(LINUXTOOLS) $(VIM)
	rm -rf $(LIGHTDM) $(OPENCV) $(GSETTINGSDESKTOPSCHEMAS) $(GNOMEDESKTOP) $(LSB)
	rm -rf $(LESS) $(SPL) $(ZFS) $(GNOMECONTROLCENTER) $(EOG) $(ATK) $(YELP)
	rm -rf $(APTITUDE) $(ATSPI2ATK) $(NAUTILUS) $(GNOMESETTINGSDAEMON) $(WPA)
	rm -rf $(CHEESE) $(CLUTTERGST) $(CLUTTERGTK) $(BRASERO) $(APTITUDE) $(ANNA)
	rm -rf $(INSTALLATIONREPORT) $(GNOMESHELL) $(GNOMESHELLEXTENSIONS) $(GCOVR)
	rm -rf $(GNOMECONTACTS) $(CLUTTER) $(GNOMEPOWERMANAGER) $(EVINCE) $(VTE)
	rm -rf $(POPPLER) $(GNOMEMEDIA) $(COMPIZ9) $(FBSET) $(WIRELESSTOOLS) $(DCONF)
	rm -rf $(GOBJECTINTROSPECTION) $(GNOMECATALOG) $(KISMET) $(WIRESHARK) $(GVFS)
	rm -rf $(GNOMESUSHI) $(LIBATASMART) $(GCRYPT) $(GNUTLS) $(DRI2PROTO) $(UDISKS)
	rm -rf $(X11PROTOGL) $(X11PROTORANDR) $(GLU) $(FREEGLUT) $(LIBWNCK) $(GDM3)
	rm -rf $(GNOMEUSERDOCS) $(ABCDE) $(PIDGIN) $(LIBDEBIANINSTALLER) $(LIBX86)
	rm -rf $(SICKBEARD) $(GNOMEFONTVIEWER) $(GNOMESCREENSHOT) $(GNOMESEARCHTOOL)
	rm -rf $(GNOMETHEMESSTANDARD) $(MCELOG) $(LIBJPEG) $(COMPIZ) $(SCREENLETS)
	rm -rf $(GNOMEORCA) $(ATSPI) $(BANSHEE) $(INKSCAPE) $(SHOTWELL) $(WEBKIT)
	rm -rf $(LIBSOUP) $(ENCHANT) $(FREI0R) $(PACKAGEKIT) $(GNOMEDICTIONARY)
	rm -rf $(GNOMECOLORMANAGER) $(YELPXSL) $(PIXMAN) $(GNOMEDISKUTILITY) $(NETCF)
	rm -rf $(GNOMEDOCUTILS) $(LIBVIRT) $(REPORTBUG) $(GPHOTO2) $(LIBGPHOTO2) $(EXO)
	rm -rf $(NVIDIACUDATOOLKIT) $(RAZORQT) $(PCRE) $(ZEROFREE) $(GSTREAMER) $(GHEX)
	rm -rf $(ZENITY) $(AUTOKEY) $(METACITY) $(GRILO) $(LCMS2) $(COLORD) $(ECORE)
	rm -rf $(COLORDGTK) $(TELEPATHYGLIB) $(ENLIGHTENMENT) $(EINA) $(EET) $(EVAS)
	rm -rf $(EXACTIMAGE) $(EDJE) $(EFREET) $(EMBRYO) $(EDBUS) $(EEZE) $(ITSTOOL)
	rm -rf $(VIRTUALBOX) $(EMOTION) $(ELEMENTARY) $(ETHUMB) $(COGL) $(MPD) $(GDL)
	rm -rf $(MUTTER) $(LFTP) $(NCMPCPP) $(EVASGENERICLOADERS) $(GCSTAR) $(GPERF)
	rm -rf $(NFSUTILS) $(EVOLUTION) $(EVOLUTIONDATASERVER) $(CALIBRE) $(BAOBAB)
	rm -rf $(SYNCEVOLUTION) $(LIBSYNTHESIS) $(SIMPLESCAN) $(EOGPLUGINS) $(VINAGRE)
	rm -rf $(LIBXCB) $(XCBPROTO) $(XINPUT) $(GNOMEKEYRING) $(GCR) $(REAVER) $(MP4V2)
	rm -rf $(WIFITE) $(AACPLUSENC) $(FAAC) $(HANDBRAKE) $(GNOMETHEMES) $(BLUEZ)
	rm -rf $(GNOMESESSION) $(GNOMEBLUETOOTH) $(NAUTILUSSENDTO) $(LIBGNOMEKEYRING)
	rm -rf $(GNOMETERMINAL) $(XFCE4TERMINAL) $(LIBXFCE4UI) $(LIBXFCE4UTIL) $(XFCONF)
	rm -rf $(GTKHTML) $(GNOMEONLINEACCOUNTS) $(PYGOBJECT) $(YELPTOOLS) $(RTMPDUMP)
	rm -rf $(GNOMEICONTHEMEEXTRAS) $(GNOMEMENUS) $(GNOMEICONTHEMESYMBOLIC) $(GNOMEICONTHEME)
	rm -rf $(AUDIT) $(MDADM) $(IPROUTE) $(ANJUTA) $(GIMP) $(ZSH) $(BASH) $(RATPOISON)
	rm -rf $(GHOSTSCRIPT) $(JBIG2DEC) $(CUPS) $(LIBGD2) $(XORGXSERVER) $(LIBEXIF)
	rm -rf $(NUMACTL) $(LIBCAP2) $(DEVHELP) $(LIBSECRET) $(CINNAMON) $(FILEROLLER)
	rm -rf $(ALACARTE) $(X11PROTOFONTS) $(X11PROTOXEXT) $(X11PROTOCORE) $(BISON)
	rm -rf $(X11PROTODAMAGE) $(X11PROTOFIXES) $(X11PROTOINPUT) $(X11PROTOKB) $(W3M)
	rm -rf $(X11PROTORECORD) $(X11PROTORENDER) $(X11PROTORESOURCE) $(X11PROTOVIDEO)
	rm -rf $(X11PROTOSCRNSAVER) $(X11PROTOXINERAMA) $(WAYLAND) $(LIBXRENDER)
	rm -rf $(X11PROTOXF86DGA) $(X11PROTOXF86DRI) $(X11PROTOXF86VIDMODE) $(LIBTASN)
	rm -rf $(MUTT)

clobber:
	rm -rf $(FETCHED)
