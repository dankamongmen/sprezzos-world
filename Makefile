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
	libgphoto2 nvidia-cuda-toolkit pcre zerofree gstreamer zenity autokey eio \
	metacity grilo lcms2 colord colord-gtk telepathy-glib enlightenment eet \
	eina evas ecore exactimage edje efreet embryo edbus eeze itstool virtualbox \
	emotion elementary ethumb cogl mpd mutter lftp ncmpcpp evas-generic-loaders

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
ATSPI2ATKUP:=at-spi2-atk-$(shell echo $(atk-bridge_VERSION) | cut -d- -f1)
ATSPI2ATKORIG:=at-spi2-atk_$(shell echo $(atk-bridge_VERSION) | cut -d- -f1).orig.tar.xz
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
EOGUP:=eog-$(shell echo $(eog_VERSION) | cut -d: -f2- | cut -d- -f1)
EOGORIG:=eog_$(shell echo $(eog_VERSION) | cut -d: -f2- | cut -d- -f1).orig.tar.xz
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
MESAUP:=MesaLib-$(shell echo $(mesa_VERSION) | cut -d- -f1 | cut -d= -f2- | cut -d: -f2)
MESAORIG:=mesa_$(shell echo $(mesa_VERSION) | cut -d- -f1).orig.tar.bz2
MPLAYER:=mplayer_$(shell echo $(mplayer_VERSION) | tr : .)
NAUTILUSUP:=nautilus-$(shell echo $(nautilus_VERSION) | cut -d: -f2- | cut -d- -f1)
NAUTILUSORIG:=nautilus_$(shell echo $(nautilus_VERSION) | cut -d: -f2- | cut -d- -f1).orig.tar.xz
NETHOROLOGISTORIG:=nethorologist_$(shell echo $(nethorologist_VERSION) | cut -d- -f1).orig.tar.xz
NFSUTILSUP:=nfs-utils-$(shell echo $(nfs-utils_VERSION) | cut -d- -f1 | cut -d= -f2- | cut -d: -f2)
NFSUTILSORIG:=nfs-$(shell echo $(NFSUTILSUP) | cut -d- -f2- | tr - _).orig.tar.bz2
NFSUTILS:=$(shell echo $(nfs-utils_VERSION) | tr : .)
FREI0RORIG:=frei0r_$(shell echo $(frei0r_VERSION) | cut -d- -f1).orig.tar.xz
XMLSTARLETORIG:=xmlstarlet_$(shell echo $(xmlstarlet_VERSION) | cut -d- -f1).orig.tar.bz2
OPENCVUP:=OpenCV-$(shell echo $(opencv_VERSION) | cut -d- -f1 | cut -d= -f2- | cut -d: -f2)
OPENCVORIG:=opencv_$(shell echo $(opencv_VERSION) | tr : . | cut -d- -f1).orig.tar.bz2
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
YELPUP:=yelp-$(shell echo $(yelp_VERSION) | cut -d: -f2- | cut -d- -f1)
YELPORIG:=yelp_$(shell echo $(yelp_VERSION) | cut -d: -f2- | cut -d- -f1).orig.tar.xz

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
	$(TELEPATHYGLIB) $(ENLIGHTENMENT) $(EINA) $(EET) $(ECORE) $(EXACTIMAGE) \
	$(EIO) $(EDJE) $(EFREET) $(EMBRYO) $(EDBUS) $(EEZE) $(ITSTOOL) $(VIRTUALBOX) \
	$(EMOTION) $(ELEMENTARY) $(ETHUMB) $(COGL) $(MPD) $(MUTTER) $(LFTP) $(NCMPCPP) \
	$(EVASGENERICLOADERS)
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
	cd $< && debuild -k$(DEBKEY)

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

.PHONY: f2fs-tools
f2fs-tools:$(F2FSTOOLS)_$(ARCH).deb
$(F2FSTOOLS): $(SPREZZ)/f2fs-tools/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf f2fs-tools-$(f2fs-tools_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: fbset
fbset:$(FBSET)_$(ARCH).deb
$(FBSET): $(SPREZZ)/fbset/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf fbset-$(fbset_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: freeglut
freeglut:$(FREEGLUT)_$(ARCH).deb
$(FREEGLUT): $(SPREZZ)/freeglut/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf freeglut_$(freeglut_UPVER).orig.tar.gz --strip-components=1 -C $@

.PHONY: gcrypt
gcrypt:$(GCRYPT)_$(ARCH).deb
$(GCRYPT): $(SPREZZ)/gcrypt/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf libgcrypt11_$(gcrypt_UPVER).orig.tar.bz2 --strip-components=1 -C $@

.PHONY: gdm3
gdm3:$(GDM3)_$(ARCH).deb
$(GDM3): $(SPREZZ)/gdm3/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gdm-$(gdm3_UPVER).tar.xz --strip-components=1 -C $@

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

.PHONY: gnome-media
gnome-media:$(GNOMEMEDIA)_$(ARCH).deb
$(GNOMEMEDIA): $(SPREZZ)/gnome-media/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gnome-media_$(gnome-media_UPVER).orig.tar.xz --strip-components=1 -C $@

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
	tar xJvf gtk+3-$(gtk3_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: gvfs
gvfs:$(GVFS)_$(ARCH).deb
$(GVFS): $(SPREZZ)/gvfs/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf gvfs-$(gvfs_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: icu
icu:$(ICU)_$(ARCH).deb
$(ICU): $(SPREZZ)/icu/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf icu-$(icu_UPVER).tar.gz --strip-components=1 -C $@

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

.PHONY: libsoup
libsoup:$(LIBSOUP)_$(ARCH).deb
$(LIBSOUP): $(SPREZZ)/libsoup/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf libsoup2.4_$(libsoup_UPVER).orig.tar.xz --strip-components=1 -C $@

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
	tar xJvf libwnck-$(libwnck_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: libx86
libx86:$(LIBX86)_$(ARCH).deb
$(LIBX86): $(SPREZZ)/libx86/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf libx86_$(libx86_UPVER).orig.tar.gz --strip-components=1 -C $@

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

.PHONY: metacity
metacity:$(METACITY)_$(ARCH).deb
$(METACITY): $(SPREZZ)/metacity/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf metacity_$(metacity_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: mpd
mpd:$(MPD)_$(ARCH).deb
$(MPD): $(SPREZZ)/mpd/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf mpd_$(mpd_UPVER).orig.tar.gz --strip-components=1 -C $@

.PHONY: mutter
mutter:$(MUTTER)_$(ARCH).deb
$(MUTTER): $(SPREZZ)/mutter/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf mutter_$(mutter_UPVER).orig.tar.xz --strip-components=1 -C $@

.PHONY: ncmpcpp
ncmpcpp:$(NCMPCPP)_$(ARCH).deb
$(NCMPCPP): $(SPREZZ)/ncmpcpp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf ncmpcpp_$(ncmpcpp_UPVER).orig.tar.gz --strip-components=1 -C $@

.PHONY: netcf
netcf:$(NETCF)_$(ARCH).deb
$(NETCF): $(SPREZZ)/netcf/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf netcf_$(netcf_UPVER).orig.tar.gz --strip-components=1 -C $@

.PHONY: nvidia-cuda-toolkit
nvidia-cuda-toolkit:$(NVIDIACUDATOOLKIT)_$(ARCH).deb
$(NVIDIACUDATOOLKIT): $(SPREZZ)/nvidia-cuda-toolkit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf nvidia-cuda-toolkit-$(nvidia-cuda-toolkit_UPVER).tar.gz --strip-components=1 -C $@

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
	tar xJvf pidgin-$(pidgin_UPVER).tar.xz --strip-components=1 -C $@

.PHONY: pixman
pixman:$(PIXMAN)_$(ARCH).deb
$(PIXMAN): $(SPREZZ)/pixman/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xzvf pixman-$(pixman_UPVER).tar.gz --strip-components=1 -C $@

.PHONY: razorqt
razorqt:$(RAZORQT)_$(ARCH).deb
$(RAZORQT): $(SPREZZ)/razorqt/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf razorqt-$(razorqt_UPVER).tar.bz2 --strip-components=1 -C $@

.PHONY: reportbug
reportbug:$(REPORTBUG)_$(ARCH).deb
$(REPORTBUG): $(SPREZZ)/reportbug/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf reportbug-$(reportbug_UPVER).tar.bz2 --strip-components=1 -C $@

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

.PHONY: webkit
webkit:$(WEBKIT)_$(ARCH).deb
$(WEBKIT): $(SPREZZ)/webkit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xJvf webkit_$(webkit_UPVER).orig.tar.xz --strip-components=1 -C $@

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

.PHONY: x11proto-gl
x11proto-gl:$(X11PROTOGL)_$(ARCH).deb
$(X11PROTOGL): $(SPREZZ)/x11proto-gl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf glproto-$(x11proto-gl_UPVER).tar.bz2 --strip-components=1 -C $@

.PHONY: x11proto-randr
x11proto-randr:$(X11PROTORANDR)_$(ARCH).deb
$(X11PROTORANDR): $(SPREZZ)/x11proto-randr/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf randrproto-$(x11proto-randr_UPVER).tar.bz2 --strip-components=1 -C $@

.PHONY: udisks
udisks:$(UDISKS)_$(ARCH).deb
$(UDISKS): $(SPREZZ)/udisks/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download
	tar xjvf udisks_$(udisks_UPVER).orig.tar.bz2 --strip-components=1 -C $@

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

FETCHED:=$(FETCHED) $(MESAUP).tar.bz2
$(MESAUP).tar.bz2:
	wget -nc -O$@ ftp://ftp.freedesktop.org/pub/mesa/$(shell echo $(mesa_VERSION) | cut -d- -f1)/$(MESAUP).tar.bz2

$(MESAORIG): $(MESAUP).tar.bz2
	ln -s $< $@

.PHONY: mesa
mesa:$(MESA)_$(ARCH).deb
$(MESA): $(SPREZZ)/mesa/debian/changelog $(MESAORIG)
	mkdir $@
	tar xjvf $(MESAORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(NFSUTILSUP).tar.bz2
$(NFSUTILSUP).tar.bz2:
	wget -nc -O$@ http://sourceforge.net/projects/nfs/files/nfs-utils/1.2.6/$@

$(NFSUTILSORIG): $(NFSUTILSUP).tar.bz2
	ln -s $< $@

.PHONY: nfs-utils
nfs-utils:$(NFSUTILS)_$(ARCH).deb
$(NFSUTILS): $(SPREZZ)/nfs-utils/debian/changelog $(NFSUTILSORIG)
	mkdir -p $@
	tar xjvf $(NFSUTILSORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(ATSPI2ATKUP).tar.xz
$(ATSPI2ATKUP).tar.xz:
	wget -nc -O$@ http://ftp.gnome.org/pub/GNOME/sources/at-spi2-atk/2.6/$@

$(ATSPI2ATKORIG): $(ATSPI2ATKUP).tar.xz
	ln -sf $< $@

.PHONY: atk-bridge
atk-bridge:$(ATKBRIDGE)_$(ARCH).deb
$(ATKBRIDGE): $(SPREZZ)/atk-bridge/debian/changelog $(ATSPI2ATKORIG)
	mkdir -p $@
	tar xJvf $(ATSPI2ATKORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

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

FETCHED:=$(FETCHED) $(NAUTILUSUP).tar.xz
$(NAUTILUSUP).tar.xz:
	wget -nc -O$@ http://ftp.gnome.org/pub/GNOME/sources/nautilus/3.6/$@

$(NAUTILUSORIG): $(NAUTILUSUP).tar.xz
	ln -sf $< $@

.PHONY: nautilus
nautilus:$(NAUTILUS)_$(ARCH).deb
$(NAUTILUS): $(SPREZZ)/nautilus/debian/changelog $(NAUTILUSORIG)
	mkdir -p $@
	tar xJvf $(NAUTILUSORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(EOGUP).tar.xz
$(EOGUP).tar.xz:
	wget -nc -O$@ http://ftp.gnome.org/pub/GNOME/sources/eog/3.6/$@

$(EOGORIG): $(EOGUP).tar.xz
	ln -sf $< $@

.PHONY: eog
eog:$(EOG)_$(ARCH).deb
$(EOG): $(SPREZZ)/eog/debian/changelog $(EOGORIG)
	mkdir -p $@
	tar xJvf $(EOGORIG) --strip-components=1 -C $@
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

FETCHED:=$(FETCHED) $(OPENCVUP).tar.bz2
$(OPENCVUP).tar.bz2:
	wget -nc -O$@ http://sourceforge.net/projects/opencvlibrary/files/opencv-unix/2.4.2/$@

$(OPENCVORIG): $(OPENCVUP).tar.bz2
	ln -sf $< $@

.PHONY: opencv
opencv:$(OPENCV)_$(ARCH).deb
$(OPENCV): $(SPREZZ)/opencv/debian/changelog $(OPENCVORIG)
	mkdir -p $@
	tar xjvf $(OPENCVORIG) --strip-components=1 -C $@
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

FETCHED:=$(FETCHED) $(YELPUP).tar.xz
$(YELPUP).tar.xz:
	wget -nc -O$@ http://ftp.gnome.org/pub/GNOME/sources/yelp/3.6/$@

$(YELPORIG): $(YELPUP).tar.xz
	ln -sf $< $@

.PHONY: yelp
yelp:$(YELP)_$(ARCH).deb
$(YELP): $(SPREZZ)/yelp/debian/changelog $(YELPORIG)
	mkdir -p $@
	tar xJvf $(YELPORIG) --strip-components=1 -C $@
	cp -r $(<D) $@/

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
	rm -rf $(GNOMEDOCUTILS) $(LIBVIRT) $(REPORTBUG) $(GPHOTO2) $(LIBGPHOTO2)
	rm -rf $(NVIDIACUDATOOLKIT) $(RAZORQT) $(PCRE) $(ZEROFREE) $(GSTREAMER)
	rm -rf $(ZENITY) $(AUTOKEY) $(METACITY) $(GRILO) $(LCMS2) $(COLORD) $(ECORE)
	rm -rf $(COLORDGTK) $(TELEPATHYGLIB) $(ENLIGHTENMENT) $(EINA) $(EET) $(EVAS)
	rm -rf $(EXACTIMAGE) $(EDJE) $(EFREET) $(EMBRYO) $(EDBUS) $(EEZE) $(ITSTOOL)
	rm -rf $(VIRTUALBOX) $(EMOTION) $(ELEMENTARY) $(ETHUMB) $(COGL) $(MPD)
	rm -rf $(MUTTER) $(LFTP) $(NCMPCPP) $(EVASGENERICLOADERS)

clobber:
	rm -rf $(FETCHED)
