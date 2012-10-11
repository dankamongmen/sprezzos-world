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
PACKAGES:=growlight fwts util-linux linux-latest libpng libjpeg-turbo lvm2 \
	omphalos sudo systemd librsvg grub2 xmlstarlet openssh hfsutils fbi \
	conpalette strace splitvt xbmc sprezzos-grub2theme apitrace cairo \
	fbv fonts-adobe-sourcesanspro mplayer nethorologist fbterm base-files \
	netbase base-installer firmware-all gtk3 libdrm mesa pulseaudio socat \
	nfs-utils eglibc hwloc freetype pango fontconfig gdk-pixbuf glib ibus \
	harfbuzz curl libxml libxslt console-setup f2fs-tools linux-tools \
	lightdm opencv gsettings-desktop-schemas gnome-desktop less spl zfs \
	gnome-control-center nautilus eog atk aptitude atk-bridge cheese \
	gnome-settings-daemon

SPREZZ:=packaging

include $(addprefix sprezzos-world/,$(PACKAGES))

sprezzos-world/%: $(SPREZZ)/%/debian/changelog
	[ -d $(@D) ] || mkdir -p $(@D)
	( echo "# Automatically generated from $<" && \
	 echo -n "$(@F)_VERSION:=" && \
	 dpkg-parsechangelog -l$< | grep-dctrl -ensVersion -FSource . | cut -d: -f2- && \
	 echo -n "$(shell echo $(@F) | tr [:lower:] [:upper:] | tr -d -):=$(@F)_" &&\
	 dpkg-parsechangelog -l$< | grep-dctrl -ensVersion -FSource . | cut -d: -f2- ) > $@

ADOBEUP:=SourceSansPro_FontsOnly-1.036.zip
ATSPI2ATKUP:=at-spi2-atk-$(shell echo $(atk-bridge_VERSION) | cut -d- -f1)
ATSPI2ATKORIG:=at-spi2-atk_$(shell echo $(atk-bridge_VERSION) | cut -d- -f1).orig.tar.xz
ATKUP:=atk-$(shell echo $(atk_VERSION) | cut -d- -f1)
ATKORIG:=atk1.0_$(shell echo $(atk_VERSION) | cut -d- -f1).orig.tar.xz
CAIROUP:=cairo-$(shell echo $(cairo_VERSION) | cut -d= -f2- | cut -d- -f1)
CAIROORIG:=cairo_$(shell echo $(cairo_VERSION) | cut -d- -f1).orig.tar.xz
CHEESEUP:=cheese-$(shell echo $(cheese_VERSION) | cut -d: -f2- | cut -d- -f1)
CHEESEORIG:=cheese_$(shell echo $(cheese_VERSION) | cut -d: -f2- | cut -d- -f1).orig.tar.xz
CURLUP:=curl-$(shell echo $(curl_VERSION) | cut -d= -f2- | cut -d- -f1)
CURLORIG:=curl_$(shell echo $(curl_VERSION) | cut -d- -f1).orig.tar.bz2
EGLIBCUP:=glibc-$(shell echo $(eglibc_VERSION) | cut -d- -f1)
EGLIBCORIG:=eglibc_$(shell echo $(eglibc_VERSION) | cut -d- -f1).orig.tar.gz
EOGUP:=eog-$(shell echo $(eog_VERSION) | cut -d: -f2- | cut -d- -f1)
EOGORIG:=eog_$(shell echo $(eog_VERSION) | cut -d: -f2- | cut -d- -f1).orig.tar.xz
F2FSUP:=f2fs-tools-$(shell echo $(f2fs-tools_VERSION) | cut -d- -f1)
F2FSORIG:=f2fs-tools_$(shell echo $(f2fs-tools_VERSION) | cut -d- -f1).orig.tar.gz
FBIUP:=fbida-$(shell echo $(fbi_VERSION) | cut -d= -f2- | cut -d- -f1)
FBTERMUP:=nfbterm-$(shell echo $(fbterm_VERSION) | cut -d= -f2 | cut -d- -f1)
FBTERMORIG:=fbterm_$(shell echo $(fbterm_VERSION) | cut -d- -f1).orig.tar.gz
FONTCONFIGUP:=fontconfig-$(shell echo $(fontconfig_VERSION) | cut -d- -f1)
FONTCONFIGORIG:=fontconfig_$(shell echo $(fontconfig_VERSION) | cut -d- -f1).orig.tar.gz
FREETYPEUP:=freetype-$(shell echo $(freetype_VERSION) | cut -d- -f1) \
	freetype-doc-$(shell echo $(freetype_VERSION) | cut -d- -f1) \
	ft2demos-$(shell echo $(freetype_VERSION) | cut -d- -f1)
FREETYPEORIG:=freetype_$(shell echo $(freetype_VERSION) | cut -d- -f1).orig.tar.gz
GLIBUP:=glib-$(shell echo $(glib_VERSION) | cut -d- -f1)
GLIBORIG:=glib2.0_$(shell echo $(glib_VERSION) | cut -d- -f1).orig.tar.xz
GDKPIXBUFUP:=gdk-pixbuf-$(shell echo $(gdk-pixbuf_VERSION) | cut -d- -f1)
GDKPIXBUFORIG:=gdk-pixbuf_$(shell echo $(gdk-pixbuf_VERSION) | cut -d- -f1).orig.tar.xz
GNOMECONTROLCENTERUP:=gnome-control-center-$(shell echo $(gnome-control-center_VERSION) | cut -d: -f2- | cut -d- -f1)
GNOMECONTROLCENTERORIG:=gnome-control-center_$(shell echo $(gnome-control-center_VERSION) | cut -d: -f2- | cut -d- -f1).orig.tar.xz
GNOMESETTINGSDAEMONUP:=gnome-settings-daemon-$(shell echo $(gnome-settings-daemon_VERSION) | cut -d: -f2- | cut -d- -f1)
GNOMESETTINGSDAEMONORIG:=gnome-settings-daemon_$(shell echo $(gnome-settings-daemon_VERSION) | cut -d: -f2- | cut -d- -f1).orig.tar.xz
GNOMEDESKTOPUP:=gnome-desktop-$(shell echo $(gnome-desktop_VERSION) | cut -d- -f1)
GNOMEDESKTOPORIG:=gnome-desktop_$(shell echo $(gnome-desktop_VERSION) | cut -d- -f1).orig.tar.xz
GSETSCHEMASUP:=gsettings-desktop-schemas-$(shell echo $(gsettings-desktop-schemas_VERSION) | cut -d- -f1)
GSETSCHEMASORIG:=gsettings-desktop-schemas_$(shell echo $(gsettings-desktop-schemas_VERSION) | cut -d- -f1).orig.tar.xz
SPLORIG:=spl_$(shell echo $(spl_VERSION) | cut -d- -f1).orig.tar.xz
ZFSORIG:=zfs_$(shell echo $(zfs_VERSION) | cut -d- -f1).orig.tar.xz
GROWLIGHTORIG:=growlight_$(shell echo $(growlight_VERSION) | cut -d- -f1).orig.tar.bz2
GRUBUP:=grub-$(shell echo $(grub2_VERSION) | cut -d- -f1 | cut -d= -f2- | tr : -)
GTK3UP:=gtk+-$(shell echo $(gtk3_VERSION) | cut -d= -f2 | cut -d- -f1)
GTK3ORIG:=gtk+3.0_$(shell echo $(gtk3_VERSION) | cut -d- -f1).orig.tar.xz
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
MESAUP:=MesaLib-$(shell echo $(mesa_VERSION) | cut -d- -f1 | cut -d= -f2- | cut -d: -f2)
MESAORIG:=mesa_$(shell echo $(mesa_VERSION) | cut -d- -f1).orig.tar.bz2
MPLAYER:=mplayer_$(shell echo $(mplayer_VERSION) | tr : .)
NAUTILUSUP:=nautilus-$(shell echo $(nautilus_VERSION) | cut -d: -f2- | cut -d- -f1)
NAUTILUSORIG:=nautilus_$(shell echo $(nautilus_VERSION) | cut -d: -f2- | cut -d- -f1).orig.tar.xz
NETHOROLOGISTORIG:=nethorologist_$(shell echo $(nethorologist_VERSION) | cut -d- -f1).orig.tar.xz
NFSUTILSUP:=nfs-utils-$(shell echo $(nfs-utils_VERSION) | cut -d- -f1 | cut -d= -f2- | cut -d: -f2)
NFSUTILSORIG:=nfs-$(shell echo $(NFSUTILSUP) | cut -d- -f2- | tr - _).orig.tar.bz2
NFSUTILS:=$(shell echo $(nfs-utils_VERSION) | tr : .)
OMPHALOSORIG:=omphalos_$(shell echo $(omphalos_VERSION) | cut -d- -f1).orig.tar.bz2
XMLSTARLETORIG:=xmlstarlet_$(shell echo $(xmlstarlet_VERSION) | cut -d- -f1).orig.tar.bz2
OPENCVUP:=OpenCV-$(shell echo $(opencv_VERSION) | cut -d- -f1 | cut -d= -f2- | cut -d: -f2)
OPENCVORIG:=opencv_$(shell echo $(opencv_VERSION) | tr : . | cut -d- -f1).orig.tar.bz2
OPENSSH:=openssh_$(shell echo $(openssh_VERSION) | tr : .)
OPENSSHUP:=openssh-$(shell echo $(openssh_VERSION) | cut -d- -f1 | cut -d= -f2- | cut -d: -f2)
PANGOUP:=pango-$(shell echo $(pango_VERSION) | cut -d- -f1)
PANGOORIG:=pango1.0_$(shell echo $(PANGOUP) | cut -d- -f2 | tr - _).orig.tar.xz
PULSEAUDIOUP:=pulseaudio-$(shell echo $(pulseaudio_VERSION) | cut -d- -f1)
PULSEAUDIOORIG:=$(shell echo $(PULSEAUDIOUP) | tr - _).orig.tar.xz
SOCATUP:=socat-$(shell echo $(socat_VERSION) | cut -d- -f1 | tr \~ -)
SOCATORIG:=socat_$(shell echo $(socat_VERSION) | cut -d- -f1).orig.tar.bz2

DEBS:=$(GROWLIGHT) $(LIBRSVG) $(GRUB2) $(LVM2) $(OPENSSH) $(LIBPNG) $(FWTS) \
	$(UTILLINUX) $(LINUXLATEST) $(LIBJPEGTURBO) $(OMPHALOS) $(SUDO) \
	$(GRUBTHEME) $(ADOBE) $(STRACE) $(SPLITVT) $(HFSUTILS) $(APTITUDE) \
	$(NETHOROLOGIST) $(XBMC) $(MPLAYER) $(CONPALETTE) $(APITRACE) \
	$(SYSTEMD) $(BASEFILES) $(NETBASE) $(FBI) $(CAIRO) $(XMLSTARLET) \
	$(GTK3) $(LIBDRM) $(PULSEAUDIO) $(SOCAT) $(NFSUTILS) $(EGLIBC) \
	$(FREETYPE) $(PANGO) $(GDKPIXBUF) $(GLIB) $(HARFBUZZ) $(CURL) $(IBUS) \
	$(LIBXSLT) $(LIBXML) $(F2FSTOOLS) $(LINUXTOOLS) $(LIGHTDM) $(OPENCV) \
	$(GSETTINGSDESKTOPSCHEMAS) $(LESS) $(ZFS) $(SPL) $(EOG) $(ATK) \
	$(GNOMECONTROLCENTER) $(NAUTILUS) $(GNOMESETTINGSDAEMON) $(CHEESE)
UDEBS:=$(FBV) $(BASEINSTALLER) $(FIRMWAREALL)
DUPUDEBS:=$(GROWLIGHT) $(FBTERM) $(CONPALETTE) $(STRACE) $(SPLITVT) \
	$(NETHOROLOGIST) $(FWTS) $(UTILLINUX) $(HFSUTILS) $(LIBPNG) $(EGLIBC) \
	$(FREETYPE) $(CURL) $(LIBXSLT) $(LIBXML) $(F2FSTOOLS) $(ZFS) $(SPL)

DEBS:=$(subst :,.,$(DEBS))
UDEBS:=$(subst :,.,$(UDEBS))

DSCS:=$(addsuffix .dsc,$(DEBS) $(UDEBS))
CHANGES:=$(addsuffix .changes,$(DEBS) $(UDEBS))

DEBS:=$(addsuffix _$(ARCH).deb,$(DEBS))
UDEBS:=$(addsuffix _$(ARCH).udeb,$(UDEBS))

world: $(DEBS) $(UDEBS)

#cd $< && apt-get -y build-dep $(shell echo $@ | cut -d_ -f1) || true # source package might not exist
%_$(ARCH).udeb %_$(ARCH).deb: %
	cd $< && dpkg-buildpackage -k$(DEBKEY)

# Packages which we take from upstream source repositories rather than a
# release tarball. We must make our own *.orig.tar.* files for these.
.PHONY: growlight
growlight: $(GROWLIGHT)_$(ARCH).deb
$(GROWLIGHT): $(SPREZZ)/growlight/debian/changelog
	git clone https://github.com/dankamongmen/growlight.git $@
	cd $@ && autoreconf -sif
	tar cjf $(GROWLIGHTORIG) $@ --exclude-vcs
	cp -r $(<D) $@/

.PHONY: omphalos
omphalos:$(OMPHALOS)_$(ARCH).deb
$(OMPHALOS): $(SPREZZ)/omphalos/debian/changelog
	git clone https://github.com/dankamongmen/omphalos.git $@
	cd $@ && autoreconf -sif
	tar cjf $(OMPHALOSORIG) $@ --exclude-vcs
	cp -r $(<D) $@/

.PHONY: xmlstarlet
xmlstarlet:$(XMLSTARLET)_$(ARCH).deb
$(XMLSTARLET): $(SPREZZ)/xmlstarlet/debian/changelog
	git clone https://github.com/dankamongmen/xmlstarlet.git $@
	tar cjf $(XMLSTARLETORIG) $@ --exclude-vcs
	cp -r $(<D) $@/

.PHONY: spl
spl: $(SPL)_$(ARCH).deb
$(SPL): $(SPREZZ)/spl/debian/changelog
	git clone https://github.com/Sprezzatech/spl.git $@
	cd $@ && ./autogen.sh
	tar cJf $(SPLORIG) $@ --exclude-vcs
	cp -r $(<D) $@/

.PHONY: zfs
zfs: $(ZFS)_$(ARCH).deb
$(ZFS): $(SPREZZ)/zfs/debian/changelog
	git clone https://github.com/Sprezzatech/zfs.git $@
	cd $@ && ./autogen.sh
	tar cJf $(ZFSORIG) $@ --exclude-vcs
	cp -r $(<D) $@/

.PHONY: nethorologist
nethorologist: $(NETHOROLOGIST)_$(ARCH).deb
$(NETHOROLOGIST): $(SPREZZ)/nethorologist/debian/changelog
	git clone https://github.com/Sprezzatech/nethorologist.git $@
	tar cJf $(NETHOROLOGISTORIG) $@ --exclude-vcs
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
	tar cjf $(LINUXTOOLSORIG) $@ --exclude-vcs

.PHONY: fbv
fbv:$(FBV).udeb
$(FBV): $(SPREZZ)/fbv/debian/changelog
	git clone git://repo.or.cz/fbv.git $@

.PHONY: apitrace
apitrace:$(APITRACE)_$(ARCH).deb
$(APITRACE): $(SPREZZ)/apitrace/debian/changelog
	git clone https://github.com/apitrace/apitrace.git $@
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

.PHONY: linux-latest
linux-latest:$(LINUXLATEST)_$(ARCH).deb
$(LINUXLATEST): $(SPREZZ)/linux-latest/debian/changelog
	mkdir $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(CAIROUP).tar.xz
$(CAIROUP).tar.xz:
	wget -nc -O$@ http://cairographics.org/releases/$@

$(CAIROORIG): $(CAIROUP).tar.xz
	ln -s $< $@

.PHONY: cairo
cairo:$(CAIRO)_$(ARCH).deb
$(CAIRO): $(SPREZZ)/cairo/debian/changelog $(CAIROORIG)
	mkdir $@
	tar xJvf $(CAIROUP).tar.xz --strip-components=1 -C $@
	cp -r $(<D) $@/

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

FETCHED:=$(FETCHED) util-linux_2.20.1.tar.gz
util-linux-2.20.1.tar.gz:
	wget -nc -O$@ ftp://ftp.kernel.org/pub/linux/utils/util-linux/v2.20/$@

.PHONY: util-linux
util-linux:$(UTILLINUX)_$(ARCH).deb
$(UTILLINUX): $(SPREZZ)/util-linux/debian/changelog util-linux-2.20.1.tar.gz
	mkdir $@
	tar xzvf util-linux-2.20.1.tar.gz --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) libjpeg-turbo-1.2.1.tar.gz
libjpeg-turbo-1.2.1.tar.gz:
	wget -nc -O$@ http://sourceforge.net/projects/libjpeg-turbo/files/1.2.1/libjpeg-turbo-1.2.1.tar.gz/download

.PHONY: libjpeg-turbo
libjpeg-turbo:$(LIBJPEGTURBO)_$(ARCH).deb
$(LIBJPEGTURBO): $(SPREZZ)/libjpeg-turbo/debian/changelog libjpeg-turbo-1.2.1.tar.gz
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

FETCHED:=$(FETCHED) $(F2FSUP).tar.gz
$(F2FSUP).tar.gz:
	wget -nc -O$@ http://sourceforge.net/projects/f2fs-tools/files/$@

$(F2FSORIG): $(F2FSUP).tar.gz
	ln -s $< $@

.PHONY: f2fstools
f2fstools:$(F2FSTOOLS)_$(ARCH).deb
$(F2FSTOOLS): $(SPREZZ)/f2fs-tools/debian/changelog $(F2FSORIG)
	mkdir $@
	tar xzvf $(F2FSORIG) --strip-components=1 -C $@
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

FETCHED:=$(FETCHED) $(GLIBUP).tar.xz
$(GLIBUP).tar.xz:
	wget -nc -O$@ http://ftp.acc.umu.se/pub/gnome/sources/glib/2.33/$@

$(GLIBORIG): $(GLIBUP).tar.xz
	ln -s $< $@

.PHONY: glib
glib:$(GLIB)_$(ARCH).deb
$(GLIB): $(SPREZZ)/glib/debian/changelog $(GLIBORIG)
	mkdir $@
	tar xJvf $(GLIBORIG) --strip-components=1 -C $@
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

FETCHED:=$(FETCHED) $(GTK3UP).tar.xz
$(GTK3UP).tar.xz:
	wget -nc -O$@ http://ftp.acc.umu.se/pub/GNOME/sources/gtk+/3.6/$(GTK3UP).tar.xz

$(GTK3ORIG): $(GTK3UP).tar.xz
	ln -s $< $@

.PHONY: gtk3
gtk3:$(GTK3)_$(ARCH).deb
$(GTK3): $(SPREZZ)/gtk3/debian/changelog $(GTK3ORIG)
	mkdir $@
	tar xJvf $(GTK3ORIG) --strip-components=1 -C $@
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

FETCHED:=$(FETCHED) http://www.freedesktop.org/software/systemd/systemd-189.tar.xz
$(SYSTEMD).tar.xz:
	wget -nc -O$@ http://www.freedesktop.org/software/systemd/systemd-189.tar.xz

.PHONY: systemd
systemd:$(SYSTEMD)_$(ARCH).deb
$(SYSTEMD): $(SPREZZ)/systemd/debian/changelog $(SYSTEMD).tar.xz
	mkdir -p $@
	tar xJvf $(SYSTEMD).tar.xz --strip-components=1 -C $@
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

FETCHED:=$(FETCHED) $(GNOMESETTINGSDAEMONUP).tar.xz
$(GNOMESETTINGSDAEMONUP).tar.xz:
	wget -nc -O$@ http://ftp.gnome.org/pub/GNOME/sources/gnome-settings-daemon/3.6/$@

$(GNOMESETTINGSDAEMONORIG): $(GNOMESETTINGSDAEMONUP).tar.xz
	ln -sf $< $@

.PHONY: gnome-settings-daemon
gnome-settings-daemon:$(GNOMESETTINGSDAEMON)_$(ARCH).deb
$(GNOMESETTINGSDAEMON): $(SPREZZ)/gnome-settings-daemon/debian/changelog $(GNOMESETTINGSDAEMONORIG)
	mkdir -p $@
	tar xJvf $(GNOMESETTINGSDAEMONORIG) --strip-components=1 -C $@
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

FETCHED:=$(FETCHED) $(ADOBEUP)
$(ADOBEUP):
	wget -nc -O$@ http://sourceforge.net/projects/sourcesans.adobe/files/$@

.PHONY: adobe
adobe:$(ADOBE)_$(ARCH).deb
$(ADOBE): $(SPREZZ)/fonts-adobe-sourcesanspro/debian/changelog $(ADOBEUP)
	unzip $(ADOBEUP)
	mv $(basename $(ADOBEUP)) $@
	cp -r $(<D) $@/

# Native packages (those containing their own source)
.PHONY: base-files
base-files:$(BASEFILES)_$(ARCH).deb
$(BASEFILES): $(SPREZZ)/base-files/debian/changelog
	cp -r $(<D)/.. $@

.PHONY: base-installer
base-installer:$(BASEINSTALLER)_$(ARCH).udeb
$(BASEINSTALLER): $(SPREZZ)/base-installer/debian/changelog
	cp -r $(<D)/.. $@

.PHONY: console-setup
console-setup:$(CONSOLESETUP)_$(ARCH).deb
$(CONSOLESETUP): $(SPREZZ)/console-setup/debian/changelog
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
	rm -rf $(GRUBTHEME) $(OMPHALOS) $(GROWLIGHT) $(FBV) $(LVM2) $(CAIRO)
	rm -rf $(ADOBE) $(FBTERM) $(CONPALETTE) $(APITRACE) $(SUDO) $(LIBPNG)
	rm -rf $(DEBS) $(UDEBS) $(LIBJPEGTURBO) $(STRACE) $(SPLITVT) $(GTK+3)
	rm -rf $(LINUXLATEST) $(NETHOROLOGIST) $(FWTS) $(UTILLINUX) $(SYSTEMD)
	rm -rf $(LIBRSVG) $(GRUB2) $(XMLSTARLET) $(OPENSSH) $(HFSUTILS) $(IBUS)
	rm -rf $(BASEFILES) $(NETBASE) $(BASEINSTALLER) $(FIRMWAREALL) $(FBI)
	rm -rf $(LIBDRM) $(MESA) $(PULSEAUDIO) $(SOCAT) $(EGLIBC) $(FREETYPE)
	rm -rf $(PANGO) $(GDKPIXBUF) $(FONTCONFIG) $(GLIB) $(HARFBUZZ) $(CURL)
	rm -rf $(LIBXSLT) $(LIBXML) $(CONSOLESETUP) $(F2FSTOOLS) $(LINUXTOOLS)
	rm -rf $(LIGHTDM) $(OPENCV) $(GSETTINGSDESKTOPSCHEMAS) $(GNOMEDESKTOP)
	rm -rf $(LESS) $(SPL) $(ZFS) $(GNOMECONTROLCENTER) $(EOG) $(ATK)
	rm -rf $(APTITUDE) $(ATSPI2ATK) $(NAUTILUS) $(GNOMESETTINGSDAEMON)
	rm -rf $(CHEESE)

clobber:
	rm -rf $(FETCHED)
