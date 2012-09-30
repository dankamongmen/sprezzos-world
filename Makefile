.DELETE_ON_ERROR:
.PHONY: world all clean
.DEFAULT_GOAL:=all

all: world

ARCH:=amd64

DEBKEY:=9978711C
DEBFULLNAME:='nick black'
DEBEMAIL:=nick.black@sprezzatech.com

PACKAGES:=growlight fwts util-linux linux-latest libpng libjpeg-turbo lvm2 \
	omphalos sudo systemd librsvg grub-pc xmlstarlet openssh hfsutils fbi \
	conpalette strace splitvt xbmc sprezzos-grub2theme apitrace cairo \
	fbv fonts-adobe-sourcesanspro mplayer nethorologist fbterm base-files \
	netbase base-installer firmware-all

SPREZZ:=packaging

include $(addprefix sprezzos-world/,$(PACKAGES))

sprezzos-world/%: $(SPREZZ)/%/debian/changelog
	@[ -d $(@D) ] || mkdir -p $(@D)
	( echo "# Automatically generated from $<" && \
	 echo -n "$(@F)_VERSION:=" && \
	 dpkg-parsechangelog -l$< | grep-dctrl -ensVersion -FSource . ) > $@

ADOBE:=fonts-adobe-sourcesanspro_$(fonts-adobe-sourcesanspro_VERSION)
ADOBEUP:=SourceSansPro_FontsOnly-1.036.zip
CAIRO:=cairo_$(cairo_VERSION)
CAIROUP:=cairo-$(shell echo $(cairo_VERSION) | cut -d= -f2- | cut -d- -f1)
FBI:=fbi_$(fbi_VERSION)
FBIUP:=fbida-$(shell echo $(fbi_VERSION) | cut -d= -f2- | cut -d- -f1)
GRUBPC:=grub-pc_$(grub-pc_VERSION)
GRUBUP:=grub-$(shell echo $(grub-pc_VERSION) | cut -d- -f1 | cut -d= -f2- | tr : -)
HFSUTILS:=hfsutils_$(shell echo $(hfsutils_VERSION) | tr : .)
HFSUTILSUP:=hfsutils-$(shell echo $(hfsutils_VERSION) | cut -d- -f1 | cut -d= -f2- | cut -d: -f2)
LIBPNG:=libpng_$(libpng_VERSION)
LIBPNGUP:=libpng-$(shell echo $(libpng_VERSION) | cut -d- -f1 | cut -d= -f2- | cut -d: -f2)
LVM2:=lvm2_$(shell echo $(lvm2_VERSION) | tr : .)
LVM2UP:=LVM2.$(shell echo $(lvm2_VERSION) | cut -d- -f1 | cut -d= -f2- | cut -d: -f2)
MPLAYER:=mplayer_$(shell echo $(mplayer_VERSION) | tr : .)
OPENSSH:=openssh_$(shell echo $(openssh_VERSION) | tr : .)
OPENSSHUP:=openssh-$(shell echo $(openssh_VERSION) | cut -d- -f1 | cut -d= -f2- | cut -d: -f2)

GROWLIGHT:=growlight_$(growlight_VERSION)
XMLSTARLET:=xmlstarlet-$(xmlstarlet_VERSION)
LIBRSVG:=librsvg-$(librsvg_VERSION)
LINUXLATEST:=linux-latest_$(linux-latest_VERSION)
UTILLINUX:=util-linux_$(util-linux_VERSION)
LIBJPEGTURBO:=libjpeg-turbo_$(libjpeg-turbo_VERSION)
OMPHALOS:=omphalos_$(omphalos_VERSION)
FWTS:=fwts_$(fwts_VERSION)
SYSTEMD:=systemd_$(systemd_VERSION)
SUDO:=sudo_$(sudo_VERSION)
XBMC:=xbmc_$(xbmc_VERSION)
NETHOROLOGIST:=nethorologist_$(nethorologist_VERSION)
FBTERM:=fbterm_$(fbterm_VERSION)
STRACE:=strace_$(strace_VERSION)
SPLITVT:=splitvt_$(splitvt_VERSION)
FBV:=fbv_$(fbv_VERSION)
APITRACE:=apitrace_$(apitrace_VERSION)
GRUBTHEME:=sprezzos-grub2theme_$(sprezzos-grub2theme_VERSION)
CONPALETTE:=conpalette_$(conpalette_VERSION)

# native packages
BASEFILES:=base-files_$(base-files_VERSION)
NETBASE:=netbase_$(netbase_VERSION)
BASEINSTALLER:=base-installer_$(base-installer_VERSION)
FIRMWAREALL:=firmware-all_$(firmware-all_VERSION)

DEBS:=$(GROWLIGHT) $(LIBRSVG) $(GRUBPC) $(LVM2) $(OPENSSH) $(LIBPNG) $(XMLSTARLET) $(FWTS) \
	$(UTILLINUX) $(LINUXLATEST) $(LIBJPEGTURBO) $(OMPHALOS) $(SUDO) \
	$(GRUBTHEME) $(ADOBE) $(STRACE) $(SPLITVT) $(HFSUTILS) \
	$(NETHOROLOGIST) $(XBMC) $(MPLAYER) $(CONPALETTE) $(APITRACE) \
	$(SYSTEMD) $(BASEFILES) $(NETBASE) $(FBI) $(CAIRO)
UDEBS:=$(FBV) $(BASEINSTALLER) $(FIRMWAREALL)
DUPUDEBS:=$(GROWLIGHT) $(FBTERM) $(CONPALETTE) $(STRACE) $(SPLITVT) \
	$(NETHOROLOGIST) $(FWTS) $(UTILLINUX) $(HFSUTILS)

DEBS:=$(subst :,.,$(DEBS))
UDEBS:=$(subst :,.,$(UDEBS))

DSCS:=$(addsuffix .dsc,$(DEBS) $(UDEBS))
CHANGES:=$(addsuffix .changes,$(DEBS) $(UDEBS))

DEBS:=$(addsuffix _$(ARCH).deb,$(DEBS))
UDEBS:=$(addsuffix _$(ARCH).udeb,$(UDEBS))

world: $(DEBS) $(UDEBS)

#cd $< && apt-get -y build-dep $(shell echo $@ | cut -d_ -f1) || true # source package might not exist
%_$(ARCH).udeb %_$(ARCH).deb: %
	{ [ ! -e $</configure.in ] && [ ! -e $</configure.ac ] ; } || \
		{ [ -e $</configure ] || [ -e $</bootstrap ] ; } || \
		{ cd $< && autoreconf -sif ; }
	tar cjf $(shell echo $< | sed -e 's/\(.*\)-.*/\1/' | sed -e 's/\(.*\)-/\1_/').orig.tar.bz2 $< --exclude-vcs --exclude=\*/debian/
	cd $< && dpkg-buildpackage -k$(DEBKEY)

.PHONY: growlight
growlight: $(GROWLIGHT)_$(ARCH).deb
$(GROWLIGHT): $(SPREZZ)/growlight/debian/changelog
	git clone https://github.com/dankamongmen/growlight.git $@
	cp -r $(<D) $@/

.PHONY: xmlstarlet
xmlstarlet:$(XMLSTARLET)_$(ARCH).deb
$(XMLSTARLET): $(SPREZZ)/xmlstarlet/debian/changelog
	git clone https://github.com/dankamongmen/xmlstarlet.git $@
	cp -r $(<D) $@/

.PHONY: nethorologist
nethorologist: $(NETHOROLOGIST)_$(ARCH).deb
$(NETHOROLOGIST): $(SPREZZ)/nethorologist/debian/changelog
	git clone https://github.com/Sprezzatech/nethorologist.git $@
	cp -r $(<D) $@/

.PHONY: strace
strace: $(STRACE)_$(ARCH).deb
$(STRACE): $(SPREZZ)/strace/debian/changelog
	git clone git://strace.git.sourceforge.net/gitroot/strace/strace $@
	cp -r $(<D) $@/

# Ubuntu native packages ship their own debian/
.PHONY: fwts
fwts:$(FWTS)_$(ARCH).deb
$(FWTS): $(SPREZZ)/fwts/debian/changelog
	git clone git://kernel.ubuntu.com/hwe/fwts $@
	rm -rf $@/debian
	cp -r $(<D) $@/

.PHONY: omphalos
omphalos:$(OMPHALOS)_$(ARCH).deb
$(OMPHALOS): $(SPREZZ)/omphalos/debian/changelog
	git clone https://github.com/dankamongmen/omphalos.git $@
	cp -r $(<D) $@/

.PHONY: fbv
fbv:$(FBV).udeb
$(FBV): $(SPREZZ)/fbv/debian/changelog
	git clone git://repo.or.cz/fbv.git $@

.PHONY: fbterm
fbterm:$(FBTERM)_$(ARCH).deb
$(FBTERM): $(SPREZZ)/fbterm/debian/changelog
	git clone https://github.com/dankamongmen/nfbterm.git $@
	cp -r $(<D) $@/

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

.PHONY: linux-latest
linux-latest:$(LINUXLATEST)_$(ARCH).deb
$(LINUXLATEST): $(SPREZZ)/linux-latest/debian/changelog
	mkdir $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(CAIROUP).tar.xz
$(CAIROUP).tar.xz:
	wget -nc -O$@ http://cairographics.org/releases/$@

.PHONY: cairo
cairo:$(CAIRO)_$(ARCH).deb
$(CAIRO): $(SPREZZ)/cairo/debian/changelog $(CAIROUP).tar.xz
	mkdir $@
	tar xJvf $(CAIROUP).tar.xz --strip-components=1 -C $@
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

FETCHED:=$(FETCHED) $(LIBPNGUP).tar.bz2
$(LIBPNGUP).tar.bz2:
	wget -nc -O$@ ftp://ftp.simplesystems.org/pub/libpng/png/src/$(LIBPNGUP).tar.bz2

.PHONY: libpng
libpng:$(LIBPNG)_$(ARCH).deb
$(LIBPNG): $(SPREZZ)/libpng/debian/changelog $(LIBPNGUP).tar.bz2
	mkdir $@
	tar xjvf $(LIBPNGUP).tar.bz2 --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) sudo-1.8.5p3.tar.gz
sudo-1.8.5p3.tar.gz:
	wget -nc -O$@ http://www.gratisoft.us/sudo/dist/sudo-1.8.5p3.tar.gz

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

.PHONY: grub-pc
grub-pc:$(GRUBPC)_$(ARCH).deb
$(GRUBPC): $(SPREZZ)/grub-pc/debian/changelog $(GRUBUP).tar.xz
	mkdir -p $@
	tar xJvf $(GRUBUP).tar.xz --strip-components=1 -C $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) $(LIBRSVG).tar.xz
$(LIBRSVG).tar.xz:
	wget -nc -O$@ http://ftp.gnome.org/pub/gnome/sources/librsvg/2.36/librsvg-2.36.3.tar.xz

.PHONY: librsvg
librsvg:$(LIBRSVG)_$(ARCH).deb
$(LIBRSVG): $(SPREZZ)/librsvg/debian/changelog $(LIBRSVG).tar.xz
	mkdir -p $@
	tar xJvf $(LIBRSVG).tar.xz --strip-components=1 -C $@
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

.PHONY: netbase
netbase:$(NETBASE)_$(ARCH).deb
$(NETBASE): $(SPREZZ)/netbase/debian/changelog
	cp -r $(<D)/.. $@

.PHONY: firmware-all
firmware-all:$(FIRMWAREALL)_$(ARCH).deb
$(FIRMWAREALL): $(SPREZZ)/firmware-all/debian/changelog
	cp -r $(<D)/.. $@

clean:
	rm -rf sprezzos-world $(FETCHED) $(DEBS) $(UDEBS) $(DSCS) $(CHANGES)
	rm -rf $(GRUBTHEME) $(OMPHALOS) $(GROWLIGHT) $(FBV) $(LVM2) $(CAIRO)
	rm -rf $(ADOBE) $(FBTERM) $(CONPALETTE) $(APITRACE) $(SUDO) $(LIBPNG)
	rm -rf $(DEBS) $(UDEBS) $(LIBJPEGTURBO) $(STRACE) $(SPLITVT)
	rm -rf $(LINUXLATEST) $(NETHOROLOGIST) $(FWTS) $(UTILLINUX) $(SYSTEMD)
	rm -rf $(LIBRSVG) $(GRUBPC) $(XMLSTARLET) $(OPENSSH) $(HFSUTILS)
	rm -rf $(BASEFILES) $(NETBASE) $(BASEINSTALLER) $(FIRMWAREALL) $(FBI)

clobber:
	rm -rf $(ADOBEUP) $(CAIROUP) $(FBIUP) $(GRUBUP) $(HFSUTILSUP)
	rm -rf $(LIBPNGUP) $(LVM2UP) $(OPENSSHUP)
