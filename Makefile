.DELETE_ON_ERROR:
.PHONY: world all clean
.DEFAULT_GOAL:=all

all: world

ARCH:=amd64

DEBKEY:=9978711C
DEBFULLNAME:='nick black'
DEBEMAIL:=nick.black@sprezzatech.com

TARARGS=--strip-components=1 --exclude=$(shell echo $@ | tr _ -)/debian -C

SPREZZ:=packaging/

# These spur generation of definition files in sprezzos-world from
# debian/changelog files in packaging/*.
PACKAGES:=$(wildcard $(SPREZZ)*)

-include $(subst $(SPREZZ),sprezzos-world/,$(PACKAGES))

include worlds/ml.mk
include worlds/kde.mk
include worlds/java.mk
include worlds/ruby.mk
include worlds/perl.mk
include worlds/lxde.mk
include worlds/xfce.mk
include worlds/mint.mk
include worlds/core.mk
include worlds/scala.mk
include worlds/gnome.mk
include worlds/xapps.mk
include worlds/python.mk
include worlds/erlang.mk
include worlds/debian.mk
include worlds/ubuntu.mk
include worlds/kernels.mk
include worlds/haskell.mk
include worlds/mozilla.mk
include worlds/obsolete.mk
include worlds/sprezzatech.mk

sprezzos-world/%: $(SPREZZ)/%/debian/changelog
	[ -d $(@D) ] || mkdir -p $(@D)
	( echo "# Automatically generated from $<" && \
	 echo -n "$(shell echo $(@F) | tr [:lower:] [:upper:] | tr -d -):=$(@F)_" &&\
	 rapt-parsechangelog -l$< | grep-dctrl -ensVersion -FSource . | cut -d: -f2- | cut -d- -f1 && \
	 echo -n "$(@F)_UPVER:=" && \
	 rapt-parsechangelog -l$< | grep-dctrl -ensVersion -FSource . | cut -d: -f2- | sed -e 's/[+-].*SprezzOS[0-9]*//' -e 's/+sfsg//g' \
	 ) > $@

#cd $< && apt-get -y build-dep $(shell echo $@ | cut -d_ -f1) || true # source package might not exist
%_$(ARCH).udeb %_$(ARCH).deb: %
	cd $< && debuild -k$(DEBKEY) -j8

# Packages which we take from upstream source repositories rather than a
# release tarball. We must make our own *.orig.tar.* files for these.

.PHONY: babl
babl:$(BABL)_$(ARCH).deb
$(BABL): $(SPREZZ)/babl/debian/changelog
	git clone git@github.com:dankamongmen/babl.git $@
	tar cJf babl-$(babl_UPVER).tar.xz $@ --exclude-vcs
	ln -sf babl-$(babl_UPVER).tar.xz babl_$(babl_UPVER).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: gegl
gegl:$(GEGL)_$(ARCH).deb
$(GEGL): $(SPREZZ)/gegl/debian/changelog
	git clone git@github.com:dankamongmen/gegl.git $@
	tar cJf gegl-$(gegl_UPVER).tar.xz $@ --exclude-vcs
	ln -sf gegl-$(gegl_UPVER).tar.xz gegl_$(gegl_UPVER).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: nemo
nemo:$(NEMO)_$(ARCH).deb
$(NEMO): $(SPREZZ)/nemo/debian/changelog
	git clone git@github.com:dankamongmen/nemo.git $@
	tar cJf nemo-$(nemo_UPVER).tar.xz $@ --exclude-vcs
	ln -sf nemo-$(nemo_UPVER).tar.xz nemo_$(nemo_UPVER).orig.tar.xz
	cp -r $(<D) $@/

#.PHONY: nemo
#nemo:$(NEMO)_$(ARCH).deb
#$(NEMO): $(SPREZZ)/nemo/debian/changelog
#	mkdir $@
#	cp -r $(<D) $@/
#	cd $@ && uscan --force-download --download-current-version
#	tar xzvf nemo_$(nemo_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: fish
fish:$(FISH)_$(ARCH).deb
$(FISH): $(SPREZZ)/fish/debian/changelog
	git clone git://github.com/fish-shell/fish-shell.git $@
	rm -rf $@/debian
	tar cJf fish-$(fish_UPVER).tar.xz $@ --exclude-vcs
	ln -sf fish-$(fish_UPVER).tar.xz fish_$(fish_UPVER).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: usbutils
usbutils:$(USBUTILS)_$(ARCH).deb
$(USBUTILS): $(SPREZZ)/usbutils/debian/changelog
	git clone git@github.com:gregkh/usbutils.git $@
	rm -rf $@/debian
	cd $@ && git submodule init usbhid-dump && git submodule update usbhid-dump
	tar cJf usbutils-$(usbutils_UPVER).tar.xz $@ --exclude-vcs
	ln -sf usbutils-$(usbutils_UPVER).tar.xz usbutils_$(usbutils_UPVER).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: everpad
everpad:$(EVERPAD)_$(ARCH).deb
$(EVERPAD): $(SPREZZ)/everpad/debian/changelog
	git clone git@github.com:nvbn/everpad.git $@
	rm -rf $@/debian
	tar cJf everpad-$(everpad_UPVER).tar.xz $@ --exclude-vcs
	ln -sf everpad-$(everpad_UPVER).tar.xz everpad_$(everpad_UPVER).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: gtkparasite
gtkparasite:$(GTKPARASITE)_$(ARCH).deb
$(GTKPARASITE): $(SPREZZ)/gtkparasite/debian/changelog
	git clone git://github.com/chipx86/gtkparasite $@
	rm -rf $@/debian
	tar cJf gtkparasite-$(gtkparasite_UPVER).tar.xz $@ --exclude-vcs
	ln -sf gtkparasite-$(gtkparasite_UPVER).tar.xz gtkparasite_$(gtkparasite_UPVER).orig.tar.xz
	cp -r $(<D) $@/

#.PHONY: gpm
#gpm:$(GPM)_$(ARCH).deb
#$(GPM): $(SPREZZ)/gpm/debian/changelog
#	mkdir $@
#	cp -r $(<D) $@/
#	cd $@ && uscan --force-download --download-current-version
#	tar xzvf gpm-$(gpm_UPVER).tar.gz $(TARARGS) $@

.PHONY: gpm
gpm:$(GPM)_$(ARCH).deb
$(GPM): $(SPREZZ)/gpm/debian/changelog
	git clone https://github.com/dankamongmen/gpm.git $@
	rm -rf $@/debian
	tar cJf gpm-$(gpm_UPVER).tar.xz $@ --exclude-vcs
	ln -sf gpm-$(gpm_UPVER).tar.xz gpm_$(gpm_UPVER).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: mawk
mawk:$(MAWK)_$(ARCH).deb
$(MAWK): $(SPREZZ)/mawk/debian/changelog
	bzr branch nosmart+http://bzr.debian.org/bzr/users/vorlon/mawk/trunk/ $@
	rm -rf $@/debian
	tar cJf mawk-$(mawk_UPVER).tar.xz $@ --exclude-vcs
	ln -sf mawk-$(mawk_UPVER).tar.xz mawk_$(mawk_UPVER).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: software-properties
software-properties:$(SOFTWAREPROPERTIES)_$(ARCH).deb
$(SOFTWAREPROPERTIES): $(SPREZZ)/software-properties/debian/changelog
	bzr branch lp:~juliank/software-properties/debian $@
	rm -rf $@/debian
	tar cJf software-properties-$(software-properties_UPVER).tar.xz $@ --exclude-vcs
	ln -sf software-properties-$(software-properties_UPVER).tar.xz $(SOFTWAREPROPERTIES).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: avant-window-navigator
avant-window-navigator:$(AVANTWINDOWNAVIGATOR)_$(ARCH).deb
$(AVANTWINDOWNAVIGATOR): $(SPREZZ)/avant-window-navigator/debian/changelog
	bzr branch lp:awn $@
	rm -rf $@/debian
	tar cJf avant-window-navigator-$(avant-window-navigator_UPVER).tar.xz $@ --exclude-vcs
	ln -sf avant-window-navigator-$(avant-window-navigator_UPVER).tar.xz avant-window-navigator_$(avant-window-navigator_UPVER).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: dockmanager
dockmanager:$(DOCKMANAGER)_$(ARCH).deb
$(DOCKMANAGER): $(SPREZZ)/dockmanager/debian/changelog
	bzr branch lp:dockmanager $@
	rm -rf $@/debian
	tar cJf dockmanager-$(dockmanager_UPVER).tar.xz $@ --exclude-vcs
	ln -fs dockmanager-$(dockmanager_UPVER).tar.xz dockmanager_$(dockmanager_UPVER).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: synaptic
synaptic:$(SYNAPTIC)_$(ARCH).deb
$(SYNAPTIC): $(SPREZZ)/synaptic/debian/changelog
	bzr branch lp:synaptic $@
	rm -rf $@/debian
	tar cJf synaptic-$(synaptic_UPVER).tar.xz $@ --exclude-vcs
	ln -sf synaptic-$(synaptic_UPVER).tar.xz synaptic_$(synaptic_UPVER).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: python-defaults
python-defaults:$(PYTHONDEFAULTS)_$(ARCH).deb
$(PYTHONDEFAULTS): $(SPREZZ)/python-defaults/debian/changelog
	bzr branch http://alioth.debian.org/anonscm/bzr/pkg-python/python-defaults-debian $@
	tar cJf python-defaults-$(python-defaults_UPVER).tar.xz $@ --exclude-vcs
	ln -sf python-defaults-$(python-defaults_UPVER).tar.xz python-defaults_$(python-defaults_UPVER).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: x264
x264:$(X264)_$(ARCH).deb
$(X264): $(SPREZZ)/x264/debian/changelog
	git clone git://git.videolan.org/x264.git $@
	tar cJf x264-$(x264_UPVER).tar.xz $@ --exclude-vcs
	ln -sf x264-$(x264_UPVER).tar.xz x264_$(x264_UPVER).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: cpupower
cpupower:$(CPUPOWER)_$(ARCH).deb
$(CPUPOWER): $(SPREZZ)/cpupower/debian/changelog
	git clone git://git.kernel.org/pub/scm/linux/kernel/git/brodo/cpupowerutils.git $@
	tar cJf cpupower-$(cpupower_UPVER).tar.xz $@ --exclude-vcs
	ln -sf cpupower-$(cpupower_UPVER).tar.xz cpupower_$(cpupower_UPVER).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: mx
mx:$(MX)_$(ARCH).deb
$(MX): $(SPREZZ)/mx/debian/changelog
	git clone git://github.com/clutter-project/mx.git $@
	tar cJf mx-$(mx_UPVER).tar.xz $@ --exclude-vcs --exclude=debian
	ln -sf mx-$(mx_UPVER).tar.xz mx_$(mx_UPVER).orig.tar.xz
	cp -r $(<D) $@/

# our version compiles against libgee-0.9
#.PHONY: cheese
#cheese:$(CHEESE)_$(ARCH).deb
#$(CHEESE): $(SPREZZ)/cheese/debian/changelog
#	git clone git://github.com/dankamongmen/dankcheese.git $@
#	tar cJf cheese-$(cheese_UPVER).tar.xz $@ --exclude-vcs --exclude=debian
#	ln -sf cheese-$(cheese_UPVER).tar.xz cheese_$(cheese_UPVER).orig.tar.xz
#	cp -r $(<D) $@/

.PHONY: cheese
cheese:$(CHEESE)_$(ARCH).deb
$(CHEESE): $(SPREZZ)/cheese/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf cheese-$(cheese_UPVER).tar.xz $(TARARGS) $@

.PHONY: apitrace
apitrace:$(APITRACE)_$(ARCH).deb
$(APITRACE): $(SPREZZ)/apitrace/debian/changelog
	@[ ! -e $@ ] || { echo "Removing $@..." && rm -rf $@ ; }
	git clone git://github.com/apitrace/apitrace.git $@
	tar cJf apitrace-$(apitrace_UPVER).tar.xz $@ --exclude-vcs
	ln -sf apitrace-$(apitrace_UPVER).tar.xz apitrace_$(apitrace_UPVER).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: libshairport
libshairport:$(LIBSHAIRPORT)_$(ARCH).deb
$(LIBSHAIRPORT): $(SPREZZ)/libshairport/debian/changelog
	@[ ! -e $@ ] || { echo "Removing $@..." && rm -rf $@ ; }
	git clone https://github.com/amejia1/libshairport.git $@
	tar cJf libshairport-$(libshairport_UPVER).tar.xz $@ --exclude-vcs
	ln -sf libshairport-$(libshairport_UPVER).tar.xz libshairport_$(libshairport_UPVER).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: emap
emap:$(EMAP)_$(ARCH).deb
$(EMAP): $(SPREZZ)/emap/debian/changelog
	@[ ! -e $@ ] || { echo "Removing $@..." && rm -rf $@ ; }
	git clone git://git.enlightenment.fr/vcs/svn/PROTO/emap.git $@
	tar cJf emap-$(emap_UPVER).tar.xz $@ --exclude-vcs
	ln -sf emap-$(emap_UPVER).tar.xz emap_$(emap_UPVER).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: xmlrpc-c
xmlrpc-c:$(XMLRPCC)_$(ARCH).deb
$(XMLRPCC): $(SPREZZ)/xmlrpc-c/debian/changelog
	@[ ! -e $@ ] || { echo "Removing $@..." && rm -rf $@ ; }
	svn checkout http://svn.code.sf.net/p/xmlrpc-c/code/advanced $@
	tar cJf xmlrpc-c-$(xmlrpc-c_UPVER).tar.xz $@ --exclude-vcs
	ln -sf xmlrpc-c-$(xmlrpc-c_UPVER).tar.xz xmlrpc-c_$(xmlrpc-c_UPVER).orig.tar.xz
	cp -r $(<D) $@/

#.PHONY: xmlrpc-c
#xmlrpc-c:$(XMLRPCC)_$(ARCH).deb
#$(XMLRPCC): $(SPREZZ)/xmlrpc-c/debian/changelog
#	mkdir $@
#	cp -r $(<D) $@
#	cd $@ && uscan --force-download --download-current-version
#	tar xzvf xmlrpc-c_$(xmlrpc-c_UPVER).orig.tar.gz $(TARARGS) $@

#.PHONY: webkit
#webkit:$(WEBKIT)_$(ARCH).deb
#$(WEBKIT): $(SPREZZ)/webkit/debian/changelog
#	@[ ! -e $@ ] || { echo "Removing $@..." && rm -rf $@ ; }
#	svn checkout https://svn.webkit.org/repository/webkit/trunk $@
#	tar cJf webkit-$(webkit_UPVER).tar.xz $@ --exclude-vcs
#	ln -sf webkit-$(webkit_UPVER).tar.xz webkit_$(webkit_UPVER).orig.tar.xz
#	cp -r $(<D) $@/

.PHONY: webkit
webkit:$(WEBKIT)_$(ARCH).deb
$(WEBKIT): $(SPREZZ)/webkit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf webkit_$(webkit_UPVER).orig.tar.xz $(TARARGS) $@

#.PHONY: gtk-theme-config
#gtk-theme-config:$(GTKTHEMECONFIG)_$(ARCH).deb
#$(GTKTHEMECONFIG): $(SPREZZ)/gtk-theme-config/debian/changelog
#	@[ ! -e $@ ] || { echo "Removing $@..." && rm -rf $@ ; }
#	git clone git://github.com/satya164/gtk-theme-config.git $@
#	tar cJf gtk-theme-config-$(gtk-theme-config_UPVER).tar.xz $@ --exclude-vcs
#	ln -sf gtk-theme-config-$(gtk-theme-config_UPVER).tar.xz gtk-theme-config_$(gtk-theme-config_UPVER).orig.tar.xz
#	cp -r $(<D) $@/

.PHONY: gtk-theme-config
gtk-theme-config:$(GTKTHEMECONFIG)_$(ARCH).deb
$(GTKTHEMECONFIG): $(SPREZZ)/gtk-theme-config/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gtk-theme-config_$(gtk-theme-config_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: gnome-xcf-thumbnailer
gnome-xcf-thumbnailer:$(GNOMEXCFTHUMBNAILER)_$(ARCH).deb
$(GNOMEXCFTHUMBNAILER): $(SPREZZ)/gnome-xcf-thumbnailer/debian/changelog
	@[ ! -e $@ ] || { echo "Removing $@..." && rm -rf $@ ; }
	git clone git@github.com:dankamongmen/gnome-xcf-thumbnailer.git $@
	tar cJf gnome-xcf-thumbnailer-$(gnome-xcf-thumbnailer_UPVER).tar.xz $@ --exclude-vcs
	ln -sf gnome-xcf-thumbnailer-$(gnome-xcf-thumbnailer_UPVER).tar.xz gnome-xcf-thumbnailer_$(gnome-xcf-thumbnailer_UPVER).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: libclc
libclc: $(LIBCLC)_$(ARCH).deb
$(LIBCLC): $(SPREZZ)/libclc/debian/changelog
	@[ ! -e $@ ] || { echo "Removing $@..." && rm -rf $@ ; }
	git clone http://llvm.org/git/libclc.git $@
	tar cJf libclc-$(libclc_UPVER).tar.xz $@ --exclude-vcs
	ln -sf libclc-$(libclc_UPVER).tar.xz $(LIBCLC).orig.tar.xz
	cp -r $(<D) $@/

#.PHONY: growlight
#growlight: $(GROWLIGHT)_$(ARCH).deb
#$(GROWLIGHT): $(SPREZZ)/growlight/debian/changelog
#	@[ ! -e $@ ] || { echo "Removing $@..." && rm -rf $@ ; }
#	git clone git://github.com/dankamongmen/growlight.git $@
#	tar cJf growlight-$(growlight_UPVER) $@ --exclude-vcs
#	ln -sf growlight-$(growlight_UPVER).tar.xz growlight_$(growlight_UPVER).orig.tar.xz
#	cp -r $(<D) $@/

.PHONY: growlight
growlight:$(GROWLIGHT)_$(ARCH).deb
$(GROWLIGHT): $(SPREZZ)/growlight/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf growlight-$(growlight_UPVER).tar.xz $(TARARGS) $@

.PHONY: graphviz
graphviz:$(GRAPHVIZ)_$(ARCH).deb
$(GRAPHVIZ): $(SPREZZ)/graphviz/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf graphviz-$(graphviz_UPVER).tar.gz $(TARARGS) $@

.PHONY: grcompiler
grcompiler:$(GRCOMPILER)_$(ARCH).deb
$(GRCOMPILER): $(SPREZZ)/grcompiler/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf grcompiler-$(grcompiler_UPVER).tar.gz $(TARARGS) $@

.PHONY: reportbug
reportbug: $(REPORTBUG)_$(ARCH).deb
$(REPORTBUG): $(SPREZZ)/reportbug/debian/changelog
	@[ ! -e $@ ] || { echo "Removing $@..." && rm -rf $@ ; }
	git clone git://anonscm.debian.org/reportbug/reportbug.git $@
	tar cJf reportbug-$(reportbug_UPVER).tar.xz $@ --exclude-vcs
	ln -sf reportbug-$(reportbug_UPVER).tar.xz reportbug_$(reportbug_UPVER).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: mcelog
mcelog:$(MCELOG)_$(ARCH).deb
$(MCELOG): $(SPREZZ)/mcelog/debian/changelog
	@[ ! -e $@ ] || { echo "Removing $@..." && rm -rf $@ ; }
	git clone git://git.kernel.org/pub/scm/utils/cpu/mce/mcelog.git $@
	tar cJf $(MCELOGORIG) $@ --exclude-vcs
	cp -r $(<D) $@/

#.PHONY: gtkpod
#gtkpod:$(GTKPOD)_$(ARCH).deb
#$(GTKPOD): $(SPREZZ)/gtkpod/debian/changelog
#	@[ ! -e $@ ] || { echo "Removing $@..." && rm -rf $@ ; }
#	git clone git@github.com:Sprezzatech/gtkpod.git $@
#	echo $(gtkpod_UPVER) > $@/version
#	tar cJf gtkpod-$(gtkpod_UPVER).tar.xz $@ --exclude-vcs
#	ln -sf gtkpod-$(gtkpod_UPVER).tar.xz gtkpod_$(gtkpod_UPVER).orig.tar.xz
#	cp -r $(<D) $@/

.PHONY: gtkpod
gtkpod:$(GTKPOD)_$(ARCH).deb
$(GTKPOD): $(SPREZZ)/gtkpod/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gtkpod-$(gtkpod_UPVER).tar.gz $(TARARGS) $@

#.PHONY: miro
#miro:$(MIRO)_$(ARCH).deb
#$(MIRO): $(SPREZZ)/miro/debian/changelog
#	@[ ! -e $@ ] || { echo "Removing $@..." && rm -rf $@ ; }
#	git clone git@github.com:dankamongmen/miro.git $@
#	tar cJf miro-$(miro_UPVER).tar.xz $@ --exclude-vcs
#	ln -sf miro-$(miro_UPVER).tar.xz miro_$(miro_UPVER).orig.tar.xz
#	cp -r $(<D) $@/

.PHONY: miro
miro:$(MIRO)_$(ARCH).deb
$(MIRO): $(SPREZZ)/miro/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf miro-$(miro_UPVER).tar.gz $(TARARGS) $@

.PHONY: mitmproxy
mitmproxy:$(MITMPROXY)_$(ARCH).deb
$(MITMPROXY): $(SPREZZ)/mitmproxy/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mitmproxy_$(mitmproxy_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: spikeproxy
spikeproxy:$(SPIKEPROXY)_$(ARCH).deb
$(SPIKEPROXY): $(SPREZZ)/spikeproxy/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf spkproxy_$(spikeproxy_UPVER).orig.tar.gz $(TARARGS) $@

#.PHONY: omphalos
#omphalos:$(OMPHALOS)_$(ARCH).deb
#$(OMPHALOS): $(SPREZZ)/omphalos/debian/changelog
#	git clone git://github.com/dankamongmen/omphalos.git $@
#	tar cJf $(OMPHALOSORIG) $@ --exclude-vcs
#	cp -r $(<D) $@/

.PHONY: frei0r
frei0r:$(FREI0R)_$(ARCH).deb
$(FREI0R): $(SPREZZ)/frei0r/debian/changelog
	git clone git://code.dyne.org/frei0r.git $@
	tar cJf $(FREI0RORIG) $@ --exclude-vcs
	cp -r $(<D) $@/

.PHONY: aptdaemon
aptdaemon: $(APTDAEMON)_$(ARCH).deb
$(APTDAEMON): $(SPREZZ)/aptdaemon/debian/changelog
	bzr branch lp:aptdaemon $@
	rm -rf $@/debian
	tar cJf aptdaemon-$(aptdaemon_UPVER).tar.xz $@ --exclude-vcs
	ln -sf aptdaemon-$(aptdaemon_UPVER).tar.xz aptdaemon_$(aptdaemon_UPVER).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: Sick-Beard
Sick-Beard: $(SICKBEARD)_$(ARCH).deb
$(SICKBEARD): $(SPREZZ)/Sick-Beard/debian/changelog
	git clone git://github.com/midgetspy/Sick-Beard.git $@
	tar cJf $(SICKBEARDORIG) $@ --exclude-vcs
	cp -r $(<D) $@/

.PHONY: spl
spl: $(SPL)_$(ARCH).deb
$(SPL): $(SPREZZ)/spl/debian/changelog
	@[ ! -e $@ ] || { echo "Removing $@..." && rm -rf $@ ; }
	git clone git://github.com/zfsonlinux/spl.git $@
	tar cJf $(SPL).orig.tar.xz $@ --exclude-vcs
	false

.PHONY: zfs
zfs: $(ZFS)_$(ARCH).deb
$(ZFS): $(SPREZZ)/zfs/debian/changelog
	@[ ! -e $@ ] || { echo "Removing $@..." && rm -rf $@ ; }
	git clone git://github.com/zfsonlinux/zfs.git $@
	rm -rf $@/debian
	tar cJf $(ZFS).orig.tar.xz $@ --exclude-vcs
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
	@[ ! -e $@ ] || { echo "Removing $@..." && rm -rf $@ ; }
	git clone git@github.com:dankamongmen/nfbv.git $@
	tar cJf nfbv-$(fbv_UPVER).tar.xz $@ --exclude-vcs
	ln -sf nfbv-$(fbv_UPVER).tar.xz nfbv_$(fbv_UPVER).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: slicer
slicer:$(SLICER)_$(ARCH).deb
$(SLICER): $(SPREZZ)/slicer/debian/changelog
	svn co http://svn.slicer.org/Slicer3/trunk $@
	rm -rf $@/debian
	cp -r $(<D) $@/
	tar cJf slicer-$(slicer_UPVER).tar.xz $@ --exclude-vcs --exclude=debian
	ln -sf slicer-$(slicer_UPVER).tar.xz slicer_$(slicer_UPVER).orig.tar.xz

.PHONY: gyp
gyp:$(GYP)_$(ARCH).deb
$(GYP): $(SPREZZ)/gyp/debian/changelog
	svn co http://gyp.googlecode.com/svn/trunk/ $@
	rm -rf $@/debian
	cp -r $(<D) $@/
	tar cJf gyp-$(gyp_UPVER).tar.xz $@ --exclude-vcs --exclude=debian
	ln -sf gyp-$(gyp_UPVER).tar.xz gyp_$(gyp_UPVER).orig.tar.xz

.PHONY: despotify
despotify:$(DESPOTIFY)_$(ARCH).deb
$(DESPOTIFY): $(SPREZZ)/despotify/debian/changelog
	svn co https://despotify.svn.sourceforge.net/svnroot/despotify/src $@
	rm -rf $@/debian
	cp -r $(<D) $@/
	tar cJf despotify-$(despotify_UPVER).tar.xz $@ --exclude-vcs --exclude=debian
	ln -sf despotify-$(despotify_UPVER).tar.xz despotify_$(despotify_UPVER).orig.tar.xz

.PHONY: mplayer
mplayer:$(MPLAYER)_$(ARCH).deb
$(MPLAYER): $(SPREZZ)/mplayer/debian/changelog
	svn co svn://svn.mplayerhq.hu/mplayer/trunk $@
	rm -rf $@/debian
	cp -r $(<D) $@/
	tar cJf mplayer-$(mplayer_UPVER).tar.xz $@ --exclude-vcs --exclude=debian
	ln -sf mplayer-$(mplayer_UPVER).tar.xz mplayer_$(mplayer_UPVER).orig.tar.xz

.PHONY: skia
skia:$(SKIA)_$(ARCH).deb
$(SKIA): $(SPREZZ)/skia/debian/changelog
	svn co http://skia.googlecode.com/svn/trunk/ $@
	rm -rf $@/debian
	cp -r $(<D) $@/
	tar cJf skia-$(skia_UPVER).tar.xz $@ --exclude-vcs --exclude=debian
	ln -sf skia-$(skia_UPVER).tar.xz skia_$(skia_UPVER).orig.tar.xz

.PHONY: ramen
ramen:$(RAMEN)_$(ARCH).deb
$(RAMEN): $(SPREZZ)/ramen/debian/changelog
	svn co https://ramenhdr.svn.sourceforge.net/svnroot/ramenhdr/trunk $@
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
	tar cJf linux-latest-$(linux-latest_UPVER).tar.xz $@ --exclude-vcs --exclude=debian
	ln -s linux-latest-$(linux-latest_UPVER).tar.xz linux-latest_$(linux-latest_UPVER).orig.tar.xz

.PHONY: javascript-common
javascript-common:$(JAVASCRIPTCOMMON)_$(ARCH).deb
$(JAVASCRIPTCOMMON): $(SPREZZ)/javascript-common/debian/changelog
	[ ! -e $@ ] || { echo "$@ already exists; remove it" >&2 ; false ; }
	cp -r $(<D)/.. $@/

.PHONY: abcde
abcde:$(ABCDE)_$(ARCH).deb
$(ABCDE): $(SPREZZ)/abcde/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf abcde-$(abcde_UPVER).tar.gz $(TARARGS) $@

.PHONY: aacplusenc
aacplusenc:$(AACPLUSENC)_$(ARCH).deb
$(AACPLUSENC): $(SPREZZ)/aacplusenc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf aacplusenc_$(aacplusenc_UPVER).tar.gz $(TARARGS) $@
	rm -rf $@/debian
	cp -r $(<D) $@/

.PHONY: aalib
aalib:$(AALIB)_$(ARCH).deb
$(AALIB): $(SPREZZ)/aalib/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf $(AALIB).orig.tar.gz $(TARARGS) $@

.PHONY: aaphoto
aaphoto:$(AAPHOTO)_$(ARCH).deb
$(AAPHOTO): $(SPREZZ)/aaphoto/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf aaphoto_$(aaphoto_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ace
ace:$(ACE)_$(ARCH).deb
$(ACE): $(SPREZZ)/ace/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf ACE+TAO-src-$(ace_UPVER).tar.bz2 $(TARARGS) $@
	rm -rf $@/debian
	cp -r $(<D) $@/

.PHONY: acetoneiso
acetoneiso:$(ACETONEISO)_$(ARCH).deb
$(ACETONEISO): $(SPREZZ)/acetoneiso/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf acetoneiso_$(acetoneiso_UPVER).tar.gz $(TARARGS) $@

.PHONY: accerciser
accerciser:$(ACCERCISER)_$(ARCH).deb
$(ACCERCISER): $(SPREZZ)/accerciser/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf accerciser-$(accerciser_UPVER).tar.xz $(TARARGS) $@

.PHONY: accountsservice
accountsservice:$(ACCOUNTSSERVICE)_$(ARCH).deb
$(ACCOUNTSSERVICE): $(SPREZZ)/accountsservice/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf accountsservice-$(accountsservice_UPVER).tar.gz $(TARARGS) $@

.PHONY: aspell-sk
aspell-sk:$(ASPELLSK)_$(ARCH).deb
$(ASPELLSK): $(SPREZZ)/aspell-sk/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf aspell6-sk-$(aspell-sk_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: asterisk
asterisk:$(ASTERISK)_$(ARCH).deb
$(ASTERISK): $(SPREZZ)/asterisk/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf asterisk-$(asterisk_UPVER).tar.gz $(TARARGS) $@

.PHONY: gimp-help
gimp-help:$(GIMPHELP)_$(ARCH).deb
$(GIMPHELP): $(SPREZZ)/gimp-help/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf gimp-help-$(gimp-help_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: gimplensfun
gimplensfun:$(GIMPLENSFUN)_$(ARCH).deb
$(GIMPLENSFUN): $(SPREZZ)/gimplensfun/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gimplensfun-$(gimplensfun_UPVER).tar.gz $(TARARGS) $@

.PHONY: lensfun
lensfun:$(LENSFUN)_$(ARCH).deb
$(LENSFUN): $(SPREZZ)/lensfun/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf lensfun-$(lensfun_UPVER).tar.gz $(TARARGS) $@

.PHONY: less
less:$(LESS)_$(ARCH).deb
$(LESS): $(SPREZZ)/less/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf less-$(less_UPVER).tar.gz $(TARARGS) $@

.PHONY: luminance-hdr
luminance-hdr:$(LUMINANCEHDR)_$(ARCH).deb
$(LUMINANCEHDR): $(SPREZZ)/luminance-hdr/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf luminance-hdr-$(luminance-hdr_UPVER).tar.bz2 --exclude=$(shell echo $@ | tr _ -)/debian -C $@

.PHONY: lvm2
lvm2:$(LVM2)_$(ARCH).deb
$(LVM2): $(SPREZZ)/lvm2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf lvm2_$(lvm2_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: lldpad
lldpad:$(LLDPAD)_$(ARCH).deb
$(LLDPAD): $(SPREZZ)/lldpad/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf lldpad-$(lldpad_UPVER).tar.gz $(TARARGS) $@

.PHONY: llvm-3.2
llvm-3.2:$(LLVM3.2)_$(ARCH).deb
$(LLVM3.2): $(SPREZZ)/llvm-3.2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf llvm-3.2-$(llvm-3.2_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: links2
links2:$(LINKS2)_$(ARCH).deb
$(LINKS2): $(SPREZZ)/links2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf links-$(links2_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: linphone
linphone:$(LINPHONE)_$(ARCH).deb
$(LINPHONE): $(SPREZZ)/linphone/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf linphone-$(linphone_UPVER).tar.gz $(TARARGS) $@

.PHONY: libaacplus
libaacplus:$(LIBAACPLUS)_$(ARCH).deb
$(LIBAACPLUS): $(SPREZZ)/libaacplus/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libaacplus-$(libaacplus_UPVER).tar.gz $(TARARGS) $@

.PHONY: libaacs
libaacs:$(LIBAACS)_$(ARCH).deb
$(LIBAACS): $(SPREZZ)/libaacs/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libaacs-$(libaacs_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libao
libao:$(LIBAO)_$(ARCH).deb
$(LIBAO): $(SPREZZ)/libao/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libao-$(libao_UPVER).tar.gz $(TARARGS) $@

.PHONY: libappindicator
libappindicator:$(LIBAPPINDICATOR)_$(ARCH).deb
$(LIBAPPINDICATOR): $(SPREZZ)/libappindicator/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libappindicator-$(libappindicator_UPVER).tar.gz $(TARARGS) $@

.PHONY: libasyncns
libasyncns:$(LIBASYNCNS)_$(ARCH).deb
$(LIBASYNCNS): $(SPREZZ)/libasyncns/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libasyncns-$(libasyncns_UPVER).tar.gz $(TARARGS) $@

.PHONY: sdl-image1.2
sdl-image1.2:$(SDLIMAGE1.2)_$(ARCH).deb
$(SDLIMAGE1.2): $(SPREZZ)/sdl-image1.2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf SDL_image-$(sdl-image1.2_UPVER).tar.gz $(TARARGS) $@

.PHONY: libsdl1.2
libsdl1.2:$(LIBSDL1.2)_$(ARCH).deb
$(LIBSDL1.2): $(SPREZZ)/libsdl1.2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf SDL-$(libsdl1.2_UPVER).tar.gz $(TARARGS) $@

.PHONY: libsigsegv
libsigsegv:$(LIBSIGSEGV)_$(ARCH).deb
$(LIBSIGSEGV): $(SPREZZ)/libsigsegv/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libsigsegv-$(libsigsegv_UPVER).tar.gz $(TARARGS) $@

.PHONY: libsub-name-perl
libsub-name-perl:$(LIBSUBNAMEPERL)_$(ARCH).deb
$(LIBSUBNAMEPERL): $(SPREZZ)/libsub-name-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Sub-Name-$(libsub-name-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libsndfile
libsndfile:$(LIBSNDFILE)_$(ARCH).deb
$(LIBSNDFILE): $(SPREZZ)/libsndfile/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libsndfile-$(libsndfile_UPVER).tar.gz $(TARARGS) $@

.PHONY: libalgorithm-permute-perl
libalgorithm-permute-perl:$(LIBALGORITHMPERMUTEPERL)_$(ARCH).deb
$(LIBALGORITHMPERMUTEPERL): $(SPREZZ)/libalgorithm-permute-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Algorithm-Permute-$(libalgorithm-permute-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libalgorithm-diff-xs-perl
libalgorithm-diff-xs-perl:$(LIBALGORITHMDIFFXSPERL)_$(ARCH).deb
$(LIBALGORITHMDIFFXSPERL): $(SPREZZ)/libalgorithm-diff-xs-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Algorithm-Diff-XS-$(libalgorithm-diff-xs-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libpackage-stash-xs-perl
libpackage-stash-xs-perl:$(LIBPACKAGESTASHXSPERL)_$(ARCH).deb
$(LIBPACKAGESTASHXSPERL): $(SPREZZ)/libpackage-stash-xs-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Package-Stash-XS-$(libpackage-stash-xs-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libparams-classify-perl
libparams-classify-perl:$(LIBPARAMSCLASSIFYPERL)_$(ARCH).deb
$(LIBPARAMSCLASSIFYPERL): $(SPREZZ)/libparams-classify-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Params-Classify-$(libparams-classify-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libparams-util-perl
libparams-util-perl:$(LIBPARAMSUTILPERL)_$(ARCH).deb
$(LIBPARAMSUTILPERL): $(SPREZZ)/libparams-util-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Params-Util-$(libparams-util-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libparams-validate-perl
libparams-validate-perl:$(LIBPARAMSVALIDATEPERL)_$(ARCH).deb
$(LIBPARAMSVALIDATEPERL): $(SPREZZ)/libparams-validate-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Params-Validate-$(libparams-validate-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: liblist-moreutils-perl
liblist-moreutils-perl:$(LIBLISTMOREUTILSPERL)_$(ARCH).deb
$(LIBLISTMOREUTILSPERL): $(SPREZZ)/liblist-moreutils-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf List-MoreUtils-$(liblist-moreutils-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libdbi-perl
libdbi-perl:$(LIBDBIPERL)_$(ARCH).deb
$(LIBDBIPERL): $(SPREZZ)/libdbi-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf DBI-$(libdbi-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libterm-readkey-perl
libterm-readkey-perl:$(LIBTERMREADKEYPERL)_$(ARCH).deb
$(LIBTERMREADKEYPERL): $(SPREZZ)/libterm-readkey-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf TermReadKey-$(libterm-readkey-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libclone-perl
libclone-perl:$(LIBCLONEPERL)_$(ARCH).deb
$(LIBCLONEPERL): $(SPREZZ)/libclone-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Clone-$(libclone-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libcurses-perl
libcurses-perl:$(LIBCURSESPERL)_$(ARCH).deb
$(LIBCURSESPERL): $(SPREZZ)/libcurses-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Curses-$(libcurses-perl_UPVER).tgz $(TARARGS) $@

.PHONY: libperl4-corelibs-perl
libperl4-corelibs-perl:$(LIBPERL4CORELIBSPERL)_$(ARCH).deb
$(LIBPERL4CORELIBSPERL): $(SPREZZ)/libperl4-corelibs-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Perl4-CoreLibs-$(libperl4-corelibs-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libperlio-gzip-perl
libperlio-gzip-perl:$(LIBPERLIOGZIPPERL)_$(ARCH).deb
$(LIBPERLIOGZIPPERL): $(SPREZZ)/libperlio-gzip-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf PerlIO-gzip-$(libperlio-gzip-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libsocket6-perl
libsocket6-perl:$(LIBSOCKET6PERL)_$(ARCH).deb
$(LIBSOCKET6PERL): $(SPREZZ)/libsocket6-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Socket6-$(libsocket6-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libdbd-mysql-perl
libdbd-mysql-perl:$(LIBDBDMYSQLPERL)_$(ARCH).deb
$(LIBDBDMYSQLPERL): $(SPREZZ)/libdbd-mysql-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf DBD-mysql-$(libdbd-mysql-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libdbd-sqlite3-perl
libdbd-sqlite3-perl:$(LIBDBDSQLITE3PERL)_$(ARCH).deb
$(LIBDBDSQLITE3PERL): $(SPREZZ)/libdbd-sqlite3-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf DBD-SQLite-$(libdbd-sqlite3-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libnet-dbus-perl
libnet-dbus-perl:$(LIBNETDBUSPERL)_$(ARCH).deb
$(LIBNETDBUSPERL): $(SPREZZ)/libnet-dbus-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Net-DBus-$(libnet-dbus-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libnet-dns-perl
libnet-dns-perl:$(LIBNETDNSPERL)_$(ARCH).deb
$(LIBNETDNSPERL): $(SPREZZ)/libnet-dns-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Net-DNS-$(libnet-dns-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libnetaddr-ip-perl
libnetaddr-ip-perl:$(LIBNETADDRIPPERL)_$(ARCH).deb
$(LIBNETADDRIPPERL): $(SPREZZ)/libnetaddr-ip-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf NetAddr-IP-$(libnetaddr-ip-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libnet-ssleay-perl
libnet-ssleay-perl:$(LIBNETSSLEAYPERL)_$(ARCH).deb
$(LIBNETSSLEAYPERL): $(SPREZZ)/libnet-ssleay-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Net-SSLeay-$(libnet-ssleay-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libxml-libxml-perl
libxml-libxml-perl:$(LIBXMLLIBXMLPERL)_$(ARCH).deb
$(LIBXMLLIBXMLPERL): $(SPREZZ)/libxml-libxml-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf XML-LibXML-$(libxml-libxml-perl_UPVER).tar.gz $(TARARGS) $@
	rm -rf $@/debian
	cp -r $(<D) $@/

.PHONY: libxml-libxslt-perl
libxml-libxslt-perl:$(LIBXMLLIBXSLTPERL)_$(ARCH).deb
$(LIBXMLLIBXSLTPERL): $(SPREZZ)/libxml-libxslt-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf XML-LibXSLT-$(libxml-libxslt-perl_UPVER).tar.gz $(TARARGS) $@
	rm -rf $@/debian
	cp -r $(<D) $@/

.PHONY: libxml-parser-perl
libxml-parser-perl:$(LIBXMLPARSERPERL)_$(ARCH).deb
$(LIBXMLPARSERPERL): $(SPREZZ)/libxml-parser-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf XML-Parser-$(libxml-parser-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libtext-iconv-perl
libtext-iconv-perl:$(LIBTEXTICONVPERL)_$(ARCH).deb
$(LIBTEXTICONVPERL): $(SPREZZ)/libtext-iconv-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Text-Iconv-$(libtext-iconv-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libhtml-parser-perl
libhtml-parser-perl:$(LIBHTMLPARSERPERL)_$(ARCH).deb
$(LIBHTMLPARSERPERL): $(SPREZZ)/libhtml-parser-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf HTML-Parser-$(libhtml-parser-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libhtml-tree-perl
libhtml-tree-perl:$(LIBHTMLTREEPERL)_$(ARCH).deb
$(LIBHTMLTREEPERL): $(SPREZZ)/libhtml-tree-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf HTML-Tree-$(libhtml-tree-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libhttp-cookies-perl
libhttp-cookies-perl:$(LIBHTTPCOOKIESPERL)_$(ARCH).deb
$(LIBHTTPCOOKIESPERL): $(SPREZZ)/libhttp-cookies-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf HTTP-Cookies-$(libhttp-cookies-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libhttp-message-perl
libhttp-message-perl:$(LIBHTTPMESSAGEPERL)_$(ARCH).deb
$(LIBHTTPMESSAGEPERL): $(SPREZZ)/libhttp-message-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf HTTP-Message-$(libhttp-message-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libswitch-perl
libswitch-perl:$(LIBSWITCHPERL)_$(ARCH).deb
$(LIBSWITCHPERL): $(SPREZZ)/libswitch-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Switch-$(libswitch-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: signon
signon:$(SIGNON)_$(ARCH).deb
$(SIGNON): $(SPREZZ)/signon/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf signon-$(signon_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: sox
sox:$(SOX)_$(ARCH).deb
$(SOX): $(SPREZZ)/sox/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf sox-$(sox_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: swi-prolog
swi-prolog:$(SWIPROLOG)_$(ARCH).deb
$(SWIPROLOG): $(SPREZZ)/swi-prolog/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pl-$(swi-prolog_UPVER).tar.gz $(TARARGS) $@

.PHONY: swig
swig:$(SWIG2.0)_$(ARCH).deb
$(SWIG2.0): $(SPREZZ)/swig2.0/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf swig-$(swig2.0_UPVER).tar.gz $(TARARGS) $@

.PHONY: synaesthesia
synaesthesia:$(SYNAESTHESIA)_$(ARCH).deb
$(SYNAESTHESIA): $(SPREZZ)/synaesthesia/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf synaesthesia-$(synaesthesia_UPVER).tar.gz $(TARARGS) $@

.PHONY: libsignon-glib
libsignon-glib:$(LIBSIGNONGLIB)_$(ARCH).deb
$(LIBSIGNONGLIB): $(SPREZZ)/libsignon-glib/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libsignon-glib-$(libsignon-glib_UPVER).tar.gz $(TARARGS) $@

.PHONY: libaccounts-glib
libaccounts-glib:$(LIBACCOUNTSGLIB)_$(ARCH).deb
$(LIBACCOUNTSGLIB): $(SPREZZ)/libaccounts-glib/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libaccounts-glib-$(libaccounts-glib_UPVER).tar.gz $(TARARGS) $@

.PHONY: a52dec
a52dec:$(A52DEC)_$(ARCH).deb
$(A52DEC): $(SPREZZ)/a52dec/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf a52dec-$(a52dec_UPVER).tar.gz $(TARARGS) $@

.PHONY: acl
acl:$(ACL)_$(ARCH).deb
$(ACL): $(SPREZZ)/acl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf acl-$(acl_UPVER).src.tar.gz $(TARARGS) $@

.PHONY: acpi
acpi:$(ACPI)_$(ARCH).deb
$(ACPI): $(SPREZZ)/acpi/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf acpi-$(acpi_UPVER).tar.gz $(TARARGS) $@

.PHONY: acpi-support
acpi-support:$(ACPISUPPORT)_$(ARCH).deb
$(ACPISUPPORT): $(SPREZZ)/acpi-support/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf acpi-support-$(acpi-support_UPVER).tar.gz $(TARARGS) $@

.PHONY: acpid
acpid:$(ACPID)_$(ARCH).deb
$(ACPID): $(SPREZZ)/acpid/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf acpid-$(acpid_UPVER).tar.xz $(TARARGS) $@

.PHONY: afpfs-ng
afpfs-ng:$(AFPFSNG)_$(ARCH).deb
$(AFPFSNG): $(SPREZZ)/afpfs-ng/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf afpfs-ng-$(afpfs-ng_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: aften
aften:$(AFTEN)_$(ARCH).deb
$(AFTEN): $(SPREZZ)/aften/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf aften-$(aften_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: alacarte
alacarte:$(ALACARTE)_$(ARCH).deb
$(ALACARTE): $(SPREZZ)/alacarte/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf alacarte-$(alacarte_UPVER).tar.xz $(TARARGS) $@

.PHONY: alsa-lib
alsa-lib:$(ALSALIB)_$(ARCH).deb
$(ALSALIB): $(SPREZZ)/alsa-lib/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf alsa-lib-$(alsa-lib_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: alsa-tools
alsa-tools:$(ALSATOOLS)_$(ARCH).deb
$(ALSATOOLS): $(SPREZZ)/alsa-tools/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf alsa-tools-$(alsa-tools_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: alsa-utils
alsa-utils:$(ALSAUTILS)_$(ARCH).deb
$(ALSAUTILS): $(SPREZZ)/alsa-utils/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf alsa-utils-$(alsa-utils_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: alsaplayer
alsaplayer:$(ALSAPLAYER)_$(ARCH).deb
$(ALSAPLAYER): $(SPREZZ)/alsaplayer/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf alsaplayer-$(alsaplayer_UPVER).tar.gz $(TARARGS) $@

.PHONY: antlr
antlr:$(ANTLR)_$(ARCH).deb
$(ANTLR): $(SPREZZ)/antlr/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf antlr-$(antlr_UPVER).tar.gz $(TARARGS) $@

.PHONY: apache
apache:$(APACHE)_$(ARCH).deb
$(APACHE): $(SPREZZ)/apache/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf httpd-$(apache_UPVER).tar.gz $(TARARGS) $@

.PHONY: apachetop
apachetop:$(APACHETOP)_$(ARCH).deb
$(APACHETOP): $(SPREZZ)/apachetop/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf apachetop-$(apachetop_UPVER).tar.gz $(TARARGS) $@

.PHONY: apparmor
apparmor:$(APPARMOR)_$(ARCH).deb
$(APPARMOR): $(SPREZZ)/apparmor/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf apparmor-$(apparmor_UPVER).tar.gz $(TARARGS) $@

.PHONY: ardour
ardour:$(ARDOUR)_$(ARCH).deb
$(ARDOUR): $(SPREZZ)/ardour/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xjvf ardour-$(ardour_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: argyll
argyll:$(ARGYLL)_$(ARCH).deb
$(ARGYLL): $(SPREZZ)/argyll/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf Argyll_V$(argyll_UPVER)_src.tar.gz $(TARARGS) $@

.PHONY: ario
ario:$(ARIO)_$(ARCH).deb
$(ARIO): $(SPREZZ)/ario/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf ario-$(ario_UPVER).tar.gz $(TARARGS) $@

.PHONY: arp-scan
arp-scan:$(ARPSCAN)_$(ARCH).deb
$(ARPSCAN): $(SPREZZ)/arp-scan/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf arp-scan-$(arp-scan_UPVER).tar.gz $(TARARGS) $@

.PHONY: arpwatch
arpwatch:$(ARPWATCH)_$(ARCH).deb
$(ARPWATCH): $(SPREZZ)/arpwatch/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf arpwatch-$(arpwatch_UPVER).tar.gz $(TARARGS) $@

.PHONY: libautobox-perl
libautobox-perl:$(LIBAUTOBOXPERL)_$(ARCH).deb
$(LIBAUTOBOXPERL): $(SPREZZ)/libautobox-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf autobox-$(libautobox-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libass
libass:$(LIBASS)_$(ARCH).deb
$(LIBASS): $(SPREZZ)/libass/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libass-$(libass_UPVER).tar.xz $(TARARGS) $@

.PHONY: audacity
audacity:$(AUDACITY)_$(ARCH).deb
$(AUDACITY): $(SPREZZ)/audacity/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf audacity-minsrc-$(audacity_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: audiofile
audiofile:$(AUDIOFILE)_$(ARCH).deb
$(AUDIOFILE): $(SPREZZ)/audiofile/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf audiofile-$(audiofile_UPVER).tar.gz $(TARARGS) $@

.PHONY: audit
audit:$(AUDIT)_$(ARCH).deb
$(AUDIT): $(SPREZZ)/audit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf audit-$(audit_UPVER).tar.gz $(TARARGS) $@

.PHONY: augeas
augeas:$(AUGEAS)_$(ARCH).deb
$(AUGEAS): $(SPREZZ)/augeas/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf augeas-$(augeas_UPVER).tar.gz $(TARARGS) $@

.PHONY: at
at:$(AT)_$(ARCH).deb
$(AT): $(SPREZZ)/at/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf at_$(at_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: attr
attr:$(ATTR)_$(ARCH).deb
$(ATTR): $(SPREZZ)/attr/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf attr-$(attr_UPVER).src.tar.gz $(TARARGS) $@

.PHONY: autoconf
autoconf:$(AUTOCONF)_$(ARCH).deb
$(AUTOCONF): $(SPREZZ)/autoconf/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf autoconf-$(autoconf_UPVER).tar.gz $(TARARGS) $@

.PHONY: autoconf-archive
autoconf-archive:$(AUTOCONFARCHIVE)_$(ARCH).deb
$(AUTOCONFARCHIVE): $(SPREZZ)/autoconf-archive/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf autoconf-archive_$(autoconf-archive_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: autodock-suite
autodock-suite:$(AUTODOCKSUITE)_$(ARCH).deb
$(AUTODOCKSUITE): $(SPREZZ)/autodocksuite/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf autodocksuite-$(autodocksuite_UPVER)-src.tar.gz $(TARARGS) $@

.PHONY: autogen
autogen:$(AUTOGEN)_$(ARCH).deb
$(AUTOGEN): $(SPREZZ)/autogen/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf autogen-$(autogen_UPVER).tar.gz $(TARARGS) $@

.PHONY: autokey
autokey:$(AUTOKEY)_$(ARCH).deb
$(AUTOKEY): $(SPREZZ)/autokey/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf autokey_$(autokey_UPVER).tar.gz $(TARARGS) $@

.PHONY: automake1.11
automake1.11:$(AUTOMAKE1.11)_$(ARCH).deb
$(AUTOMAKE1.11): $(SPREZZ)/automake1.11/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf automake-$(automake1.11_UPVER).tar.gz $(TARARGS) $@

.PHONY: automake1.13
automake1.13:$(AUTOMAKE1.13)_$(ARCH).deb
$(AUTOMAKE1.13): $(SPREZZ)/automake1.13/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf automake-$(automake1.13_UPVER).tar.gz $(TARARGS) $@

.PHONY: avahi
avahi:$(AVAHI)_$(ARCH).deb
$(AVAHI): $(SPREZZ)/avahi/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf avahi-$(avahi_UPVER).tar.gz $(TARARGS) $@

.PHONY: avidemux
avidemux:$(AVIDEMUX)_$(ARCH).deb
$(AVIDEMUX): $(SPREZZ)/avidemux/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf avidemux_$(avidemux_UPVER).tar.gz $(TARARGS) $@

.PHONY: avogadro
avogadro:$(AVOGADRO)_$(ARCH).deb
$(AVOGADRO): $(SPREZZ)/avogadro/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf avogadro-$(avogadro_UPVER).tar.gz $(TARARGS) $@

.PHONY: awesome
awesome:$(AWESOME)_$(ARCH).deb
$(AWESOME): $(SPREZZ)/awesome/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf awesome-$(awesome_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: awn-extras-applets
awn-extras-applets:$(AWNEXTRASAPPLETS)_$(ARCH).deb
$(AWNEXTRASAPPLETS): $(SPREZZ)/awn-extras-applets/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf awn-extras-$(awn-extras-applets_UPVER).tar.gz $(TARARGS) $@

.PHONY: babeltrace
babeltrace:$(BABELTRACE)_$(ARCH).deb
$(BABELTRACE): $(SPREZZ)/babeltrace/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf babeltrace-$(babeltrace_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: banshee
banshee:$(BANSHEE)_$(ARCH).deb
$(BANSHEE): $(SPREZZ)/banshee/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf banshee-$(banshee_UPVER).tar.xz $(TARARGS) $@

.PHONY: baobab
baobab:$(BAOBAB)_$(ARCH).deb
$(BAOBAB): $(SPREZZ)/baobab/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf baobab-$(baobab_UPVER).tar.xz $(TARARGS) $@

.PHONY: bash
bash:$(BASH)_$(ARCH).deb
$(BASH): $(SPREZZ)/bash/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bash-$(bash_UPVER).tar.gz $(TARARGS) $@

.PHONY: bash-completion
bash-completion:$(BASHCOMPLETION)_$(ARCH).deb
$(BASHCOMPLETION): $(SPREZZ)/bash-completion/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf bash-completion-$(bash-completion_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: bc
bc:$(BC)_$(ARCH).deb
$(BC): $(SPREZZ)/bc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf bc-$(bc_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: bcfg2
bcfg2:$(BCFG2)_$(ARCH).deb
$(BCFG2): $(SPREZZ)/bcfg2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bcfg2-$(bcfg2_UPVER).tar.gz $(TARARGS) $@

.PHONY: beets
beets:$(BEETS)_$(ARCH).deb
$(BEETS): $(SPREZZ)/beets/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf $(BEETS).orig.tar.gz $(TARARGS) $@

.PHONY: bind9
bind9:$(BIND9)_$(ARCH).deb
$(BIND9): $(SPREZZ)/bind9/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bind-$(bind9_UPVER).tar.gz $(TARARGS) $@

.PHONY: binutils
binutils:$(BINUTILS)_$(ARCH).deb
$(BINUTILS): $(SPREZZ)/binutils/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf binutils-$(binutils_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: binutils-mingw-w64
binutils-mingw-w64:$(BINUTILSMINGWW64)_$(ARCH).deb
$(BINUTILSMINGWW64): $(SPREZZ)/binutils-mingw-w64/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf binutils-mingw-w64_$(binutils-mingw-w64_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: binwalk
binwalk:$(BINWALK)_$(ARCH).deb
$(BINWALK): $(SPREZZ)/binwalk/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf binwalk-$(binwalk_UPVER).tar.gz $(TARARGS) $@

.PHONY: biosdevname
biosdevname:$(BIOSDEVNAME)_$(ARCH).deb
$(BIOSDEVNAME): $(SPREZZ)/biosdevname/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf biosdevname-$(biosdevname_UPVER).tar.gz $(TARARGS) $@

.PHONY: bison
bison:$(BISON)_$(ARCH).deb
$(BISON): $(SPREZZ)/bison/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bison-$(bison_UPVER).tar.gz $(TARARGS) $@

.PHONY: bitcoin
bitcoin:$(BITCOIN)_$(ARCH).deb
$(BITCOIN): $(SPREZZ)/bitcoin/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bitcoin_$(bitcoin_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: linux86
linux86:$(LINUX86)_$(ARCH).deb
$(LINUX86): $(SPREZZ)/linux86/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bin86-$(linux86_UPVER).tar.gz $(TARARGS) $@

.PHONY: bsnes
bsnes:$(BSNES)_$(ARCH).deb
$(BSNES): $(SPREZZ)/bsnes/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bsnes-$(bsnes_UPVER).tar.gz $(TARARGS) $@

.PHONY: bwm-ng
bwm-ng:$(BWMNG)_$(ARCH).deb
$(BWMNG): $(SPREZZ)/bwm-ng/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bwm-ng-$(bwm-ng_UPVER).tar.gz $(TARARGS) $@

.PHONY: byobu
byobu:$(BYOBU)_$(ARCH).deb
$(BYOBU): $(SPREZZ)/byobu/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf byobu_$(byobu_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: flex
flex:$(FLEX)_$(ARCH).deb
$(FLEX): $(SPREZZ)/flex/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf flex-$(flex_UPVER).tar.gz $(TARARGS) $@

.PHONY: flexc
flexc:$(FLEXC)_$(ARCH).deb
$(FLEXC): $(SPREZZ)/flexc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf flexc++-$(flexc_UPVER).tar.gz $(TARARGS) $@

.PHONY: flickcurl
flickcurl:$(FLICKCURL)_$(ARCH).deb
$(FLICKCURL): $(SPREZZ)/flickcurl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf flickcurl-$(flickcurl_UPVER).tar.gz $(TARARGS) $@

.PHONY: flickrbackup
flickrbackup:$(FLICKRBACKUP)_$(ARCH).deb
$(FLICKRBACKUP): $(SPREZZ)/flickrbackup/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf FlickrBackup-$(flickrbackup_UPVER).tar.gz $(TARARGS) $@

.PHONY: fstransform
fstransform:$(FSTRANSFORM)_$(ARCH).deb
$(FSTRANSFORM): $(SPREZZ)/fstransform/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf fstransform-$(fstransform_UPVER)-src.tar.bz2 $(TARARGS) $@

.PHONY: ftgl
ftgl:$(FTGL)_$(ARCH).deb
$(FTGL): $(SPREZZ)/ftgl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ftgl-$(ftgl_UPVER).tar.gz $(TARARGS) $@

.PHONY: fwknop
fwknop:$(FWKNOP)_$(ARCH).deb
$(FWKNOP): $(SPREZZ)/fwknop/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf fwknop-$(fwknop_UPVER).tar.gz $(TARARGS) $@

.PHONY: blas
blas:$(BLAS)_$(ARCH).deb
$(BLAS): $(SPREZZ)/blas/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf blas-$(blas_UPVER).tar.gz $(TARARGS) $@

.PHONY: blender
blender:$(BLENDER)_$(ARCH).deb
$(BLENDER): $(SPREZZ)/blender/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf blender-$(blender_UPVER).tar.gz $(TARARGS) $@

.PHONY: bless
bless:$(BLESS)_$(ARCH).deb
$(BLESS): $(SPREZZ)/bless/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bless-$(bless_UPVER).tar.gz $(TARARGS) $@

.PHONY: bluez
bluez:$(BLUEZ)_$(ARCH).deb
$(BLUEZ): $(SPREZZ)/bluez/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bluez-$(bluez_UPVER).tar.gz $(TARARGS) $@

.PHONY: boost1.50
boost1.50:$(boost1.50150)_$(ARCH).deb
$(boost1.50150): $(SPREZZ)/boost1.50/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	{ cd $@ && TARBALL=`uscan --no-symlink --force-download --download-current-version --dehs | xmlstarlet sel -t -v //target` && \
	  cd - && ln -sf $$TARBALL boost-build_2.0.m10.orig.tar.bz2 && tar xjvf $$TARBALL $(TARARGS) $@ ; }

.PHONY: boost1.52
boost1.52:$(BOOST1.52)_$(ARCH).deb
$(BOOST1.52): $(SPREZZ)/boost1.52/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf boost1.52_$(boost1.52_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: bowtie2
bowtie2:$(BOWTIE2)_$(ARCH).deb
$(BOWTIE2): $(SPREZZ)/bowtie2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf bowtie2-$(bowtie2_UPVER).tar.gz $(TARARGS) $@

.PHONY: brasero
brasero:$(BRASERO)_$(ARCH).deb
$(BRASERO): $(SPREZZ)/brasero/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf brasero-$(brasero_UPVER).tar.xz $(TARARGS) $@

.PHONY: bustle
bustle:$(BUSTLE)_$(ARCH).deb
$(BUSTLE): $(SPREZZ)/bustle/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bustle-$(bustle_UPVER).tar.gz $(TARARGS) $@

.PHONY: busybox
busybox:$(BUSYBOX)_$(ARCH).deb
$(BUSYBOX): $(SPREZZ)/busybox/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf busybox-$(busybox_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: bzip2
bzip2:$(BZIP2)_$(ARCH).deb
$(BZIP2): $(SPREZZ)/bzip2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bzip2-$(bzip2_UPVER).tar.gz $(TARARGS) $@

.PHONY: bzflag
bzflag:$(BZFLAG)_$(ARCH).deb
$(BZFLAG): $(SPREZZ)/bzflag/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bzflag-$(bzflag_UPVER).tar.gz $(TARARGS) $@

.PHONY: c-icap
c-icap:$(CICAP)_$(ARCH).deb
$(CICAP): $(SPREZZ)/c-icap/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf c_icap-$(c-icap_UPVER).tar.gz $(TARARGS) $@

.PHONY: c2esp
c2esp:$(C2ESP)_$(ARCH).deb
$(C2ESP): $(SPREZZ)/c2esp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf c2esp$(c2esp_UPVER).tar.gz $(TARARGS) $@

.PHONY: airport-utils
airport-utils:$(AIRPORTUTILS)_$(ARCH).deb
$(AIRPORTUTILS): $(SPREZZ)/airport-utils/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf airport-utils-$(airport-utils_UPVER).tar.xz $(TARARGS) $@

.PHONY: arpack
arpack:$(ARPACK)_$(ARCH).deb
$(ARPACK): $(SPREZZ)/arpack/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf arpack-$(arpack_UPVER).tar.gz $(TARARGS) $@

.PHONY: cairo
cairo:$(CAIRO)_$(ARCH).deb
$(CAIRO): $(SPREZZ)/cairo/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf cairo-$(cairo_UPVER).tar.xz $(TARARGS) $@

.PHONY: cairomm
cairomm:$(CAIROMM)_$(ARCH).deb
$(CAIROMM): $(SPREZZ)/cairomm/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cairomm-$(cairomm_UPVER).tar.gz $(TARARGS) $@

.PHONY: cairo-dock
cairo-dock:$(CAIRODOCK)_$(ARCH).deb
$(CAIRODOCK): $(SPREZZ)/cairo-dock/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cairo-dock-$(cairo-dock_UPVER).tar.gz $(TARARGS) $@

.PHONY: cairo-dock-plug-ins
cairo-dock-plug-ins:$(CAIRODOCKPLUGINS)_$(ARCH).deb
$(CAIRODOCKPLUGINS): $(SPREZZ)/cairo-dock-plug-ins/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf cairo-dock-plugins-$(cairo-dock-plug-ins_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: cccc
cccc:$(CCCC)_$(ARCH).deb
$(CCCC): $(SPREZZ)/cccc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cccc-$(cccc_UPVER).tar.gz $(TARARGS) $@

.PHONY: cclive
cclive:$(CCLIVE)_$(ARCH).deb
$(CCLIVE): $(SPREZZ)/cclive/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf cclive-$(cclive_UPVER).tar.xz $(TARARGS) $@

.PHONY: cdparanoia
cdparanoia:$(CDPARANOIA)_$(ARCH).deb
$(CDPARANOIA): $(SPREZZ)/cdparanoia/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cdparanoia_$(cdparanoia_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: cdrdao
cdrdao:$(CDRDAO)_$(ARCH).deb
$(CDRDAO): $(SPREZZ)/cdrdao/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf cdrdao-$(cdrdao_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: cdrkit
cdrkit:$(CDRKIT)_$(ARCH).deb
$(CDRKIT): $(SPREZZ)/cdrkit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cdrkit-$(cdrkit_UPVER).tar.gz $(TARARGS) $@

.PHONY: libcanberra
libcanberra:$(LIBCANBERRA)_$(ARCH).deb
$(LIBCANBERRA): $(SPREZZ)/libcanberra/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libcanberra-$(libcanberra_UPVER).tar.xz $(TARARGS) $@

.PHONY: libcdio
libcdio:$(LIBCDIO)_$(ARCH).deb
$(LIBCDIO): $(SPREZZ)/libcdio/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libcdio-$(libcdio_UPVER).tar.gz $(TARARGS) $@

.PHONY: libcdk5
libcdk5:$(LIBCDK5)_$(ARCH).deb
$(LIBCDK5): $(SPREZZ)/libcdk5/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cdk.tar.gz $(TARARGS) $@

.PHONY: libcdr
libcdr:$(LIBCDR)_$(ARCH).deb
$(LIBCDR): $(SPREZZ)/libcdr/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libcdr-$(libcdr_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libcec
libcec:$(LIBCEC)_$(ARCH).deb
$(LIBCEC): $(SPREZZ)/libcec/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libcec-$(libcec_UPVER).tar.gz $(TARARGS) $@
	rm -rf $@/debian
	cp -r $(<D) $@/

.PHONY: libchamplain
libchamplain:$(LIBCHAMPLAIN)_$(ARCH).deb
$(LIBCHAMPLAIN): $(SPREZZ)/libchamplain/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libchamplain-$(libchamplain_UPVER).tar.xz $(TARARGS) $@

.PHONY: libcroco
libcroco:$(LIBCROCO)_$(ARCH).deb
$(LIBCROCO): $(SPREZZ)/libcroco/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libcroco-$(libcroco_UPVER).tar.xz $(TARARGS) $@

.PHONY: libcryptui
libcryptui:$(LIBCRYPTUI)_$(ARCH).deb
$(LIBCRYPTUI): $(SPREZZ)/libcryptui/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libcryptui-$(libcryptui_UPVER).tar.xz $(TARARGS) $@

.PHONY: libcrypt-ssleay-perl
libcrypt-ssleay-perl:$(LIBCRYPTSSLEAYPERL)_$(ARCH).deb
$(LIBCRYPTSSLEAYPERL): $(SPREZZ)/libcrypt-ssleay-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Crypt-SSLeay-$(libcrypt-ssleay-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: lsof
lsof:$(LSOF)_$(ARCH).deb
$(LSOF): $(SPREZZ)/lsof/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf lsof_$(lsof_UPVER).tar.gz $(TARARGS) $@

.PHONY: lsscsi
lsscsi:$(LSSCSI)_$(ARCH).deb
$(LSSCSI): $(SPREZZ)/lsscsi/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf lsscsi-$(lsscsi_UPVER).tgz $(TARARGS) $@

.PHONY: lshw
lshw:$(LSHW)_$(ARCH).deb
$(LSHW): $(SPREZZ)/lshw/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf lshw-B.$(lshw_UPVER).tar.gz $(TARARGS) $@

.PHONY: calibre
calibre:$(CALIBRE)_$(ARCH).deb
$(CALIBRE): $(SPREZZ)/calibre/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf calibre-$(calibre_UPVER).tar.xz $(TARARGS) $@

.PHONY: celestia-gnome
celestia-gnome:$(CELESTIAGNOME)_$(ARCH).deb
$(CELESTIAGNOME): $(SPREZZ)/celestia-gnome/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf celestia-gnome-$(celestia-gnome_UPVER).tar.gz $(TARARGS) $@

.PHONY: ceph
ceph:$(CEPH)_$(ARCH).deb
$(CEPH): $(SPREZZ)/ceph/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf ceph-$(ceph_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: check
check:$(CHECK)_$(ARCH).deb
$(CHECK): $(SPREZZ)/check/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf check-$(check_UPVER).tar.gz $(TARARGS) $@

.PHONY: cmake
cmake:$(CMAKE)_$(ARCH).deb
$(CMAKE): $(SPREZZ)/cmake/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cmake-$(cmake_UPVER).tar.gz $(TARARGS) $@

.PHONY: cmtk
cmtk:$(CMTK)_$(ARCH).deb
$(CMTK): $(SPREZZ)/cmtk/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf CMTK-$(cmtk_UPVER)-Source.tar.gz $(TARARGS) $@

.PHONY: collectd
collectd:$(COLLECTD)_$(ARCH).deb
$(COLLECTD): $(SPREZZ)/collectd/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf collectd-$(collectd_UPVER).tar.gz $(TARARGS) $@

.PHONY: colormake
colormake:$(COLORMAKE)_$(ARCH).deb
$(COLORMAKE): $(SPREZZ)/colormake/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf colormake-$(colormake_UPVER).tar.gz $(TARARGS) $@

.PHONY: command-not-found
command-not-found:$(COMMANDNOTFOUND)_$(ARCH).deb
$(COMMANDNOTFOUND): $(SPREZZ)/command-not-found/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf command-not-found_$(command-not-found_UPVER).orig.tar.gz $(TARARGS) $@
	rm -rf $@/debian
	cp -r $(<D) $@/

.PHONY: cpptest
cpptest:$(CPPTEST)_$(ARCH).deb
$(CPPTEST): $(SPREZZ)/cpptest/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cpptest-$(cpptest_UPVER).tar.gz $(TARARGS) $@

.PHONY: cwidget
cwidget:$(CWIDGET)_$(ARCH).deb
$(CWIDGET): $(SPREZZ)/cwidget/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cwidget-$(cwidget_UPVER).tar.gz $(TARARGS) $@

.PHONY: taglib
taglib:$(TAGLIB)_$(ARCH).deb
$(TAGLIB): $(SPREZZ)/taglib/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf taglib-$(taglib_UPVER).tar.gz $(TARARGS) $@

.PHONY: talloc
talloc:$(TALLOC)_$(ARCH).deb
$(TALLOC): $(SPREZZ)/talloc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf talloc-$(talloc_UPVER).tar.gz $(TARARGS) $@

.PHONY: tar
tar:$(TAR)_$(ARCH).deb
$(TAR): $(SPREZZ)/tar/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf tar-$(tar_UPVER).tar.xz $(TARARGS) $@

.PHONY: tcpdump
tcpdump:$(TCPDUMP)_$(ARCH).deb
$(TCPDUMP): $(SPREZZ)/tcpdump/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tcpdump-$(tcpdump_UPVER).tar.gz $(TARARGS) $@

.PHONY: tmux
tmux:$(TMUX)_$(ARCH).deb
$(TMUX): $(SPREZZ)/tmux/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tmux-$(tmux_UPVER).tar.gz $(TARARGS) $@

.PHONY: tslib
tslib:$(TSLIB)_$(ARCH).deb
$(TSLIB): $(SPREZZ)/tslib/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf tslib-$(tslib_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: cpufrequtils
cpufrequtils:$(CPUFREQUTILS)_$(ARCH).deb
$(CPUFREQUTILS): $(SPREZZ)/cpufrequtils/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cpufrequtils-$(cpufrequtils_UPVER).tar.gz $(TARARGS) $@

.PHONY: terminology
terminology:$(TERMINOLOGY)_$(ARCH).deb
$(TERMINOLOGY): $(SPREZZ)/terminology/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf terminology-$(terminology_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: ttf-dejavu
ttf-dejavu:$(TTFDEJAVU)_$(ARCH).deb
$(TTFDEJAVU): $(SPREZZ)/ttf-dejavu/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf dejavu-fonts-$(ttf-dejavu_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: ttfautohint
ttfautohint:$(TTFAUTOHINT)_$(ARCH).deb
$(TTFAUTOHINT): $(SPREZZ)/ttfautohint/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ttfautohint-$(ttfautohint_UPVER).tar.gz $(TARARGS) $@

.PHONY: clang
clang:$(CLANG)_$(ARCH).deb
$(CLANG): $(SPREZZ)/clang/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf clang-$(clang_UPVER).src.tar.gz $(TARARGS) $@

.PHONY: cliquer
cliquer:$(CLIQUER)_$(ARCH).deb
$(CLIQUER): $(SPREZZ)/cliquer/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cliquer-$(cliquer_UPVER).tar.gz $(TARARGS) $@

.PHONY: cloud-init
cloud-init:$(CLOUDINIT)_$(ARCH).deb
$(CLOUDINIT): $(SPREZZ)/cloud-init/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cloud-init-$(cloud-init_UPVER).tar.gz $(TARARGS) $@

.PHONY: clucene-core
clucene-core:$(CLUCENECORE)_$(ARCH).deb
$(CLUCENECORE): $(SPREZZ)/clucene-core/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf clucene-core-$(clucene-core_UPVER).tar.gz $(TARARGS) $@

.PHONY: clutter
clutter:$(CLUTTER)_$(ARCH).deb
$(CLUTTER): $(SPREZZ)/clutter/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf clutter-1.0-$(clutter_UPVER).tar.xz $(TARARGS) $@

.PHONY: clutter-gst
clutter-gst:$(CLUTTERGST)_$(ARCH).deb
$(CLUTTERGST): $(SPREZZ)/clutter-gst/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf clutter-gst-$(clutter-gst_UPVER).tar.xz $(TARARGS) $@

.PHONY: clutter-gtk
clutter-gtk:$(CLUTTERGTK)_$(ARCH).deb
$(CLUTTERGTK): $(SPREZZ)/clutter-gtk/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf clutter-gtk-$(clutter-gtk_UPVER).tar.xz $(TARARGS) $@

.PHONY: codeblocks
codeblocks:$(CODEBLOCKS)_$(ARCH).deb
$(CODEBLOCKS): $(SPREZZ)/codeblocks/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf codeblocks_$(codeblocks_UPVER).tar.gz $(TARARGS) $@

.PHONY: cogl
cogl:$(COGL)_$(ARCH).deb
$(COGL): $(SPREZZ)/cogl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf cogl-$(cogl_UPVER).tar.xz $(TARARGS) $@

.PHONY: colord
colord:$(COLORD)_$(ARCH).deb
$(COLORD): $(SPREZZ)/colord/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf colord-$(colord_UPVER).tar.xz $(TARARGS) $@

.PHONY: colord-gtk
colord-gtk:$(COLORDGTK)_$(ARCH).deb
$(COLORDGTK): $(SPREZZ)/colord-gtk/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf colord-gtk-$(colord-gtk_UPVER).tar.xz $(TARARGS) $@

.PHONY: ccsm
ccsm:$(CCSM)_$(ARCH).deb
$(CCSM): $(SPREZZ)/ccsm/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf simple-$(CCSM).tar.bz2 $(TARARGS) $@

.PHONY: compizconfig-python
compizconfig-python:$(COMPIZCONFIGPYTHON)_$(ARCH).deb
$(COMPIZCONFIGPYTHON): $(SPREZZ)/compizconfig-python/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf compizconfig-python-$(compizconfig-python_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: compizconfig-settings-manager
compizconfig-settings-manager:$(COMPIZCONFIGSETTINGSMANAGER)_$(ARCH).deb
$(COMPIZCONFIGSETTINGSMANAGER): $(SPREZZ)/compizconfig-settings-manager/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf ccsm-$(compizconfig-settings-manager_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libcompizconfig
libcompizconfig:$(LIBCOMPIZCONFIG)_$(ARCH).deb
$(LIBCOMPIZCONFIG): $(SPREZZ)/libcompizconfig/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libcompizconfig-$(libcompizconfig_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: compiz
compiz:$(COMPIZ)_$(ARCH).deb
$(COMPIZ): $(SPREZZ)/compiz/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf compiz-$(compiz_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: compiz-boxmenu
compiz-boxmenu:$(COMPIZBOXMENU)_$(ARCH).deb
$(COMPIZBOXMENU): $(SPREZZ)/compiz-boxmenu/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf compiz-boxmenu_-_$(compiz-boxmenu_UPVER).tar.gz $(TARARGS) $@

.PHONY: conky
conky:$(CONKY)_$(ARCH).deb
$(CONKY): $(SPREZZ)/conky/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf conky-$(conky_UPVER).tar.gz $(TARARGS) $@

.PHONY: conntrack
conntrack:$(CONNTRACK)_$(ARCH).deb
$(CONNTRACK): $(SPREZZ)/conntrack/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf conntrack-tools-$(conntrack_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: conpalette
conpalette:$(CONPALETTE)_$(ARCH).deb
$(CONPALETTE): $(SPREZZ)/conpalette/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf conpalette_$(conpalette_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: console-tools
console-tools:$(CONSOLETOOLS)_$(ARCH).deb
$(CONSOLETOOLS): $(SPREZZ)/console-tools/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf console-tools-$(console-tools_UPVER).tar.gz $(TARARGS) $@

.PHONY: consolekit
consolekit:$(CONSOLEKIT)_$(ARCH).deb
$(CONSOLEKIT): $(SPREZZ)/consolekit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf ConsoleKit-$(consolekit_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: coreutils
coreutils:$(COREUTILS)_$(ARCH).deb
$(COREUTILS): $(SPREZZ)/coreutils/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf coreutils-$(coreutils_UPVER).tar.xz $(TARARGS) $@

.PHONY: cramfs
cramfs:$(CRAMFS)_$(ARCH).deb
$(CRAMFS): $(SPREZZ)/cramfs/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cramfs-$(cramfs_UPVER).tar.gz $(TARARGS) $@

.PHONY: burg
burg:$(BURG)_$(ARCH).deb
$(BURG): $(SPREZZ)/burg/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf burg-src-$(burg_UPVER).tar.gz $(TARARGS) $@

.PHONY: cron
cron:$(CRON)_$(ARCH).deb
$(CRON): $(SPREZZ)/cron/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	unshar ../cron-$(cron_UPVER).shar $@

.PHONY: csstidy
csstidy:$(CSSTIDY)_$(ARCH).deb
$(CSSTIDY): $(SPREZZ)/csstidy/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf csstidy-$(csstidy_UPVER)-source.tar.gz $(TARARGS) $@

.PHONY: cups
cups:$(CUPS)_$(ARCH).deb
$(CUPS): $(SPREZZ)/cups/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf cups-$(cups_UPVER)-source.tar.bz2 $(TARARGS) $@

.PHONY: cups-filters
cups-filters:$(CUPSFILTERS)_$(ARCH).deb
$(CUPSFILTERS): $(SPREZZ)/cups-filters/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf cups-filters-$(cups-filters_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: curl
curl:$(CURL)_$(ARCH).deb
$(CURL): $(SPREZZ)/curl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf curl-$(curl_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: cyrus-sasl
cyrus-sasl:$(CYRUSSASL)_$(ARCH).deb
$(CYRUSSASL): $(SPREZZ)/cyrus-sasl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cyrus-sasl-$(cyrus-sasl_UPVER).tar.gz $(TARARGS) $@

.PHONY: d-conf
d-conf:$(DCONF)_$(ARCH).deb
$(DCONF): $(SPREZZ)/d-conf/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf dconf-$(d-conf_UPVER).tar.xz $(TARARGS) $@

.PHONY: darktable
darktable:$(DARKTABLE)_$(ARCH).deb
$(DARKTABLE): $(SPREZZ)/darktable/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf darktable-$(darktable_UPVER).tar.gz $(TARARGS) $@

.PHONY: dash
dash:$(DASH)_$(ARCH).deb
$(DASH): $(SPREZZ)/dash/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dash-$(dash_UPVER).tar.gz $(TARARGS) $@

.PHONY: db5.1
db5.1:$(DB5.1)_$(ARCH).deb
$(DB5.1): $(SPREZZ)/db5.1/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf db5.1-$(db5.1_UPVER).tar.gz $(TARARGS) $@

.PHONY: db5.3
db5.3:$(DB5.3)_$(ARCH).deb
$(DB5.3): $(SPREZZ)/db5.3/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf db5.3-$(db5.3_UPVER).tar.gz $(TARARGS) $@

.PHONY: db-defaults
db-defaults:$(DBDEFAULTS)_$(ARCH).deb
$(DBDEFAULTS): $(SPREZZ)/db-defaults/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf db-defaults-$(db-defaults_UPVER).tar.gz $(TARARGS) $@

.PHONY: dbus
dbus:$(DBUS)_$(ARCH).deb
$(DBUS): $(SPREZZ)/dbus/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dbus-$(dbus_UPVER).tar.gz $(TARARGS) $@

.PHONY: dbus-glib
dbus-glib:$(DBUSGLIB)_$(ARCH).deb
$(DBUSGLIB): $(SPREZZ)/dbus-glib/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dbus-glib-$(dbus-glib_UPVER).tar.gz $(TARARGS) $@

.PHONY: dbus-python
dbus-python:$(DBUSPYTHON)_$(ARCH).deb
$(DBUSPYTHON): $(SPREZZ)/dbus-python/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dbus-python-$(dbus-python_UPVER).tar.gz $(TARARGS) $@

.PHONY: dcmtk
dcmtk:$(DCMTK)_$(ARCH).deb
$(DCMTK): $(SPREZZ)/dcmtk/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dcmtk-$(dcmtk_UPVER).tar.gz $(TARARGS) $@

.PHONY: dcraw
dcraw:$(DCRAW)_$(ARCH).deb
$(DCRAW): $(SPREZZ)/dcraw/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dcraw-$(dcraw_UPVER).tar.gz $(TARARGS) $@

.PHONY: deluge
deluge:$(DELUGE)_$(ARCH).deb
$(DELUGE): $(SPREZZ)/deluge/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf deluge-$(deluge_UPVER).tar.gz $(TARARGS) $@

.PHONY: devhelp
devhelp:$(DEVHELP)_$(ARCH).deb
$(DEVHELP): $(SPREZZ)/devhelp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf devhelp-$(devhelp_UPVER).tar.xz $(TARARGS) $@

.PHONY: dia
dia:$(DIA)_$(ARCH).deb
$(DIA): $(SPREZZ)/dia/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf dia-$(dia_UPVER).tar.xz $(TARARGS) $@

.PHONY: dialog
dialog:$(DIALOG)_$(ARCH).deb
$(DIALOG): $(SPREZZ)/dialog/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dialog-$(dialog_UPVER).tar.gz $(TARARGS) $@

.PHONY: diffstat
diffstat:$(DIFFSTAT)_$(ARCH).deb
$(DIFFSTAT): $(SPREZZ)/diffstat/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf diffstat-$(diffstat_UPVER).tgz $(TARARGS) $@

.PHONY: dirac
dirac:$(DIRAC)_$(ARCH).deb
$(DIRAC): $(SPREZZ)/dirac/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dirac-$(dirac_UPVER).tar.gz $(TARARGS) $@

.PHONY: directfb
directfb:$(DIRECTFB)_$(ARCH).deb
$(DIRECTFB): $(SPREZZ)/directfb/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf DirectFB-$(directfb_UPVER).tar.gz $(TARARGS) $@

.PHONY: dispcalgui
dispcalgui:$(DISPCALGUI)_$(ARCH).deb
$(DISPCALGUI): $(SPREZZ)/dispcalgui/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dispcalGUI-$(dispcalgui_UPVER).tar.gz $(TARARGS) $@

.PHONY: distribute
distribute:$(DISTRIBUTE)_$(ARCH).deb
$(DISTRIBUTE): $(SPREZZ)/distribute/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf distribute-$(distribute_UPVER).tar.gz $(TARARGS) $@

.PHONY: djmount
djmount:$(DJMOUNT)_$(ARCH).deb
$(DJMOUNT): $(SPREZZ)/djmount/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf djmount-$(djmount_UPVER).tar.gz $(TARARGS) $@

.PHONY: dnsmasq
dnsmasq:$(DNSMASQ)_$(ARCH).deb
$(DNSMASQ): $(SPREZZ)/dnsmasq/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf dnsmasq-$(dnsmasq_UPVER).tar.xz $(TARARGS) $@

.PHONY: docbook
docbook:$(DOCBOOK)_$(ARCH).deb
$(DOCBOOK): $(SPREZZ)/docbook/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf docbook-$(docbook_UPVER).tar.gz $(TARARGS) $@

.PHONY: docbook2x
docbook2x:$(DOCBOOK2X)_$(ARCH).deb
$(DOCBOOK2X): $(SPREZZ)/docbook2x/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf docbook2X-$(docbook2x_UPVER).tar.gz $(TARARGS) $@

.PHONY: docbook-xml
docbook-xml:$(DOCBOOKXML)_$(ARCH).deb
$(DOCBOOKXML): $(SPREZZ)/docbook-xml/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf docbook-xml-$(docbook-xml_UPVER).tar.gz $(TARARGS) $@

.PHONY: dosbox
dosbox:$(DOSBOX)_$(ARCH).deb
$(DOSBOX): $(SPREZZ)/dosbox/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf dosbox-$(dosbox_UPVER).tar.gz $(TARARGS) $@

.PHONY: dosfstools
dosfstools:$(DOSFSTOOLS)_$(ARCH).deb
$(DOSFSTOOLS): $(SPREZZ)/dosfstools/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf dosfstools-$(dosfstools_UPVER).tar.gz $(TARARGS) $@

.PHONY: dracut
dracut:$(DRACUT)_$(ARCH).deb
$(DRACUT): $(SPREZZ)/dracut/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf dracut-$(dracut_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: dri2proto
dri2proto:$(DRI2PROTO)_$(ARCH).deb
$(DRI2PROTO): $(SPREZZ)/dri2proto/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf dri2proto-$(dri2proto_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: dsniff
dsniff:$(DSNIFF)_$(ARCH).deb
$(DSNIFF): $(SPREZZ)/dsniff/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dsniff-$(dsniff_UPVER).tar.gz $(TARARGS) $@

.PHONY: eglibc
eglibc:$(EGLIBC)_$(ARCH).deb
$(EGLIBC): $(SPREZZ)/eglibc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf glibc-$(eglibc_UPVER).tar.xz $(TARARGS) $@

.PHONY: elfutils
elfutils:$(ELFUTILS)_$(ARCH).deb
$(ELFUTILS): $(SPREZZ)/elfutils/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf elfutils-$(elfutils_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: emerillon
emerillon:$(EMERILLON)_$(ARCH).deb
$(EMERILLON): $(SPREZZ)/emerillon/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf emerillon-$(emerillon_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: empathy
empathy:$(EMPATHY)_$(ARCH).deb
$(EMPATHY): $(SPREZZ)/empathy/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf empathy-$(empathy_UPVER).tar.xz $(TARARGS) $@

.PHONY: enca
enca:$(ENCA)_$(ARCH).deb
$(ENCA): $(SPREZZ)/enca/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf enca-$(enca_UPVER).tar.gz $(TARARGS) $@

.PHONY: enchant
enchant:$(ENCHANT)_$(ARCH).deb
$(ENCHANT): $(SPREZZ)/enchant/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf enchant-$(enchant_UPVER).tar.gz $(TARARGS) $@

.PHONY: enlightenment
enlightenment:$(ENLIGHTENMENT)_$(ARCH).deb
$(ENLIGHTENMENT): $(SPREZZ)/enlightenment/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf enlightenment-$(enlightenment_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: ekiga
ekiga:$(EKIGA)_$(ARCH).deb
$(EKIGA): $(SPREZZ)/ekiga/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf ekiga_$(ekiga_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: elinks
elinks:$(ELINKS)_$(ARCH).deb
$(ELINKS): $(SPREZZ)/elinks/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf elinks_$(elinks_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: eog
eog:$(EOG)_$(ARCH).deb
$(EOG): $(SPREZZ)/eog/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf eog_$(eog_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: eog-plugins
eog-plugins:$(EOGPLUGINS)_$(ARCH).deb
$(EOGPLUGINS): $(SPREZZ)/eog-plugins/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf eog-plugins_$(eog-plugins_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: ettercap
ettercap:$(ETTERCAP)_$(ARCH).deb
$(ETTERCAP): $(SPREZZ)/ettercap/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ettercap-$(ettercap_UPVER).tar.gz $(TARARGS) $@

.PHONY: f2fs-tools
f2fs-tools:$(F2FSTOOLS)_$(ARCH).deb
$(F2FSTOOLS): $(SPREZZ)/f2fs-tools/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf f2fs-tools-$(f2fs-tools_UPVER).tar.gz $(TARARGS) $@

.PHONY: faac
faac:$(FAAC)_$(ARCH).deb
$(FAAC): $(SPREZZ)/faac/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf faac-$(faac_UPVER).tar.gz $(TARARGS) $@

.PHONY: fakeroot
fakeroot:$(FAKEROOT)_$(ARCH).deb
$(FAKEROOT): $(SPREZZ)/fakeroot/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf fakeroot-$(fakeroot_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: farstream
farstream:$(FARSTREAM)_$(ARCH).deb
$(FARSTREAM): $(SPREZZ)/farstream/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf farstream-$(farstream_UPVER).tar.gz $(TARARGS) $@

.PHONY: fatrat
fatrat:$(FATRAT)_$(ARCH).deb
$(FATRAT): $(SPREZZ)/fatrat/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf fatrat_$(fatrat_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libtorrent-rasterbar
libtorrent-rasterbar:$(LIBTORRENTRASTERBAR)_$(ARCH).deb
$(LIBTORRENTRASTERBAR): $(SPREZZ)/libtorrent-rasterbar/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libtorrent-rasterbar-$(libtorrent-rasterbar_UPVER).tar.gz $(TARARGS) $@

.PHONY: fbset
fbset:$(FBSET)_$(ARCH).deb
$(FBSET): $(SPREZZ)/fbset/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf fbset-$(fbset_UPVER).tar.gz $(TARARGS) $@

.PHONY: fbterm
fbterm:$(FBTERM)_$(ARCH).deb
$(FBTERM): $(SPREZZ)/fbterm/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf nfbterm-$(fbterm_UPVER).tar.gz $(TARARGS) $@

.PHONY: festival
festival:$(FESTIVAL)_$(ARCH).deb
$(FESTIVAL): $(SPREZZ)/festival/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf festival-$(festival_UPVER)-release.tar.gz $(TARARGS) $@

.PHONY: field3d
field3d:$(FIELD3D)_$(ARCH).deb
$(FIELD3D): $(SPREZZ)/field3d/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libfield3d_$(field3d_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: file-roller
file-roller:$(FILEROLLER)_$(ARCH).deb
$(FILEROLLER): $(SPREZZ)/file-roller/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf file-roller-$(file-roller_UPVER).tar.xz $(TARARGS) $@

.PHONY: fftw3
fftw3:$(FFTW3)_$(ARCH).deb
$(FFTW3): $(SPREZZ)/fftw3/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf fftw-$(fftw3_UPVER).tar.gz $(TARARGS) $@

.PHONY: findutils
findutils:$(FINDUTILS)_$(ARCH).deb
$(FINDUTILS): $(SPREZZ)/findutils/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf findutils-$(findutils_UPVER).tar.gz $(TARARGS) $@

.PHONY: fontconfig
fontconfig:$(FONTCONFIG)_$(ARCH).deb
$(FONTCONFIG): $(SPREZZ)/fontconfig/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf fontconfig-$(fontconfig_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: fonts-awesome
fonts-awesome:$(FONTSAWESOME)_$(ARCH).deb
$(FONTSAWESOME): $(SPREZZ)/fonts-awesome/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf $(fonts-awesome_UPVER).tar.gz $(TARARGS) $@

.PHONY: fonts-cantarell
fonts-cantarell:$(FONTSCANTARELL)_$(ARCH).deb
$(FONTSCANTARELL): $(SPREZZ)/fonts-cantarell/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf cantarell-fonts-$(fonts-cantarell_UPVER).tar.xz $(TARARGS) $@

.PHONY: fonts-freefont
fonts-freefont:$(FONTSFREEFONT)_$(ARCH).deb
$(FONTSFREEFONT): $(SPREZZ)/fonts-freefont/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf freefont-src-$(fonts-freefont_UPVER).tar.gz $(TARARGS) $@

.PHONY: fonts-junicode
fonts-junicode:$(FONTSJUNICODE)_$(ARCH).deb
$(FONTSJUNICODE): $(SPREZZ)/fonts-junicode/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf junicode-$(fonts-junicode_UPVER).tar.gz $(TARARGS) $@

.PHONY: fonts-liberation
fonts-liberation:$(FONTSLIBERATION)_$(ARCH).deb
$(FONTSLIBERATION): $(SPREZZ)/fonts-liberation/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf liberation-fonts-$(fonts-liberation_UPVER).tar.gz $(TARARGS) $@

.PHONY: fonts-linuxlibertine
fonts-linuxlibertine:$(FONTSLINUXLIBERTINE)_$(ARCH).deb
$(FONTSLINUXLIBERTINE): $(SPREZZ)/fonts-linuxlibertine/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf fonts-linuxlibertine_$(fonts-linuxlibertine_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: fonts-sourcesanspro
fonts-sourcesanspro:$(FONTSADOBESOURCESANSPRO)_$(ARCH).deb
$(FONTSADOBESOURCESANSPRO): $(SPREZZ)/fonts-adobe-sourcesanspro/debian/changelog
	mkdir -p $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	unzip SourceSansPro_FontsOnly-$(fonts-adobe-sourcesanspro_UPVER).zip -d $@

.PHONY: fotoxx
fotoxx:$(FOTOXX)_$(ARCH).deb
$(FOTOXX): $(SPREZZ)/fotoxx/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf fotoxx-$(fotoxx_UPVER).tar.gz $(TARARGS) $@

.PHONY: fraqtive
fraqtive:$(FRAQTIVE)_$(ARCH).deb
$(FRAQTIVE): $(SPREZZ)/fraqtive/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf fraqtive-$(fraqtive_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: freeglut
freeglut:$(FREEGLUT)_$(ARCH).deb
$(FREEGLUT): $(SPREZZ)/freeglut/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf freeglut_$(freeglut_UPVER).orig.tar.gz $(TARARGS) $@

FREETYPEUP:=freetype-$(shell echo $(freetype_UPVER) | cut -d- -f1) \
	freetype-doc-$(shell echo $(freetype_UPVER) | cut -d- -f1) \
	ft2demos-$(shell echo $(freetype_UPVER) | cut -d- -f1)
FREETYPEORIG:=freetype_$(shell echo $(freetype_UPVER) | cut -d- -f1).orig.tar.gz

.PHONY: freetype
freetype:$(FREETYPE)_$(ARCH).deb
$(FREETYPE): $(SPREZZ)/freetype/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	cd $@ && for i in $(FREETYPEUP) ; do wget -nc -O$$i.tar.gz http://download.savannah.gnu.org/releases/freetype/$$i.tar.gz ; done
	#tar xzvf freetype-$(freetype_UPVER).tar.gz $(TARARGS) $@

.PHONY: fribidi
fribidi:$(FRIBIDI)_$(ARCH).deb
$(FRIBIDI): $(SPREZZ)/fribidi/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf fribidi-$(fribidi_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: fuse
fuse:$(FUSE)_$(ARCH).deb
$(FUSE): $(SPREZZ)/fuse/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf fuse-$(fuse_UPVER).tar.gz $(TARARGS) $@

.PHONY: fuse-umfuse-fat
fuse-umfuse-fat:$(FUSEUMFUSEFAT)_$(ARCH).deb
$(FUSEUMFUSEFAT): $(SPREZZ)/fuse-umfuse-fat/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf fuse-umfuse-fat-$(fuse-umfuse-fat_UPVER).tar.gz $(TARARGS) $@

.PHONY: unicode
unicode:$(UNICODE)_$(ARCH).deb
$(UNICODE): $(SPREZZ)/unicode/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf unicode_$(unicode_UPVER).tar.gz $(TARARGS) $@

.PHONY: unionfs-fuse
unionfs-fuse:$(UNIONFSFUSE)_$(ARCH).deb
$(UNIONFSFUSE): $(SPREZZ)/unionfs-fuse/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf unionfs-fuse-$(unionfs-fuse_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: unison
unison:$(UNISON)_$(ARCH).deb
$(UNISON): $(SPREZZ)/unison/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf unison-$(unison_UPVER).tar.gz $(TARARGS) $@

.PHONY: fuseiso
fuseiso:$(FUSEISO)_$(ARCH).deb
$(FUSEISO): $(SPREZZ)/fuseiso/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf fuseiso-$(fuseiso_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: game-music-emu
game-music-emu:$(GAMEMUSICEMU)_$(ARCH).deb
$(GAMEMUSICEMU): $(SPREZZ)/game-music-emu/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf game-music-emu-$(game-music-emu_UPVER).tbz2 $(TARARGS) $@

.PHONY: gamin
gamin:$(GAMIN)_$(ARCH).deb
$(GAMIN): $(SPREZZ)/gamin/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gamin-$(gamin_UPVER).tar.gz $(TARARGS) $@

.PHONY: ganglia
ganglia:$(GANGLIA)_$(ARCH).deb
$(GANGLIA): $(SPREZZ)/ganglia/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ganglia-$(ganglia_UPVER).tar.gz $(TARARGS) $@

.PHONY: gant
gant:$(GANT)_$(ARCH).deb
$(GANT): $(SPREZZ)/gant/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gant_src-$(gant_UPVER).tgz $(TARARGS) $@

.PHONY: gawk
gawk:$(GAWK)_$(ARCH).deb
$(GAWK): $(SPREZZ)/gawk/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gawk-$(gawk_UPVER).tar.gz $(TARARGS) $@

.PHONY: gcc-4.7
gcc-4.7:$(GCC4.7)_$(ARCH).deb
$(GCC4.7): $(SPREZZ)/gcc-4.7/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf gcc-$(gcc-4.7_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: gcr
gcr:$(GCR)_$(ARCH).deb
$(GCR): $(SPREZZ)/gcr/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gcr-$(gcr_UPVER).tar.xz $(TARARGS) $@

.PHONY: cryptsetup
cryptsetup:$(CRYPTSETUP)_$(ARCH).deb
$(CRYPTSETUP): $(SPREZZ)/cryptsetup/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf cryptsetup-$(cryptsetup_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: gcrypt
gcrypt:$(GCRYPT)_$(ARCH).deb
$(GCRYPT): $(SPREZZ)/gcrypt/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libgcrypt11_$(gcrypt_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: gcstar
gcstar:$(GCSTAR)_$(ARCH).deb
$(GCSTAR): $(SPREZZ)/gcstar/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gcstar-$(gcstar_UPVER).tar.gz $(TARARGS) $@

.PHONY: gdbm
gdbm:$(GDBM)_$(ARCH).deb
$(GDBM): $(SPREZZ)/gdbm/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gdbm-$(gdbm_UPVER).tar.gz $(TARARGS) $@

.PHONY: gdisk
gdisk:$(GDISK)_$(ARCH).deb
$(GDISK): $(SPREZZ)/gdisk/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gptfdisk-$(gdisk_UPVER).tar.gz $(TARARGS) $@

.PHONY: gdk-pixbuf
gdk-pixbuf:$(GDKPIXBUF)_$(ARCH).deb
$(GDKPIXBUF): $(SPREZZ)/gdk-pixbuf/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gdk-pixbuf-$(gdk-pixbuf_UPVER).tar.xz $(TARARGS) $@

.PHONY: gdl
gdl:$(GDL)_$(ARCH).deb
$(GDL): $(SPREZZ)/gdl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gdl-$(gdl_UPVER).tar.xz $(TARARGS) $@

.PHONY: gdm3
gdm3:$(GDM3)_$(ARCH).deb
$(GDM3): $(SPREZZ)/gdm3/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gdm-$(gdm3_UPVER).tar.xz $(TARARGS) $@

.PHONY: geary
geary:$(GEARY)_$(ARCH).deb
$(GEARY): $(SPREZZ)/geary/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf geary-$(geary_UPVER).tar.xz $(TARARGS) $@

.PHONY: gee
gee:$(GEE)_$(ARCH).deb
$(GEE): $(SPREZZ)/gee/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libgee-$(gee_UPVER).tar.xz $(TARARGS) $@

.PHONY: geocode-glib
geocode-glib:$(GEOCODEGLIB)_$(ARCH).deb
$(GEOCODEGLIB): $(SPREZZ)/geocode-glib/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf geocode-glib-$(geocode-glib_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: gettext
gettext:$(GETTEXT)_$(ARCH).deb
$(GETTEXT): $(SPREZZ)/gettext/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gettext-$(gettext_UPVER).tar.gz $(TARARGS) $@

.PHONY: ghex
ghex:$(GHEX)_$(ARCH).deb
$(GHEX): $(SPREZZ)/ghex/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf ghex-$(ghex_UPVER).tar.xz $(TARARGS) $@

.PHONY: ghostscript
ghostscript:$(GHOSTSCRIPT)_$(ARCH).deb
$(GHOSTSCRIPT): $(SPREZZ)/ghostscript/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ghostscript-$(ghostscript_UPVER).tar.gz $(TARARGS) $@

.PHONY: giflib
giflib:$(GIFLIB)_$(ARCH).deb
$(GIFLIB): $(SPREZZ)/giflib/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf giflib-$(giflib_UPVER).tar.gz $(TARARGS) $@

.PHONY: gimp
gimp:$(GIMP)_$(ARCH).deb
$(GIMP): $(SPREZZ)/gimp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf gimp_$(gimp_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: git
git:$(GIT)_$(ARCH).deb
$(GIT): $(SPREZZ)/git/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf git-$(git_UPVER).tar.gz $(TARARGS) $@

.PHONY: git-extras
git-extras:$(GITEXTRAS)_$(ARCH).deb
$(GITEXTRAS): $(SPREZZ)/git-extras/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf $(git-extras_UPVER).tar.gz $(TARARGS) $@

.PHONY: gjs
gjs:$(GJS)_$(ARCH).deb
$(GJS): $(SPREZZ)/gjs/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gjs-$(gjs_UPVER).tar.xz $(TARARGS) $@

.PHONY: glade
glade:$(GLADE)_$(ARCH).deb
$(GLADE): $(SPREZZ)/glade/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf glade-$(glade_UPVER).tar.xz $(TARARGS) $@

.PHONY: glances
glances:$(GLANCES)_$(ARCH).deb
$(GLANCES): $(SPREZZ)/glances/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf glances-$(glances_UPVER).tar.gz $(TARARGS) $@

.PHONY: glew
glew:$(GLEW)_$(ARCH).deb
$(GLEW): $(SPREZZ)/glew/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf glew-$(glew_UPVER).tgz $(TARARGS) $@

.PHONY: glib
glib:$(GLIB)_$(ARCH).deb
$(GLIB): $(SPREZZ)/glib/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf glib2.0-$(glib_UPVER).tar.xz $(TARARGS) $@

.PHONY: glibmm2.4
glibmm2.4:$(GLIBMM2.4)_$(ARCH).deb
$(GLIBMM2.4): $(SPREZZ)/glibmm2.4/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf glibmm-$(glibmm2.4_UPVER).tar.xz $(TARARGS) $@

.PHONY: glib-networking
glib-networking:$(GLIBNETWORKING)_$(ARCH).deb
$(GLIBNETWORKING): $(SPREZZ)/glib-networking/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf glib-networking-$(glib-networking_UPVER).tar.xz $(TARARGS) $@

.PHONY: gloox
gloox:$(GLOOX)_$(ARCH).deb
$(GLOOX): $(SPREZZ)/gloox/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf gloox-$(gloox_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: GLU
GLU:$(GLU)_$(ARCH).deb
$(GLU): $(SPREZZ)/GLU/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf glu_$(GLU_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: glui
glui:$(GLUI)_$(ARCH).deb
$(GLUI): $(SPREZZ)/glui/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf glui-$(glui_UPVER).tgz $(TARARGS) $@

.PHONY: glyr
glyr:$(GLYR)_$(ARCH).deb
$(GLYR): $(SPREZZ)/glyr/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf glyr-$(glyr_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: lmms
lmms:$(LMMS)_$(ARCH).deb
$(LMMS): $(SPREZZ)/lmms/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf lmms-$(lmms_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: lm-sensors
lm-sensors:$(LMSENSORS)_$(ARCH).deb
$(LMSENSORS): $(SPREZZ)/lm-sensors/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf lm_sensors-$(lm-sensors_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libgd2
libgd2:$(LIBGD2)_$(ARCH).deb
$(LIBGD2): $(SPREZZ)/libgd2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libgd2-$(libgd2_UPVER).tar.gz $(TARARGS) $@

.PHONY: libglade2
libglade2:$(LIBGLADE2)_$(ARCH).deb
$(LIBGLADE2): $(SPREZZ)/libglade2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libglade-$(libglade2_UPVER).tar.gz $(TARARGS) $@

.PHONY: libgnome
libgnome:$(LIBGNOME)_$(ARCH).deb
$(LIBGNOME): $(SPREZZ)/libgnome/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libgnome-$(libgnome_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libgnomecanvas
libgnomecanvas:$(LIBGNOMECANVAS)_$(ARCH).deb
$(LIBGNOMECANVAS): $(SPREZZ)/libgnomecanvas/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libgnomecanvas-$(libgnomecanvas_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libgnomecups
libgnomecups:$(LIBGNOMECUPS)_$(ARCH).deb
$(LIBGNOMECUPS): $(SPREZZ)/libgnomecups/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libgnomecups-$(libgnomecups_UPVER).tar.gz $(TARARGS) $@

.PHONY: libgnomeprint
libgnomeprint:$(LIBGNOMEPRINT)_$(ARCH).deb
$(LIBGNOMEPRINT): $(SPREZZ)/libgnomeprint/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libgnomeprint-$(libgnomeprint_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libgnome-keyring
libgnome-keyring:$(LIBGNOMEKEYRING)_$(ARCH).deb
$(LIBGNOMEKEYRING): $(SPREZZ)/libgnome-keyring/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libgnome-keyring_$(libgnome-keyring_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: gpgme1.0
gpgme1.0:$(GPGME1.0)_$(ARCH).deb
$(GPGME1.0): $(SPREZZ)/gpgme1.0/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gpgme1.0-$(gpgme1.0_UPVER).tar.gz $(TARARGS) $@

.PHONY: libgpg-error
libgpg-error:$(LIBGPGERROR)_$(ARCH).deb
$(LIBGPGERROR): $(SPREZZ)/libgpg-error/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libgpg-error-$(libgpg-error_UPVER).tar.gz $(TARARGS) $@

.PHONY: libgpod
libgpod:$(LIBGPOD)_$(ARCH).deb
$(LIBGPOD): $(SPREZZ)/libgpod/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libgpod-$(libgpod_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libgweather
libgweather:$(LIBGWEATHER)_$(ARCH).deb
$(LIBGWEATHER): $(SPREZZ)/libgweather/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libgweather-$(libgweather_UPVER).tar.xz $(TARARGS) $@

.PHONY: gwibber
gwibber:$(GWIBBER)_$(ARCH).deb
$(GWIBBER): $(SPREZZ)/gwibber/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gwibber-$(gwibber_UPVER).tar.gz $(TARARGS) $@

.PHONY: gnome-media
gnome-media:$(GNOMEMEDIA)_$(ARCH).deb
$(GNOMEMEDIA): $(SPREZZ)/gnome-media/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-media_$(gnome-media_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: gnome-menus
gnome-menus:$(GNOMEMENUS)_$(ARCH).deb
$(GNOMEMENUS): $(SPREZZ)/gnome-menus/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-menus_$(gnome-menus_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: gnome-online-accounts
gnome-online-accounts:$(GNOMEONLINEACCOUNTS)_$(ARCH).deb
$(GNOMEONLINEACCOUNTS): $(SPREZZ)/gnome-online-accounts/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-online-accounts_$(gnome-online-accounts_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: gnome-paint
gnome-paint:$(GNOMEPAINT)_$(ARCH).deb
$(GNOMEPAINT): $(SPREZZ)/gnome-paint/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gnome-paint-$(gnome-paint_UPVER).tar.gz $(TARARGS) $@

.PHONY: gnome-panel
gnome-panel:$(GNOMEPANEL)_$(ARCH).deb
$(GNOMEPANEL): $(SPREZZ)/gnome-panel/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-panel-$(gnome-panel_UPVER).tar.xz $(TARARGS) $@

.PHONY: gnome-photo-printer
gnome-photo-printer:$(GNOMEPHOTOPRINTER)_$(ARCH).deb
$(GNOMEPHOTOPRINTER): $(SPREZZ)/gnome-photo-printer/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-photo-printer-$(gnome-photo-printer_UPVER).tar.xz $(TARARGS) $@

.PHONY: gnome-power-manager
gnome-power-manager:$(GNOMEPOWERMANAGER)_$(ARCH).deb
$(GNOMEPOWERMANAGER): $(SPREZZ)/gnome-power-manager/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-power-manager-$(gnome-power-manager_UPVER).tar.xz $(TARARGS) $@

.PHONY: gnome-scan
gnome-scan:$(GNOMESCAN)_$(ARCH).deb
$(GNOMESCAN): $(SPREZZ)/gnome-scan/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf gnome-scan-$(gnome-scan_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: gnome-screenshot
gnome-screenshot:$(GNOMESCREENSHOT)_$(ARCH).deb
$(GNOMESCREENSHOT): $(SPREZZ)/gnome-screenshot/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-screenshot_$(gnome-screenshot_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: gnome-search-tool
gnome-search-tool:$(GNOMESEARCHTOOL)_$(ARCH).deb
$(GNOMESEARCHTOOL): $(SPREZZ)/gnome-search-tool/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-search-tool_$(gnome-search-tool_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: gnome-session
gnome-session:$(GNOMESESSION)_$(ARCH).deb
$(GNOMESESSION): $(SPREZZ)/gnome-session/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-session_$(gnome-session_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: gnome-settings-daemon
gnome-settings-daemon:$(GNOMESETTINGSDAEMON)_$(ARCH).deb
$(GNOMESETTINGSDAEMON): $(SPREZZ)/gnome-settings-daemon/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-settings-daemon_$(gnome-settings-daemon_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: gnome-user-docs
gnome-user-docs:$(GNOMEUSERDOCS)_$(ARCH).deb
$(GNOMEUSERDOCS): $(SPREZZ)/gnome-user-docs/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-user-docs_$(gnome-user-docs_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: gnome-vfs
gnome-vfs:$(GNOMEVFS)_$(ARCH).deb
$(GNOMEVFS): $(SPREZZ)/gnome-vfs/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gnome-vfs-$(gnome-vfs_UPVER).tar.gz $(TARARGS) $@

.PHONY: gnomecatalog
gnomecatalog:$(GNOMECATALOG)_$(ARCH).deb
$(GNOMECATALOG): $(SPREZZ)/gnomecatalog/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf gnomecatalog_$(gnomecatalog_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: gnu-fdisk
gnu-fdisk:$(GNUFDISK)_$(ARCH).deb
$(GNUFDISK): $(SPREZZ)/gnu-fdisk/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf fdisk-$(gnu-fdisk_UPVER).tar.gz $(TARARGS) $@

.PHONY: gnucash
gnucash:$(GNUCASH)_$(ARCH).deb
$(GNUCASH): $(SPREZZ)/gnucash/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf gnucash-$(gnucash_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: gnupg
gnupg:$(GNUPG)_$(ARCH).deb
$(GNUPG): $(SPREZZ)/gnupg/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gnupg-$(gnupg_UPVER).tar.gz $(TARARGS) $@

.PHONY: gnupg2
gnupg2:$(GNUPG2)_$(ARCH).deb
$(GNUPG2): $(SPREZZ)/gnupg2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf gnupg-$(gnupg2_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: gnuplot
gnuplot:$(GNUPLOT)_$(ARCH).deb
$(GNUPLOT): $(SPREZZ)/gnuplot/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gnuplot-$(gnuplot_UPVER).tar.gz $(TARARGS) $@

.PHONY: gnuradio
gnuradio:$(GNURADIO)_$(ARCH).deb
$(GNURADIO): $(SPREZZ)/gnuradio/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gnuradio-$(gnuradio_UPVER).tar.gz $(TARARGS) $@

.PHONY: gnutls
gnutls:$(GNUTLS)_$(ARCH).deb
$(GNUTLS): $(SPREZZ)/gnutls/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnutls-$(gnutls_UPVER).tar.xz $(TARARGS) $@

.PHONY: gnutls26
gnutls26:$(GNUTLS26)_$(ARCH).deb
$(GNUTLS26): $(SPREZZ)/gnutls26/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf gnutls26-$(gnutls26_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: gobject-introspection
gobject-introspection:$(GOBJECTINTROSPECTION)_$(ARCH).deb
$(GOBJECTINTROSPECTION): $(SPREZZ)/gobject-introspection/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gobject-introspection-$(gobject-introspection_UPVER).tar.xz $(TARARGS) $@

.PHONY: gocr
gocr:$(GOCR)_$(ARCH).deb
$(GOCR): $(SPREZZ)/gocr/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gocr-$(gocr_UPVER).tar.gz $(TARARGS) $@

.PHONY: google-gadgets
google-gadgets:$(GOOGLEGADGETS)_$(ARCH).deb
$(GOOGLEGADGETS): $(SPREZZ)/google-gadgets/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf google-gadgets-for-linux-$(google-gadgets_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: gource
gource:$(GOURCE)_$(ARCH).deb
$(GOURCE): $(SPREZZ)/gource/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gource-$(gource_UPVER).tar.gz $(TARARGS) $@

.PHONY: ffmpeg
ffmpeg:$(FFMPEG)_$(ARCH).deb
$(FFMPEG): $(SPREZZ)/ffmpeg/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf ffmpeg-$(ffmpeg_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: ffms2
ffms2:$(FFMS2)_$(ARCH).deb
$(FFMS2): $(SPREZZ)/ffms2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ffms2-$(ffms2_UPVER).tar.gz $(TARARGS) $@

.PHONY: gpac
gpac:$(GPAC)_$(ARCH).deb
$(GPAC): $(SPREZZ)/gpac/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gpac-$(gpac_UPVER).tar.gz $(TARARGS) $@

.PHONY: gperf
gperf:$(GPERF)_$(ARCH).deb
$(GPERF): $(SPREZZ)/gperf/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gperf-$(gperf_UPVER).tar.gz $(TARARGS) $@

.PHONY: gphoto2
gphoto2:$(GPHOTO2)_$(ARCH).deb
$(GPHOTO2): $(SPREZZ)/gphoto2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gphoto2-$(gphoto2_UPVER).tar.gz $(TARARGS) $@

.PHONY: gpodder
gpodder:$(GPODDER)_$(ARCH).deb
$(GPODDER): $(SPREZZ)/gpodder/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gpodder-$(gpodder_UPVER).tar.gz $(TARARGS) $@

.PHONY: gpsd
gpsd:$(GPSD)_$(ARCH).deb
$(GPSD): $(SPREZZ)/gpsd/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gpsd-$(gpsd_UPVER).tar.gz $(TARARGS) $@

.PHONY: xgridfit
xgridfit:$(XGRIDFIT)_$(ARCH).deb
$(XGRIDFIT): $(SPREZZ)/xgridfit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xgridfit-$(xgridfit_UPVER).tar.gz $(TARARGS) $@

.PHONY: grilo
grilo:$(GRILO)_$(ARCH).deb
$(GRILO): $(SPREZZ)/grilo/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf grilo-$(grilo_UPVER).tar.xz $(TARARGS) $@

.PHONY: groff
groff:$(GROFF)_$(ARCH).deb
$(GROFF): $(SPREZZ)/groff/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf groff-$(groff_UPVER).tar.gz $(TARARGS) $@

.PHONY: grub2
grub2:$(GRUB2)_$(ARCH).deb
$(GRUB2): $(SPREZZ)/grub2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf grub-$(grub2_UPVER).tar.xz $(TARARGS) $@

.PHONY: gsettings-desktop-schemas
gsettings-desktop-schemas:$(GSETTINGSDESKTOPSCHEMAS)_$(ARCH).deb
$(GSETTINGSDESKTOPSCHEMAS): $(SPREZZ)/gsettings-desktop-schemas/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gsettings-desktop-schemas-$(gsettings-desktop-schemas_UPVER).tar.xz $(TARARGS) $@

.PHONY: gsoap
gsoap:$(GSOAP)_$(ARCH).deb
$(GSOAP): $(SPREZZ)/gsoap/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf gsoap_$(gsoap_UPVER).tar.gz $(TARARGS) $@

.PHONY: gssdp
gssdp:$(GSSDP)_$(ARCH).deb
$(GSSDP): $(SPREZZ)/gssdp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gssdp-$(gssdp_UPVER).tar.xz $(TARARGS) $@

.PHONY: gthumb
gthumb:$(GTHUMB)_$(ARCH).deb
$(GTHUMB): $(SPREZZ)/gthumb/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gthumb_$(gthumb_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: gtkglext
gtkglext:$(GTKGLEXT)_$(ARCH).deb
$(GTKGLEXT): $(SPREZZ)/gtkglext/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gtkglext-$(gtkglext_UPVER).tar.gz $(TARARGS) $@

.PHONY: gtkhtml
gtkhtml:$(GTKHTML)_$(ARCH).deb
$(GTKHTML): $(SPREZZ)/gtkhtml/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gtkhtml4.0-$(gtkhtml_UPVER).tar.xz $(TARARGS) $@

.PHONY: gtk-vnc
gtk-vnc:$(GTKVNC)_$(ARCH).deb
$(GTKVNC): $(SPREZZ)/gtk-vnc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gtk-vnc-$(gtk-vnc_UPVER).tar.xz $(TARARGS) $@

.PHONY: gtk2
gtk2:$(GTK2)_$(ARCH).deb
$(GTK2): $(SPREZZ)/gtk2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gtk+2.0-$(gtk2_UPVER).tar.xz $(TARARGS) $@

.PHONY: gtk3
gtk3:$(GTK3)_$(ARCH).deb
$(GTK3): $(SPREZZ)/gtk3/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gtk+3.0+-$(gtk3_UPVER).tar.xz $(TARARGS) $@

.PHONY: gtkam
gtkam:$(GTKAM)_$(ARCH).deb
$(GTKAM): $(SPREZZ)/gtkam/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gtkam-$(gtkam_UPVER).tar.gz $(TARARGS) $@

.PHONY: guake
guake:$(GUAKE)_$(ARCH).deb
$(GUAKE): $(SPREZZ)/guake/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf guake-$(guake_UPVER).tar.gz $(TARARGS) $@

.PHONY: gucharmap
gucharmap:$(GUCHARMAP)_$(ARCH).deb
$(GUCHARMAP): $(SPREZZ)/gucharmap/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gucharmap-$(gucharmap_UPVER).tar.xz $(TARARGS) $@

.PHONY: gupnp
gupnp:$(GUPNP)_$(ARCH).deb
$(GUPNP): $(SPREZZ)/gupnp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gupnp-$(gupnp_UPVER).tar.xz $(TARARGS) $@

.PHONY: gupnp-av
gupnp-av:$(GUPNPAV)_$(ARCH).deb
$(GUPNPAV): $(SPREZZ)/gupnp-av/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gupnp-av-$(gupnp-av_UPVER).tar.xz $(TARARGS) $@

.PHONY: gupnp-dlna
gupnp-dlna:$(GUPNPDLNA)_$(ARCH).deb
$(GUPNPDLNA): $(SPREZZ)/gupnp-dlna/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gupnp-dlna-$(gupnp-dlna_UPVER).tar.xz $(TARARGS) $@

.PHONY: gutenprint
gutenprint:$(GUTENPRINT)_$(ARCH).deb
$(GUTENPRINT): $(SPREZZ)/gutenprint/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf gutenprint-$(gutenprint_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: guvcview
guvcview:$(GUVCVIEW)_$(ARCH).deb
$(GUVCVIEW): $(SPREZZ)/guvcview/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf guvcview-src-$(guvcview_UPVER).tar.gz $(TARARGS) $@

.PHONY: gvfs
gvfs:$(GVFS)_$(ARCH).deb
$(GVFS): $(SPREZZ)/gvfs/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gvfs-$(gvfs_UPVER).tar.xz $(TARARGS) $@

.PHONY: handbrake
handbrake:$(HANDBRAKE)_$(ARCH).deb
$(HANDBRAKE): $(SPREZZ)/handbrake/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf HandBrake-$(handbrake_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: harfbuzz
harfbuzz:$(HARFBUZZ)_$(ARCH).deb
$(HARFBUZZ): $(SPREZZ)/harfbuzz/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf harfbuzz-$(harfbuzz_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: hddtemp
hddtemp:$(HDDTEMP)_$(ARCH).deb
$(HDDTEMP): $(SPREZZ)/hddtemp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf hddtemp-$(hddtemp_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: hdf5
hdf5:$(HDF5)_$(ARCH).deb
$(HDF5): $(SPREZZ)/hdf5/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hdf5-$(hdf5_UPVER).tar.gz $(TARARGS) $@

.PHONY: hdparm
hdparm:$(HDPARM)_$(ARCH).deb
$(HDPARM): $(SPREZZ)/hdparm/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hdparm-$(hdparm_UPVER).tar.gz $(TARARGS) $@

.PHONY: hfsutils
hfsutils:$(HFSUTILS)_$(ARCH).deb
$(HFSUTILS): $(SPREZZ)/hfsutils/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hfsutils-$(hfsutils_UPVER).tar.gz $(TARARGS) $@

.PHONY: hicolor-icon-theme
hicolor-icon-theme:$(HICOLORICONTHEME)_$(ARCH).deb
$(HICOLORICONTHEME): $(SPREZZ)/hicolor-icon-theme/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hicolor-icon-theme-$(hicolor-icon-theme_UPVER).tar.gz $(TARARGS) $@

.PHONY: hplip
hplip:$(HPLIP)_$(ARCH).deb
$(HPLIP): $(SPREZZ)/hplip/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hplip-$(hplip_UPVER).tar.gz $(TARARGS) $@

.PHONY: htop
htop:$(HTOP)_$(ARCH).deb
$(HTOP): $(SPREZZ)/htop/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf htop-$(htop_UPVER).tar.gz $(TARARGS) $@

.PHONY: hwloc
hwloc:$(HWLOC)_$(ARCH).deb
$(HWLOC): $(SPREZZ)/hwloc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf hwloc-$(hwloc_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: ibus
ibus:$(IBUS)_$(ARCH).deb
$(IBUS): $(SPREZZ)/ibus/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ibus-$(ibus_UPVER).tar.gz $(TARARGS) $@

.PHONY: ibus-table
ibus-table:$(IBUSTABLE)_$(ARCH).deb
$(IBUSTABLE): $(SPREZZ)/ibus-table/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ibus-table-$(ibus-table_UPVER).tar.gz $(TARARGS) $@

.PHONY: icedove
icedove:$(ICEDOVE)_$(ARCH).deb
$(ICEDOVE): $(SPREZZ)/icedove/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf icedove-$(icedove_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: icedtea-web
icedtea-web:$(ICEDTEAWEB)_$(ARCH).deb
$(ICEDTEAWEB): $(SPREZZ)/icedtea-web/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf icedtea-web-$(icedtea-web_UPVER).tar.gz $(TARARGS) $@

.PHONY: icu
icu:$(ICU)_$(ARCH).deb
$(ICU): $(SPREZZ)/icu/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf icu_$(icu_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ideviceinstaller
ideviceinstaller:$(IDEVICEINSTALLER)_$(ARCH).deb
$(IDEVICEINSTALLER): $(SPREZZ)/ideviceinstaller/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf ideviceinstaller-$(ideviceinstaller_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: ifuse
ifuse:$(IFUSE)_$(ARCH).deb
$(IFUSE): $(SPREZZ)/ifuse/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf ifuse-$(ifuse_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: ijs
ijs:$(IJS)_$(ARCH).deb
$(IJS): $(SPREZZ)/ijs/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ijs_$(ijs_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: irda-utils
irda-utils:$(IRDAUTILS)_$(ARCH).deb
$(IRDAUTILS): $(SPREZZ)/irda-utils/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf irda-utils-$(irda-utils_UPVER).tar.gz $(TARARGS) $@

.PHONY: libio-pty-perl
libio-pty-perl:$(LIBIOPTYPERL)_$(ARCH).deb
$(LIBIOPTYPERL): $(SPREZZ)/libio-pty-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf IO-Tty-$(libio-pty-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libiksemel
libiksemel:$(LIBIKSEMEL)_$(ARCH).deb
$(LIBIKSEMEL): $(SPREZZ)/libiksemel/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf iksemel-$(libiksemel_UPVER).tar.gz $(TARARGS) $@

.PHONY: liquidsoap
liquidsoap:$(LIQUIDSOAP)_$(ARCH).deb
$(LIQUIDSOAP): $(SPREZZ)/liquidsoap/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf liquidsoap-$(liquidsoap_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: iproute
iproute:$(IPROUTE)_$(ARCH).deb
$(IPROUTE): $(SPREZZ)/iproute/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf iproute2-$(iproute_UPVER).tar.gz $(TARARGS) $@

.PHONY: iptables
iptables:$(IPTABLES)_$(ARCH).deb
$(IPTABLES): $(SPREZZ)/iptables/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf iptables-$(iptables_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: itstool
itstool:$(ITSTOOL)_$(ARCH).deb
$(ITSTOOL): $(SPREZZ)/itstool/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf itstool-$(itstool_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: inkscape
inkscape:$(INKSCAPE)_$(ARCH).deb
$(INKSCAPE): $(SPREZZ)/inkscape/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf inkscape-$(inkscape_UPVER).tar.gz $(TARARGS) $@

.PHONY: insserv
insserv:$(INSSERV)_$(ARCH).deb
$(INSSERV): $(SPREZZ)/insserv/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf insserv-$(insserv_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: jbig2dec
jbig2dec:$(JBIG2DEC)_$(ARCH).deb
$(JBIG2DEC): $(SPREZZ)/jbig2dec/debian/changelog
	git clone git://github.com/dankamongmen/jbig2dec.git $@
	tar cJf jbig2dec-$(jbig2dec_UPVER).tar.xz $@ --exclude-vcs
	ln -sf jbig2dec-$(jbig2dec_UPVER).tar.xz jbig2dec_$(jbig2dec_UPVER).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: opensp
opensp:$(OPENSP)_$(ARCH).deb
$(OPENSP): $(SPREZZ)/opensp/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf OpenSP-$(opensp_UPVER).tar.gz $(TARARGS) $@

.PHONY: jadetex
jadetex:$(JADETEX)_$(ARCH).deb
$(JADETEX): $(SPREZZ)/jadetex/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf jadetex-$(jadetex_UPVER).tar.gz $(TARARGS) $@

.PHONY: jasper
jasper:$(JASPER)_$(ARCH).deb
$(JASPER): $(SPREZZ)/jasper/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf jasper-$(jasper_UPVER).tar.gz $(TARARGS) $@

.PHONY: jfsutils
jfsutils:$(JFSUTILS)_$(ARCH).deb
$(JFSUTILS): $(SPREZZ)/jfsutils/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf jfsutils-$(jfsutils_UPVER).tar.gz $(TARARGS) $@

.PHONY: jigit
jigit:$(JIGIT)_$(ARCH).deb
$(JIGIT): $(SPREZZ)/jigit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf jigit_$(jigit_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: json-c
json-c:$(JSONC)_$(ARCH).deb
$(JSONC): $(SPREZZ)/json-c/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf json-c_$(json-c_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: kbd
kbd:$(KBD)_$(ARCH).deb
$(KBD): $(SPREZZ)/kbd/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf kbd-$(kbd_UPVER).tar.gz $(TARARGS) $@

.PHONY: kdenlive
kdenlive:$(KDENLIVE)_$(ARCH).deb
$(KDENLIVE): $(SPREZZ)/kdenlive/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf kdenlive-$(kdenlive_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: krb
krb:$(KRB)_$(ARCH).deb
$(KRB): $(SPREZZ)/krb/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xvf krb5-$(krb_UPVER)-signed.tar $(TARARGS) $@

.PHONY: keybinder
keybinder:$(KEYBINDER)_$(ARCH).deb
$(KEYBINDER): $(SPREZZ)/keybinder/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf keybinder-$(keybinder_UPVER).tar.gz $(TARARGS) $@

.PHONY: keyutils
keyutils:$(KEYUTILS)_$(ARCH).deb
$(KEYUTILS): $(SPREZZ)/keyutils/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf keyutils-$(keyutils_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: kismet
kismet:$(KISMET)_$(ARCH).deb
$(KISMET): $(SPREZZ)/kismet/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf kismet_$(kismet_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: klibc
klibc:$(KLIBC)_$(ARCH).deb
$(KLIBC): $(SPREZZ)/klibc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf klibc-$(klibc_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: kmod
kmod:$(KMOD)_$(ARCH).deb
$(KMOD): $(SPREZZ)/kmod/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf kmod-$(kmod_UPVER).tar.xz $(TARARGS) $@

.PHONY: kyotocabinet
kyotocabinet:$(KYOTOCABINET)_$(ARCH).deb
$(KYOTOCABINET): $(SPREZZ)/kyotocabinet/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf kyotocabinet-$(kyotocabinet_UPVER).tar.gz $(TARARGS) $@

.PHONY: lcms2
lcms2:$(LCMS2)_$(ARCH).deb
$(LCMS2): $(SPREZZ)/lcms2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf lcms2-$(lcms2_UPVER).tar.gz $(TARARGS) $@

.PHONY: libarchive
libarchive:$(LIBARCHIVE)_$(ARCH).deb
$(LIBARCHIVE): $(SPREZZ)/libarchive/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf v$(libarchive_UPVER).tar.gz $(TARARGS) $@

.PHONY: libatasmart
libatasmart:$(LIBATASMART)_$(ARCH).deb
$(LIBATASMART): $(SPREZZ)/libatasmart/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libatasmart_$(libatasmart_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: pam
pam:$(PAM)_$(ARCH).deb
$(PAM): $(SPREZZ)/pam/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf Linux-PAM-$(pam_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libpaper
libpaper:$(LIBPAPER)_$(ARCH).deb
$(LIBPAPER): $(SPREZZ)/libpaper/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libpaper-$(libpaper_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libpam-ssh
libpam-ssh:$(LIBPAMSSH)_$(ARCH).deb
$(LIBPAMSSH): $(SPREZZ)/libpam-ssh/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf pam_ssh-$(libpam-ssh_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libpipeline
libpipeline:$(LIBPIPELINE)_$(ARCH).deb
$(LIBPIPELINE): $(SPREZZ)/libpipeline/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libpipeline-$(libpipeline_UPVER).tar.gz $(TARARGS) $@

.PHONY: libpng
libpng:$(LIBPNG)_$(ARCH).deb
$(LIBPNG): $(SPREZZ)/libpng/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libpng-$(libpng_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libpst
libpst:$(LIBPST)_$(ARCH).deb
$(LIBPST): $(SPREZZ)/libpst/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libpst-$(libpst_UPVER).tar.gz $(TARARGS) $@

.PHONY: plymouth
plymouth:$(PLYMOUTH)_$(ARCH).deb
$(PLYMOUTH): $(SPREZZ)/plymouth/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf plymouth-$(plymouth_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: pulseaudio
pulseaudio:$(PULSEAUDIO)_$(ARCH).deb
$(PULSEAUDIO): $(SPREZZ)/pulseaudio/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf pulseaudio-$(pulseaudio_UPVER).tar.xz $(TARARGS) $@

.PHONY: libproxy
libproxy:$(LIBPROXY)_$(ARCH).deb
$(LIBPROXY): $(SPREZZ)/libproxy/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libproxy-$(libproxy_UPVER).tar.gz $(TARARGS) $@

.PHONY: cracklib2
cracklib2:$(CRACKLIB2)_$(ARCH).deb
$(CRACKLIB2): $(SPREZZ)/cracklib2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cracklib-$(cracklib2_UPVER).tar.gz $(TARARGS) $@

.PHONY: libpwquality
libpwquality:$(LIBPWQUALITY)_$(ARCH).deb
$(LIBPWQUALITY): $(SPREZZ)/libpwquality/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libpwquality-$(libpwquality_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: lightdm
lightdm:$(LIGHTDM)_$(ARCH).deb
$(LIGHTDM): $(SPREZZ)/lightdm/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf lightdm-$(lightdm_UPVER).tar.xz $(TARARGS) $@

.PHONY: lightspeed
lightspeed:$(LIGHTSPEED)_$(ARCH).deb
$(LIGHTSPEED): $(SPREZZ)/lightspeed/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf lightspeed-$(lightspeed_UPVER).tar.gz $(TARARGS) $@

.PHONY: lynx
lynx:$(LYNX)_$(ARCH).deb
$(LYNX): $(SPREZZ)/lynx/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf lynx$(lynx_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: e2fsprogs
e2fsprogs:$(E2FSPROGS)_$(ARCH).deb
$(E2FSPROGS): $(SPREZZ)/e2fsprogs/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf e2fsprogs-$(e2fsprogs_UPVER).tar.gz $(TARARGS) $@

.PHONY: earthorca
earthorca:$(EARTHORCA)_$(ARCH).deb
$(EARTHORCA): $(SPREZZ)/earthorca/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf firefox-$(earthorca_UPVER).source.tar.bz2 $(TARARGS) $@

.PHONY: edbus
edbus:$(EDBUS)_$(ARCH).deb
$(EDBUS): $(SPREZZ)/edbus/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf edbus_$(edbus_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: embryo
embryo:$(EMBRYO)_$(ARCH).deb
$(EMBRYO): $(SPREZZ)/embryo/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf embryo_$(embryo_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: emotion
emotion:$(EMOTION)_$(ARCH).deb
$(EMOTION): $(SPREZZ)/emotion/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf emotion_$(emotion_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: ecore
ecore:$(ECORE)_$(ARCH).deb
$(ECORE): $(SPREZZ)/ecore/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf ecore_$(ecore_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: ecryptfs-utils
ecryptfs-utils:$(ECRYPTFSUTILS)_$(ARCH).deb
$(ECRYPTFSUTILS): $(SPREZZ)/ecryptfs-utils/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ecryptfs-utils_$(ecryptfs-utils_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: euca2ools
euca2ools:$(EUCA2OOLS)_$(ARCH).deb
$(EUCA2OOLS): $(SPREZZ)/euca2ools/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf euca2ools_$(euca2ools_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: edje
edje:$(EDJE)_$(ARCH).deb
$(EDJE): $(SPREZZ)/edje/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf edje_$(edje_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: eet
eet:$(EET)_$(ARCH).deb
$(EET): $(SPREZZ)/eet/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf eet_$(eet_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: eeze
eeze:$(EEZE)_$(ARCH).deb
$(EEZE): $(SPREZZ)/eeze/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf eeze_$(eeze_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: efibootmgr
efibootmgr:$(EFIBOOTMGR)_$(ARCH).deb
$(EFIBOOTMGR): $(SPREZZ)/efibootmgr/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf efibootmgr-$(efibootmgr_UPVER).tar.gz $(TARARGS) $@

.PHONY: efreet
efreet:$(EFREET)_$(ARCH).deb
$(EFREET): $(SPREZZ)/efreet/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf efreet_$(efreet_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: eina
eina:$(EINA)_$(ARCH).deb
$(EINA): $(SPREZZ)/eina/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf eina_$(eina_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: eio
eio:$(EIO)_$(ARCH).deb
$(EIO): $(SPREZZ)/eio/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf eio_$(eio_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: elementary
elementary:$(ELEMENTARY)_$(ARCH).deb
$(ELEMENTARY): $(SPREZZ)/elementary/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf elementary_$(elementary_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: eterm
eterm:$(ETERM)_$(ARCH).deb
$(ETERM): $(SPREZZ)/eterm/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Eterm-$(eterm_UPVER).tar.gz $(TARARGS) $@

.PHONY: ethumb
ethumb:$(ETHUMB)_$(ARCH).deb
$(ETHUMB): $(SPREZZ)/ethumb/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf ethumb_$(ethumb_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: evas
evas:$(EVAS)_$(ARCH).deb
$(EVAS): $(SPREZZ)/evas/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf evas_$(evas_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: evas-generic-loaders
evas-generic-loaders:$(EVASGENERICLOADERS)_$(ARCH).deb
$(EVASGENERICLOADERS): $(SPREZZ)/evas-generic-loaders/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf evas-generic-loaders_$(evas-generic-loaders_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: ewebkit
ewebkit:$(EWEBKIT)_$(ARCH).deb
$(EWEBKIT): $(SPREZZ)/ewebkit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf webkit-efl-svn-r$(ewebkit_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: exactimage
exactimage:$(EXACTIMAGE)_$(ARCH).deb
$(EXACTIMAGE): $(SPREZZ)/exactimage/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf exactimage_$(exactimage_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: exempi
exempi:$(EXEMPI)_$(ARCH).deb
$(EXEMPI): $(SPREZZ)/exempi/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf exempi-$(exempi_UPVER).tar.gz $(TARARGS) $@

.PHONY: gexiv2
gexiv2:$(GEXIV2)_$(ARCH).deb
$(GEXIV2): $(SPREZZ)/gexiv2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libgexiv2-$(gexiv2_UPVER).tar.xz $(TARARGS) $@

.PHONY: exif
exif:$(EXIF)_$(ARCH).deb
$(EXIF): $(SPREZZ)/exif/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf exif-$(exif_UPVER).tar.gz $(TARARGS) $@

.PHONY: exim4
exim4:$(EXIM4)_$(ARCH).deb
$(EXIM4): $(SPREZZ)/exim4/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf exim-$(exim4_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: exiv2
exiv2:$(EXIV2)_$(ARCH).deb
$(EXIV2): $(SPREZZ)/exiv2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf exiv2-$(exiv2_UPVER).tar.gz $(TARARGS) $@

.PHONY: exo
exo:$(EXO)_$(ARCH).deb
$(EXO): $(SPREZZ)/exo/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf exo-$(exo_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: expat
expat:$(EXPAT)_$(ARCH).deb
$(EXPAT): $(SPREZZ)/expat/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf expat-$(expat_UPVER).tar.gz $(TARARGS) $@

.PHONY: expect
expect:$(EXPECT)_$(ARCH).deb
$(EXPECT): $(SPREZZ)/expect/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf expect$(expect_UPVER).tar.gz $(TARARGS) $@

.PHONY: ext3grep
ext3grep:$(EXT3GREP)_$(ARCH).deb
$(EXT3GREP): $(SPREZZ)/ext3grep/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ext3grep-$(ext3grep_UPVER).tar.gz $(TARARGS) $@

.PHONY: gdb
gdb:$(GDB)_$(ARCH).deb
$(GDB): $(SPREZZ)/gdb/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf gdb-$(gdb_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: heimdal
heimdal:$(HEIMDAL)_$(ARCH).deb
$(HEIMDAL): $(SPREZZ)/heimdal/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf heimdal-$(heimdal_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: imagemagick
imagemagick:$(IMAGEMAGICK)_$(ARCH).deb
$(IMAGEMAGICK): $(SPREZZ)/imagemagick/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf imagemagick_$(imagemagick_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: imlib
imlib:$(IMLIB)_$(ARCH).deb
$(IMLIB): $(SPREZZ)/imlib/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf imlib2-$(imlib_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: iucode-tool
iucode-tool:$(IUCODETOOL)_$(ARCH).deb
$(IUCODETOOL): $(SPREZZ)/iucode-tool/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf iucode-tool-$(iucode-tool_UPVER).tar.gz $(TARARGS) $@

.PHONY: lame
lame:$(LAME)_$(ARCH).deb
$(LAME): $(SPREZZ)/lame/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf lame-$(lame_UPVER).tar.gz $(TARARGS) $@

.PHONY: lapack
lapack:$(LAPACK)_$(ARCH).deb
$(LAPACK): $(SPREZZ)/lapack/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf lapack-$(lapack_UPVER).tgz $(TARARGS) $@

.PHONY: lastfmsubmitd
lastfmsubmitd:$(LASTFMSUBMITD)_$(ARCH).deb
$(LASTFMSUBMITD): $(SPREZZ)/lastfmsubmitd/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf lastfmsubmitd-$(lastfmsubmitd_UPVER).tar.gz $(TARARGS) $@

.PHONY: libav
libav:$(LIBAV)_$(ARCH).deb
$(LIBAV): $(SPREZZ)/libav/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf $(LIBAV).orig.tar.gz $(TARARGS) $@

.PHONY: libcaca
libcaca:$(LIBCACA)_$(ARCH).deb
$(LIBCACA): $(SPREZZ)/libcaca/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libcaca-$(libcaca_UPVER).tar.gz $(TARARGS) $@

.PHONY: libcacard
libcacard:$(LIBCACARD)_$(ARCH).deb
$(LIBCACARD): $(SPREZZ)/libcacard/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libcacard-$(libcacard_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libcap2
libcap2:$(LIBCAP2)_$(ARCH).deb
$(LIBCAP2): $(SPREZZ)/libcap2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libcap-$(libcap2_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libestr
libestr:$(LIBESTR)_$(ARCH).deb
$(LIBESTR): $(SPREZZ)/libestr/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libestr-$(libestr_UPVER).tar.gz $(TARARGS) $@

.PHONY: libetpan
libetpan:$(LIBETPAN)_$(ARCH).deb
$(LIBETPAN): $(SPREZZ)/libetpan/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libetpan-$(libetpan_UPVER).tar.gz $(TARARGS) $@

.PHONY: libev
libev:$(LIBEV)_$(ARCH).deb
$(LIBEV): $(SPREZZ)/libev/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libev-$(libev_UPVER).tar.gz $(TARARGS) $@

.PHONY: libevent
libevent:$(LIBEVENT)_$(ARCH).deb
$(LIBEVENT): $(SPREZZ)/libevent/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libevent-$(libevent_UPVER).tar.gz $(TARARGS) $@

.PHONY: libedit
libedit:$(LIBEDIT)_$(ARCH).deb
$(LIBEDIT): $(SPREZZ)/libedit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libedit-$(libedit_UPVER).tar.gz $(TARARGS) $@

.PHONY: libexif
libexif:$(LIBEXIF)_$(ARCH).deb
$(LIBEXIF): $(SPREZZ)/libexif/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libexif-$(libexif_UPVER).tar.gz $(TARARGS) $@

.PHONY: libextractor
libextractor:$(LIBEXTRACTOR)_$(ARCH).deb
$(LIBEXTRACTOR): $(SPREZZ)/libextractor/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libextractor-$(libextractor_UPVER).tar.gz $(TARARGS) $@

.PHONY: libgadu
libgadu:$(LIBGADU)_$(ARCH).deb
$(LIBGADU): $(SPREZZ)/libgadu/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libgadu-$(libgadu_UPVER).tar.gz $(TARARGS) $@

.PHONY: libgphoto2
libgphoto2:$(LIBGPHOTO2)_$(ARCH).deb
$(LIBGPHOTO2): $(SPREZZ)/libgphoto2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libgphoto2-$(libgphoto2_UPVER).tar.gz $(TARARGS) $@

.PHONY: libimobiledevice
libimobiledevice:$(LIBIMOBILEDEVICE)_$(ARCH).deb
$(LIBIMOBILEDEVICE): $(SPREZZ)/libimobiledevice/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libimobiledevice_$(libimobiledevice_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: indigo
indigo:$(INDIGO)_$(ARCH).deb
$(INDIGO): $(SPREZZ)/indigo/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf indigo-$(indigo_UPVER).tar.gz $(TARARGS) $@

.PHONY: libbluray
libbluray:$(LIBBLURAY)_$(ARCH).deb
$(LIBBLURAY): $(SPREZZ)/libbluray/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libbluray-$(libbluray_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libburn
libburn:$(LIBBURN)_$(ARCH).deb
$(LIBBURN): $(SPREZZ)/libburn/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libburn-$(libburn_UPVER).tar.gz $(TARARGS) $@

.PHONY: libical
libical:$(LIBICAL)_$(ARCH).deb
$(LIBICAL): $(SPREZZ)/libical/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libical-$(libical_UPVER).tar.gz $(TARARGS) $@

.PHONY: libice
libice:$(LIBICE)_$(ARCH).deb
$(LIBICE): $(SPREZZ)/libice/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libICE-$(libice_UPVER).tar.gz $(TARARGS) $@

.PHONY: libidl
libidl:$(LIBIDL)_$(ARCH).deb
$(LIBIDL): $(SPREZZ)/libidl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libIDL-$(libidl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libidn11
libidn11:$(LIBIDN11)_$(ARCH).deb
$(LIBIDN11): $(SPREZZ)/libidn11/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libidn-$(libidn11_UPVER).tar.gz $(TARARGS) $@

.PHONY: libisoburn
libisoburn:$(LIBISOBURN)_$(ARCH).deb
$(LIBISOBURN): $(SPREZZ)/libisoburn/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libisoburn-$(libisoburn_UPVER).tar.gz $(TARARGS) $@

.PHONY: libisofs
libisofs:$(LIBISOFS)_$(ARCH).deb
$(LIBISOFS): $(SPREZZ)/libisofs/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libisofs-$(libisofs_UPVER).tar.gz $(TARARGS) $@

.PHONY: liblastfm
liblastfm:$(LIBLASTFM)_$(ARCH).deb
$(LIBLASTFM): $(SPREZZ)/liblastfm/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf liblastfm_$(liblastfm_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: liblockfile
liblockfile:$(LIBLOCKFILE)_$(ARCH).deb
$(LIBLOCKFILE): $(SPREZZ)/liblockfile/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf liblockfile-$(liblockfile_UPVER).tar.gz $(TARARGS) $@

.PHONY: upower
upower:$(UPOWER)_$(ARCH).deb
$(UPOWER): $(SPREZZ)/upower/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf upower_$(upower_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: usbmuxd
usbmuxd:$(USBMUXD)_$(ARCH).deb
$(USBMUXD): $(SPREZZ)/usbmuxd/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf usbmuxd_$(usbmuxd_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: usbview
usbview:$(USBVIEW)_$(ARCH).deb
$(USBVIEW): $(SPREZZ)/usbview/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf usbview-$(usbview_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: vgabios
vgabios:$(VGABIOS)_$(ARCH).deb
$(VGABIOS): $(SPREZZ)/vgabios/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf vgabios-$(vgabios_UPVER).tgz $(TARARGS) $@

.PHONY: vlc
vlc:$(VLC)_$(ARCH).deb
$(VLC): $(SPREZZ)/vlc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf vlc-$(vlc_UPVER).tar.xz $(TARARGS) $@

.PHONY: libdatetime-perl
libdatetime-perl:$(LIBDATETIMEPERL)_$(ARCH).deb
$(LIBDATETIMEPERL): $(SPREZZ)/libdatetime-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libdatetime-perl_$(libdatetime-perl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libdatrie
libdatrie:$(LIBDATRIE)_$(ARCH).deb
$(LIBDATRIE): $(SPREZZ)/libdatrie/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libdatrie-$(libdatrie_UPVER).tar.xz $(TARARGS) $@

.PHONY: libdca
libdca:$(LIBDCA)_$(ARCH).deb
$(LIBDCA): $(SPREZZ)/libdca/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libdca-$(libdca_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libdmx
libdmx:$(LIBDMX)_$(ARCH).deb
$(LIBDMX): $(SPREZZ)/libdmx/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libdmx-$(libdmx_UPVER).tar.gz $(TARARGS) $@

.PHONY: libdrm
libdrm:$(LIBDRM)_$(ARCH).deb
$(LIBDRM): $(SPREZZ)/libdrm/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libdrm-$(libdrm_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libxml
libxml:$(LIBXML)_$(ARCH).deb
$(LIBXML): $(SPREZZ)/libxml/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libxml2_$(libxml_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libxtst
libxtst:$(LIBXTST)_$(ARCH).deb
$(LIBXTST): $(SPREZZ)/libxtst/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libXtst-$(libxtst_UPVER).tar.gz $(TARARGS) $@

.PHONY: xapian-core
xapian-core:$(XAPIANCORE)_$(ARCH).deb
$(XAPIANCORE): $(SPREZZ)/xapian-core/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xapian-core-$(xapian-core_UPVER).tar.gz $(TARARGS) $@

.PHONY: xauth
xauth:$(XAUTH)_$(ARCH).deb
$(XAUTH): $(SPREZZ)/xauth/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xauth-$(xauth_UPVER).tar.gz $(TARARGS) $@

.PHONY: xawtv
xawtv:$(XAWTV)_$(ARCH).deb
$(XAWTV): $(SPREZZ)/xawtv/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf xawtv-$(xawtv_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: xen
xen:$(XEN)_$(ARCH).deb
$(XEN): $(SPREZZ)/xen/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xen-$(xen_UPVER).tar.gz $(TARARGS) $@

.PHONY: xft
xft:$(XFT)_$(ARCH).deb
$(XFT): $(SPREZZ)/xft/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libXft-$(xft_UPVER).tar.gz $(TARARGS) $@

.PHONY: libxp
libxp:$(LIBXP)_$(ARCH).deb
$(LIBXP): $(SPREZZ)/libxp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libXp-$(libxp_UPVER).tar.gz $(TARARGS) $@

.PHONY: xfsprogs
xfsprogs:$(XFSPROGS)_$(ARCH).deb
$(XFSPROGS): $(SPREZZ)/xfsprogs/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xfsprogs-$(xfsprogs_UPVER).tar.gz $(TARARGS) $@

.PHONY: libjpeg8-turbo
libjpeg8-turbo:$(LIBJPEG8TURBO)_$(ARCH).deb
$(LIBJPEG8TURBO): $(SPREZZ)/libjpeg8-turbo/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libjpeg-turbo-$(libjpeg8-turbo_UPVER).tar.gz $(TARARGS) $@

.PHONY: libjpeg
libjpeg:$(LIBJPEG)_$(ARCH).deb
$(LIBJPEG): $(SPREZZ)/libjpeg/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libjpeg9_$(libjpeg_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libsamplerate
libsamplerate:$(LIBSAMPLERATE)_$(ARCH).deb
$(LIBSAMPLERATE): $(SPREZZ)/libsamplerate/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libsamplerate-$(libsamplerate_UPVER).tar.gz $(TARARGS) $@

.PHONY: libsecret
libsecret:$(LIBSECRET)_$(ARCH).deb
$(LIBSECRET): $(SPREZZ)/libsecret/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libsecret-$(libsecret_UPVER).tar.xz $(TARARGS) $@

.PHONY: texinfo
texinfo:$(TEXINFO)_$(ARCH).deb
$(TEXINFO): $(SPREZZ)/texinfo/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf texinfo-$(texinfo_UPVER).tar.xz $(TARARGS) $@

.PHONY: mpclib
mpclib:$(MPCLIB)_$(ARCH).deb
$(MPCLIB): $(SPREZZ)/mpclib/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mpc-$(mpclib_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: libsemanage
libsemanage:$(LIBSEMANAGE)_$(ARCH).deb
$(LIBSEMANAGE): $(SPREZZ)/libsemanage/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libsemanage-$(libsemanage_UPVER).tar.gz $(TARARGS) $@

.PHONY: libselinux
libselinux:$(LIBSELINUX)_$(ARCH).deb
$(LIBSELINUX): $(SPREZZ)/libselinux/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libselinux-$(libselinux_UPVER).tar.gz $(TARARGS) $@

.PHONY: libsepol
libsepol:$(LIBSEPOL)_$(ARCH).deb
$(LIBSEPOL): $(SPREZZ)/libsepol/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libsepol-$(libsepol_UPVER).tar.gz $(TARARGS) $@

.PHONY: libsm
libsm:$(LIBSM)_$(ARCH).deb
$(LIBSM): $(SPREZZ)/libsm/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libSM-$(libsm_UPVER).tar.gz $(TARARGS) $@

.PHONY: libsmbios
libsmbios:$(LIBSMBIOS)_$(ARCH).deb
$(LIBSMBIOS): $(SPREZZ)/libsmbios/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libsmbios-$(libsmbios_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libsoup
libsoup:$(LIBSOUP)_$(ARCH).deb
$(LIBSOUP): $(SPREZZ)/libsoup/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libsoup2.4_$(libsoup_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: libssh
libssh:$(LIBSSH)_$(ARCH).deb
$(LIBSSH): $(SPREZZ)/libssh/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libssh-$(libssh_UPVER).tar.gz $(TARARGS) $@

.PHONY: libssh2
libssh2:$(LIBSSH2)_$(ARCH).deb
$(LIBSSH2): $(SPREZZ)/libssh2/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libssh2-$(libssh2_UPVER).tar.gz $(TARARGS) $@

.PHONY: libsynthesis
libsynthesis:$(LIBSYNTHESIS)_$(ARCH).deb
$(LIBSYNTHESIS): $(SPREZZ)/libsynthesis/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libsynthesis_$(synthesis_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libtasn
libtasn:$(LIBTASN)_$(ARCH).deb
$(LIBTASN): $(SPREZZ)/libtasn/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libtasn1-$(libtasn_UPVER).tar.gz $(TARARGS) $@

.PHONY: libtar
libtar:$(LIBTAR)_$(ARCH).deb
$(LIBTAR): $(SPREZZ)/libtar/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libtar-$(libtar_UPVER).tar.gz $(TARARGS) $@

.PHONY: libtemplate-perl
libtemplate-perl:$(LIBTEMPLATEPERL)_$(ARCH).deb
$(LIBTEMPLATEPERL): $(SPREZZ)/libtemplate-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Template-Toolkit-$(libtemplate-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libtext-charwidth-perl
libtext-charwidth-perl:$(LIBTEXTCHARWIDTHPERL)_$(ARCH).deb
$(LIBTEXTCHARWIDTHPERL): $(SPREZZ)/libtext-charwidth-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Text-CharWidth-$(libtext-charwidth-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libtextwrap
libtextwrap:$(LIBTEXTWRAP)_$(ARCH).deb
$(LIBTEXTWRAP): $(SPREZZ)/libtextwrap/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libtextwrap-$(libtextwrap_UPVER).tar.gz $(TARARGS) $@

.PHONY: libtheora
libtheora:$(LIBTHEORA)_$(ARCH).deb
$(LIBTHEORA): $(SPREZZ)/libtheora/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf libtheora-$(libtheora_UPVER).tar.gz $(TARARGS) $@

.PHONY: libthrust
libthrust:$(LIBTHRUST)_$(ARCH).deb
$(LIBTHRUST): $(SPREZZ)/libthrust/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf thrust-$(libthrust_UPVER).tar.gz $(TARARGS) $@

.PHONY: libtirpc
libtirpc:$(LIBTIRPC)_$(ARCH).deb
$(LIBTIRPC): $(SPREZZ)/libtirpc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libtirpc-$(libtirpc_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libtool
libtool:$(LIBTOOL)_$(ARCH).deb
$(LIBTOOL): $(SPREZZ)/libtool/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libtool-$(libtool_UPVER).tar.gz $(TARARGS) $@

.PHONY: libtorrent14
libtorrent14:$(LIBTORRENT14)_$(ARCH).deb
$(LIBTORRENT14): $(SPREZZ)/libtorrent14/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libtorrent-$(libtorrent14_UPVER).tar.gz $(TARARGS) $@

.PHONY: libva
libva:$(LIBVA)_$(ARCH).deb
$(LIBVA): $(SPREZZ)/libva/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libva-$(libva_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libvdpau
libvdpau:$(LIBVDPAU)_$(ARCH).deb
$(LIBVDPAU): $(SPREZZ)/libvdpau/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libvdpau-$(libvdpau_UPVER).tar.gz $(TARARGS) $@

.PHONY: libvirt
libvirt:$(LIBVIRT)_$(ARCH).deb
$(LIBVIRT): $(SPREZZ)/libvirt/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libvirt_$(libvirt_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libvirt-glib
libvirt-glib:$(LIBVIRTGLIB)_$(ARCH).deb
$(LIBVIRTGLIB): $(SPREZZ)/libvirt-glib/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libvirt-glib-$(libvirt-glib_UPVER).tar.gz $(TARARGS) $@

.PHONY: libvpx
libvpx:$(LIBVPX)_$(ARCH).deb
$(LIBVPX): $(SPREZZ)/libvpx/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libvpx-v$(libvpx_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: wxwidgets2.8
wxwidgets2.8:$(WXWIDGETS2.8)_$(ARCH).deb
$(WXWIDGETS2.8): $(SPREZZ)/wxwidgets2.8/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf wxwidgets2.8-$(wxwidgets2.8_UPVER).tar.gz $(TARARGS) $@

.PHONY: wxwidgets2.9
wxwidgets2.9:$(WXWIDGETS2.9)_$(ARCH).deb
$(WXWIDGETS2.9): $(SPREZZ)/wxwidgets2.9/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf wxWidgets-$(wxwidgets2.9_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libwacom
libwacom:$(LIBWACOM)_$(ARCH).deb
$(LIBWACOM): $(SPREZZ)/libwacom/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libwacom-$(libwacom_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libcurses-ui-perl
libcurses-ui-perl:$(LIBCURSESUIPERL)_$(ARCH).deb
$(LIBCURSESUIPERL): $(SPREZZ)/libcurses-ui-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Curses-UI-$(libcurses-ui-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libev-perl
libev-perl:$(LIBEVPERL)_$(ARCH).deb
$(LIBEVPERL): $(SPREZZ)/libev-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf EV-$(libev-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libunwind
libunwind:$(LIBUNWIND)_$(ARCH).deb
$(LIBUNWIND): $(SPREZZ)/libunwind/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libunwind-$(libunwind_UPVER).tar.gz $(TARARGS) $@

.PHONY: libuuid-perl
libuuid-perl:$(LIBUUIDPERL)_$(ARCH).deb
$(LIBUUIDPERL): $(SPREZZ)/libuuid-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf UUID-$(libuuid-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libwww-perl
libwww-perl:$(LIBWWWPERL)_$(ARCH).deb
$(LIBWWWPERL): $(SPREZZ)/libwww-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libwww-perl-$(libwww-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libxcomposite
libxcomposite:$(LIBXCOMPOSITE)_$(ARCH).deb
$(LIBXCOMPOSITE): $(SPREZZ)/libxcomposite/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libXcomposite-$(libxcomposite_UPVER).tar.gz $(TARARGS) $@

.PHONY: libxdamage
libxdamage:$(LIBXDAMAGE)_$(ARCH).deb
$(LIBXDAMAGE): $(SPREZZ)/libxdamage/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libxdamage_$(libxdamage_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libxfixes
libxfixes:$(LIBXFIXES)_$(ARCH).deb
$(LIBXFIXES): $(SPREZZ)/libxfixes/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libxfixes_$(libxfixes_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libxfont
libxfont:$(LIBXFONT)_$(ARCH).deb
$(LIBXFONT): $(SPREZZ)/libxfont/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libxfont_$(libxfont_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: xfonts-encodings
xfonts-encodings:$(XFONTSENCODINGS)_$(ARCH).deb
$(XFONTSENCODINGS): $(SPREZZ)/xfonts-encodings/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf encodings-$(xfonts-encodings_UPVER).tar.gz $(TARARGS) $@

.PHONY: libfcgi
libfcgi:$(LIBFCGI)_$(ARCH).deb
$(LIBFCGI): $(SPREZZ)/libfcgi/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf fcgi-$(libfcgi_UPVER).tar.gz $(TARARGS) $@

.PHONY: libfile-fcntllock-perl
libfile-fcntllock-perl:$(LIBFILEFCNTLLOCKPERL)_$(ARCH).deb
$(LIBFILEFCNTLLOCKPERL): $(SPREZZ)/libfile-fcntllock-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf File-FcntlLock-$(libfile-fcntllock-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libffi
libffi:$(LIBFFI)_$(ARCH).deb
$(LIBFFI): $(SPREZZ)/libffi/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libffi-$(libffi_UPVER).tar.gz $(TARARGS) $@

.PHONY: libfontenc
libfontenc:$(LIBFONTENC)_$(ARCH).deb
$(LIBFONTENC): $(SPREZZ)/libfontenc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libfontenc-$(libfontenc_UPVER).tar.gz $(TARARGS) $@

.PHONY: libfont-ttf-perl
libfont-ttf-perl:$(LIBFONTTTFPERL)_$(ARCH).deb
$(LIBFONTTTFPERL): $(SPREZZ)/libfont-ttf-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Font-TTF-$(libfont-ttf-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: liblocale-gettext-perl
liblocale-gettext-perl:$(LIBLOCALEGETTEXTPERL)_$(ARCH).deb
$(LIBLOCALEGETTEXTPERL): $(SPREZZ)/liblocale-gettext-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gettext-$(liblocale-gettext-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libxi
libxi:$(LIBXI)_$(ARCH).deb
$(LIBXI): $(SPREZZ)/libxi/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libXi-$(libxi_UPVER).tar.gz $(TARARGS) $@

.PHONY: libxdmcp
libxdmcp:$(LIBXDMCP)_$(ARCH).deb
$(LIBXDMCP): $(SPREZZ)/libxdmcp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libXdmcp-$(libxdmcp_UPVER).tar.gz $(TARARGS) $@

.PHONY: libxext
libxext:$(LIBXEXT)_$(ARCH).deb
$(LIBXEXT): $(SPREZZ)/libxext/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libXext-$(libxext_UPVER).tar.gz $(TARARGS) $@

.PHONY: libzrtpcpp
libzrtpcpp:$(LIBZRTPCPP)_$(ARCH).deb
$(LIBZRTPCPP): $(SPREZZ)/libzrtpcpp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libzrtpcpp-$(libzrtpcpp_UPVER).tar.gz $(TARARGS) $@

.PHONY: libxau
libxau:$(LIBXAU)_$(ARCH).deb
$(LIBXAU): $(SPREZZ)/libxau/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libXau-$(libxau_UPVER).tar.gz $(TARARGS) $@

.PHONY: libxaw
libxaw:$(LIBXAW)_$(ARCH).deb
$(LIBXAW): $(SPREZZ)/libxaw/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libXaw-$(libxaw_UPVER).tar.gz $(TARARGS) $@

.PHONY: libxmu
libxmu:$(LIBXMU)_$(ARCH).deb
$(LIBXMU): $(SPREZZ)/libxmu/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libXmu-$(libxmu_UPVER).tar.gz $(TARARGS) $@

.PHONY: libxpm
libxpm:$(LIBXPM)_$(ARCH).deb
$(LIBXPM): $(SPREZZ)/libxpm/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libXpm-$(libxpm_UPVER).tar.gz $(TARARGS) $@

.PHONY: libxspf
libxspf:$(LIBXSPF)_$(ARCH).deb
$(LIBXSPF): $(SPREZZ)/libxspf/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libxspf-$(libxspf_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libxt
libxt:$(LIBXT)_$(ARCH).deb
$(LIBXT): $(SPREZZ)/libxt/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libXt-$(libxt_UPVER).tar.gz $(TARARGS) $@

.PHONY: xvidcore
xvidcore:$(XVIDCORE)_$(ARCH).deb
$(XVIDCORE): $(SPREZZ)/xvidcore/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xvidcore-$(xvidcore_UPVER).tar.gz $(TARARGS) $@

.PHONY: xkeyboard-config
xkeyboard-config:$(XKEYBOARDCONFIG)_$(ARCH).deb
$(XKEYBOARDCONFIG): $(SPREZZ)/xkeyboard-config/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xkeyboard-config-$(xkeyboard-config_UPVER).tar.gz $(TARARGS) $@

.PHONY: libxkbcommon
libxkbcommon:$(LIBXKBCOMMON)_$(ARCH).deb
$(LIBXKBCOMMON): $(SPREZZ)/libxkbcommon/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libxkbcommon-xkbcommon-$(libxkbcommon_UPVER).tar.gz $(TARARGS) $@

.PHONY: libxkbfile
libxkbfile:$(LIBXKBFILE)_$(ARCH).deb
$(LIBXKBFILE): $(SPREZZ)/libxkbfile/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libxkbfile-$(libxkbfile_UPVER).tar.gz $(TARARGS) $@

.PHONY: libxklavier
libxklavier:$(LIBXKLAVIER)_$(ARCH).deb
$(LIBXKLAVIER): $(SPREZZ)/libxklavier/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libxklavier-$(libxklavier_UPVER).tar.xz $(TARARGS) $@

.PHONY: libxv
libxv:$(LIBXV)_$(ARCH).deb
$(LIBXV): $(SPREZZ)/libxv/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libXv-$(libxv_UPVER).tar.gz $(TARARGS) $@

.PHONY: libxvmc
libxvmc:$(LIBXVMC)_$(ARCH).deb
$(LIBXVMC): $(SPREZZ)/libxvmc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libXvMC-$(libxvmc_UPVER).tar.gz $(TARARGS) $@

.PHONY: libxxf86dga
libxxf86dga:$(libxxf86dga)_$(ARCH).deb
$(libxxf86dga): $(SPREZZ)/libxxf86dga/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libXxf86dga-$(libxxf86dga_UPVER).tar.gz $(TARARGS) $@

.PHONY: libxxf86vm
libxxf86vm:$(LIBXXF86VM)_$(ARCH).deb
$(LIBXXF86VM): $(SPREZZ)/libxxf86vm/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libXxf86vm-$(libxxf86vm_UPVER).tar.gz $(TARARGS) $@

.PHONY: libx11
libx11:$(LIBX11)_$(ARCH).deb
$(LIBX11): $(SPREZZ)/libx11/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libx11_$(libx11_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libx11-protocol-perl
libx11-protocol-perl:$(LIBX11PROTOCOLPERL)_$(ARCH).deb
$(LIBX11PROTOCOLPERL): $(SPREZZ)/libx11-protocol-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf X11-Protocol-$(libx11-protocol-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libx11-protocol-other-perl
libx11-protocol-other-perl:$(LIBX11PROTOCOLOTHERPERL)_$(ARCH).deb
$(LIBX11PROTOCOLOTHERPERL): $(SPREZZ)/libx11-protocol-other-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf X11-Protocol-Other-$(libx11-protocol-other-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: x11-session-utils
x11-session-utils:$(X11SESSIONUTILS)_$(ARCH).deb
$(X11SESSIONUTILS): $(SPREZZ)/x11-session-utils/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --watchfile debian/watch.xsm --force-download --download-current-version
	tar xzvf xsm-$(x11-session-utils_UPVER).tar.gz $(TARARGS) $@

.PHONY: libx86
libx86:$(LIBX86)_$(ARCH).deb
$(LIBX86): $(SPREZZ)/libx86/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libx86_$(libx86_UPVER).orig.tar.gz $(TARARGS) $@
	rm -rf $@/debian
	cp -r $(<D) $@/

.PHONY: libx86emu
libx86emu:$(LIBX86EMU)_$(ARCH).deb
$(LIBX86EMU): $(SPREZZ)/libx86emu/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libx86emu-$(libx86emu_UPVER).tar.gz $(TARARGS) $@

.PHONY: xfonts-scalable
xfonts-scalable:$(XFONTSSCALABLE)_$(ARCH).deb
$(XFONTSSCALABLE): $(SPREZZ)/xfonts-scalable/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf font-bitstream-type1-$(xfonts-scalable_UPVER).tar.gz $(TARARGS) $@

.PHONY: xfonts-terminus
xfonts-terminus:$(XFONTSTERMINUS)_$(ARCH).deb
$(XFONTSTERMINUS): $(SPREZZ)/xfonts-terminus/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf terminus-font-$(xfonts-terminus_UPVER).tar.gz $(TARARGS) $@

.PHONY: xcb-util
xcb-util:$(XCBUTIL)_$(ARCH).deb
$(XCBUTIL): $(SPREZZ)/xcb-util/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xcb-util_$(xcb-util_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: xcb-util-image
xcb-util-image:$(XCBUTILIMAGE)_$(ARCH).deb
$(XCBUTILIMAGE): $(SPREZZ)/xcb-util-image/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xcb-util-image-$(xcb-util-image_UPVER).tar.gz $(TARARGS) $@

.PHONY: xcb-util-keysyms
xcb-util-keysyms:$(XCBUTILKEYSYMS)_$(ARCH).deb
$(XCBUTILKEYSYMS): $(SPREZZ)/xcb-util-keysyms/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xcb-util-keysyms-$(xcb-util-keysyms_UPVER).tar.gz $(TARARGS) $@

.PHONY: xcb-util-renderutil
xcb-util-renderutil:$(XCBUTILRENDERUTIL)_$(ARCH).deb
$(XCBUTILRENDERUTIL): $(SPREZZ)/xcb-util-renderutil/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xcb-util-renderutil-$(xcb-util-renderutil_UPVER).tar.gz $(TARARGS) $@

.PHONY: xcb-util-wm
xcb-util-wm:$(XCBUTILWM)_$(ARCH).deb
$(XCBUTILWM): $(SPREZZ)/xcb-util-wm/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xcb-util-wm-$(xcb-util-wm_UPVER).tar.gz $(TARARGS) $@

.PHONY: libxcb
libxcb:$(LIBXCB)_$(ARCH).deb
$(LIBXCB): $(SPREZZ)/libxcb/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libxcb_$(libxcb_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libxdg-basedir
libxdg-basedir:$(LIBXDGBASEDIR)_$(ARCH).deb
$(LIBXDGBASEDIR): $(SPREZZ)/libxdg-basedir/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libxdg-basedir-$(libxdg-basedir_UPVER).tar.gz $(TARARGS) $@

.PHONY: xdg-utils
xdg-utils:$(XDGUTILS)_$(ARCH).deb
$(XDGUTILS): $(SPREZZ)/xdg-utils/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xdg-utils-$(xdg-utils_UPVER).tar.gz $(TARARGS) $@

.PHONY: libxml2
libxml2:$(LIBXML2)_$(ARCH).deb
$(LIBXML2): $(SPREZZ)/libxml2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libxml2_$(libxml2_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libxslt
libxslt:$(LIBXSLT)_$(ARCH).deb
$(LIBXSLT): $(SPREZZ)/libxslt/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libxslt-$(libxslt_UPVER).tar.gz $(TARARGS) $@

.PHONY: libxss
libxss:$(LIBXSS)_$(ARCH).deb
$(LIBXSS): $(SPREZZ)/libxss/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libXScrnSaver-$(libxss_UPVER).tar.gz $(TARARGS) $@

.PHONY: gmake
gmake:$(GMAKE)_$(ARCH).deb
$(GMAKE): $(SPREZZ)/gmake/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf make-$(gmake_UPVER).tar.gz $(TARARGS) $@

.PHONY: gmp
gmp:$(GMP)_$(ARCH).deb
$(GMP): $(SPREZZ)/gmp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gmp-$(gmp_UPVER).tar.xz $(TARARGS) $@

.PHONY: gmpc
gmpc:$(GMPC)_$(ARCH).deb
$(GMPC): $(SPREZZ)/gmpc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gmpc-$(gmpc_UPVER).tar.gz $(TARARGS) $@

.PHONY: gmpc-plugins
gmpc-plugins:$(GMPCPLUGINS)_$(ARCH).deb
$(GMPCPLUGINS): $(SPREZZ)/gmpc-plugins/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gmpc-plugins-$(gmpc_UPVER).tar.gz $(TARARGS) $@

.PHONY: grep
grep:$(GREP)_$(ARCH).deb
$(GREP): $(SPREZZ)/grep/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf grep-$(grep_UPVER).tar.xz $(TARARGS) $@

.PHONY: lightspark
lightspark:$(LIGHTSPARK)_$(ARCH).deb
$(LIGHTSPARK): $(SPREZZ)/lightspark/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf lightspark-$(lightspark_UPVER).tar.gz $(TARARGS) $@

.PHONY: lftp
lftp:$(LFTP)_$(ARCH).deb
$(LFTP): $(SPREZZ)/lftp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf lftp_$(lftp_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: mingetty
mingetty:$(MINGETTY)_$(ARCH).deb
$(MINGETTY): $(SPREZZ)/mingetty/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mingetty-$(mingetty_UPVER).tar.gz $(TARARGS) $@

.PHONY: minitube
minitube:$(MINITUBE)_$(ARCH).deb
$(MINITUBE): $(SPREZZ)/minitube/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf minitube-$(minitube_UPVER).tar.gz $(TARARGS) $@

.PHONY: tzdata
tzdata:$(TZDATA)_$(ARCH).deb
$(TZDATA): $(SPREZZ)/tzdata/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tzdata$(tzdata_UPVER).tar.gz -C $@

.PHONY: miniupnpc
miniupnpc:$(MINIUPNPC)_$(ARCH).deb
$(MINIUPNPC): $(SPREZZ)/miniupnpc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf miniupnpc-$(miniupnpc_UPVER).tar.gz $(TARARGS) $@

.PHONY: mm-common
mm-common:$(MMCOMMON)_$(ARCH).deb
$(MMCOMMON): $(SPREZZ)/mm-common/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mm-common-$(mm-common_UPVER).tar.gz $(TARARGS) $@

.PHONY: m4
m4:$(M4)_$(ARCH).deb
$(M4): $(SPREZZ)/m4/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf m4-$(m4_UPVER).tar.gz $(TARARGS) $@

.PHONY: man-db
man-db:$(MANDB)_$(ARCH).deb
$(MANDB): $(SPREZZ)/man-db/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf man-db-$(man-db_UPVER).tar.xz $(TARARGS) $@

.PHONY: make
make:$(MAKE)_$(ARCH).deb
$(MAKE): $(SPREZZ)/make/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf make-$(make_UPVER).tar.gz $(TARARGS) $@

.PHONY: manpages
manpages:$(MANPAGES)_$(ARCH).deb
$(MANPAGES): $(SPREZZ)/manpages/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf man-pages-$(manpages_UPVER).tar.gz $(TARARGS) $@

.PHONY: mash
mash:$(MASH)_$(ARCH).deb
$(MASH): $(SPREZZ)/mash/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mash_$(mash_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: mdadm
mdadm:$(MDADM)_$(ARCH).deb
$(MDADM): $(SPREZZ)/mdadm/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mdadm_$(mdadm_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: mdds
mdds:$(MDDS)_$(ARCH).deb
$(MDDS): $(SPREZZ)/mdds/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf mdds_$(mdds_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: meanwhile
meanwhile:$(MEANWHILE)_$(ARCH).deb
$(MEANWHILE): $(SPREZZ)/meanwhile/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf meanwhile-$(meanwhile_UPVER).tar.gz $(TARARGS) $@

.PHONY: mediawiki
mediawiki:$(MEDIAWIKI)_$(ARCH).deb
$(MEDIAWIKI): $(SPREZZ)/mediawiki/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mediawiki-$(mediawiki_UPVER).tar.gz $(TARARGS) $@

.PHONY: memcached
memcached:$(MEMCACHED)_$(ARCH).deb
$(MEMCACHED): $(SPREZZ)/memcached/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf memcached-$(memcached_UPVER).tar.gz $(TARARGS) $@

.PHONY: metacity
metacity:$(METACITY)_$(ARCH).deb
$(METACITY): $(SPREZZ)/metacity/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf metacity_$(metacity_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: mesa
mesa:$(MESA)_$(ARCH).deb
$(MESA): $(SPREZZ)/mesa/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mesa_$(mesa_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: mesa-demos
mesa-demos:$(MESADEMOS)_$(ARCH).deb
$(MESADEMOS): $(SPREZZ)/mesa-demos/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mesa-demos_$(mesa-demos_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: mlocate
mlocate:$(MLOCATE)_$(ARCH).deb
$(MLOCATE): $(SPREZZ)/mlocate/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf mlocate-$(mlocate_UPVER).tar.xz $(TARARGS) $@

.PHONY: mlt
mlt:$(MLT)_$(ARCH).deb
$(MLT): $(SPREZZ)/mlt/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mlt-$(mlt_UPVER).tar.gz $(TARARGS) $@

.PHONY: p11-kit
p11-kit:$(P11KIT)_$(ARCH).deb
$(P11KIT): $(SPREZZ)/p11-kit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf p11-kit-$(p11-kit_UPVER).tar.gz $(TARARGS) $@

.PHONY: penguintv
penguintv:$(PENGUINTV)_$(ARCH).deb
$(PENGUINTV): $(SPREZZ)/penguintv/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf penguintv_$(penguintv_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: photofilmstrip
photofilmstrip:$(photofilmstrip)_$(ARCH).deb
$(photofilmstrip): $(SPREZZ)/photofilmstrip/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf photofilmstrip-$(photofilmstrip_UPVER).tar.gz $(TARARGS) $@

.PHONY: php5
php5:$(PHP5)_$(ARCH).deb
$(PHP5): $(SPREZZ)/php5/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf php-$(php5_UPVER).tar.gz $(TARARGS) $@

.PHONY: ldb
ldb:$(LDB)_$(ARCH).deb
$(LDB): $(SPREZZ)/ldb/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ldb-$(ldb_UPVER).tar.gz $(TARARGS) $@

.PHONY: ldns
ldns:$(LDNS)_$(ARCH).deb
$(LDNS): $(SPREZZ)/ldns/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ldns-$(ldns_UPVER).tar.gz $(TARARGS) $@

.PHONY: logstalgia
logstalgia:$(LOGSTALGIA)_$(ARCH).deb
$(LOGSTALGIA): $(SPREZZ)/logstalgia/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf logstalgia-$(logstalgia_UPVER).tar.gz $(TARARGS) $@

.PHONY: moin
moin:$(MOIN)_$(ARCH).deb
$(MOIN): $(SPREZZ)/moin/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf moin-$(moin_UPVER).tar.gz $(TARARGS) $@

.PHONY: mon
mon:$(MON)_$(ARCH).deb
$(MON): $(SPREZZ)/mon/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mon_$(mon_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: mono
mono:$(MONO)_$(ARCH).deb
$(MONO): $(SPREZZ)/mono/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf mono-$(mono_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: mosh
mosh:$(MOSH)_$(ARCH).deb
$(MOSH): $(SPREZZ)/mosh/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mosh-$(mosh_UPVER).tar.gz $(TARARGS) $@
	rm -rf $@/debian
	cp -r $(<D) $@/

.PHONY: mp4v2
mp4v2:$(MP4V2)_$(ARCH).deb
$(MP4V2): $(SPREZZ)/mp4v2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf mp4v2_$(mp4v2_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: mpeg2dec
mpeg2dec:$(MPEG2DEC)_$(ARCH).deb
$(MPEG2DEC): $(SPREZZ)/mpeg2dec/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libmpeg2-$(mpeg2dec_UPVER).tar.gz $(TARARGS) $@

.PHONY: mpich2
mpich2:$(MPICH2)_$(ARCH).deb
$(MPICH2): $(SPREZZ)/mpich2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mpich-$(mpich2_UPVER).tar.gz $(TARARGS) $@

.PHONY: libmediainfo
libmediainfo:$(LIBMEDIAINFO)_$(ARCH).deb
$(LIBMEDIAINFO): $(SPREZZ)/libmediainfo/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libmediainfo_$(libmediainfo_UPVER).tar.gz $(TARARGS) $@
	rm -rf $@/debian
	cp -r $(<D) $@/

.PHONY: libmemcached
libmemcached:$(LIBMEMCACHED)_$(ARCH).deb
$(LIBMEMCACHED): $(SPREZZ)/libmemcached/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libmemcached-$(libmemcached_UPVER).tar.gz $(TARARGS) $@

.PHONY: libmicrohttpd
libmicrohttpd:$(LIBMICROHTTPD)_$(ARCH).deb
$(LIBMICROHTTPD): $(SPREZZ)/libmicrohttpd/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libmicrohttpd-$(libmicrohttpd_UPVER).tar.gz $(TARARGS) $@

.PHONY: libmpeg3
libmpeg3:$(LIBMPEG3)_$(ARCH).deb
$(LIBMPEG3): $(SPREZZ)/libmpeg3/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libmpeg3-$(libmpeg3_UPVER)-src.tar.bz2 $(TARARGS) $@

.PHONY: libmusicbrainz-discid-perl
libmusicbrainz-discid-perl:$(LIBMUSICBRAINZDISCIDPERL)_$(ARCH).deb
$(LIBMUSICBRAINZDISCIDPERL): $(SPREZZ)/libmusicbrainz-discid-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf MusicBrainz-DiscID-$(libmusicbrainz-discid-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: mpd
mpd:$(MPD)_$(ARCH).deb
$(MPD): $(SPREZZ)/mpd/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf mpd_$(mpd_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: mpfr4
mpfr4:$(MPFR4)_$(ARCH).deb
$(MPFR4): $(SPREZZ)/mpfr4/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mpfr-$(mpfr4_UPVER).tar.gz $(TARARGS) $@

.PHONY: mpg123
mpg123:$(MPG123)_$(ARCH).deb
$(MPG123): $(SPREZZ)/mpg123/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf mpg123-$(mpg123_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libmad
libmad:$(LIBMAD)_$(ARCH).deb
$(LIBMAD): $(SPREZZ)/libmad/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libmad-$(libmad_UPVER).tar.gz $(TARARGS) $@

.PHONY: libmms
libmms:$(LIBMMS)_$(ARCH).deb
$(LIBMMS): $(SPREZZ)/libmms/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libmms-$(libmms_UPVER).tar.gz $(TARARGS) $@

.PHONY: libmng
libmng:$(LIBMNG)_$(ARCH).deb
$(LIBMNG): $(SPREZZ)/libmng/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libmng-$(libmng_UPVER).tar.gz $(TARARGS) $@

.PHONY: libmnl
libmnl:$(LIBMNL)_$(ARCH).deb
$(LIBMNL): $(SPREZZ)/libmnl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libmnl-$(libmnl_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libmpd
libmpd:$(LIBMPD)_$(ARCH).deb
$(LIBMPD): $(SPREZZ)/libmpd/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libmpd-$(libmpd_UPVER).tar.gz $(TARARGS) $@

.PHONY: libmpdclient
libmpdclient:$(LIBMPDCLIENT)_$(ARCH).deb
$(LIBMPDCLIENT): $(SPREZZ)/libmpdclient/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libmpdclient-$(libmpdclient_UPVER).tar.gz $(TARARGS) $@

.PHONY: libmtp
libmtp:$(LIBMTP)_$(ARCH).deb
$(LIBMTP): $(SPREZZ)/libmtp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libmtp-$(libmtp_UPVER).tar.gz $(TARARGS) $@

.PHONY: mutt
mutt:$(MUTT)_$(ARCH).deb
$(MUTT): $(SPREZZ)/mutt/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mutt-$(mutt_UPVER).tar.gz $(TARARGS) $@

.PHONY: mutter
mutter:$(MUTTER)_$(ARCH).deb
$(MUTTER): $(SPREZZ)/mutter/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf mutter_$(mutter_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: nagios3
nagios3:$(NAGIOS3)_$(ARCH).deb
$(NAGIOS3): $(SPREZZ)/nagios3/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf nagios-$(nagios3_UPVER).tar.gz $(TARARGS) $@

.PHONY: nasm
nasm:$(NASM)_$(ARCH).deb
$(NASM): $(SPREZZ)/nasm/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf nasm-$(nasm_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: nautilus
nautilus:$(NAUTILUS)_$(ARCH).deb
$(NAUTILUS): $(SPREZZ)/nautilus/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf nautilus_$(nautilus_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: nautilus-sendto
nautilus-sendto:$(NAUTILUSSENDTO)_$(ARCH).deb
$(NAUTILUSSENDTO): $(SPREZZ)/nautilus-sendto/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf nautilus-sendto_$(nautilus-sendto_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: ncmpcpp
ncmpcpp:$(NCMPCPP)_$(ARCH).deb
$(NCMPCPP): $(SPREZZ)/ncmpcpp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf ncmpcpp_$(ncmpcpp_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: ncurses
ncurses:$(NCURSES)_$(ARCH).deb
$(NCURSES): $(SPREZZ)/ncurses/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ncurses_$(ncurses_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: neon
neon:$(NEON)_$(ARCH).deb
$(NEON): $(SPREZZ)/neon/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf neon27_$(neon_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: netcf
netcf:$(NETCF)_$(ARCH).deb
$(NETCF): $(SPREZZ)/netcf/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf netcf_$(netcf_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: nethack
nethack:$(NETHACK)_$(ARCH).deb
$(NETHACK): $(SPREZZ)/nethack/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf nethack_$(nethack_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: netsniff-ng
netsniff-ng:$(NETSNIFFNG)_$(ARCH).deb
$(NETSNIFFNG): $(SPREZZ)/netsniff-ng/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf netsniff-ng-$(netsniff-ng_UPVER).tar.gz $(TARARGS) $@

.PHONY: nettle
nettle:$(NETTLE)_$(ARCH).deb
$(NETTLE): $(SPREZZ)/nettle/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf nettle-$(nettle_UPVER).tar.gz $(TARARGS) $@

.PHONY: net-snmp
net-snmp:$(NETSNMP)_$(ARCH).deb
$(NETSNMP): $(SPREZZ)/net-snmp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf net-snmp-$(net-snmp_UPVER).tar.gz $(TARARGS) $@

.PHONY: network-manager
network-manager:$(NETWORKMANAGER)_$(ARCH).deb
$(NETWORKMANAGER): $(SPREZZ)/network-manager/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf NetworkManager-$(network-manager_UPVER).tar.xz $(TARARGS) $@

.PHONY: network-manager-applet
network-manager-applet:$(NETWORKMANAGERAPPLET)_$(ARCH).deb
$(NETWORKMANAGERAPPLET): $(SPREZZ)/network-manager-applet/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf network-manager-applet-$(network-manager-applet_UPVER).tar.xz $(TARARGS) $@

.PHONY: newsbeuter
newsbeuter:$(NEWSBEUTER)_$(ARCH).deb
$(NEWSBEUTER): $(SPREZZ)/newsbeuter/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf newsbeuter_$(newsbeuter_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: nginx
nginx:$(NGINX)_$(ARCH).deb
$(NGINX): $(SPREZZ)/nginx/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf nginx-$(nginx_UPVER).tar.gz $(TARARGS) $@

.PHONY: ntfs-3g
ntfs-3g:$(NTFS3G)_$(ARCH).deb
$(NTFS3G): $(SPREZZ)/ntfs-3g/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ntfs-3g_ntfsprogs-$(ntfs-3g_UPVER).tgz $(TARARGS) $@

.PHONY: nfacct
nfacct:$(NFACCT)_$(ARCH).deb
$(NFACCT): $(SPREZZ)/nfacct/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf nfacct-$(nfacct_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: nfs-utils
nfs-utils:$(NFSUTILS)_$(ARCH).deb
$(NFSUTILS): $(SPREZZ)/nfs-utils/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf nfs-utils_$(nfs-utils_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: libnetfilter-acct
libnetfilter-acct:$(LIBNETFILTERACCT)_$(ARCH).deb
$(LIBNETFILTERACCT): $(SPREZZ)/libnetfilter-acct/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libnetfilter_acct-$(libnetfilter-acct_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libnetfilter-cthelper
libnetfilter-cthelper:$(LIBNETFILTERCTHELPER)_$(ARCH).deb
$(LIBNETFILTERCTHELPER): $(SPREZZ)/libnetfilter-cthelper/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libnetfilter_cthelper-$(libnetfilter-cthelper_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libnetfilter-cttimeout
libnetfilter-cttimeout:$(LIBNETFILTERCTTIMEOUT)_$(ARCH).deb
$(LIBNETFILTERCTTIMEOUT): $(SPREZZ)/libnetfilter-cttimeout/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libnetfilter_cttimeout-$(libnetfilter-cttimeout_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libnetfilter-conntrack
libnetfilter-conntrack:$(LIBNETFILTERCONNTRACK)_$(ARCH).deb
$(LIBNETFILTERCONNTRACK): $(SPREZZ)/libnetfilter-conntrack/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libnetfilter_conntrack-$(libnetfilter-conntrack_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libnetfilter-queue
libnetfilter-queue:$(LIBNETFILTERQUEUE)_$(ARCH).deb
$(LIBNETFILTERQUEUE): $(SPREZZ)/libnetfilter-queue/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libnetfilter_queue-$(libnetfilter-queue_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libnfnetlink
libnfnetlink:$(LIBNFNETLINK)_$(ARCH).deb
$(LIBNFNETLINK): $(SPREZZ)/libnfnetlink/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libnfnetlink-$(libnfnetlink_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libnfs
libnfs:$(LIBNFS)_$(ARCH).deb
$(LIBNFS): $(SPREZZ)/libnfs/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libnfs-$(libnfs_UPVER).tar.gz $(TARARGS) $@

.PHONY: libnfsidmap
libnfsidmap:$(LIBNFSIDMAP)_$(ARCH).deb
$(LIBNFSIDMAP): $(SPREZZ)/libnfsidmap/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libnfsidmap-$(libnfsidmap_UPVER).tar.gz $(TARARGS) $@

.PHONY: libnice
libnice:$(LIBNICE)_$(ARCH).deb
$(LIBNICE): $(SPREZZ)/libnice/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libnice-$(libnice_UPVER).tar.gz $(TARARGS) $@

.PHONY: libnids
libnids:$(LIBNIDS)_$(ARCH).deb
$(LIBNIDS): $(SPREZZ)/libnids/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libnids-$(libnids_UPVER).tar.gz $(TARARGS) $@

.PHONY: libnl3
libnl3:$(LIBNL3)_$(ARCH).deb
$(LIBNL3): $(SPREZZ)/libnl3/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libnl-$(libnl3_UPVER).tar.gz $(TARARGS) $@

.PHONY: libnotify
libnotify:$(LIBNOTIFY)_$(ARCH).deb
$(LIBNOTIFY): $(SPREZZ)/libnotify/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libnotify-$(libnotify_UPVER).tar.xz $(TARARGS) $@

.PHONY: nmap
nmap:$(NMAP)_$(ARCH).deb
$(NMAP): $(SPREZZ)/nmap/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf nmap-$(nmap_UPVER).tgz $(TARARGS) $@

.PHONY: notify-osd
notify-osd:$(NOTIFYOSD)_$(ARCH).deb
$(NOTIFYOSD): $(SPREZZ)/notify-osd/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf notify-osd-$(notify-osd_UPVER).tar.gz $(TARARGS) $@

.PHONY: notification-daemon
notification-daemon:$(NOTIFICATIONDAEMON)_$(ARCH).deb
$(NOTIFICATIONDAEMON): $(SPREZZ)/notification-daemon/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf notification-daemon-$(notification-daemon_UPVER).tar.xz $(TARARGS) $@

.PHONY: nss
nss:$(NSS)_$(ARCH).deb
$(NSS): $(SPREZZ)/nss/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf nss-$(nss_UPVER).tar.gz $(TARARGS) $@

.PHONY: numactl
numactl:$(NUMACTL)_$(ARCH).deb
$(NUMACTL): $(SPREZZ)/numactl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf numactl-$(numactl_UPVER).tar.gz $(TARARGS) $@

.PHONY: nvidia-cuda-toolkit
nvidia-cuda-toolkit:$(NVIDIACUDATOOLKIT)_$(ARCH).deb
$(NVIDIACUDATOOLKIT): $(SPREZZ)/nvidia-cuda-toolkit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf nvidia-cuda-toolkit-$(nvidia-cuda-toolkit_UPVER).tar.gz $(TARARGS) $@

.PHONY: nvidia-graphics-drivers
nvidia-graphics-drivers:$(NVIDIAGRAPHICSDRIVERS)_$(ARCH).deb
$(NVIDIAGRAPHICSDRIVERS): $(SPREZZ)/nvidia-graphics-drivers/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	mv NVIDIA-Linux-*.run $@
	tar czvf $(NVIDIAGRAPHICSDRIVERS).orig.tar.gz $@ --exclude-vcs --exclude=debian

.PHONY: nvidia-kernel-dkms
nvidia-kernel-dkms:$(NVIDIAKERNELDKMS)_$(ARCH).deb
$(NVIDIAKERNELDKMS): $(SPREZZ)/nvidia-kernel-dkms/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	mv NVIDIA-Linux-*.run $@
	tar czvf $(NVIDIAKERNELDKMS).orig.tar.gz $@ --exclude-vcs --exclude=debian

.PHONY: nvidia-settings
nvidia-settings:$(NVIDIASETTINGS)_$(ARCH).deb
$(NVIDIASETTINGS): $(SPREZZ)/nvidia-settings/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf nvidia-settings-$(nvidia-settings_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: liboauth
liboauth:$(LIBOAUTH)_$(ARCH).deb
$(LIBOAUTH): $(SPREZZ)/liboauth/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf liboauth-$(liboauth_UPVER).tar.gz $(TARARGS) $@

.PHONY: libofa
libofa:$(LIBOFA)_$(ARCH).deb
$(LIBOFA): $(SPREZZ)/libofa/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libofa-$(libofa_UPVER).tar.gz $(TARARGS) $@

.PHONY: libopenraw
libopenraw:$(LIBOPENRAW)_$(ARCH).deb
$(LIBOPENRAW): $(SPREZZ)/libopenraw/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libopenraw-$(libopenraw_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libosinfo
libosinfo:$(LIBOSINFO)_$(ARCH).deb
$(LIBOSINFO): $(SPREZZ)/libosinfo/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libosinfo-$(libosinfo_UPVER).tar.gz $(TARARGS) $@

.PHONY: libonig
libonig:$(LIBONIG)_$(ARCH).deb
$(LIBONIG): $(SPREZZ)/libonig/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf onig-$(libonig_UPVER).tar.gz $(TARARGS) $@

.PHONY: libogg
libogg:$(LIBOGG)_$(ARCH).deb
$(LIBOGG): $(SPREZZ)/libogg/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libogg-$(libogg_UPVER).tar.xz $(TARARGS) $@

.PHONY: liboggz
liboggz:$(LIBOGGZ)_$(ARCH).deb
$(LIBOGGZ): $(SPREZZ)/liboggz/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf liboggz-$(liboggz_UPVER).tar.gz $(TARARGS) $@

.PHONY: libcmis
libcmis:$(LIBCMIS)_$(ARCH).deb
$(LIBCMIS): $(SPREZZ)/libcmis/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libcmis_$(libcmis_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libconfig-inifiles-perl
libconfig-inifiles-perl:$(LIBCONFIGINIFILESPERL)_$(ARCH).deb
$(LIBCONFIGINIFILESPERL): $(SPREZZ)/libconfig-inifiles-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Config-IniFiles-$(libconfig-inifiles-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libconfig-inihash-perl
libconfig-inihash-perl:$(LIBCONFIGINIHASHPERL)_$(ARCH).deb
$(LIBCONFIGINIHASHPERL): $(SPREZZ)/libconfig-inihash-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Config-IniHash-$(libconfig-inihash-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libvigraimpex
libvigraimpex:$(LIBVIGRAIMPEX)_$(ARCH).deb
$(LIBVIGRAIMPEX): $(SPREZZ)/libvigraimpex/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf vigra-$(libvigraimpex_UPVER)-src.tar.gz $(TARARGS) $@

.PHONY: libvisio
libvisio:$(LIBVISIO)_$(ARCH).deb
$(LIBVISIO): $(SPREZZ)/libvisio/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libvisio-$(libvisio_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libvisual
libvisual:$(LIBVISUAL)_$(ARCH).deb
$(LIBVISUAL): $(SPREZZ)/libvisual/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libvisual-$(libvisual_UPVER).tar.gz $(TARARGS) $@

.PHONY: libvisual-plugins
libvisual-plugins:$(LIBVISUALPLUGINS)_$(ARCH).deb
$(LIBVISUALPLUGINS): $(SPREZZ)/libvisual-plugins/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libvisual-plugins-$(libvisual-plugins_UPVER).tar.gz $(TARARGS) $@

.PHONY: libvorbis
libvorbis:$(LIBVORBIS)_$(ARCH).deb
$(LIBVORBIS): $(SPREZZ)/libvorbis/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libvorbis-$(libvorbis_UPVER).tar.xz $(TARARGS) $@

.PHONY: f-spot
f-spot:$(fspot)_$(ARCH).deb
$(fspot): $(SPREZZ)/f-spot/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf f-spot-$(f-spot_UPVER).tar.gz $(TARARGS) $@

.PHONY: flac
flac:$(FLAC)_$(ARCH).deb
$(FLAC): $(SPREZZ)/flac/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf flac-$(flac_UPVER).tar.gz $(TARARGS) $@

.PHONY: id3lib
id3lib:$(ID3LIB)_$(ARCH).deb
$(ID3LIB): $(SPREZZ)/id3lib/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf id3lib-$(id3lib_UPVER).tar.gz $(TARARGS) $@

.PHONY: libzapojit
libzapojit:$(LIBZAPOJIT)_$(ARCH).deb
$(LIBZAPOJIT): $(SPREZZ)/libzapojit/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libzapojit-$(libzapojit_UPVER).tar.xz $(TARARGS) $@

.PHONY: libopenobex
libopenobex:$(LIBOPENOBEX)_$(ARCH).deb
$(LIBOPENOBEX): $(SPREZZ)/libopenobex/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf openobex-$(libopenobex_UPVER)-Source.tar.gz $(TARARGS) $@

.PHONY: obexd
obexd:$(OBEXD)_$(ARCH).deb
$(OBEXD): $(SPREZZ)/obexd/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf obexd-$(obexd_UPVER).tar.gz $(TARARGS) $@

.PHONY: obex-data-server
obex-data-server:$(OBEXDATASERVER)_$(ARCH).deb
$(OBEXDATASERVER): $(SPREZZ)/obex-data-server/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf obex-data-server-$(obex-data-server_UPVER).tar.gz $(TARARGS) $@

.PHONY: obexfs
obexfs:$(OBEXFS)_$(ARCH).deb
$(OBEXFS): $(SPREZZ)/obexfs/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf obexfs-$(obexfs_UPVER).tar.gz $(TARARGS) $@

.PHONY: obexftp
obexftp:$(OBEXFTP)_$(ARCH).deb
$(OBEXFTP): $(SPREZZ)/obexftp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf obexftp-$(obexftp_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: obexpushd
obexpushd:$(OBEXPUSHD)_$(ARCH).deb
$(OBEXPUSHD): $(SPREZZ)/obexpushd/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf obexpushd-$(obexpushd_UPVER)-source.tar.gz $(TARARGS) $@

.PHONY: octave
octave:$(OCTAVE)_$(ARCH).deb
$(OCTAVE): $(SPREZZ)/octave/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf octave-$(octave_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: omphalos
omphalos:$(OMPHALOS)_$(ARCH).deb
$(OMPHALOS): $(SPREZZ)/omphalos/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf omphalos_$(omphalos_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: opal
opal:$(OPAL)_$(ARCH).deb
$(OPAL): $(SPREZZ)/opal/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf opal-$(opal_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: corosync
corosync:$(COROSYNC)_$(ARCH).deb
$(COROSYNC): $(SPREZZ)/corosync/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf corosync-$(corosync_UPVER).tar.gz $(TARARGS) $@

.PHONY: openais
openais:$(OPENAIS)_$(ARCH).deb
$(OPENAIS): $(SPREZZ)/openais/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf openais-$(openais_UPVER).tar.gz $(TARARGS) $@

.PHONY: openchange
openchange:$(OPENCHANGE)_$(ARCH).deb
$(OPENCHANGE): $(SPREZZ)/openchange/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf openchange-$(openchange_UPVER)-QUADRANT.tar.gz $(TARARGS) $@

.PHONY: opencore-amr
opencore-amr:$(OPENCOREAMR)_$(ARCH).deb
$(OPENCOREAMR): $(SPREZZ)/opencore-amr/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf opencore-amr-$(opencore-amr_UPVER).tar.gz $(TARARGS) $@

.PHONY: opencv
opencv:$(OPENCV)_$(ARCH).deb
$(OPENCV): $(SPREZZ)/opencv/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf opencv-$(opencv_UPVER).tar.gz $(TARARGS) $@

.PHONY: openipmi
openipmi:$(OPENIPMI)_$(ARCH).deb
$(OPENIPMI): $(SPREZZ)/openipmi/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf OpenIPMI-$(openipmi_UPVER).tar.gz $(TARARGS) $@

.PHONY: openmpi
openmpi:$(OPENMPI)_$(ARCH).deb
$(OPENMPI): $(SPREZZ)/openmpi/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf openmpi-$(openmpi_UPVER).tar.gz $(TARARGS) $@

.PHONY: openresolv
openresolv:$(OPENRESOLV)_$(ARCH).deb
$(OPENRESOLV): $(SPREZZ)/openresolv/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf openresolv-$(openresolv_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: openshot
openshot:$(OPENSHOT)_$(ARCH).deb
$(OPENSHOT): $(SPREZZ)/openshot/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf openshot-$(openshot_UPVER).tar.gz $(TARARGS) $@

.PHONY: orc
orc:$(ORC)_$(ARCH).deb
$(ORC): $(SPREZZ)/orc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf orc-$(orc_UPVER).tar.gz $(TARARGS) $@

.PHONY: enblend-enfuse
enblend-enfuse:$(ENBLENDENFUSE)_$(ARCH).deb
$(ENBLENDENFUSE): $(SPREZZ)/enblend-enfuse/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf enblend-enfuse-$(enblend-enfuse_UPVER).tar.gz $(TARARGS) $@

.PHONY: openexr
openexr:$(OPENEXR)_$(ARCH).deb
$(OPENEXR): $(SPREZZ)/openexr/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf openexr-$(openexr_UPVER).tar.gz $(TARARGS) $@

.PHONY: ilmbase
ilmbase:$(ILMBASE)_$(ARCH).deb
$(ILMBASE): $(SPREZZ)/ilmbase/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ilmbase-$(ilmbase_UPVER).tar.gz $(TARARGS) $@

.PHONY: isc-dhcp
isc-dhcp:$(ISCDHCP)_$(ARCH).deb
$(ISCDHCP): $(SPREZZ)/isc-dhcp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dhcp-$(isc-dhcp_UPVER).tar.gz $(TARARGS) $@

.PHONY: isomaster
isomaster:$(ISOMASTER)_$(ARCH).deb
$(ISOMASTER): $(SPREZZ)/isomaster/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf isomaster-$(isomaster_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: ivy
ivy:$(IVY)_$(ARCH).deb
$(IVY): $(SPREZZ)/ivy/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf apache-ivy-$(ivy_UPVER)-src.tar.gz $(TARARGS) $@

.PHONY: opencolorio
opencolorio:$(OPENCOLORIO)_$(ARCH).deb
$(OPENCOLORIO): $(SPREZZ)/opencolorio/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf v$(opencolorio_UPVER).tar.gz $(TARARGS) $@

.PHONY: opengtl
opengtl:$(OPENGTL)_$(ARCH).deb
$(OPENGTL): $(SPREZZ)/opengtl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf OpenGTL-$(opengtl_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: openimageio
openimageio:$(OPENIMAGEIO)_$(ARCH).deb
$(OPENIMAGEIO): $(SPREZZ)/openimageio/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Release-$(openimageio_UPVER).tar.gz $(TARARGS) $@

.PHONY: openjpeg
openjpeg:$(OPENJPEG2)_$(ARCH).deb
$(OPENJPEG2): $(SPREZZ)/openjpeg2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf openjpeg-$(openjpeg2_UPVER).tar.gz $(TARARGS) $@

.PHONY: openldap
openldap:$(OPENLDAP)_$(ARCH).deb
$(OPENLDAP): $(SPREZZ)/openldap/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf openldap_$(openldap_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: openslp
openslp:$(OPENSLP)_$(ARCH).deb
$(OPENSLP): $(SPREZZ)/openslp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf openslp-$(openslp_UPVER).tar.gz $(TARARGS) $@

.PHONY: openssl
openssl:$(OPENSSL)_$(ARCH).deb
$(OPENSSL): $(SPREZZ)/openssl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf openssl_$(openssl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: openssh
openssh:$(OPENSSH)_$(ARCH).deb
$(OPENSSH): $(SPREZZ)/openssh/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf openssh-$(openssh_UPVER).tar.gz $(TARARGS) $@

.PHONY: openvpn
openvpn:$(OPENVPN)_$(ARCH).deb
$(OPENVPN): $(SPREZZ)/openvpn/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf openvpn-$(openvpn_UPVER).tar.gz $(TARARGS) $@

.PHONY: opus
opus:$(OPUS)_$(ARCH).deb
$(OPUS): $(SPREZZ)/opus/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf opus-$(opus_UPVER).tar.gz $(TARARGS) $@

.PHONY: libpciaccess
libpciaccess:$(LIBPCIACCESS)_$(ARCH).deb
$(LIBPCIACCESS): $(SPREZZ)/libpciaccess/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf libpciaccess-$(libpciaccess_UPVER).tar.gz $(TARARGS) $@

.PHONY: libpthread-stubs
libpthread-stubs:$(LIBPTHREADSTUBS)_$(ARCH).deb
$(LIBPTHREADSTUBS): $(SPREZZ)/libpthread-stubs/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf libpthread-stubs-$(libpthread-stubs_UPVER).tar.gz $(TARARGS) $@

.PHONY: libquicktime
libquicktime:$(LIBQUICKTIME)_$(ARCH).deb
$(LIBQUICKTIME): $(SPREZZ)/libquicktime/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf libquicktime-$(libquicktime_UPVER).tar.gz $(TARARGS) $@

.PHONY: gparted
gparted:$(GPARTED)_$(ARCH).deb
$(GPARTED): $(SPREZZ)/gparted/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf gparted-$(gparted_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: parted
parted:$(PARTED)_$(ARCH).deb
$(PARTED): $(SPREZZ)/parted/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf parted-$(parted_UPVER).tar.xz $(TARARGS) $@

.PHONY: patch
patch:$(PATCH)_$(ARCH).deb
$(PATCH): $(SPREZZ)/patch/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf patch_$(patch_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: patchutils
patchutils:$(PATCHUTILS)_$(ARCH).deb
$(PATCHUTILS): $(SPREZZ)/patchutils/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf patchutils-$(patchutils_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: pciutils
pciutils:$(PCIUTILS)_$(ARCH).deb
$(PCIUTILS): $(SPREZZ)/pciutils/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf pciutils-$(pciutils_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: pcmciautils
pcmciautils:$(PCMCIAUTILS)_$(ARCH).deb
$(PCMCIAUTILS): $(SPREZZ)/pcmciautils/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf pcmciautils-$(pcmciautils_UPVER).tar.gz $(TARARGS) $@

.PHONY: pcre
pcre:$(PCRE)_$(ARCH).deb
$(PCRE): $(SPREZZ)/pcre/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf pcre-$(pcre_UPVER).tar.gz $(TARARGS) $@

.PHONY: pcsc-lite
pcsc-lite:$(PCSCLITE)_$(ARCH).deb
$(PCSCLITE): $(SPREZZ)/pcsc-lite/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf pcsc-lite-$(pcsc-lite_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: pdftk
pdftk:$(PDFTK)_$(ARCH).deb
$(PDFTK): $(SPREZZ)/pdftk/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf pdftk-$(pdftk_UPVER)-src.tar.gz $(TARARGS) $@

.PHONY: perl
perl:$(PERL)_$(ARCH).deb
$(PERL): $(SPREZZ)/perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf perl-$(perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: packagekit
packagekit:$(PACKAGEKIT)_$(ARCH).deb
$(PACKAGEKIT): $(SPREZZ)/packagekit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf packagekit_$(packagekit_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: ppl
ppl:$(PPL)_$(ARCH).deb
$(PPL): $(SPREZZ)/ppl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf ppl-$(ppl_UPVER).tar.xz $(TARARGS) $@

.PHONY: picard
picard:$(PICARD)_$(ARCH).deb
$(PICARD): $(SPREZZ)/picard/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf picard-$(picard_UPVER).tar.gz $(TARARGS) $@

.PHONY: pidgin
pidgin:$(PIDGIN)_$(ARCH).deb
$(PIDGIN): $(SPREZZ)/pidgin/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf pidgin-$(pidgin_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: pidgin-otr
pidgin-otr:$(PIDGINOTR)_$(ARCH).deb
$(PIDGINOTR): $(SPREZZ)/pidgin-otr/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pidgin-otr-$(pidgin-otr_UPVER).tar.gz $(TARARGS) $@

.PHONY: libotr
libotr:$(LIBOTR)_$(ARCH).deb
$(LIBOTR): $(SPREZZ)/libotr/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libotr-$(libotr_UPVER).tar.gz $(TARARGS) $@

.PHONY: pinentry
pinentry:$(PINENTRY)_$(ARCH).deb
$(PINENTRY): $(SPREZZ)/pinentry/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pinentry-$(pinentry_UPVER).tar.gz $(TARARGS) $@

.PHONY: pixman
pixman:$(PIXMAN)_$(ARCH).deb
$(PIXMAN): $(SPREZZ)/pixman/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pixman-$(pixman_UPVER).tar.gz $(TARARGS) $@

.PHONY: pkcs11-helper
pkcs11-helper:$(PKCS11HELPER)_$(ARCH).deb
$(PKCS11HELPER): $(SPREZZ)/pkcs11-helper/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pkcs11-helper-$(pkcs11-helper_UPVER).tar.gz $(TARARGS) $@

.PHONY: pkg-config
pkg-config:$(PKGCONFIG)_$(ARCH).deb
$(PKGCONFIG): $(SPREZZ)/pkg-config/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pkg-config-$(pkg-config_UPVER).tar.gz $(TARARGS) $@

.PHONY: libplist
libplist:$(LIBPLIST)_$(ARCH).deb
$(LIBPLIST): $(SPREZZ)/libplist/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf libplist-$(libplist_UPVER).tar.gz $(TARARGS) $@

.PHONY: policykit
policykit:$(POLICYKIT)_$(ARCH).deb
$(POLICYKIT): $(SPREZZ)/policykit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf polkit-$(policykit_UPVER).tar.gz $(TARARGS) $@

.PHONY: policykit-gnome
policykit-gnome:$(POLICYKITGNOME)_$(ARCH).deb
$(POLICYKITGNOME): $(SPREZZ)/policykit-gnome/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf polkit-gnome-$(policykit-gnome_UPVER).tar.xz $(TARARGS) $@

.PHONY: popt
popt:$(POPT)_$(ARCH).deb
$(POPT): $(SPREZZ)/popt/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf popt-$(popt_UPVER).tar.gz $(TARARGS) $@

.PHONY: postfix
postfix:$(POSTFIX)_$(ARCH).deb
$(POSTFIX): $(SPREZZ)/postfix/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf postfix-$(postfix_UPVER).tar.gz $(TARARGS) $@

.PHONY: postgresql
postgresql:$(POSTGRESQL)_$(ARCH).deb
$(POSTGRESQL): $(SPREZZ)/postgresql/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf postgresql-$(postgresql_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: powerman
powerman:$(POWERMAN)_$(ARCH).deb
$(POWERMAN): $(SPREZZ)/powerman/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf powerman-$(powerman_UPVER).tar.gz $(TARARGS) $@

.PHONY: powertop
powertop:$(POWERTOP)_$(ARCH).deb
$(POWERTOP): $(SPREZZ)/powertop/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf powertop-$(powertop_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: procmail
procmail:$(PROCMAIL)_$(ARCH).deb
$(PROCMAIL): $(SPREZZ)/procmail/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf procmail-$(procmail_UPVER).tar.gz $(TARARGS) $@

.PHONY: procps
procps:$(PROCPS)_$(ARCH).deb
$(PROCPS): $(SPREZZ)/procps/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf procps-$(procps_UPVER).tar.xz $(TARARGS) $@

.PHONY: protobuf
protobuf:$(PROTOBUF)_$(ARCH).deb
$(PROTOBUF): $(SPREZZ)/protobuf/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf protobuf-$(protobuf_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: ptlib
ptlib:$(PTLIB)_$(ARCH).deb
$(PTLIB): $(SPREZZ)/ptlib/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf ptlib-$(ptlib_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: pycairo
pycairo:$(PYCAIRO)_$(ARCH).deb
$(PYCAIRO): $(SPREZZ)/pycairo/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pycairo-$(pycairo_UPVER).tar.gz $(TARARGS) $@

.PHONY: py3cairo
py3cairo:$(PY3CAIRO)_$(ARCH).deb
$(PY3CAIRO): $(SPREZZ)/py3cairo/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf pycairo-$(py3cairo_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: pycurl
pycurl:$(PYCURL)_$(ARCH).deb
$(PYCURL): $(SPREZZ)/pycurl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pycurl-$(pycurl_UPVER).tar.gz $(TARARGS) $@

.PHONY: pygobject
pygobject:$(PYGOBJECT)_$(ARCH).deb
$(PYGOBJECT): $(SPREZZ)/pygobject/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf pygobject-$(pygobject_UPVER).tar.xz $(TARARGS) $@

.PHONY: pygtk
pygtk:$(PYGTK)_$(ARCH).deb
$(PYGTK): $(SPREZZ)/pygtk/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf pygtk-$(pygtk_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: pyrex
pyrex:$(PYREX)_$(ARCH).deb
$(PYREX): $(SPREZZ)/pyrex/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Pyrex-$(pyrex_UPVER).tar.gz $(TARARGS) $@

.PHONY: python2.6
python2.6:$(PYTHON2.6)_$(ARCH).deb
$(PYTHON2.6): $(SPREZZ)/python2.6/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Python-$(python2.6_UPVER).tgz $(TARARGS) $@

.PHONY: python2.7
python2.7:$(PYTHON2.7)_$(ARCH).deb
$(PYTHON2.7): $(SPREZZ)/python2.7/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Python-$(python2.7_UPVER).tgz $(TARARGS) $@

.PHONY: python3.3
python3.3:$(PYTHON3.3)_$(ARCH).deb
$(PYTHON3.3): $(SPREZZ)/python3.3/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf Python-$(python3.3_UPVER).tar.xz $(TARARGS) $@

.PHONY: python-coverage
python-coverage:$(PYTHONCOVERAGE)_$(ARCH).deb
$(PYTHONCOVERAGE): $(SPREZZ)/python-coverage/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf python-coverage_$(python-coverage_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: python-docutils
python-docutils:$(PYTHONDOCUTILS)_$(ARCH).deb
$(PYTHONDOCUTILS): $(SPREZZ)/python-docutils/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf python-docutils-$(python-docutils_UPVER).tar.gz $(TARARGS) $@

.PHONY: python-gnutls
python-gnutls:$(PYTHONGNUTLS)_$(ARCH).deb
$(PYTHONGNUTLS): $(SPREZZ)/python-gnutls/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf python-gnutls-$(python-gnutls_UPVER).tar.gz $(TARARGS) $@

.PHONY: python-numpy
python-numpy:$(PYTHONNUMPY)_$(ARCH).deb
$(PYTHONNUMPY): $(SPREZZ)/python-numpy/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf $(PYTHONNUMPY).orig.tar.gz $(TARARGS) $@

.PHONY: python-qt4
python-qt4:$(PYTHONQT4)_$(ARCH).deb
$(PYTHONQT4): $(SPREZZ)/python-qt4/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf PyQt-x11-gpl-$(python-qt4_UPVER).tar.gz $(TARARGS) $@

.PHONY: newt
newt:$(NEWT)_$(ARCH).deb
$(NEWT): $(SPREZZ)/newt/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf newt-$(newt_UPVER).tar.gz $(TARARGS) $@

.PHONY: notify-python
notify-python:$(NOTIFYPYTHON)_$(ARCH).deb
$(NOTIFYPYTHON): $(SPREZZ)/notify-python/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf notify-python-$(notify-python_UPVER).tar.gz $(TARARGS) $@

.PHONY: qemu-kvm
qemu-kvm:$(QEMUKVM)_$(ARCH).deb
$(QEMUKVM): $(SPREZZ)/qemu-kvm/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf qemu-kvm-$(qemu-kvm_UPVER).tar.gz $(TARARGS) $@

.PHONY: qemu-system
qemu-system:$(QEMUSYSTEM)_$(ARCH).deb
$(QEMUSYSTEM): $(SPREZZ)/qemu-system/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf qemu-system-$(qemu-system_UPVER).tar.gz $(TARARGS) $@

.PHONY: qpdf
qpdf:$(QPDF)_$(ARCH).deb
$(QPDF): $(SPREZZ)/qpdf/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf qpdf-$(qpdf_UPVER).tar.gz $(TARARGS) $@

.PHONY: qrencode
qrencode:$(QRENCODE)_$(ARCH).deb
$(QRENCODE): $(SPREZZ)/qrencode/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf qrencode-$(qrencode_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: quilt
quilt:$(QUILT)_$(ARCH).deb
$(QUILT): $(SPREZZ)/quilt/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf quilt-$(quilt_UPVER).tar.gz $(TARARGS) $@

.PHONY: ratpoison
ratpoison:$(RATPOISON)_$(ARCH).deb
$(RATPOISON): $(SPREZZ)/ratpoison/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ratpoison-$(ratpoison_UPVER).tar.gz $(TARARGS) $@

.PHONY: reiserfsprogs
reiserfsprogs:$(REISERFSPROGS)_$(ARCH).deb
$(REISERFSPROGS): $(SPREZZ)/reiserfsprogs/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xjvf reiserfsprogs-$(reiserfsprogs_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: remotecontrol
remotecontrol:$(REMOTECONTROL)_$(ARCH).deb
$(REMOTECONTROL): $(SPREZZ)/remotecontrol/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf remotecontrol-$(remotecontrol_UPVER).tar.gz $(TARARGS) $@

.PHONY: ripit
ripit:$(RIPIT)_$(ARCH).deb
$(RIPIT): $(SPREZZ)/ripit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ripit-$(ripit_UPVER).tar.gz $(TARARGS) $@

.PHONY: rrdtool
rrdtool:$(RRDTOOL)_$(ARCH).deb
$(RRDTOOL): $(SPREZZ)/rrdtool/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf rrdtool-$(rrdtool_UPVER).tar.gz $(TARARGS) $@

.PHONY: rsync
rsync:$(RSYNC)_$(ARCH).deb
$(RSYNC): $(SPREZZ)/rsync/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf rsync-$(rsync_UPVER).tar.gz $(TARARGS) $@

.PHONY: rsyslog
rsyslog:$(RSYSLOG)_$(ARCH).deb
$(RSYSLOG): $(SPREZZ)/rsyslog/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf rsyslog-$(rsyslog_UPVER).tar.gz $(TARARGS) $@

.PHONY: rubberband
rubberband:$(RUBBERBAND)_$(ARCH).deb
$(RUBBERBAND): $(SPREZZ)/rubberband/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf rubberband-$(rubberband_UPVER).tar.gz $(TARARGS) $@

.PHONY: libraw
libraw:$(LIBRAW)_$(ARCH).deb
$(LIBRAW): $(SPREZZ)/libraw/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf LibRaw-$(libraw_UPVER).tar.gz $(TARARGS) $@

.PHONY: librdmacm
librdmacm:$(LIBRDMACM)_$(ARCH).deb
$(LIBRDMACM): $(SPREZZ)/librdmacm/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf librdmacm-$(librdmacm_UPVER).tar.gz $(TARARGS) $@

.PHONY: librelp
librelp:$(LIBRELP)_$(ARCH).deb
$(LIBRELP): $(SPREZZ)/librelp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf librelp-$(librelp_UPVER).tar.gz $(TARARGS) $@

.PHONY: libreadonly-xs-perl
libreadonly-xs-perl:$(LIBREADONLYXSPERL)_$(ARCH).deb
$(LIBREADONLYXSPERL): $(SPREZZ)/libreadonly-xs-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf Readonly-XS-$(libreadonly-xs-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libreoffice
libreoffice:$(LIBREOFFICE)_$(ARCH).deb
$(LIBREOFFICE): $(SPREZZ)/libreoffice/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf libreoffice-$(libreoffice_UPVER).tar.gz $(TARARGS) $@

.PHONY: librsvg
librsvg:$(LIBRSVG)_$(ARCH).deb
$(LIBRSVG): $(SPREZZ)/librsvg/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xJvf librsvg-$(librsvg_UPVER).tar.xz $(TARARGS) $@

.PHONY: libgssapi-perl
libgssapi-perl:$(LIBGSSAPIPERL)_$(ARCH).deb
$(LIBGSSAPIPERL): $(SPREZZ)/libgssapi-perl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf GSSAPI-$(libgssapi-perl_UPVER).tar.gz $(TARARGS) $@

.PHONY: libgssglue
libgssglue:$(LIBGSSGLUE)_$(ARCH).deb
$(LIBGSSGLUE): $(SPREZZ)/libgssglue/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf libgssglue-$(libgssglue_UPVER).tar.gz $(TARARGS) $@

.PHONY: librpcsecgss
librpcsecgss:$(LIBRPCSECGSS)_$(ARCH).deb
$(LIBRPCSECGSS): $(SPREZZ)/librpcsecgss/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf librpcsecgss-$(librpcsecgss_UPVER).tar.gz $(TARARGS) $@

.PHONY: rails
rails:$(RAILS)_$(ARCH).deb
$(RAILS): $(SPREZZ)/rails/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf rails-$(rails_UPVER).tar.gz $(TARARGS) $@

.PHONY: raptor2
raptor2:$(RAPTOR2)_$(ARCH).deb
$(RAPTOR2): $(SPREZZ)/raptor2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf raptor2-$(raptor2_UPVER).tar.gz $(TARARGS) $@

.PHONY: rasqal
rasqal:$(RASQAL)_$(ARCH).deb
$(RASQAL): $(SPREZZ)/rasqal/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf rasqal-$(rasqal_UPVER).tar.gz $(TARARGS) $@

.PHONY: rawstudio
rawstudio:$(RAWSTUDIO)_$(ARCH).deb
$(RAWSTUDIO): $(SPREZZ)/rawstudio/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf rawstudio-$(rawstudio_UPVER).tar.gz $(TARARGS) $@

.PHONY: razor
razor:$(RAZOR)_$(ARCH).deb
$(RAZOR): $(SPREZZ)/razor/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf razor-agents-$(razor_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: razorqt
razorqt:$(RAZORQT)_$(ARCH).deb
$(RAZORQT): $(SPREZZ)/razorqt/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf razorqt-$(razorqt_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: readline6
readline6:$(READLINE6)_$(ARCH).deb
$(READLINE6): $(SPREZZ)/readline6/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf readline-$(readline6_UPVER).tar.gz $(TARARGS) $@

.PHONY: reaver
reaver:$(REAVER)_$(ARCH).deb
$(REAVER): $(SPREZZ)/reaver/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf reaver-$(reaver_UPVER).tar.gz $(TARARGS) $@

.PHONY: redland
redland:$(REDLAND)_$(ARCH).deb
$(REDLAND): $(SPREZZ)/redland/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf redland-$(redland_UPVER).tar.gz $(TARARGS) $@

.PHONY: redland-bindings
redland-bindings:$(REDLANDBINDINGS)_$(ARCH).deb
$(REDLANDBINDINGS): $(SPREZZ)/redland-bindings/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf redland-bindings-$(redland-bindings_UPVER).tar.gz $(TARARGS) $@

.PHONY: rpcbind
rpcbind:$(rpcbind)_$(ARCH).deb
$(rpcbind): $(SPREZZ)/rpcbind/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf rpcbind-$(rpcbind_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: rpm
rpm:$(RPM)_$(ARCH).deb
$(RPM): $(SPREZZ)/rpm/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf rpm-$(rpm_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: rtaudio
rtaudio:$(RTAUDIO)_$(ARCH).deb
$(RTAUDIO): $(SPREZZ)/rtaudio/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf rtaudio-$(rtaudio_UPVER).tar.gz $(TARARGS) $@

.PHONY: rtmidi
rtmidi:$(RTMIDI)_$(ARCH).deb
$(RTMIDI): $(SPREZZ)/rtmidi/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf rtmidi-$(rtmidi_UPVER).tar.gz $(TARARGS) $@

.PHONY: rtmpdump
rtmpdump:$(RTMPDUMP)_$(ARCH).deb
$(RTMPDUMP): $(SPREZZ)/rtmpdump/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf rtmpdump-$(rtmpdump_UPVER).tar.gz $(TARARGS) $@

.PHONY: rt-tests
rt-tests:$(RTTESTS)_$(ARCH).deb
$(RTTESTS): $(SPREZZ)/rt-tests/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf rt-tests-$(rt-tests_UPVER).tar.gz $(TARARGS) $@

.PHONY: rtorrent
rtorrent:$(RTORRENT)_$(ARCH).deb
$(RTORRENT): $(SPREZZ)/rtorrent/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf rtorrent-$(rtorrent_UPVER).tar.gz $(TARARGS) $@

.PHONY: rygel
rygel:$(RYGEL)_$(ARCH).deb
$(RYGEL): $(SPREZZ)/rygel/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf rygel-$(rygel_UPVER).tar.xz $(TARARGS) $@

.PHONY: sshfp
sshfp:$(SSHFP)_$(ARCH).deb
$(SSHFP): $(SPREZZ)/sshfp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf $(sshfp_UPVER).tar.gz $(TARARGS) $@

.PHONY: samba
samba:$(SAMBA)_$(ARCH).deb
$(SAMBA): $(SPREZZ)/samba/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf samba-$(samba_UPVER).tar.gz $(TARARGS) $@

.PHONY: samba4
samba4:$(SAMBA4)_$(ARCH).deb
$(SAMBA4): $(SPREZZ)/samba4/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf samba-$(samba4_UPVER).tar.gz $(TARARGS) $@

.PHONY: sanlock
sanlock:$(SANLOCK)_$(ARCH).deb
$(SANLOCK): $(SPREZZ)/sanlock/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf sanlock-$(sanlock_UPVER).tar.gz $(TARARGS) $@

.PHONY: sbc
sbc:$(SBC)_$(ARCH).deb
$(SBC): $(SPREZZ)/sbc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf sbc-$(sbc_UPVER).tar.gz $(TARARGS) $@

.PHONY: snes9x
snes9x:$(SNES9X)_$(ARCH).deb
$(SNES9X): $(SPREZZ)/snes9x/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf snes9x-$(snes9x_UPVER)-src.tar.bz2 $(TARARGS) $@

.PHONY: sqlite3
sqlite3:$(SQLITE3)_$(ARCH).deb
$(SQLITE3): $(SPREZZ)/sqlite3/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf sqlite3_$(sqlite3_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: scorched3d
scorched3d:$(SCORCHED3D)_$(ARCH).deb
$(SCORCHED3D): $(SPREZZ)/scorched3d/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Scorched3D-$(scorched3d_UPVER)-src.tar.gz $(TARARGS) $@

.PHONY: schroedinger
schroedinger:$(SCHROEDINGER)_$(ARCH).deb
$(SCHROEDINGER): $(SPREZZ)/schroedinger/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf schroedinger-$(schroedinger_UPVER).tar.gz $(TARARGS) $@

.PHONY: scowl
scowl:$(SCOWL)_$(ARCH).deb
$(SCOWL): $(SPREZZ)/scowl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf scowl-$(scowl_UPVER).tar.gz $(TARARGS) $@

.PHONY: screen
screen:$(SCREEN)_$(ARCH).deb
$(SCREEN): $(SPREZZ)/screen/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf screen_$(screen_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: screenlets
screenlets:$(SCREENLETS)_$(ARCH).deb
$(SCREENLETS): $(SPREZZ)/screenlets/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf screenlets-$(screenlets_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: scribus
scribus:$(SCRIBUS)_$(ARCH).deb
$(SCRIBUS): $(SPREZZ)/scribus/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf scribus-$(scribus_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: scrub
scrub:$(SCRUB)_$(ARCH).deb
$(SCRUB): $(SPREZZ)/scrub/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf scrub-$(scrub_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: seahorse
seahorse:$(SEAHORSE)_$(ARCH).deb
$(SEAHORSE): $(SPREZZ)/seahorse/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf seahorse-$(seahorse_UPVER).tar.xz $(TARARGS) $@

.PHONY: sed
sed:$(SED)_$(ARCH).deb
$(SED): $(SPREZZ)/sed/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf sed-$(sed_UPVER).tar.gz $(TARARGS) $@

.PHONY: shadow
shadow:$(SHADOW)_$(ARCH).deb
$(SHADOW): $(SPREZZ)/shadow/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf shadow-$(shadow_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: sheepdog
sheepdog:$(SHEEPDOG)_$(ARCH).deb
$(SHEEPDOG): $(SPREZZ)/sheepdog/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf sheepdog_$(sheepdog_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: shotwell
shotwell:$(SHOTWELL)_$(ARCH).deb
$(SHOTWELL): $(SPREZZ)/shotwell/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf shotwell_$(shotwell_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: shutter
shutter:$(SHUTTER)_$(ARCH).deb
$(SHUTTER): $(SPREZZ)/shutter/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf shutter_$(shutter_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: silgraphite
silgraphite:$(SILGRAPHITE)_$(ARCH).deb
$(SILGRAPHITE): $(SPREZZ)/silgraphite/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf graphite2-$(silgraphite_UPVER).tgz $(TARARGS) $@

.PHONY: sip4
sip4:$(SIP4)_$(ARCH).deb
$(SIP4): $(SPREZZ)/sip4/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf sip-$(sip4_UPVER).tar.gz $(TARARGS) $@

.PHONY: spacefm
spacefm:$(SPACEFM)_$(ARCH).deb
$(SPACEFM): $(SPREZZ)/spacefm/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf spacefm-$(spacefm_UPVER).tar.xz $(TARARGS) $@

.PHONY: spamassassin
spamassassin:$(SPAMASSASSIN)_$(ARCH).deb
$(SPAMASSASSIN): $(SPREZZ)/spamassassin/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf Mail-SpamAssassin-$(spamassassin_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: speech-tools
speech-tools:$(SPEECHTOOLS)_$(ARCH).deb
$(SPEECHTOOLS): $(SPREZZ)/speech-tools/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf speech-tools_$(speech-tools_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: speex
speex:$(SPEEX)_$(ARCH).deb
$(SPEEX): $(SPREZZ)/speex/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf $(SPEEX).orig.tar.gz $(TARARGS) $@

.PHONY: pocketsphinx
pocketsphinx:$(POCKETSPHINX)_$(ARCH).deb
$(POCKETSPHINX): $(SPREZZ)/pocketsphinx/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pocketsphinx-$(pocketsphinx_UPVER).tar.gz $(TARARGS) $@

.PHONY: sphinx
sphinx:$(SPHINX)_$(ARCH).deb
$(SPHINX): $(SPREZZ)/sphinx/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Sphinx-$(sphinx_UPVER).tar.gz $(TARARGS) $@

.PHONY: sphinxbase
sphinxbase:$(SPHINXBASE)_$(ARCH).deb
$(SPHINXBASE): $(SPREZZ)/sphinxbase/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf sphinxbase-$(sphinxbase_UPVER).tar.gz $(TARARGS) $@

.PHONY: spice-gtk
spice-gtk:$(SPICEGTK)_$(ARCH).deb
$(SPICEGTK): $(SPREZZ)/spice-gtk/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf spice-gtk-$(spice-gtk_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: startup-notification
startup-notification:$(STARTUPNOTIFICATION)_$(ARCH).deb
$(STARTUPNOTIFICATION): $(SPREZZ)/startup-notification/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf startup-notification-$(startup-notification_UPVER).tar.gz $(TARARGS) $@

.PHONY: stk
stk:$(STK)_$(ARCH).deb
$(STK): $(SPREZZ)/stk/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf stk-$(stk_UPVER).tar.gz $(TARARGS) $@

.PHONY: sysfsutils
sysfsutils:$(SYSFSUTILS)_$(ARCH).deb
$(SYSFSUTILS): $(SPREZZ)/sysfsutils/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf sysfsutils-$(sysfsutils_UPVER).tar.gz $(TARARGS) $@

.PHONY: syslinux
syslinux:$(SYSLINUX)_$(ARCH).deb
$(SYSLINUX): $(SPREZZ)/syslinux/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf syslinux-$(syslinux_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: systemd
systemd:$(SYSTEMD)_$(ARCH).deb
$(SYSTEMD): $(SPREZZ)/systemd/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf systemd_$(systemd_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: system-config-printer
system-config-printer:$(SYSTEMCONFIGPRINTER)_$(ARCH).deb
$(SYSTEMCONFIGPRINTER): $(SPREZZ)/system-config-printer/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf system-config-printer-$(system-config-printer_UPVER).tar.xz $(TARARGS) $@

.PHONY: unbound
unbound:$(UNBOUND)_$(ARCH).deb
$(UNBOUND): $(SPREZZ)/unbound/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf unbound-$(unbound_UPVER).tar.gz $(TARARGS) $@

.PHONY: util-linux
util-linux:$(UTILLINUX)_$(ARCH).deb
$(UTILLINUX): $(SPREZZ)/util-linux/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf util-linux-$(util-linux_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: tbb
tbb:$(TBB)_$(ARCH).deb
$(TBB): $(SPREZZ)/tbb/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tbb_$(tbb_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: tdb
tdb:$(TDB)_$(ARCH).deb
$(TDB): $(SPREZZ)/tdb/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tdb-$(tdb_UPVER).tar.gz $(TARARGS) $@

.PHONY: telepathy-farstream
telepathy-farstream:$(TELEPATHYFARSTREAM)_$(ARCH).deb
$(TELEPATHYFARSTREAM): $(SPREZZ)/telepathy-farstream/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf telepathy-farstream-$(telepathy-farstream_UPVER).tar.gz $(TARARGS) $@

.PHONY: telepathy-gabble
telepathy-gabble:$(TELEPATHYGABBLE)_$(ARCH).deb
$(TELEPATHYGABBLE): $(SPREZZ)/telepathy-gabble/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf telepathy-gabble-$(telepathy-gabble_UPVER).tar.gz $(TARARGS) $@

.PHONY: telepathy-glib
telepathy-glib:$(TELEPATHYGLIB)_$(ARCH).deb
$(TELEPATHYGLIB): $(SPREZZ)/telepathy-glib/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf telepathy-glib-$(telepathy-glib_UPVER).tar.gz $(TARARGS) $@

.PHONY: telepathy-qt
telepathy-qt:$(TELEPATHYQT)_$(ARCH).deb
$(TELEPATHYQT): $(SPREZZ)/telepathy-qt/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf telepathy-qt-$(telepathy-qt_UPVER).tar.gz $(TARARGS) $@

.PHONY: tevent
tevent:$(TEVENT)_$(ARCH).deb
$(TEVENT): $(SPREZZ)/tevent/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tevent-$(tevent_UPVER).tar.gz $(TARARGS) $@

.PHONY: tiff3
tiff3:$(TIFF3)_$(ARCH).deb
$(TIFF3): $(SPREZZ)/tiff3/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tiff-$(tiff3_UPVER).tar.gz $(TARARGS) $@

.PHONY: tiff4
tiff4:$(TIFF4)_$(ARCH).deb
$(TIFF4): $(SPREZZ)/tiff4/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tiff-$(tiff4_UPVER).tar.gz $(TARARGS) $@

.PHONY: time
time:$(TIME)_$(ARCH).deb
$(TIME): $(SPREZZ)/time/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf time-$(time_UPVER).tar.gz $(TARARGS) $@

.PHONY: tinycdb
tinycdb:$(TINYCDB)_$(ARCH).deb
$(TINYCDB): $(SPREZZ)/tinycdb/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tinycdb-$(tinycdb_UPVER).tar.gz $(TARARGS) $@

.PHONY: totem-pl-parser
totem-pl-parser:$(TOTEMPLPARSER)_$(ARCH).deb
$(TOTEMPLPARSER): $(SPREZZ)/totem-pl-parser/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf totem-pl-parser-$(totem-pl-parser_UPVER).tar.xz $(TARARGS) $@

.PHONY: tokyocabinet
tokyocabinet:$(TOKYOCABINET)_$(ARCH).deb
$(TOKYOCABINET): $(SPREZZ)/tokyocabinet/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tokyocabinet-$(tokyocabinet_UPVER).tar.gz $(TARARGS) $@

.PHONY: tracker
tracker:$(TRACKER)_$(ARCH).deb
$(TRACKER): $(SPREZZ)/tracker/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf tracker_$(tracker_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: trousers
trousers:$(TROUSERS)_$(ARCH).deb
$(TROUSERS): $(SPREZZ)/trousers/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf trousers-$(trousers_UPVER).tar.gz $(TARARGS) $@

.PHONY: tulip
tulip:$(TULIP)_$(ARCH).deb
$(TULIP): $(SPREZZ)/tulip/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tulip-$(tulip_UPVER)-src.tar.gz $(TARARGS) $@

.PHONY: tvtime
tvtime:$(TVTIME)_$(ARCH).deb
$(TVTIME): $(SPREZZ)/tvtime/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tvtime-$(tvtime_UPVER).tar.gz $(TARARGS) $@

.PHONY: twisted
twisted:$(TWISTED)_$(ARCH).deb
$(TWISTED): $(SPREZZ)/twisted/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf TwistedCore-$(twisted_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: twolame
twolame:$(TWOLAME)_$(ARCH).deb
$(TWOLAME): $(SPREZZ)/twolame/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf twolame-$(twolame_UPVER).tar.gz $(TARARGS) $@

.PHONY: uwsgi
uwsgi:$(UWSGI)_$(ARCH).deb
$(UWSGI): $(SPREZZ)/uwsgi/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf uwsgi-$(uwsgi_UPVER).tar.gz $(TARARGS) $@

.PHONY: libv8
libv8:$(LIBV8)_$(ARCH).deb
$(LIBV8): $(SPREZZ)/libv8/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libv8_$(libv8_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: valgrind
valgrind:$(VALGRIND)_$(ARCH).deb
$(VALGRIND): $(SPREZZ)/valgrind/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf valgrind-$(valgrind_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: vamp-plugin-sdk
vamp-plugin-sdk:$(VAMPPLUGINSDK)_$(ARCH).deb
$(VAMPPLUGINSDK): $(SPREZZ)/vamp-plugin-sdk/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf vamp-plugin-sdk-$(vamp-plugin-sdk_UPVER).tar.gz $(TARARGS) $@

.PHONY: v4l-utils
v4l-utils:$(V4LUTILS)_$(ARCH).deb
$(V4LUTILS): $(SPREZZ)/v4l-utils/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf v4l-utils-$(v4l-utils_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: vcdimager
vcdimager:$(VCDIMAGER)_$(ARCH).deb
$(VCDIMAGER): $(SPREZZ)/vcdimager/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf vcdimager-$(vcdimager_UPVER).tar.gz $(TARARGS) $@

.PHONY: vim
vim:$(VIM)_$(ARCH).deb
$(VIM): $(SPREZZ)/vim/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	{ cd $@ && TARBALL=`uscan --force-download --download-current-version --dehs | xmlstarlet sel -t -v //upstream-version` && \
		cd - && tar xjvf vim-$$TARBALL.tar.bz2 $(TARARGS) $@ ; }


.PHONY: vinagre
vinagre:$(VINAGRE)_$(ARCH).deb
$(VINAGRE): $(SPREZZ)/vinagre/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf vinagre_$(vinagre_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: virtualbox
virtualbox:$(VIRTUALBOX)_$(ARCH).deb
$(VIRTUALBOX): $(SPREZZ)/virtualbox/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf virtualbox_$(virtualbox_UPVER).orig.tar.bz2 $(TARARGS) $@
	rm -rf $@/debian
	cp -r $(<D) $@/

.PHONY: virtuoso-opensource
virtuoso-opensource:$(VIRTUOSOOPENSOURCE)_$(ARCH).deb
$(VIRTUOSOOPENSOURCE): $(SPREZZ)/virtuoso-opensource/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf virtuoso-opensource-$(virtuoso-opensource_UPVER).tar.gz $(TARARGS) $@

.PHONY: vo-aacenc
vo-aacenc:$(VOAACENC)_$(ARCH).deb
$(VOAACENC): $(SPREZZ)/vo-aacenc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf vo-aacenc-$(vo-aacenc_UPVER).tar.gz $(TARARGS) $@

.PHONY: vo-amrwbenc
vo-amrwbenc:$(VOAMRWBENC)_$(ARCH).deb
$(VOAMRWBENC): $(SPREZZ)/vo-amrwbenc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf vo-amrwbenc-$(vo-amrwbenc_UPVER).tar.gz $(TARARGS) $@

.PHONY: wavpack
wavpack:$(WAVPACK)_$(ARCH).deb
$(WAVPACK): $(SPREZZ)/wavpack/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf wavpack-$(wavpack_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: wayland
wayland:$(WAYLAND)_$(ARCH).deb
$(WAYLAND): $(SPREZZ)/wayland/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf wayland-$(wayland_UPVER).tar.xz $(TARARGS) $@

.PHONY: w3m
w3m:$(W3M)_$(ARCH).deb
$(W3M): $(SPREZZ)/w3m/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf w3m-$(w3m_UPVER).tar.gz $(TARARGS) $@

.PHONY: libwebp
libwebp:$(LIBWEBP)_$(ARCH).deb
$(LIBWEBP): $(SPREZZ)/libwebp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libwebp-$(libwebp_UPVER).tar.gz $(TARARGS) $@

.PHONY: libwibble
libwibble:$(LIBWIBBLE)_$(ARCH).deb
$(LIBWIBBLE): $(SPREZZ)/libwibble/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libwibble-$(libwibble_UPVER).tar.gz $(TARARGS) $@

.PHONY: wget
wget:$(WGET)_$(ARCH).deb
$(WGET): $(SPREZZ)/wget/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf wget_$(wget_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: wicd
wicd:$(WICD)_$(ARCH).deb
$(WICD): $(SPREZZ)/wicd/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf wicd-$(wicd_UPVER).tar.gz $(TARARGS) $@

.PHONY: wifite
wifite:$(WIFITE)_$(ARCH).deb
$(WIFITE): $(SPREZZ)/wifite/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf wifite-$(wifite_UPVER).tar.gz $(TARARGS) $@

.PHONY: wireless-tools
wireless-tools:$(WIRELESSTOOLS)_$(ARCH).deb
$(WIRELESSTOOLS): $(SPREZZ)/wireless-tools/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf wireless-tools_$(wireless-tools_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: wireshark
wireshark:$(WIRESHARK)_$(ARCH).deb
$(WIRESHARK): $(SPREZZ)/wireshark/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf wireshark_$(wireshark_UPVER).orig.tar.bz2 $(TARARGS) $@
	rm -rf $@/debian
	cp -r $(<D) $@/

.PHONY: wpa
wpa:$(WPA)_$(ARCH).deb
$(WPA): $(SPREZZ)/wpa/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf wpa_$(wpa_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: x11-xkb-utils
x11-xkb-utils:$(X11XKBUTILS)_$(ARCH).deb
$(X11XKBUTILS): $(SPREZZ)/x11-xkb-utils/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xproto-$(x11-xkb-utils_UPVER).tar.gz $(TARARGS) $@

.PHONY: x11proto-bigreqs
x11proto-bigreqs:$(X11PROTOBIGREQS)_$(ARCH).deb
$(X11PROTOBIGREQS): $(SPREZZ)/x11proto-bigreqs/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bigreqsproto-$(x11proto-bigreqs_UPVER).tar.gz $(TARARGS) $@

.PHONY: x11proto-core
x11proto-core:$(X11PROTOCORE)_$(ARCH).deb
$(X11PROTOCORE): $(SPREZZ)/x11proto-core/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xproto-$(x11proto-core_UPVER).tar.gz $(TARARGS) $@

.PHONY: x11proto-damage
x11proto-damage:$(X11PROTODAMAGE)_$(ARCH).deb
$(X11PROTODAMAGE): $(SPREZZ)/x11proto-damage/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf damageproto-$(x11proto-damage_UPVER).tar.gz $(TARARGS) $@

.PHONY: x11proto-dmx
x11proto-dmx:$(X11PROTODMX)_$(ARCH).deb
$(X11PROTODMX): $(SPREZZ)/x11proto-dmx/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dmxproto-$(x11proto-dmx_UPVER).tar.gz $(TARARGS) $@

.PHONY: x11proto-fixes
x11proto-fixes:$(X11PROTOFIXES)_$(ARCH).deb
$(X11PROTOFIXES): $(SPREZZ)/x11proto-fixes/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf fixesproto-$(x11proto-fixes_UPVER).tar.gz $(TARARGS) $@

.PHONY: x11proto-fonts
x11proto-fonts:$(X11PROTOFONTS)_$(ARCH).deb
$(X11PROTOFONTS): $(SPREZZ)/x11proto-fonts/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf fontsproto-$(x11proto-fonts_UPVER).tar.gz $(TARARGS) $@

.PHONY: x11proto-gl
x11proto-gl:$(X11PROTOGL)_$(ARCH).deb
$(X11PROTOGL): $(SPREZZ)/x11proto-gl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf glproto-$(x11proto-gl_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: x11proto-input
x11proto-input:$(X11PROTOINPUT)_$(ARCH).deb
$(X11PROTOINPUT): $(SPREZZ)/x11proto-input/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf inputproto-$(x11proto-input_UPVER).tar.gz $(TARARGS) $@

.PHONY: x11proto-kb
x11proto-kb:$(X11PROTOKB)_$(ARCH).deb
$(X11PROTOKB): $(SPREZZ)/x11proto-kb/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf kbproto-$(x11proto-kb_UPVER).tar.gz $(TARARGS) $@

.PHONY: x11proto-print
x11proto-print:$(X11PROTOPRINT)_$(ARCH).deb
$(X11PROTOPRINT): $(SPREZZ)/x11proto-print/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf printproto-$(x11proto-print_UPVER).tar.gz $(TARARGS) $@

.PHONY: x11proto-randr
x11proto-randr:$(X11PROTORANDR)_$(ARCH).deb
$(X11PROTORANDR): $(SPREZZ)/x11proto-randr/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf randrproto-$(x11proto-randr_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: x11proto-record
x11proto-record:$(X11PROTORECORD)_$(ARCH).deb
$(X11PROTORECORD): $(SPREZZ)/x11proto-record/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf recordproto-$(x11proto-record_UPVER).tar.gz $(TARARGS) $@

.PHONY: x11proto-render
x11proto-render:$(X11PROTORENDER)_$(ARCH).deb
$(X11PROTORENDER): $(SPREZZ)/x11proto-render/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf renderproto-$(x11proto-render_UPVER).tar.gz $(TARARGS) $@

.PHONY: x11proto-resource
x11proto-resource:$(X11PROTORESOURCE)_$(ARCH).deb
$(X11PROTORESOURCE): $(SPREZZ)/x11proto-resource/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf resourceproto-$(x11proto-resource_UPVER).tar.gz $(TARARGS) $@

.PHONY: x11proto-scrnsaver
x11proto-scrnsaver:$(X11PROTOSCRNSAVER)_$(ARCH).deb
$(X11PROTOSCRNSAVER): $(SPREZZ)/x11proto-scrnsaver/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf scrnsaverproto-$(x11proto-scrnsaver_UPVER).tar.gz $(TARARGS) $@

.PHONY: x11proto-video
x11proto-video:$(X11PROTOVIDEO)_$(ARCH).deb
$(X11PROTOVIDEO): $(SPREZZ)/x11proto-video/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf videoproto-$(x11proto-video_UPVER).tar.gz $(TARARGS) $@

.PHONY: x11proto-xcmisc
x11proto-xcmisc:$(X11PROTOXCMISC)_$(ARCH).deb
$(X11PROTOXCMISC): $(SPREZZ)/x11proto-xcmisc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xcmiscproto-$(x11proto-xcmisc_UPVER).tar.gz $(TARARGS) $@

.PHONY: x11proto-xext
x11proto-xext:$(X11PROTOXEXT)_$(ARCH).deb
$(X11PROTOXEXT): $(SPREZZ)/x11proto-xext/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xextproto-$(x11proto-xext_UPVER).tar.gz $(TARARGS) $@

.PHONY: x11proto-xf86dga
x11proto-xf86dga:$(X11PROTOXF86DGA)_$(ARCH).deb
$(X11PROTOXF86DGA): $(SPREZZ)/x11proto-xf86dga/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xf86dgaproto-$(x11proto-xf86dga_UPVER).tar.gz $(TARARGS) $@

.PHONY: x11proto-xf86dri
x11proto-xf86dri:$(X11PROTOXF86DRI)_$(ARCH).deb
$(X11PROTOXF86DRI): $(SPREZZ)/x11proto-xf86dri/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xf86driproto-$(x11proto-xf86dri_UPVER).tar.gz $(TARARGS) $@

.PHONY: x11proto-xf86bigfont
x11proto-xf86bigfont:$(X11PROTOXF86BIGFONT)_$(ARCH).deb
$(X11PROTOXF86BIGFONT): $(SPREZZ)/x11proto-xf86bigfont/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xf86bigfontproto-$(x11proto-xf86bigfont_UPVER).tar.gz $(TARARGS) $@

.PHONY: x11proto-xf86vidmode
x11proto-xf86vidmode:$(X11PROTOXF86VIDMODE)_$(ARCH).deb
$(X11PROTOXF86VIDMODE): $(SPREZZ)/x11proto-xf86vidmode/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xf86vidmodeproto-$(x11proto-xf86vidmode_UPVER).tar.gz $(TARARGS) $@

.PHONY: x11proto-xinerama
x11proto-xinerama:$(X11PROTOXINERAMA)_$(ARCH).deb
$(X11PROTOXINERAMA): $(SPREZZ)/x11proto-xinerama/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xineramaproto-$(x11proto-xinerama_UPVER).tar.gz $(TARARGS) $@

.PHONY: xcb-proto
xcb-proto:$(XCBPROTO)_$(ARCH).deb
$(XCBPROTO): $(SPREZZ)/xcb-proto/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xcb-proto_$(xcb-proto_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: xine-lib
xine-lib:$(XINELIB)_$(ARCH).deb
$(XINELIB): $(SPREZZ)/xine-lib/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf xine-lib-$(xine-lib_UPVER).tar.xz $(TARARGS) $@

.PHONY: xqilla
xqilla:$(xqilla)_$(ARCH).deb
$(xqilla): $(SPREZZ)/xqilla/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf XQilla-$(xqilla_UPVER).tar.gz $(TARARGS) $@

.PHONY: libxinerama
libxinerama:$(LIBXINERAMA)_$(ARCH).deb
$(LIBXINERAMA): $(SPREZZ)/libxinerama/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libXinerama-$(libxinerama_UPVER).tar.gz $(TARARGS) $@

.PHONY: libxcursor
libxcursor:$(LIBXCURSOR)_$(ARCH).deb
$(LIBXCURSOR): $(SPREZZ)/libxcursor/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libXcursor-$(libxcursor_UPVER).tar.gz $(TARARGS) $@

.PHONY: libxrandr
libxrandr:$(LIBXRANDR)_$(ARCH).deb
$(LIBXRANDR): $(SPREZZ)/libxrandr/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libXrandr-$(libxrandr_UPVER).tar.gz $(TARARGS) $@

.PHONY: libxrender
libxrender:$(LIBXRENDER)_$(ARCH).deb
$(LIBXRENDER): $(SPREZZ)/libxrender/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libXrender-$(libxrender_UPVER).tar.gz $(TARARGS) $@

.PHONY: libxres
libxres:$(LIBXRES)_$(ARCH).deb
$(LIBXRES): $(SPREZZ)/libxres/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libXres-$(libxres_UPVER).tar.gz $(TARARGS) $@

.PHONY: xinit
xinit:$(XINIT)_$(ARCH).deb
$(XINIT): $(SPREZZ)/xinit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xinit-$(xinit_UPVER).tar.gz $(TARARGS) $@

.PHONY: xinput
xinput:$(XINPUT)_$(ARCH).deb
$(XINPUT): $(SPREZZ)/xinput/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xinput-$(xinput_UPVER).tar.gz $(TARARGS) $@

.PHONY: xmlstarlet
xmlstarlet:$(XMLSTARLET)_$(ARCH).deb
$(XMLSTARLET): $(SPREZZ)/xmlstarlet/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xmlstarlet-$(xmlstarlet_UPVER).tar.gz $(TARARGS) $@

.PHONY: xmlto
xmlto:$(XMLTO)_$(ARCH).deb
$(XMLTO): $(SPREZZ)/xmlto/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf xmlto-$(xmlto_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: xmms2
xmms2:$(XMMS2)_$(ARCH).deb
$(XMMS2): $(SPREZZ)/xmms2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf xmms2_$(xmms2_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: xorg
xorg:$(XORG)_$(ARCH).deb
$(XORG): $(SPREZZ)/xorg/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf xorg-server-$(xorg_UPVER).tar.xz $(TARARGS) $@

.PHONY: xorg-sgml-doctools
xorg-sgml-doctools:$(XORGSGMLDOCTOOLS)_$(ARCH).deb
$(XORGSGMLDOCTOOLS): $(SPREZZ)/xorg-sgml-doctools/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xorg-sgml-doctools-$(xorg-sgml-doctools_UPVER).tar.gz $(TARARGS) $@

.PHONY: xorg-xserver
xorg-xserver:$(XORGXSERVER)_$(ARCH).deb
$(XORGXSERVER): $(SPREZZ)/xorg-xserver/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xorg-server-$(xorg-xserver_UPVER).tar.gz $(TARARGS) $@

.PHONY: xosd
xosd:$(XOSD)_$(ARCH).deb
$(XOSD): $(SPREZZ)/xosd/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xosd-$(xosd_UPVER).tar.gz $(TARARGS) $@

.PHONY: xserver-xorg-input-evdev
xserver-xorg-input-evdev:$(XSERVERXORGINPUTEVDEV)_$(ARCH).deb
$(XSERVERXORGINPUTEVDEV): $(SPREZZ)/xserver-xorg-input-evdev/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xf86-input-evdev-$(xserver-xorg-input-evdev_UPVER).tar.gz $(TARARGS) $@

.PHONY: xserver-xorg-input-keyboard
xserver-xorg-input-keyboard:$(XSERVERXORGINPUTKEYBOARD)_$(ARCH).deb
$(XSERVERXORGINPUTKEYBOARD): $(SPREZZ)/xserver-xorg-input-keyboard/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xf86-input-keyboard-$(xserver-xorg-input-keyboard_UPVER).tar.gz $(TARARGS) $@

.PHONY: xserver-xorg-input-mouse
xserver-xorg-input-mouse:$(XSERVERXORGINPUTMOUSE)_$(ARCH).deb
$(XSERVERXORGINPUTMOUSE): $(SPREZZ)/xserver-xorg-input-mouse/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xf86-input-mouse-$(xserver-xorg-input-mouse_UPVER).tar.gz $(TARARGS) $@

.PHONY: xserver-xorg-input-wacom
xserver-xorg-input-wacom:$(XSERVERXORGINPUTWACOM)_$(ARCH).deb
$(XSERVERXORGINPUTWACOM): $(SPREZZ)/xserver-xorg-input-wacom/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf xf86-input-wacom-$(xserver-xorg-input-wacom_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: xserver-xorg-video-ati
xserver-xorg-video-ati:$(XSERVERXORGVIDEOATI)_$(ARCH).deb
$(XSERVERXORGVIDEOATI): $(SPREZZ)/xserver-xorg-video-ati/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xf86-video-ati-$(xserver-xorg-video-ati_UPVER).tar.gz $(TARARGS) $@

.PHONY: xserver-xorg-video-cirrus
xserver-xorg-video-cirrus:$(XSERVERXORGVIDEOCIRRUS)_$(ARCH).deb
$(XSERVERXORGVIDEOCIRRUS): $(SPREZZ)/xserver-xorg-video-cirrus/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xf86-video-cirrus-$(xserver-xorg-video-cirrus_UPVER).tar.gz $(TARARGS) $@

.PHONY: xserver-xorg-video-dummy
xserver-xorg-video-dummy:$(XSERVERXORGVIDEODUMMY)_$(ARCH).deb
$(XSERVERXORGVIDEODUMMY): $(SPREZZ)/xserver-xorg-video-dummy/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xf86-video-dummy-$(xserver-xorg-video-dummy_UPVER).tar.gz $(TARARGS) $@

.PHONY: xserver-xorg-video-fbdev
xserver-xorg-video-fbdev:$(XSERVERXORGVIDEOFBDEV)_$(ARCH).deb
$(XSERVERXORGVIDEOFBDEV): $(SPREZZ)/xserver-xorg-video-fbdev/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xf86-video-fbdev-$(xserver-xorg-video-fbdev_UPVER).tar.gz $(TARARGS) $@

.PHONY: xserver-xorg-video-geode
xserver-xorg-video-geode:$(XSERVERXORGVIDEOGEODE)_$(ARCH).deb
$(XSERVERXORGVIDEOGEODE): $(SPREZZ)/xserver-xorg-video-geode/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xf86-video-geode-$(xserver-xorg-video-geode_UPVER).tar.gz $(TARARGS) $@

.PHONY: xserver-xorg-video-intel
xserver-xorg-video-intel:$(XSERVERXORGVIDEOINTEL)_$(ARCH).deb
$(XSERVERXORGVIDEOINTEL): $(SPREZZ)/xserver-xorg-video-intel/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xf86-video-intel-$(xserver-xorg-video-intel_UPVER).tar.gz $(TARARGS) $@

.PHONY: xserver-xorg-video-r128
xserver-xorg-video-r128:$(XSERVERXORGVIDEOR128)_$(ARCH).deb
$(XSERVERXORGVIDEOR128): $(SPREZZ)/xserver-xorg-video-r128/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xf86-video-r128-$(xserver-xorg-video-r128_UPVER).tar.gz $(TARARGS) $@

.PHONY: xserver-xorg-video-mach64
xserver-xorg-video-mach64:$(XSERVERXORGVIDEOMACH64)_$(ARCH).deb
$(XSERVERXORGVIDEOMACH64): $(SPREZZ)/xserver-xorg-video-mach64/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xf86-video-mach64-$(xserver-xorg-video-mach64_UPVER).tar.gz $(TARARGS) $@

.PHONY: xserver-xorg-video-modesetting
xserver-xorg-video-modesetting:$(XSERVERXORGVIDEOMODESETTING)_$(ARCH).deb
$(XSERVERXORGVIDEOMODESETTING): $(SPREZZ)/xserver-xorg-video-modesetting/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xf86-video-modesetting-$(xserver-xorg-video-modesetting_UPVER).tar.gz $(TARARGS) $@

.PHONY: xserver-xorg-video-nouveau
xserver-xorg-video-nouveau:$(XSERVERXORGVIDEONOUVEAU)_$(ARCH).deb
$(XSERVERXORGVIDEONOUVEAU): $(SPREZZ)/xserver-xorg-video-nouveau/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xf86-video-nouveau-$(xserver-xorg-video-nouveau_UPVER).tar.gz $(TARARGS) $@

.PHONY: xserver-xorg-video-vesa
xserver-xorg-video-vesa:$(XSERVERXORGVIDEOVESA)_$(ARCH).deb
$(XSERVERXORGVIDEOVESA): $(SPREZZ)/xserver-xorg-video-vesa/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xf86-video-vesa-$(xserver-xorg-video-vesa_UPVER).tar.gz $(TARARGS) $@

.PHONY: xserver-xorg-video-vmware
xserver-xorg-video-vmware:$(XSERVERXORGVIDEOVMWARE)_$(ARCH).deb
$(XSERVERXORGVIDEOVMWARE): $(SPREZZ)/xserver-xorg-video-vmware/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xf86-video-vmware-$(xserver-xorg-video-vmware_UPVER).tar.gz $(TARARGS) $@

.PHONY: xterm
xterm:$(XTERM)_$(ARCH).deb
$(XTERM): $(SPREZZ)/xterm/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xterm-$(xterm_UPVER).tgz $(TARARGS) $@

.PHONY: xtermcontrol
xtermcontrol:$(XTERMCONTROL)_$(ARCH).deb
$(XTERMCONTROL): $(SPREZZ)/xtermcontrol/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xtermcontrol-$(xtermcontrol_UPVER).tar.gz $(TARARGS) $@

.PHONY: xtrace
xtrace:$(XTRACE)_$(ARCH).deb
$(XTRACE): $(SPREZZ)/xtrace/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xtrace_$(xtrace_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: xtrans
xtrans:$(XTRANS)_$(ARCH).deb
$(XTRANS): $(SPREZZ)/xtrans/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xtrans_$(xtrans_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: xz-utils
xz-utils:$(XZUTILS)_$(ARCH).deb
$(XZUTILS): $(SPREZZ)/xz-utils/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xz-utils_$(xz-utils_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: cclib
cclib:$(CCLIB)_$(ARCH).deb
$(CCLIB): $(SPREZZ)/cclib/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cclib-$(cclib_UPVER).tar.gz $(TARARGS) $@

.PHONY: libunique3
libunique3:$(LIBUNIQUE3)_$(ARCH).deb
$(LIBUNIQUE3): $(SPREZZ)/libunique3/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libunique-$(libunique3_UPVER).tar.xz $(TARARGS) $@

.PHONY: libupnp
libupnp:$(LIBUPNP)_$(ARCH).deb
$(LIBUPNP): $(SPREZZ)/libupnp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libupnp-$(libupnp_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libusbx
libusbx:$(LIBUSBX)_$(ARCH).deb
$(LIBUSBX): $(SPREZZ)/libusbx/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libusbx-$(libusbx_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: udisks2
udisks2:$(UDISKS2)_$(ARCH).deb
$(UDISKS2): $(SPREZZ)/udisks2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf udisks-$(udisks2_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: ustr
ustr:$(USTR)_$(ARCH).deb
$(USTR): $(SPREZZ)/ustr/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ustr-$(ustr_UPVER).tar.gz $(TARARGS) $@

.PHONY: yasm
yasm:$(YASM)_$(ARCH).deb
$(YASM): $(SPREZZ)/yasm/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf yasm-$(yasm_UPVER).tar.gz $(TARARGS) $@

.PHONY: yelp
yelp:$(YELP)_$(ARCH).deb
$(YELP): $(SPREZZ)/yelp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf yelp-$(yelp_UPVER).tar.xz $(TARARGS) $@

.PHONY: yelp-tools
yelp-tools:$(YELPTOOLS)_$(ARCH).deb
$(YELPTOOLS): $(SPREZZ)/yelp-tools/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf yelp-tools-$(yelp-tools_UPVER).tar.xz $(TARARGS) $@

.PHONY: yelp-xsl
yelp-xsl:$(YELPXSL)_$(ARCH).deb
$(YELPXSL): $(SPREZZ)/yelp-xsl/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf yelp-xsl-$(yelp-xsl_UPVER).tar.xz $(TARARGS) $@

.PHONY: yum
yum:$(YUM)_$(ARCH).deb
$(YUM): $(SPREZZ)/yum/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf yum-$(yum_UPVER).tar.gz $(TARARGS) $@

.PHONY: zerofree
zerofree:$(ZEROFREE)_$(ARCH).deb
$(ZEROFREE): $(SPREZZ)/zerofree/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf zerofree-$(zerofree_UPVER).tgz $(TARARGS) $@

.PHONY: zenity
zenity:$(ZENITY)_$(ARCH).deb
$(ZENITY): $(SPREZZ)/zenity/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf zenity-$(zenity_UPVER).tar.xz $(TARARGS) $@

.PHONY: unzip
unzip:$(unzip)_$(ARCH).deb
$(unzip): $(SPREZZ)/unzip/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf unzip_$(unzip_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: zip
zip:$(ZIP)_$(ARCH).deb
$(ZIP): $(SPREZZ)/zip/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf zip_$(zip_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: zlib
zlib:$(ZLIB)_$(ARCH).deb
$(ZLIB): $(SPREZZ)/zlib/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf zlib-$(zlib_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: zookeeper
zookeeper:$(ZOOKEEPER)_$(ARCH).deb
$(ZOOKEEPER): $(SPREZZ)/zookeeper/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf zookeeper-$(zookeeper_UPVER).tar.gz $(TARARGS) $@
	rm -rf $@/debian
	cp -r $(<D) $@/

.PHONY: zsh
zsh:$(ZSH)_$(ARCH).deb
$(ZSH): $(SPREZZ)/zsh/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf zsh-$(zsh_UPVER).tar.bz2 $(TARARGS) $@

FETCHED:=$(FETCHED) $(FBIUP).tar.gz
$(FBIUP).tar.gz:
	wget -nc -O$@ http://www.kraxel.org/releases/fbida/$@

.PHONY: fbi
fbi:$(FBI)_$(ARCH).deb
$(FBI): $(SPREZZ)/fbi/debian/changelog $(FBIUP).tar.gz
	mkdir $@
	tar xzvf $(FBIUP).tar.gz $(TARARGS) $@
	cp -r $(<D) $@/

FETCHED:=$(FETCHED) sudo-1.8.5p3.tar.gz
sudo-1.8.5p3.tar.gz:
	wget -nc -O$@ http://www.gratisoft.us/sudo/dist/sudo-1.8.5p3.tar.gz

.PHONY: pandoc
pandoc:$(PANDOC)_$(ARCH).deb
$(PANDOC): $(SPREZZ)/pandoc/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pandoc-$(pandoc_UPVER).tar.gz $(TARARGS) $@

.PHONY: pango
pango:$(PANGO)_$(ARCH).deb
$(PANGO): $(SPREZZ)/pango/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf pango-$(pango_UPVER).tar.xz $(TARARGS) $@

.PHONY: pangomm
pangomm:$(PANGOMM)_$(ARCH).deb
$(PANGOMM): $(SPREZZ)/pangomm/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf pangomm-$(pangomm_UPVER).tar.xz $(TARARGS) $@

.PHONY: gtkmm3
gtkmm3:$(GTKMM3)_$(ARCH).deb
$(GTKMM3): $(SPREZZ)/gtkmm3/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gtkmm-$(gtkmm3_UPVER).tar.xz $(TARARGS) $@

.PHONY: pngquant
pngquant:$(PNGQUANT)_$(ARCH).deb
$(PNGQUANT): $(SPREZZ)/pngquant/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pngquant-$(pngquant_UPVER)-src.tgz $(TARARGS) $@

.PHONY: poppler
poppler:$(POPPLER)_$(ARCH).deb
$(POPPLER): $(SPREZZ)/poppler/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf poppler-$(poppler_UPVER).tar.gz $(TARARGS) $@

.PHONY: slrn
slrn:$(SLRN)_$(ARCH).deb
$(SLRN): $(SPREZZ)/slrn/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf slrn-$(slrn_UPVER).tar.gz $(TARARGS) $@

.PHONY: slang2
slang2:$(SLANG2)_$(ARCH).deb
$(SLANG2): $(SPREZZ)/slang2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf slang-$(slang2_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: subversion
subversion:$(SUBVERSION)_$(ARCH).deb
$(SUBVERSION): $(SPREZZ)/subversion/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf subversion-$(subversion_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: sudo
sudo:$(SUDO)_$(ARCH).deb
$(SUDO): $(SPREZZ)/sudo/debian/changelog sudo-1.8.5p3.tar.gz
	mkdir $@
	tar xzvf sudo-1.8.5p3.tar.gz $(TARARGS) $@
	cp -r $(<D) $@/

.PHONY: supertuxkart
supertuxkart:$(SUPERTUXKART)_$(ARCH).deb
$(SUPERTUXKART): $(SPREZZ)/supertuxkart/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf supertuxkart-$(supertuxkart_UPVER)-src.tar.bz2 $(TARARGS) $@

.PHONY: sprezzos-keyring
sprezzos-keyring:$(SPREZZOSKEYRING)_$(ARCH).deb
$(SPREZZOSKEYRING): $(SPREZZ)/sprezzos-keyring/debian/changelog
	@[ ! -e $@ ] || { echo "Removing $@..." && rm -rf $@ ; }
	cp -r $(<D)/.. $@
	rm -rf $@/debian
	tar cJvf sprezzos-keyring_$(sprezzos-keyring_UPVER).orig.tar.xz $@ --exclude-vcs
	cp -r $(<D) $@

.PHONY: gnome-pkg-tools
gnome-pkg-tools:$(GNOMEPKGTOOLS)_$(ARCH).deb
$(GNOMEPKGTOOLS): $(SPREZZ)/gnome-pkg-tools/debian/changelog
	cp -r $(<D)/.. $@/
	tar cJvf gnome-pkg-tools_$(gnome-pkg-tools_UPVER).orig.tar.xz $@ --exclude-vcs --exclude=debian

.PHONY: libdebian-installer
libdebian-installer:$(LIBDEBIANINSTALLER)_$(ARCH).udeb
$(LIBDEBIANINSTALLER): $(SPREZZ)/libdebian-installer/debian/changelog
	cp -r $(<D)/.. $@

.PHONY: mklibs
mklibs:$(MKLIBS)_$(ARCH).deb
$(MKLIBS): $(SPREZZ)/mklibs/debian/changelog
	cp -r $(<D)/.. $@
	tar cJvf mklibs_$(mklibs_UPVER).orig.tar.xz $@ --exclude-vcs --exclude=debian

clean:
	rm -rf -- sprezzos-world $(DEBS) $(UDEBS) $(DSCS) $(CHANGES)

update:
	for i in $(wildcard packaging/*) ; do \
		[ ! -d $$i ] || { cd $$i && { uscan --verbose ; cd - ; } } || true ; \
	done

clobber:
	rm -rf -- $(FETCHED)
.PHONY: gsl
gsl:$(GSL)_$(ARCH).deb
$(GSL): $(SPREZZ)/gsl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gsl-$(gsl_UPVER).tar.gz $(TARARGS) $@

.PHONY: asio
asio:$(ASIO)_$(ARCH).deb
$(ASIO): $(SPREZZ)/asio/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf asio-$(asio_UPVER).tar.gz $(TARARGS) $@

.PHONY: l2tpns
l2tpns:$(L2TPNS)_$(ARCH).deb
$(L2TPNS): $(SPREZZ)/l2tpns/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf l2tpns-$(l2tpns_UPVER).tar.gz $(TARARGS) $@

.PHONY: cloog-ppl
cloog-ppl:$(CLOOGPPL)_$(ARCH).deb
$(CLOOGPPL): $(SPREZZ)/cloog-ppl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cloog-ppl-$(cloog-ppl_UPVER).tar.gz $(TARARGS) $@

.PHONY: iso-codes
iso-codes:$(ISOCODES)_$(ARCH).deb
$(ISOCODES): $(SPREZZ)/iso-codes/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf iso-codes-$(iso-codes_UPVER).tar.xz $(TARARGS) $@

.PHONY: gsasl
gsasl:$(GSASL)_$(ARCH).deb
$(GSASL): $(SPREZZ)/gsasl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gsasl-$(gsasl_UPVER).tar.gz $(TARARGS) $@

.PHONY: gupnp-igd
gupnp-igd:$(GUPNPIGD)_$(ARCH).deb
$(GUPNPIGD): $(SPREZZ)/gupnp-igd/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gupnp-igd-$(gupnp-igd_UPVER).tar.xz $(TARARGS) $@

.PHONY: libzen
libzen:$(LIBZEN)_$(ARCH).deb
$(LIBZEN): $(SPREZZ)/libzen/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libzen_$(libzen_UPVER).tar.gz $(TARARGS) $@
	rm -rf $@/debian
	cp -r $(<D) $@

.PHONY: chmlib
chmlib:$(CHMLIB)_$(ARCH).deb
$(CHMLIB): $(SPREZZ)/chmlib/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf chmlib-$(chmlib_UPVER).tar.gz $(TARARGS) $@

.PHONY: dee
dee:$(DEE)_$(ARCH).deb
$(DEE): $(SPREZZ)/dee/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dee-$(dee_UPVER).tar.gz $(TARARGS) $@

.PHONY: dnssec-tools
dnssec-tools:$(DNSSECTOOLS)_$(ARCH).deb
$(DNSSECTOOLS): $(SPREZZ)/dnssec-tools/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dnssec-tools-$(dnssec-tools_UPVER).tar.gz $(TARARGS) $@

.PHONY: alsa-plugins
alsa-plugins:$(ALSAPLUGINS)_$(ARCH).deb
$(ALSAPLUGINS): $(SPREZZ)/alsa-plugins/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf alsa-plugins-$(alsa-plugins_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: leptonlib
leptonlib:$(LEPTONLIB)_$(ARCH).deb
$(LEPTONLIB): $(SPREZZ)/leptonlib/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf leptonica-$(leptonlib_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libmusicbrainz5
libmusicbrainz5:$(LIBMUSICBRAINZ5)_$(ARCH).deb
$(LIBMUSICBRAINZ5): $(SPREZZ)/libmusicbrainz5/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libmusicbrainz-$(libmusicbrainz5_UPVER).tar.gz $(TARARGS) $@

.PHONY: libofx
libofx:$(LIBOFX)_$(ARCH).deb
$(LIBOFX): $(SPREZZ)/libofx/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libofx-$(libofx_UPVER).tar.gz $(TARARGS) $@

.PHONY: gengetopt
gengetopt:$(GENGETOPT)_$(ARCH).deb
$(GENGETOPT): $(SPREZZ)/gengetopt/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gengetopt-$(gengetopt_UPVER).tar.gz $(TARARGS) $@

.PHONY: pps-tools
pps-tools:$(PPSTOOLS)_$(ARCH).deb
$(PPSTOOLS): $(SPREZZ)/pps-tools/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pps-tools-$(pps-tools_UPVER).tar.gz $(TARARGS) $@

.PHONY: safecopy
safecopy:$(SAFECOPY)_$(ARCH).deb
$(SAFECOPY): $(SPREZZ)/safecopy/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf safecopy-$(safecopy_UPVER).tar.gz $(TARARGS) $@

.PHONY: ragel
ragel:$(RAGEL)_$(ARCH).deb
$(RAGEL): $(SPREZZ)/ragel/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ragel-$(ragel_UPVER).tar.gz $(TARARGS) $@

.PHONY: jam
jam:$(JAM)_$(ARCH).deb
$(JAM): $(SPREZZ)/jam/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	gzip jam-$(jam_UPVER).tar
	ln -s jam-$(jam_UPVER).tar.gz jam_$(jam_UPVER).orig.tar.gz
	tar xzvf jam-$(jam_UPVER).tar.gz $(TARARGS) $@

.PHONY: libmusicbrainz3
libmusicbrainz3:$(LIBMUSICBRAINZ3)_$(ARCH).deb
$(LIBMUSICBRAINZ3): $(SPREZZ)/libmusicbrainz3/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libmusicbrainz-$(libmusicbrainz3_UPVER).tar.gz $(TARARGS) $@

.PHONY: fonts-googleweb
fonts-googleweb:$(FONTSGOOGLEWEB)_$(ARCH).deb
$(FONTSGOOGLEWEB): $(SPREZZ)/fonts-googleweb/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf fonts-googleweb-$(fonts-googleweb_UPVER).tar.gz $(TARARGS) $@

.PHONY: hunspell
hunspell:$(HUNSPELL)_$(ARCH).deb
$(HUNSPELL): $(SPREZZ)/hunspell/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hunspell-$(hunspell_UPVER).tar.gz $(TARARGS) $@

.PHONY: jansson
jansson:$(JANSSON)_$(ARCH).deb
$(JANSSON): $(SPREZZ)/jansson/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf jansson-$(jansson_UPVER).tar.gz $(TARARGS) $@

.PHONY: gprolog
gprolog:$(GPROLOG)_$(ARCH).deb
$(GPROLOG): $(SPREZZ)/gprolog/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gprolog-$(gprolog_UPVER).tar.gz $(TARARGS) $@

.PHONY: hevea
hevea:$(HEVEA)_$(ARCH).deb
$(HEVEA): $(SPREZZ)/hevea/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hevea-$(hevea_UPVER).tar.gz $(TARARGS) $@

.PHONY: ocaml
ocaml:$(OCAML)_$(ARCH).deb
$(OCAML): $(SPREZZ)/ocaml/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf ocaml-$(ocaml_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libiodbc2
libiodbc2:$(LIBIODBC2)_$(ARCH).deb
$(LIBIODBC2): $(SPREZZ)/libiodbc2/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libiodbc-$(libiodbc2_UPVER).tar.gz $(TARARGS) $@
	rm -rf $@/debian
	cp -r $(<D) $@

.PHONY: rlwrap
rlwrap:$(RLWRAP)_$(ARCH).deb
$(RLWRAP): $(SPREZZ)/rlwrap/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf rlwrap-$(rlwrap_UPVER).tar.gz $(TARARGS) $@

.PHONY: camlp5
camlp5:$(CAMLP5)_$(ARCH).deb
$(CAMLP5): $(SPREZZ)/camlp5/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf camlp5-$(camlp5_UPVER).tgz $(TARARGS) $@

.PHONY: clisp
clisp:$(CLISP)_$(ARCH).deb
$(CLISP): $(SPREZZ)/clisp/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf clisp-$(clisp_UPVER).tar.gz $(TARARGS) $@

.PHONY: cjk
cjk:$(CJK)_$(ARCH).deb
$(CJK): $(SPREZZ)/cjk/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cjk-$(cjk_UPVER).tar.gz $(TARARGS) $@

.PHONY: texlive-base
texlive-base:$(TEXLIVEBASE)_$(ARCH).deb
$(TEXLIVEBASE): $(SPREZZ)/texlive-base/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf texlive-base-$(texlive-base_UPVER).tar.gz $(TARARGS) $@

.PHONY: eperl
eperl:$(EPERL)_$(ARCH).deb
$(EPERL): $(SPREZZ)/eperl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf eperl-$(eperl_UPVER).tar.gz $(TARARGS) $@

.PHONY: texlive-bin
texlive-bin:$(TEXLIVEBIN)_$(ARCH).deb
$(TEXLIVEBIN): $(SPREZZ)/texlive-bin/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf texlive-bin-$(texlive-bin_UPVER).tar.gz $(TARARGS) $@

.PHONY: tipa
tipa:$(TIPA)_$(ARCH).deb
$(TIPA): $(SPREZZ)/tipa/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tipa-$(tipa_UPVER).tar.gz $(TARARGS) $@

.PHONY: libraw1394
libraw1394:$(LIBRAW1394)_$(ARCH).deb
$(LIBRAW1394): $(SPREZZ)/libraw1394/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libraw1394-$(libraw1394_UPVER).tar.gz $(TARARGS) $@

.PHONY: sg3-utils
sg3-utils:$(SG3UTILS)_$(ARCH).deb
$(SG3UTILS): $(SPREZZ)/sg3-utils/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf sg3_utils-$(sg3-utils_UPVER).tgz $(TARARGS) $@
	rm -rf $@/debian
	cp -r $(<D) $@

.PHONY: media-player-info
media-player-info:$(MEDIAPLAYERINFO)_$(ARCH).deb
$(MEDIAPLAYERINFO): $(SPREZZ)/media-player-info/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf media-player-info-$(media-player-info_UPVER).tar.gz $(TARARGS) $@

.PHONY: rtkit
rtkit:$(RTKIT)_$(ARCH).deb
$(RTKIT): $(SPREZZ)/rtkit/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf rtkit-$(rtkit_UPVER).tar.gz $(TARARGS) $@

.PHONY: smart
smart:$(SMART)_$(ARCH).deb
$(SMART): $(SPREZZ)/smart/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf smart-$(smart_UPVER).tar.gz $(TARARGS) $@

.PHONY: pm-utils
pm-utils:$(PMUTILS)_$(ARCH).deb
$(PMUTILS): $(SPREZZ)/pm-utils/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pm-utils-$(pm-utils_UPVER).tar.gz $(TARARGS) $@

.PHONY: vbetool
vbetool:$(VBETOOL)_$(ARCH).deb
$(VBETOOL): $(SPREZZ)/vbetool/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf vbetool-$(vbetool_UPVER).tar.gz $(TARARGS) $@

.PHONY: qpid-cpp
qpid-cpp:$(QPIDCPP)_$(ARCH).deb
$(QPIDCPP): $(SPREZZ)/qpid-cpp/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf qpid-cpp-$(qpid-cpp_UPVER).tar.gz $(TARARGS) $@

.PHONY: redhat-cluster
redhat-cluster:$(REDHATCLUSTER)_$(ARCH).deb
$(REDHATCLUSTER): $(SPREZZ)/redhat-cluster/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cluster-$(redhat-cluster_UPVER).tar.gz $(TARARGS) $@

.PHONY: qpid-tools
qpid-tools:$(QPIDTOOLS)_$(ARCH).deb
$(QPIDTOOLS): $(SPREZZ)/qpid-tools/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf qpid-tools-$(qpid-tools_UPVER).tar.gz $(TARARGS) $@

.PHONY: hwdata
hwdata:$(HWDATA)_$(ARCH).deb
$(HWDATA): $(SPREZZ)/hwdata/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hwdata-$(hwdata_UPVER).tar.gz $(TARARGS) $@

.PHONY: libmikmod
libmikmod:$(LIBMIKMOD)_$(ARCH).deb
$(LIBMIKMOD): $(SPREZZ)/libmikmod/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mikmod-$(libmikmod_UPVER).tar.gz $(TARARGS) $@

.PHONY: libee
libee:$(LIBEE)_$(ARCH).deb
$(LIBEE): $(SPREZZ)/libee/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libee-$(libee_UPVER).tar.gz $(TARARGS) $@

.PHONY: liblognorm
liblognorm:$(LIBLOGNORM)_$(ARCH).deb
$(LIBLOGNORM): $(SPREZZ)/liblognorm/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf liblognorm-$(liblognorm_UPVER).tar.gz $(TARARGS) $@

.PHONY: libmongo-client
libmongo-client:$(LIBMONGOCLIENT)_$(ARCH).deb
$(LIBMONGOCLIENT): $(SPREZZ)/libmongo-client/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libmongo-client-$(libmongo-client_UPVER).tar.gz $(TARARGS) $@

.PHONY: byacc
byacc:$(BYACC)_$(ARCH).deb
$(BYACC): $(SPREZZ)/byacc/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf byacc-$(byacc_UPVER).tgz $(TARARGS) $@

.PHONY: aspell
aspell:$(ASPELL)_$(ARCH).deb
$(ASPELL): $(SPREZZ)/aspell/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf aspell-$(aspell_UPVER).tar.gz $(TARARGS) $@

.PHONY: ucommon
ucommon:$(UCOMMON)_$(ARCH).deb
$(UCOMMON): $(SPREZZ)/ucommon/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ucommon-$(ucommon_UPVER).tar.gz $(TARARGS) $@

.PHONY: libccrtp
libccrtp:$(LIBCCRTP)_$(ARCH).deb
$(LIBCCRTP): $(SPREZZ)/libccrtp/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ccrtp-$(libccrtp_UPVER).tar.gz $(TARARGS) $@

.PHONY: serf
serf:$(SERF)_$(ARCH).deb
$(SERF): $(SPREZZ)/serf/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf serf-$(serf_UPVER).tar.gz $(TARARGS) $@

.PHONY: stfl
stfl:$(STFL)_$(ARCH).deb
$(STFL): $(SPREZZ)/stfl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf stfl-$(stfl_UPVER).tar.gz $(TARARGS) $@

.PHONY: log4net
log4net:$(LOG4NET)_$(ARCH).deb
$(LOG4NET): $(SPREZZ)/log4net/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf log4net_$(log4net_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libqmi
libqmi:$(LIBQMI)_$(ARCH).deb
$(LIBQMI): $(SPREZZ)/libqmi/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libqmi-$(libqmi_UPVER).tar.xz $(TARARGS) $@

.PHONY: open-iscsi
open-iscsi:$(OPENISCSI)_$(ARCH).deb
$(OPENISCSI): $(SPREZZ)/open-iscsi/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf open-iscsi_$(open-iscsi_UPVER).orig.tar.gz $(TARARGS) $@
	rm -rf $@/debian
	cp -r $(<D) $@

.PHONY: libwpd
libwpd:$(LIBWPD)_$(ARCH).deb
$(LIBWPD): $(SPREZZ)/libwpd/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libwpd-$(libwpd_UPVER).tar.gz $(TARARGS) $@

.PHONY: editline
editline:$(EDITLINE)_$(ARCH).deb
$(EDITLINE): $(SPREZZ)/editline/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf editline_$(editline_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: svn2cl
svn2cl:$(SVN2CL)_$(ARCH).deb
$(SVN2CL): $(SPREZZ)/svn2cl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf svn2cl-$(svn2cl_UPVER).tar.gz $(TARARGS) $@

.PHONY: snappy
snappy:$(SNAPPY)_$(ARCH).deb
$(SNAPPY): $(SPREZZ)/snappy/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf snappy-$(snappy_UPVER).tar.gz $(TARARGS) $@

.PHONY: freetds
freetds:$(FREETDS)_$(ARCH).deb
$(FREETDS): $(SPREZZ)/freetds/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf freetds-$(freetds_UPVER).tar.gz $(TARARGS) $@

.PHONY: usb-modeswitch
usb-modeswitch:$(USBMODESWITCH)_$(ARCH).deb
$(USBMODESWITCH): $(SPREZZ)/usb-modeswitch/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf usb-modeswitch-$(usb-modeswitch_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: usb-modeswitch-data
usb-modeswitch-data:$(USBMODESWITCHDATA)_$(ARCH).deb
$(USBMODESWITCHDATA): $(SPREZZ)/usb-modeswitch-data/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf usb-modeswitch-data-$(usb-modeswitch-data_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: underscore
underscore:$(UNDERSCORE)_$(ARCH).deb
$(UNDERSCORE): $(SPREZZ)/underscore/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf underscore_$(underscore_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: uglifyjs
uglifyjs:$(UGLIFYJS)_$(ARCH).deb
$(UGLIFYJS): $(SPREZZ)/uglifyjs/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf uglifyjs_$(uglifyjs_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: nodejs
nodejs:$(NODEJS)_$(ARCH).deb
$(NODEJS): $(SPREZZ)/nodejs/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf nodejs-$(nodejs_UPVER).tar.gz $(TARARGS) $@

.PHONY: libgds
libgds:$(LIBGDS)_$(ARCH).deb
$(LIBGDS): $(SPREZZ)/libgds/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libgds_$(libgds_UPVER).orig.tar.gz $(TARARGS) $@
	rm -rf $@/debian
	cp -r $(<D) $@

.PHONY: luatex
luatex:$(LUATEX)_$(ARCH).deb
$(LUATEX): $(SPREZZ)/luatex/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf luatex_$(luatex_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: dnprogs
dnprogs:$(DNPROGS)_$(ARCH).deb
$(DNPROGS): $(SPREZZ)/dnprogs/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dnprogs-$(dnprogs_UPVER).tar.gz $(TARARGS) $@

.PHONY: srf
srf:$(SRF)_$(ARCH).deb
$(SRF): $(SPREZZ)/srf/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf srf_$(srf_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: context
context:$(CONTEXT)_$(ARCH).deb
$(CONTEXT): $(SPREZZ)/context/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf context-$(context_UPVER).tar.gz $(TARARGS) $@

.PHONY: xmltoman
xmltoman:$(XMLTOMAN)_$(ARCH).deb
$(XMLTOMAN): $(SPREZZ)/xmltoman/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xmltoman-$(xmltoman_UPVER).tar.gz $(TARARGS) $@

.PHONY: libsvm
libsvm:$(LIBSVM)_$(ARCH).deb
$(LIBSVM): $(SPREZZ)/libsvm/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf $(libsvm_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: seqan
seqan:$(SEQAN)_$(ARCH).deb
$(SEQAN): $(SPREZZ)/seqan/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf seqan_$(seqan_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: samtools
samtools:$(SAMTOOLS)_$(ARCH).deb
$(SAMTOOLS): $(SPREZZ)/samtools/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf samtools_$(samtools_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: docbook-utils
docbook-utils:$(DOCBOOKUTILS)_$(ARCH).deb
$(DOCBOOKUTILS): $(SPREZZ)/docbook-utils/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf docbook-utils_$(docbook-utils_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: dssi
dssi:$(DSSI)_$(ARCH).deb
$(DSSI): $(SPREZZ)/dssi/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dssi_$(dssi_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: liblo
liblo:$(LIBLO)_$(ARCH).deb
$(LIBLO): $(SPREZZ)/liblo/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf liblo_$(liblo_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: lmodern
lmodern:$(LMODERN)_$(ARCH).deb
$(LMODERN): $(SPREZZ)/lmodern/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf lmodern_$(lmodern_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: gd4o
gd4o:$(GD4O)_$(ARCH).deb
$(GD4O): $(SPREZZ)/gd4o/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gd4o_$(gd4o_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: stunnel4
stunnel4:$(STUNNEL4)_$(ARCH).deb
$(STUNNEL4): $(SPREZZ)/stunnel4/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf stunnel4_$(stunnel4_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: sdf
sdf:$(SDF)_$(ARCH).deb
$(SDF): $(SPREZZ)/sdf/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf sdf_$(sdf_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: fox1.6
fox1.6:$(FOX1.6)_$(ARCH).deb
$(FOX1.6): $(SPREZZ)/fox1.6/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf fox1.6_$(fox1.6_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: zziplib
zziplib:$(ZZIPLIB)_$(ARCH).deb
$(ZZIPLIB): $(SPREZZ)/zziplib/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf zziplib_$(zziplib_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: ebtables
ebtables:$(EBTABLES)_$(ARCH).deb
$(EBTABLES): $(SPREZZ)/ebtables/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ebtables_$(ebtables_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: systemtap
systemtap:$(SYSTEMTAP)_$(ARCH).deb
$(SYSTEMTAP): $(SPREZZ)/systemtap/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf systemtap_$(systemtap_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: tk8.6
tk8.6:$(TK8.6)_$(ARCH).deb
$(TK8.6): $(SPREZZ)/tk8.6/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tk8.6_$(tk8.6_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: tcl8.6
tcl8.6:$(TCL8.6)_$(ARCH).deb
$(TCL8.6): $(SPREZZ)/tcl8.6/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tcl8.6_$(tcl8.6_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: cm-super
cm-super:$(CMSUPER)_$(ARCH).deb
$(CMSUPER): $(SPREZZ)/cm-super/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cm-super_$(cm-super_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: tau
tau:$(TAU)_$(ARCH).deb
$(TAU): $(SPREZZ)/tau/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tau_$(tau_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: opendmarc
opendmarc:$(OPENDMARC)_$(ARCH).deb
$(OPENDMARC): $(SPREZZ)/opendmarc/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf opendmarc_$(opendmarc_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: opendkim
opendkim:$(OPENDKIM)_$(ARCH).deb
$(OPENDKIM): $(SPREZZ)/opendkim/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf opendkim_$(opendkim_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: sendmail
sendmail:$(SENDMAIL)_$(ARCH).deb
$(SENDMAIL): $(SPREZZ)/sendmail/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf sendmail_$(sendmail_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: gnu-smalltalk
gnu-smalltalk:$(GNUSMALLTALK)_$(ARCH).deb
$(GNUSMALLTALK): $(SPREZZ)/gnu-smalltalk/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gnu-smalltalk_$(gnu-smalltalk_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libpuzzle
libpuzzle:$(LIBPUZZLE)_$(ARCH).deb
$(LIBPUZZLE): $(SPREZZ)/libpuzzle/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libpuzzle_$(libpuzzle_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: rarian
rarian:$(RARIAN)_$(ARCH).deb
$(RARIAN): $(SPREZZ)/rarian/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf rarian_$(rarian_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: file
file:$(FILE)_$(ARCH).deb
$(FILE): $(SPREZZ)/file/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf file_$(file_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: slv2
slv2:$(SLV2)_$(ARCH).deb
$(SLV2): $(SPREZZ)/slv2/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf slv2_$(slv2_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: lv2
lv2:$(LV2)_$(ARCH).deb
$(LV2): $(SPREZZ)/lv2/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf lv2_$(lv2_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: lilv
lilv:$(LILV)_$(ARCH).deb
$(LILV): $(SPREZZ)/lilv/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf lilv_$(lilv_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: sord
sord:$(SORD)_$(ARCH).deb
$(SORD): $(SPREZZ)/sord/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf sord_$(sord_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: serd
serd:$(SERD)_$(ARCH).deb
$(SERD): $(SPREZZ)/serd/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf serd_$(serd_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: sratom
sratom:$(SRATOM)_$(ARCH).deb
$(SRATOM): $(SPREZZ)/sratom/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf sratom_$(sratom_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: bridge-utils
bridge-utils:$(BRIDGEUTILS)_$(ARCH).deb
$(BRIDGEUTILS): $(SPREZZ)/bridge-utils/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bridge-utils_$(bridge-utils_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ethtool
ethtool:$(ETHTOOL)_$(ARCH).deb
$(ETHTOOL): $(SPREZZ)/ethtool/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf ethtool_$(ethtool_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: purple-plugin-pack
purple-plugin-pack:$(PURPLEPLUGINPACK)_$(ARCH).deb
$(PURPLEPLUGINPACK): $(SPREZZ)/purple-plugin-pack/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf purple-plugin-pack_$(purple-plugin-pack_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: filters
filters:$(FILTERS)_$(ARCH).deb
$(FILTERS): $(SPREZZ)/filters/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf filters_$(filters_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: raptor
raptor:$(RAPTOR)_$(ARCH).deb
$(RAPTOR): $(SPREZZ)/raptor/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf raptor_$(raptor_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: uhd
uhd:$(UHD)_$(ARCH).deb
$(UHD): $(SPREZZ)/uhd/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf uhd_$(uhd_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: sgml2x
sgml2x:$(SGML2X)_$(ARCH).deb
$(SGML2X): $(SPREZZ)/sgml2x/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf sgml2x_$(sgml2x_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libwmf
libwmf:$(LIBWMF)_$(ARCH).deb
$(LIBWMF): $(SPREZZ)/libwmf/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libwmf_$(libwmf_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: netpbm
netpbm:$(NETPBM)_$(ARCH).deb
$(NETPBM): $(SPREZZ)/netpbm/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf netpbm_$(netpbm_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: mjpegtools
mjpegtools:$(MJPEGTOOLS)_$(ARCH).deb
$(MJPEGTOOLS): $(SPREZZ)/mjpegtools/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mjpegtools_$(mjpegtools_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: chromaprint
chromaprint:$(CHROMAPRINT)_$(ARCH).deb
$(CHROMAPRINT): $(SPREZZ)/chromaprint/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf chromaprint_$(chromaprint_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: mythes
mythes:$(mythes)_$(ARCH).deb
$(mythes): $(SPREZZ)/mythes/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mythes_$(mythes_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libnetfilter-log
libnetfilter-log:$(libnetfilterlog)_$(ARCH).deb
$(libnetfilterlog): $(SPREZZ)/libnetfilter-log/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libnetfilter-log_$(libnetfilter-log_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: openbabel
openbabel:$(openbabel)_$(ARCH).deb
$(openbabel): $(SPREZZ)/openbabel/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf openbabel_$(openbabel_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: blt
blt:$(blt)_$(ARCH).deb
$(blt): $(SPREZZ)/blt/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf blt_$(blt_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: gupnp-tools
gupnp-tools:$(GUPNPTOOLS)_$(ARCH).deb
$(GUPNPTOOLS): $(SPREZZ)/gupnp-tools/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gupnp-tools_$(gupnp-tools_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: radvd
radvd:$(RADVD)_$(ARCH).deb
$(RADVD): $(SPREZZ)/radvd/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf radvd_$(radvd_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: diffutils
diffutils:$(diffutils)_$(ARCH).deb
$(diffutils): $(SPREZZ)/diffutils/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf diffutils_$(diffutils_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: input-utils
input-utils:$(inpututils)_$(ARCH).deb
$(inpututils): $(SPREZZ)/input-utils/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf input-utils_$(input-utils_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: freeimage
freeimage:$(freeimage)_$(ARCH).deb
$(freeimage): $(SPREZZ)/freeimage/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf freeimage_$(freeimage_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ois
ois:$(ois)_$(ARCH).deb
$(ois): $(SPREZZ)/ois/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ois_$(ois_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: latex2html
latex2html:$(LATEX2HTML)_$(ARCH).deb
$(LATEX2HTML): $(SPREZZ)/latex2html/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf latex2html_$(latex2html_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libspiro
libspiro:$(LIBSPIRO)_$(ARCH).deb
$(LIBSPIRO): $(SPREZZ)/libspiro/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libspiro_$(libspiro_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: shtool
shtool:$(SHTOOL)_$(ARCH).deb
$(SHTOOL): $(SPREZZ)/shtool/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf shtool_$(shtool_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libjitsi
libjitsi:$(LIBJITSI)_$(ARCH).deb
$(LIBJITSI): $(SPREZZ)/libjitsi/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf libjitsi_$(libjitsi_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ears
ears:$(EARS)_$(ARCH).deb
$(EARS): $(SPREZZ)/ears/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ears_$(ears_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libcue
libcue:$(LIBCUE)_$(ARCH).deb
$(LIBCUE): $(SPREZZ)/libcue/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libcue_$(libcue_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: pidgin-privacy-please
pidgin-privacy-please:$(PIDGINPRIVACYPLEASE)_$(ARCH).deb
$(PIDGINPRIVACYPLEASE): $(SPREZZ)/pidgin-privacy-please/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pidgin-privacy-please_$(pidgin-privacy-please_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: boost1.53
boost1.53:$(BOOST1.53)_$(ARCH).deb
$(BOOST1.53): $(SPREZZ)/boost1.53/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf boost1.53_$(boost1.53_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: i18nspector
i18nspector:$(I18NSPECTOR)_$(ARCH).deb
$(I18NSPECTOR): $(SPREZZ)/i18nspector/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf i18nspector_$(i18nspector_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: silan
silan:$(SILAN)_$(ARCH).deb
$(SILAN): $(SPREZZ)/silan/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf silan_$(silan_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: openvdb
openvdb:$(OPENVDB)_$(ARCH).deb
$(OPENVDB): $(SPREZZ)/openvdb/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf openvdb_$(openvdb_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ps2eps
ps2eps:$(PS2EPS)_$(ARCH).deb
$(PS2EPS): $(SPREZZ)/ps2eps/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ps2eps_$(ps2eps_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libjs-qunit
libjs-qunit:$(LIBJSQUNIT)_$(ARCH).deb
$(LIBJSQUNIT): $(SPREZZ)/libjs-qunit/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libjs-qunit_$(libjs-qunit_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: cinfony
cinfony:$(CINFONY)_$(ARCH).deb
$(CINFONY): $(SPREZZ)/cinfony/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cinfony_$(cinfony_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: openscenegraph
openscenegraph:$(OPENSCENEGRAPH)_$(ARCH).deb
$(OPENSCENEGRAPH): $(SPREZZ)/openscenegraph/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf openscenegraph_$(openscenegraph_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: mrpt
mrpt:$(MRPT)_$(ARCH).deb
$(MRPT): $(SPREZZ)/mrpt/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mrpt_$(mrpt_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libprelude
libprelude:$(LIBPRELUDE)_$(ARCH).deb
$(LIBPRELUDE): $(SPREZZ)/libprelude/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libprelude_$(libprelude_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libgda5
libgda5:$(LIBGDA5)_$(ARCH).deb
$(LIBGDA5): $(SPREZZ)/libgda5/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libgda5_$(libgda5_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: mysql-5.5
mysql-5.5:$(MYSQL5.5)_$(ARCH).deb
$(MYSQL5.5): $(SPREZZ)/mysql-5.5/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mysql-5.5_$(mysql-5.5_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: glm
glm:$(GLM)_$(ARCH).deb
$(GLM): $(SPREZZ)/glm/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf glm_$(glm_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libquvi-scripts
libquvi-scripts:$(LIBQUVISCRIPTS)_$(ARCH).deb
$(LIBQUVISCRIPTS): $(SPREZZ)/libquvi-scripts/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libquvi-scripts_$(libquvi-scripts_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: unixodbc
unixodbc:$(UNIXODBC)_$(ARCH).deb
$(UNIXODBC): $(SPREZZ)/unixodbc/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf unixodbc_$(unixodbc_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: cloog
cloog:$(CLOOG)_$(ARCH).deb
$(CLOOG): $(SPREZZ)/cloog/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cloog_$(cloog_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: isl
isl:$(ISL)_$(ARCH).deb
$(ISL): $(SPREZZ)/isl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf isl_$(isl_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: gcc-4.8
gcc-4.8:$(GCC4.8)_$(ARCH).deb
$(GCC4.8): $(SPREZZ)/gcc-4.8/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gcc-4.8_$(gcc-4.8_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libspectre
libspectre:$(LIBSPECTRE)_$(ARCH).deb
$(LIBSPECTRE): $(SPREZZ)/libspectre/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libspectre_$(libspectre_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: zutils
zutils:$(ZUTILS)_$(ARCH).deb
$(ZUTILS): $(SPREZZ)/zutils/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf zutils_$(zutils_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: mtdev
mtdev:$(MTDEV)_$(ARCH).deb
$(MTDEV): $(SPREZZ)/mtdev/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mtdev_$(mtdev_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: flite
flite:$(FLITE)_$(ARCH).deb
$(FLITE): $(SPREZZ)/flite/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf flite_$(flite_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: libcdaudio
libcdaudio:$(LIBCDAUDIO)_$(ARCH).deb
$(LIBCDAUDIO): $(SPREZZ)/libcdaudio/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libcdaudio_$(libcdaudio_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libmimic
libmimic:$(LIBMIMIC)_$(ARCH).deb
$(LIBMIMIC): $(SPREZZ)/libmimic/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libmimic_$(libmimic_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: zvbi
zvbi:$(ZVBI)_$(ARCH).deb
$(ZVBI): $(SPREZZ)/zvbi/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf zvbi_$(zvbi_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: wildmidi
wildmidi:$(WILDMIDI)_$(ARCH).deb
$(WILDMIDI): $(SPREZZ)/wildmidi/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf wildmidi_$(wildmidi_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: scilab
scilab:$(SCILAB)_$(ARCH).deb
$(SCILAB): $(SPREZZ)/scilab/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf scilab_$(scilab_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: soundtouch
soundtouch:$(SOUNDTOUCH)_$(ARCH).deb
$(SOUNDTOUCH): $(SPREZZ)/soundtouch/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf soundtouch_$(soundtouch_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libmatio
libmatio:$(LIBMATIO)_$(ARCH).deb
$(LIBMATIO): $(SPREZZ)/libmatio/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libmatio_$(libmatio_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ecere-sdk
ecere-sdk:$(ECERESDK)_$(ARCH).deb
$(ECERESDK): $(SPREZZ)/ecere-sdk/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ecere-sdk_$(ecere-sdk_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: upx-ucl
upx-ucl:$(UPXUCL)_$(ARCH).deb
$(UPXUCL): $(SPREZZ)/upx-ucl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf upx-ucl_$(upx-ucl_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: lzma
lzma:$(LZMA)_$(ARCH).deb
$(LZMA): $(SPREZZ)/lzma/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf lzma_$(lzma_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: ucl
ucl:$(UCL)_$(ARCH).deb
$(UCL): $(SPREZZ)/ucl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ucl_$(ucl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: itk3
itk3:$(ITK3)_$(ARCH).deb
$(ITK3): $(SPREZZ)/itk3/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf itk3_$(itk3_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: itcl4
itcl4:$(ITCL4)_$(ARCH).deb
$(ITCL4): $(SPREZZ)/itcl4/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf itcl4_$(itcl4_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: html-xml-utils
html-xml-utils:$(HTMLXMLUTILS)_$(ARCH).deb
$(HTMLXMLUTILS): $(SPREZZ)/html-xml-utils/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf html-xml-utils_$(html-xml-utils_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: timidity
timidity:$(TIMIDITY)_$(ARCH).deb
$(TIMIDITY): $(SPREZZ)/timidity/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf timidity_$(timidity_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libdiscid
libdiscid:$(LIBDISCID)_$(ARCH).deb
$(LIBDISCID): $(SPREZZ)/libdiscid/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libdiscid_$(libdiscid_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: liblouis
liblouis:$(LIBLOUIS)_$(ARCH).deb
$(LIBLOUIS): $(SPREZZ)/liblouis/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf liblouis_$(liblouis_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libnet
libnet:$(LIBNET)_$(ARCH).deb
$(LIBNET): $(SPREZZ)/libnet/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libnet_$(libnet_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: lighttpd
lighttpd:$(LIGHTTPD)_$(ARCH).deb
$(LIGHTTPD): $(SPREZZ)/lighttpd/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf lighttpd_$(lighttpd_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: blcr
blcr:$(BLCR)_$(ARCH).deb
$(BLCR): $(SPREZZ)/blcr/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf blcr_$(blcr_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: wit
wit:$(WIT)_$(ARCH).deb
$(WIT): $(SPREZZ)/wit/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf wit_$(wit_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: intltool
intltool:$(INTLTOOL)_$(ARCH).deb
$(INTLTOOL): $(SPREZZ)/intltool/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf intltool_$(intltool_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: psmisc
psmisc:$(PSMISC)_$(ARCH).deb
$(PSMISC): $(SPREZZ)/psmisc/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf psmisc_$(psmisc_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libss7
libss7:$(LIBSS7)_$(ARCH).deb
$(LIBSS7): $(SPREZZ)/libss7/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libss7_$(libss7_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: dahdi-tools
dahdi-tools:$(DAHDITOOLS)_$(ARCH).deb
$(DAHDITOOLS): $(SPREZZ)/dahdi-tools/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dahdi-tools_$(dahdi-tools_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: lua-lgi
lua-lgi:$(LUALGI)_$(ARCH).deb
$(LUALGI): $(SPREZZ)/lua-lgi/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf lua-lgi_$(lua-lgi_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: liboobs
liboobs:$(LIBOOBS)_$(ARCH).deb
$(LIBOOBS): $(SPREZZ)/liboobs/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf liboobs_$(liboobs_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: xapian-bindings
xapian-bindings:$(XAPIANBINDINGS)_$(ARCH).deb
$(XAPIANBINDINGS): $(SPREZZ)/xapian-bindings/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xapian-bindings_$(xapian-bindings_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libaqbanking
libaqbanking:$(LIBAQBANKING)_$(ARCH).deb
$(LIBAQBANKING): $(SPREZZ)/libaqbanking/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libaqbanking_$(libaqbanking_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libgwenhywfar
libgwenhywfar:$(LIBGWENHYWFAR)_$(ARCH).deb
$(LIBGWENHYWFAR): $(SPREZZ)/libgwenhywfar/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libgwenhywfar_$(libgwenhywfar_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: d-feet
d-feet:$(DFEET)_$(ARCH).deb
$(DFEET): $(SPREZZ)/d-feet/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf d-feet_$(d-feet_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: surfraw
surfraw:$(SURFRAW)_$(ARCH).deb
$(SURFRAW): $(SPREZZ)/surfraw/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf surfraw_$(surfraw_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: scalasca
scalasca:$(SCALASCA)_$(ARCH).deb
$(SCALASCA): $(SPREZZ)/scalasca/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf scalasca_$(scalasca_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libsoxr
libsoxr:$(LIBSOXR)_$(ARCH).deb
$(LIBSOXR): $(SPREZZ)/libsoxr/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libsoxr_$(libsoxr_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: acpica-unix
acpica-unix:$(ACPICAUNIX)_$(ARCH).deb
$(ACPICAUNIX): $(SPREZZ)/acpica-unix/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf acpica-unix_$(acpica-unix_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: dkms
dkms:$(DKMS)_$(ARCH).deb
$(DKMS): $(SPREZZ)/dkms/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dkms_$(dkms_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ilmbase2
ilmbase2:$(ILMBASE2)_$(ARCH).deb
$(ILMBASE2): $(SPREZZ)/ilmbase2/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ilmbase2_$(ilmbase2_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: openexr2
openexr2:$(OPENEXR2)_$(ARCH).deb
$(OPENEXR2): $(SPREZZ)/openexr2/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf openexr2_$(openexr2_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libwpg
libwpg:$(LIBWPG)_$(ARCH).deb
$(LIBWPG): $(SPREZZ)/libwpg/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libwpg_$(libwpg_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: vips
vips:$(VIPS)_$(ARCH).deb
$(VIPS): $(SPREZZ)/vips/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf vips_$(vips_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libtins
libtins:$(LIBTINS)_$(ARCH).deb
$(LIBTINS): $(SPREZZ)/libtins/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libtins_$(libtins_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: gnuclad
gnuclad:$(GNUCLAD)_$(ARCH).deb
$(GNUCLAD): $(SPREZZ)/gnuclad/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gnuclad_$(gnuclad_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: protobuf-c
protobuf-c:$(PROTOBUFC)_$(ARCH).deb
$(PROTOBUFC): $(SPREZZ)/protobuf-c/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf protobuf-c_$(protobuf-c_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: vc
vc:$(VC)_$(ARCH).deb
$(VC): $(SPREZZ)/vc/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf vc_$(vc_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: media-ctl
media-ctl:$(MEDIACTL)_$(ARCH).deb
$(MEDIACTL): $(SPREZZ)/media-ctl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf media-ctl_$(media-ctl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: alsa-base
alsa-base:$(ALSABASE)_$(ARCH).deb
$(ALSABASE): $(SPREZZ)/alsa-base/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf alsa-base_$(alsa-base_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: i7z
i7z:$(I7Z)_$(ARCH).deb
$(I7Z): $(SPREZZ)/i7z/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf i7z_$(i7z_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: msr-tools
msr-tools:$(MSRTOOLS)_$(ARCH).deb
$(MSRTOOLS): $(SPREZZ)/msr-tools/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf msr-tools_$(msr-tools_UPVER).orig.tar.gz $(TARARGS) $@
.PHONY: photo-uploader
photo-uploader:$(PHOTOUPLOADER)_$(ARCH).deb
$(PHOTOUPLOADER): $(SPREZZ)/photo-uploader/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf photo-uploader_$(photo-uploader_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: httrack
httrack:$(HTTRACK)_$(ARCH).deb
$(HTTRACK): $(SPREZZ)/httrack/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf httrack_$(httrack_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: neard
neard:$(NEARD)_$(ARCH).deb
$(NEARD): $(SPREZZ)/neard/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf neard_$(neard_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: nat-traverse
nat-traverse:$(NATTRAVERSE)_$(ARCH).deb
$(NATTRAVERSE): $(SPREZZ)/nat-traverse/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf nat-traverse_$(nat-traverse_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: nicstat
nicstat:$(NICSTAT)_$(ARCH).deb
$(NICSTAT): $(SPREZZ)/nicstat/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf nicstat_$(nicstat_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: flexbar
flexbar:$(FLEXBAR)_$(ARCH).deb
$(FLEXBAR): $(SPREZZ)/flexbar/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf flexbar_$(flexbar_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: aragorn
aragorn:$(ARAGORN)_$(ARCH).deb
$(ARAGORN): $(SPREZZ)/aragorn/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf aragorn_$(aragorn_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: zalign
zalign:$(ZALIGN)_$(ARCH).deb
$(ZALIGN): $(SPREZZ)/zalign/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf zalign_$(zalign_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: zegrapher
zegrapher:$(ZEGRAPHER)_$(ARCH).deb
$(ZEGRAPHER): $(SPREZZ)/zegrapher/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version --repack
	cd $@ && 7z e ../ZeGrapher_v$(zegrapher_UPVER)_Sources.7z

.PHONY: qmidiarp
qmidiarp:$(QMIDIARP)_$(ARCH).deb
$(QMIDIARP): $(SPREZZ)/qmidiarp/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf qmidiarp_$(qmidiarp_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: portaudio
portaudio:$(PORTAUDIO)_$(ARCH).deb
$(PORTAUDIO): $(SPREZZ)/portaudio/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf portaudio_$(portaudio_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libverto
libverto:$(LIBVERTO)_$(ARCH).deb
$(LIBVERTO): $(SPREZZ)/libverto/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libverto_$(libverto_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: qmidinet
qmidinet:$(QMIDINET)_$(ARCH).deb
$(QMIDINET): $(SPREZZ)/qmidinet/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf qmidinet_$(qmidinet_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: mididings
mididings:$(MIDIDINGS)_$(ARCH).deb
$(MIDIDINGS): $(SPREZZ)/mididings/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mididings_$(mididings_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libsmf
libsmf:$(LIBSMF)_$(ARCH).deb
$(LIBSMF): $(SPREZZ)/libsmf/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libsmf_$(libsmf_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: smokekde
smokekde:$(SMOKEKDE)_$(ARCH).deb
$(SMOKEKDE): $(SPREZZ)/smokekde/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf smokekde_$(smokekde_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: mono3
mono3:$(MONO3)_$(ARCH).deb
$(MONO3): $(SPREZZ)/mono3/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf mono_$(mono3_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: dbus-sharp
dbus-sharp:$(DBUSSHARP)_$(ARCH).deb
$(DBUSSHARP): $(SPREZZ)/dbus-sharp/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dbus-sharp_$(dbus-sharp_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: nunit
nunit:$(NUNIT)_$(ARCH).deb
$(NUNIT): $(SPREZZ)/nunit/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf nunit_$(nunit_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: geoip
geoip:$(GEOIP)_$(ARCH).deb
$(GEOIP): $(SPREZZ)/geoip/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf geoip_$(geoip_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: nocache
nocache:$(NOCACHE)_$(ARCH).deb
$(NOCACHE): $(SPREZZ)/nocache/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf nocache_$(nocache_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: harvid
harvid:$(HARVID)_$(ARCH).deb
$(HARVID): $(SPREZZ)/harvid/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf harvid_$(harvid_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: padevchooser
padevchooser:$(PADEVCHOOSER)_$(ARCH).deb
$(PADEVCHOOSER): $(SPREZZ)/padevchooser/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf padevchooser_$(padevchooser_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: editorconfig
editorconfig:$(EDITORCONFIG)_$(ARCH).deb
$(EDITORCONFIG): $(SPREZZ)/editorconfig/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf editorconfig-core_$(editorconfig_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: cgminer
cgminer:$(CGMINER)_$(ARCH).deb
$(CGMINER): $(SPREZZ)/cgminer/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cgminer_$(cgminer_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ocl-icd
ocl-icd:$(OCLICD)_$(ARCH).deb
$(OCLICD): $(SPREZZ)/ocl-icd/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ocl-icd_$(ocl-icd_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: man2html
man2html:$(MAN2HTML)_$(ARCH).deb
$(MAN2HTML): $(SPREZZ)/man2html/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf man2html_$(man2html_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: fonts-font-awesome
fonts-font-awesome:$(FONTSFONTAWESOME)_$(ARCH).deb
$(FONTSFONTAWESOME): $(SPREZZ)/fonts-font-awesome/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf fonts-font-awesome_$(fonts-font-awesome_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: liblangtag
liblangtag:$(LIBLANGTAG)_$(ARCH).deb
$(LIBLANGTAG): $(SPREZZ)/liblangtag/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf liblangtag_$(liblangtag_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: fsharp
fsharp:$(FSHARP)_$(ARCH).deb
$(FSHARP): $(SPREZZ)/fsharp/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf fsharp_$(fsharp_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: bittwist
bittwist:$(BITTWIST)_$(ARCH).deb
$(BITTWIST): $(SPREZZ)/bittwist/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bittwist_$(bittwist_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: fatrace
fatrace:$(FATRACE)_$(ARCH).deb
$(FATRACE): $(SPREZZ)/fatrace/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf fatrace_$(fatrace_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: autotrace
autotrace:$(AUTOTRACE)_$(ARCH).deb
$(AUTOTRACE): $(SPREZZ)/autotrace/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf autotrace_$(autotrace_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: pstoedit
pstoedit:$(PSTOEDIT)_$(ARCH).deb
$(PSTOEDIT): $(SPREZZ)/pstoedit/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pstoedit_$(pstoedit_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: sysstat
sysstat:$(SYSSTAT)_$(ARCH).deb
$(SYSSTAT): $(SPREZZ)/sysstat/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf sysstat_$(sysstat_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: ming
ming:$(MING)_$(ARCH).deb
$(MING): $(SPREZZ)/ming/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ming_$(ming_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: pdf2djvu
pdf2djvu:$(PDF2DJVU)_$(ARCH).deb
$(PDF2DJVU): $(SPREZZ)/pdf2djvu/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pdf2djvu_$(pdf2djvu_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: pstreams
pstreams:$(PSTREAMS)_$(ARCH).deb
$(PSTREAMS): $(SPREZZ)/pstreams/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pstreams_$(pstreams_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: jxrlib
jxrlib:$(JXRLIB)_$(ARCH).deb
$(JXRLIB): $(SPREZZ)/jxrlib/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf jxrlib_$(jxrlib_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: fbcmd
fbcmd:$(FBCMD)_$(ARCH).deb
$(FBCMD): $(SPREZZ)/fbcmd/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf fbcmd_$(fbcmd_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: hatop
hatop:$(HATOP)_$(ARCH).deb
$(HATOP): $(SPREZZ)/hatop/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf hatop_$(hatop_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ibus-rime
ibus-rime:$(IBUSRIME)_$(ARCH).deb
$(IBUSRIME): $(SPREZZ)/ibus-rime/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ibus-rime_$(ibus-rime_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: librime
librime:$(LIBRIME)_$(ARCH).deb
$(LIBRIME): $(SPREZZ)/librime/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf librime_$(librime_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: compizconfig-backend-gconf
compizconfig-backend-gconf:$(COMPIZCONFIGBACKENDGCONF)_$(ARCH).deb
$(COMPIZCONFIGBACKENDGCONF): $(SPREZZ)/compizconfig-backend-gconf/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf compizconfig-backend-gconf_$(compizconfig-backend-gconf_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: compiz-fusion-bcop
compiz-fusion-bcop:$(COMPIZFUSIONBCOP)_$(ARCH).deb
$(COMPIZFUSIONBCOP): $(SPREZZ)/compiz-fusion-bcop/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf compiz-fusion-bcop_$(compiz-fusion-bcop_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: compiz-fusion-plugins-extra
compiz-fusion-plugins-extra:$(COMPIZFUSIONPLUGINSEXTRA)_$(ARCH).deb
$(COMPIZFUSIONPLUGINSEXTRA): $(SPREZZ)/compiz-fusion-plugins-extra/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf compiz-fusion-plugins-extra_$(compiz-fusion-plugins-extra_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: compiz-fusion-plugins-unsupported
compiz-fusion-plugins-unsupported:$(COMPIZFUSIONPLUGINSUNSUPPORTED)_$(ARCH).deb
$(COMPIZFUSIONPLUGINSUNSUPPORTED): $(SPREZZ)/compiz-fusion-plugins-unsupported/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf compiz-fusion-plugins-unsupported_$(compiz-fusion-plugins-unsupported_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: adns
adns:$(ADNS)_$(ARCH).deb
$(ADNS): $(SPREZZ)/adns/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf adns_$(adns_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: iw
iw:$(IW)_$(ARCH).deb
$(IW): $(SPREZZ)/iw/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf iw_$(iw_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: pinfo
pinfo:$(PINFO)_$(ARCH).deb
$(PINFO): $(SPREZZ)/pinfo/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf pinfo_$(pinfo_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: fonts-adobesourcecodepro
fonts-adobesourcecodepro:$(FONTSADOBESOURCECODEPRO)_$(ARCH).deb
$(FONTSADOBESOURCECODEPRO): $(SPREZZ)/fonts-adobe-sourcecodepro/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf fonts-adobe-sourcecodepro_$(fonts-adobe-sourcecodepro_UPVER).orig.tar.gz $(TARARGS) $@

