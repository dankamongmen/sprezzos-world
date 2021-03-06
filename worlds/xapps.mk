.PHONY: pidgin-facebookchat
pidgin-facebookchat:$(PIDGINFACEBOOKCHAT)_$(ARCH).deb
$(PIDGINFACEBOOKCHAT): $(SPREZZ)/pidgin-facebookchat/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf pidgin-facebookchat-source-$(pidgin-facebookchat_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: icoutils
icoutils:$(ICOUTILS)_$(ARCH).deb
$(ICOUTILS): $(SPREZZ)/icoutils/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf icoutils-$(icoutils_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: model-builder
model-builder:$(MODELBUILDER)_$(ARCH).deb
$(MODELBUILDER): $(SPREZZ)/model-builder/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf Model-Builder-$(model-builder_UPVER).tar.gz $(TARARGS) $@

#.PHONY: xbmc
#xbmc:$(XBMC)_$(ARCH).deb
#$(XBMC): $(SPREZZ)/xbmc/debian/changelog
#	@[ ! -e $@ ] || { echo "Removing $@..." && rm -rf $@ ; }
#	git clone git://github.com/xbmc/xbmc.git $@
#	tar cJf xbmc-$(xbmc_UPVER).tar.xz $@ --exclude-vcs
#	ln -sf xbmc-$(xbmc_UPVER).tar.xz xbmc_$(xbmc_UPVER).orig.tar.xz
#	cp -r $(<D) $@/

.PHONY: xbmc
xbmc:$(XBMC)_$(ARCH).deb
$(XBMC): $(SPREZZ)/xbmc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xbmc_$(xbmc_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: agg
agg:$(AGG)_$(ARCH).deb
$(AGG): $(SPREZZ)/agg/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf agg-$(agg_UPVER).tar.gz $(TARARGS) $@

.PHONY: plotutils
plotutils:$(PLOTUTILS)_$(ARCH).deb
$(PLOTUTILS): $(SPREZZ)/plotutils/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf plotutils-$(plotutils_UPVER).tar.gz $(TARARGS) $@

.PHONY: ball
ball:$(BALL)_$(ARCH).deb
$(BALL): $(SPREZZ)/ball/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf ball_$(ball_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: bogl
bogl:$(BOGL)_$(ARCH).deb
$(BOGL): $(SPREZZ)/bogl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bogl-$(bogl_UPVER).tar.gz $(TARARGS) $@

.PHONY: tesseract
tesseract:$(TESSERACT)_$(ARCH).deb
$(TESSERACT): $(SPREZZ)/tesseract/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tesseract-ocr-$(tesseract_UPVER).tar.gz $(TARARGS) $@

.PHONY: geeqie
geeqie:$(GEEQIE)_$(ARCH).deb
$(GEEQIE): $(SPREZZ)/geeqie/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf geeqie-$(geeqie_UPVER).tar.gz $(TARARGS) $@

.PHONY: xpaint
xpaint:$(XPAINT)_$(ARCH).deb
$(XPAINT): $(SPREZZ)/xpaint/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf xpaint-$(xpaint_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: xaw3d
xaw3d:$(XAW3D)_$(ARCH).deb
$(XAW3D): $(SPREZZ)/xaw3d/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libXaw3d-$(xaw3d_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: ufraw
ufraw:$(UFRAW)_$(ARCH).deb
$(UFRAW): $(SPREZZ)/ufraw/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ufraw-$(ufraw_UPVER).tar.gz $(TARARGS) $@

.PHONY: libxcm
libxcm:$(LIBXCM)_$(ARCH).deb
$(LIBXCM): $(SPREZZ)/libxcm/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libXcm-$(libxcm_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: oyranos
oyranos:$(OYRANOS)_$(ARCH).deb
$(OYRANOS): $(SPREZZ)/oyranos/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf oyranos-$(oyranos_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: cinepaint
cinepaint:$(CINEPAINT)_$(ARCH).deb
$(CINEPAINT): $(SPREZZ)/cinepaint/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cinepaint_$(cinepaint_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: gnokii
gnokii:$(GNOKII)_$(ARCH).deb
$(GNOKII): $(SPREZZ)/gnokii/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf gnokii-$(gnokii_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: makehuman
makehuman:$(MAKEHUMAN)_$(ARCH).deb
$(MAKEHUMAN): $(SPREZZ)/makehuman/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf makehuman-$(makehuman_UPVER).tar.gz $(TARARGS) $@

.PHONY: k3d
k3d:$(K3D)_$(ARCH).deb
$(K3D): $(SPREZZ)/k3d/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf k3d-source-$(k3d_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: ledit
ledit:$(LEDIT)_$(ARCH).deb
$(LEDIT): $(SPREZZ)/ledit/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ledit-$(ledit_UPVER).tgz $(TARARGS) $@

.PHONY: smuxi
smuxi:$(SMUXI)_$(ARCH).deb
$(SMUXI): $(SPREZZ)/smuxi/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf smuxi-$(smuxi_UPVER).tar.gz $(TARARGS) $@

.PHONY: nini
nini:$(NINI)_$(ARCH).deb
$(NINI): $(SPREZZ)/nini/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf nini-$(nini_UPVER).tar.gz $(TARARGS) $@

.PHONY: libvncserver
libvncserver:$(LIBVNCSERVER)_$(ARCH).deb
$(LIBVNCSERVER): $(SPREZZ)/libvncserver/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf LibVNCServer-$(libvncserver_UPVER).tar.gz $(TARARGS) $@

.PHONY: desktop-file-utils
desktop-file-utils:$(DESKTOPFILEUTILS)_$(ARCH).deb
$(DESKTOPFILEUTILS): $(SPREZZ)/desktop-file-utils/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf desktop-file-utils-$(desktop-file-utils_UPVER).tar.xz $(TARARGS) $@

.PHONY: freqtweak
freqtweak:$(FREQTWEAK)_$(ARCH).deb
$(FREQTWEAK): $(SPREZZ)/freqtweak/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf freqtweak-$(freqtweak_UPVER).tar.gz $(TARARGS) $@

.PHONY: xscreensaver
xscreensaver:$(XSCREENSAVER)_$(ARCH).deb
$(XSCREENSAVER): $(SPREZZ)/xscreensaver/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xscreensaver-$(xscreensaver_UPVER).tar.gz $(TARARGS) $@

.PHONY: gle
gle:$(GLE)_$(ARCH).deb
$(GLE): $(SPREZZ)/gle/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gle-$(gle_UPVER).tar.gz $(TARARGS) $@

.PHONY: xloadimage
xloadimage:$(XLOADIMAGE)_$(ARCH).deb
$(XLOADIMAGE): $(SPREZZ)/xloadimage/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xloadimage_$(xloadimage_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libxaw3dxft
libxaw3dxft:$(LIBXAW3DXFT)_$(ARCH).deb
$(LIBXAW3DXFT): $(SPREZZ)/libxaw3dxft/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libxaw3dxft_$(libxaw3dxft_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: fltk1.1
fltk1.1:$(FLTK1.1)_$(ARCH).deb
$(FLTK1.1): $(SPREZZ)/fltk1.1/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf fltk1.1_$(fltk1.1_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: openms
openms:$(OPENMS)_$(ARCH).deb
$(OPENMS): $(SPREZZ)/openms/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf openms_$(openms_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: glpk
glpk:$(GLPK)_$(ARCH).deb
$(GLPK): $(SPREZZ)/glpk/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf glpk_$(glpk_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: tuxpaint
tuxpaint:$(TUXPAINT)_$(ARCH).deb
$(TUXPAINT): $(SPREZZ)/tuxpaint/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tuxpaint_$(tuxpaint_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: icewm
icewm:$(ICEWM)_$(ARCH).deb
$(ICEWM): $(SPREZZ)/icewm/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf icewm_$(icewm_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: blackbox
blackbox:$(BLACKBOX)_$(ARCH).deb
$(BLACKBOX): $(SPREZZ)/blackbox/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf blackbox_$(blackbox_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: stumpwm
stumpwm:$(STUMPWM)_$(ARCH).deb
$(STUMPWM): $(SPREZZ)/stumpwm/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf stumpwm_$(stumpwm_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: freewrl
freewrl:$(FREEWRL)_$(ARCH).deb
$(FREEWRL): $(SPREZZ)/freewrl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf freewrl_$(freewrl_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: openmotif
openmotif:$(OPENMOTIF)_$(ARCH).deb
$(OPENMOTIF): $(SPREZZ)/openmotif/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf openmotif_$(openmotif_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: sdl-sound1.2
sdl-sound1.2:$(SDLSOUND1.2)_$(ARCH).deb
$(SDLSOUND1.2): $(SPREZZ)/sdl-sound1.2/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf sdl-sound1.2_$(sdl-sound1.2_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: smpeg
smpeg:$(SMPEG)_$(ARCH).deb
$(SMPEG): $(SPREZZ)/smpeg/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf smpeg_$(smpeg_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: icon-naming-utils
icon-naming-utils:$(ICONNAMINGUTILS)_$(ARCH).deb
$(ICONNAMINGUTILS): $(SPREZZ)/icon-naming-utils/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf icon-naming-utils_$(icon-naming-utils_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: librecad
librecad:$(LIBRECAD)_$(ARCH).deb
$(LIBRECAD): $(SPREZZ)/librecad/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf librecad_$(librecad_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: acoustid-fingerprinter
acoustid-fingerprinter:$(ACOUSTIDFINGERPRINTER)_$(ARCH).deb
$(ACOUSTIDFINGERPRINTER): $(SPREZZ)/acoustid-fingerprinter/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf acoustid-fingerprinter_$(acoustid-fingerprinter_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: shared-desktop-ontologies
shared-desktop-ontologies:$(shareddesktopontologies)_$(ARCH).deb
$(shareddesktopontologies): $(SPREZZ)/shared-desktop-ontologies/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf shared-desktop-ontologies_$(shared-desktop-ontologies_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: pidgin-extprefs
pidgin-extprefs:$(pidginextprefs)_$(ARCH).deb
$(pidginextprefs): $(SPREZZ)/pidgin-extprefs/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf pidgin-extprefs_$(pidgin-extprefs_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ogre-1.8
ogre-1.8:$(OGRE1.8)_$(ARCH).deb
$(OGRE1.8): $(SPREZZ)/ogre-1.8/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf ogre-1.8_$(ogre-1.8_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: jitsi
jitsi:$(JITSI)_$(ARCH).deb
$(JITSI): $(SPREZZ)/jitsi/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf jitsi_$(jitsi_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libsoil
libsoil:$(LIBSOIL)_$(ARCH).deb
$(LIBSOIL): $(SPREZZ)/libsoil/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libsoil_$(libsoil_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libdrawtext
libdrawtext:$(LIBDRAWTEXT)_$(ARCH).deb
$(LIBDRAWTEXT): $(SPREZZ)/libdrawtext/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libdrawtext_$(libdrawtext_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: ultracopier
ultracopier:$(ULTRACOPIER)_$(ARCH).deb
$(ULTRACOPIER): $(SPREZZ)/ultracopier/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf ultracopier_$(ultracopier_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: yafaray
yafaray:$(YAFARAY)_$(ARCH).deb
$(YAFARAY): $(SPREZZ)/yafaray/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf yafaray_$(yafaray_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: cegui
cegui:$(CEGUI)_$(ARCH).deb
$(CEGUI): $(SPREZZ)/cegui/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cegui_$(cegui_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: devil
devil:$(DEVIL)_$(ARCH).deb
$(DEVIL): $(SPREZZ)/devil/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf devil_$(devil_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: irrlicht
irrlicht:$(IRRLICHT)_$(ARCH).deb
$(IRRLICHT): $(SPREZZ)/irrlicht/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf irrlicht_$(irrlicht_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: allegro5
allegro5:$(ALLEGRO5)_$(ARCH).deb
$(ALLEGRO5): $(SPREZZ)/allegro5/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf allegro5_$(allegro5_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: simage
simage:$(SIMAGE)_$(ARCH).deb
$(SIMAGE): $(SPREZZ)/simage/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf simage_$(simage_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: osgearth
osgearth:$(OSGEARTH)_$(ARCH).deb
$(OSGEARTH): $(SPREZZ)/osgearth/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf osgearth_$(osgearth_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: coin3
coin3:$(COIN3)_$(ARCH).deb
$(COIN3): $(SPREZZ)/coin3/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf coin3_$(coin3_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: weston
weston:$(WESTON)_$(ARCH).deb
$(WESTON): $(SPREZZ)/weston/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf weston_$(weston_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: leocad
leocad:$(LEOCAD)_$(ARCH).deb
$(LEOCAD): $(SPREZZ)/leocad/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf leocad_$(leocad_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: oglft
oglft:$(OGLFT)_$(ARCH).deb
$(OGLFT): $(SPREZZ)/oglft/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf oglft_$(oglft_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: devilspie
devilspie:$(DEVILSPIE)_$(ARCH).deb
$(DEVILSPIE): $(SPREZZ)/devilspie/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf devilspie_$(devilspie_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: xserver-xorg-input-synaptics
xserver-xorg-input-synaptics:$(XSERVERXORGINPUTSYNAPTICS)_$(ARCH).deb
$(XSERVERXORGINPUTSYNAPTICS): $(SPREZZ)/xserver-xorg-input-synaptics/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xserver-xorg-input-synaptics_$(xserver-xorg-input-synaptics_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: xf86-input-tslib
xf86-input-tslib:$(uu86uuuuuuuuuu)_$(ARCH).deb
$(uu86uuuuuuuuuu): $(SPREZZ)/xf86-input-tslib/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf xf86-input-tslib_$(xf86-input-tslib_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: xserver-xorg-input-joystick
xserver-xorg-input-joystick:$(uuuuuuuuuuuuuuuuuuuuuuuu)_$(ARCH).deb
$(uuuuuuuuuuuuuuuuuuuuuuuu): $(SPREZZ)/xserver-xorg-input-joystick/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xserver-xorg-input-joystick_$(xserver-xorg-input-joystick_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: xserver-xorg-input-void
xserver-xorg-input-void:$(uuuuuuuuuuuuuuuuuuuu)_$(ARCH).deb
$(uuuuuuuuuuuuuuuuuuuu): $(SPREZZ)/xserver-xorg-input-void/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xserver-xorg-input-void_$(xserver-xorg-input-void_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: xserver-xorg-video-chips
xserver-xorg-video-chips:$(XSERVERXORGVIDEOCHIPS)_$(ARCH).deb
$(XSERVERXORGVIDEOCHIPS): $(SPREZZ)/xserver-xorg-video-chips/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xserver-xorg-video-chips_$(xserver-xorg-video-chips_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: xplanet
xplanet:$(XPLANET)_$(ARCH).deb
$(XPLANET): $(SPREZZ)/xplanet/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf xplanet_$(xplanet_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: gtk-theme-switch
gtk-theme-switch:$(GTKTHEMESWITCH)_$(ARCH).deb
$(GTKTHEMESWITCH): $(SPREZZ)/gtk-theme-switch/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gtk-theme-switch_$(gtk-theme-switch_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: gpointing-device-settings
gpointing-device-settings:$(GPOINTINGDEVICESETTINGS)_$(ARCH).deb
$(GPOINTINGDEVICESETTINGS): $(SPREZZ)/gpointing-device-settings/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gpointing-device-settings_$(gpointing-device-settings_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: flowblade
flowblade:$(FLOWBLADE)_$(ARCH).deb
$(FLOWBLADE): $(SPREZZ)/flowblade/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf flowblade_$(flowblade_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libsdl2
libsdl2:$(LIBSDL2)_$(ARCH).deb
$(LIBSDL2): $(SPREZZ)/libsdl2/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libsdl2_$(libsdl2_UPVER).orig.tar.gz $(TARARGS) $@
	rm -rf $@/debian
	cp -r $(<D) $@

.PHONY: wmctrl
wmctrl:$(WMCTRL)_$(ARCH).deb
$(WMCTRL): $(SPREZZ)/wmctrl/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf wmctrl_$(wmctrl_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: fltk1.3
fltk1.3:$(FLTK1.3)_$(ARCH).deb
$(FLTK1.3): $(SPREZZ)/fltk1.3/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf fltk1.3_$(fltk1.3_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: nestopia
nestopia:$(NESTOPIA)_$(ARCH).deb
$(NESTOPIA): $(SPREZZ)/nestopia/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf nestopia_$(nestopia_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: mygui
mygui:$(MYGUI)_$(ARCH).deb
$(MYGUI): $(SPREZZ)/mygui/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf mygui_$(mygui_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: compiz-fusion-plugins-main
compiz-fusion-plugins-main:$(COMPIZFUSIONPLUGINSMAIN)_$(ARCH).deb
$(COMPIZFUSIONPLUGINSMAIN): $(SPREZZ)/compiz-fusion-plugins-main/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf compiz-fusion-plugins-main_$(compiz-fusion-plugins-main_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: gliv
gliv:$(GLIV)_$(ARCH).deb
$(GLIV): $(SPREZZ)/gliv/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf gliv_$(gliv_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: gmic
gmic:$(GMIC)_$(ARCH).deb
$(GMIC): $(SPREZZ)/gmic/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gmic_$(gmic_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: winswitch
winswitch:$(WINSWITCH)_$(ARCH).deb
$(WINSWITCH): $(SPREZZ)/winswitch/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf winswitch_$(winswitch_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: din
din:$(DIN)_$(ARCH).deb
$(DIN): $(SPREZZ)/din/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf din_$(din_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: invesalius
invesalius:$(INVESALIUS)_$(ARCH).deb
$(INVESALIUS): $(SPREZZ)/invesalius/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf invesalius_$(invesalius_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: vtk
vtk:$(VTK)_$(ARCH).deb
$(VTK): $(SPREZZ)/vtk/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf vtk_$(vtk_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: bullet
bullet:$(BULLET)_$(ARCH).deb
$(BULLET): $(SPREZZ)/bullet/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bullet_$(bullet_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: thoggen
thoggen:$(THOGGEN)_$(ARCH).deb
$(THOGGEN): $(SPREZZ)/thoggen/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf thoggen_$(thoggen_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: toonloop
toonloop:$(TOONLOOP)_$(ARCH).deb
$(TOONLOOP): $(SPREZZ)/toonloop/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf toonloop_$(toonloop_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: tupi
tupi:$(TUPI)_$(ARCH).deb
$(TUPI): $(SPREZZ)/tupi/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tupi_$(tupi_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: sonic-visualiser
sonic-visualiser:$(SONICVISUALISER)_$(ARCH).deb
$(SONICVISUALISER): $(SPREZZ)/sonic-visualiser/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf sonic-visualiser_$(sonic-visualiser_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: zathura
zathura:$(ZATHURA)_$(ARCH).deb
$(ZATHURA): $(SPREZZ)/zathura/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf zathura_$(zathura_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: sane-backends
sane-backends:$(SANEBACKENDS)_$(ARCH).deb
$(SANEBACKENDS): $(SPREZZ)/sane-backends/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf sane-backends_$(sane-backends_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: libqglviewer
libqglviewer:$(LIBQGLVIEWER)_$(ARCH).deb
$(LIBQGLVIEWER): $(SPREZZ)/libqglviewer/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libqglviewer_$(libqglviewer_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: rasmol
rasmol:$(RASMOL)_$(ARCH).deb
$(RASMOL): $(SPREZZ)/rasmol/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf rasmol_$(rasmol_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: raster3d
raster3d:$(RASTER3D)_$(ARCH).deb
$(RASTER3D): $(SPREZZ)/raster3d/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf raster3d_$(raster3d_UPVER).orig.tar.gz $(TARARGS) $@

