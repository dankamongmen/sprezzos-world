.PHONY: appmenu-qt
appmenu-qt:$(APPMENUQT)_$(ARCH).deb
$(APPMENUQT): $(SPREZZ)/appmenu-qt/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf appmenu-qt-$(appmenu-qt_UPVER).tar.gz $(TARARGS) $@

.PHONY: bamf
bamf:$(BAMF)_$(ARCH).deb
$(BAMF): $(SPREZZ)/bamf/debian/changelog
	bzr branch lp:bamf $@
	rm -rf $@/debian
	tar cJf bamf-$(bamf_UPVER).tar.xz $@ --exclude-vcs
	ln -sf bamf-$(bamf_UPVER).tar.xz \
		$(BAMF).orig.tar.xz
	cp -r $(<D) $@/

#.PHONY: bamf
#bamf:$(BAMF)_$(ARCH).deb
#$(BAMF): $(SPREZZ)/bamf/debian/changelog
#	mkdir $@
#	cp -r $(<D) $@/
#	cd $@ && uscan --force-download --download-current-version
#	tar xzvf bamf-$(bamf_UPVER).tar.gz $(TARARGS) $@

# There's a recipe for the tip-of-bzr-trunk in Makefile
#.PHONY: compiz9
#compiz9:$(COMPIZ9)_$(ARCH).deb
#$(COMPIZ9): $(SPREZZ)/compiz9/debian/changelog
#	mkdir $@
#	cp -r $(<D) $@/
#	cd $@ && uscan --force-download --download-current-version
#	tar xjvf compiz-$(compiz9_UPVER).tar.bz2 $(TARARGS) $@
#

# There's a recipe for the released version commented out in worlds/ubuntu.mk
.PHONY: compiz9
compiz9:$(COMPIZ9)_$(ARCH).deb
$(COMPIZ9): $(SPREZZ)/compiz9/debian/changelog
	bzr branch lp:compiz $@
	rm -rf $@/debian
	tar cJf compiz-$(compiz9_UPVER).tar.xz $@ --exclude-vcs
	ln -sf compiz-$(compiz9_UPVER).tar.xz $(COMPIZ9).orig.tar.xz
	cp -r $(<D) $@/

# There's a recipe for the released version commented out in worlds/ubuntu.mk
.PHONY: nux
nux:$(NUX)_$(ARCH).deb
$(NUX): $(SPREZZ)/nux/debian/changelog
	bzr branch lp:nux $@
	rm -rf $@/debian
	tar cJf nux-$(nux_UPVER).tar.xz $@ --exclude-vcs
	ln -sf nux-$(nux_UPVER).tar.xz $(NUX).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: dbus-test-runner
dbus-test-runner:$(DBUSTESTRUNNER)_$(ARCH).deb
$(DBUSTESTRUNNER): $(SPREZZ)/dbus-test-runner/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dbus-test-runner-$(dbus-test-runner_UPVER).tar.gz $(TARARGS) $@

.PHONY: evemu
evemu:$(EVEMU)_$(ARCH).deb
$(EVEMU): $(SPREZZ)/evemu/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf evemu-$(evemu_UPVER).tar.xz $(TARARGS) $@

.PHONY: frame
frame:$(FRAME)_$(ARCH).deb
$(FRAME): $(SPREZZ)/frame/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf frame-$(frame_UPVER).tar.xz $(TARARGS) $@

.PHONY: geis
geis:$(GEIS)_$(ARCH).deb
$(GEIS): $(SPREZZ)/geis/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf geis-$(geis_UPVER).tar.xz $(TARARGS) $@

.PHONY: grail
grail:$(GRAIL)_$(ARCH).deb
$(GRAIL): $(SPREZZ)/grail/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf grail-$(grail_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: humanity-icon-theme
humanity-icon-theme:$(HUMANITYICONTHEME)_$(ARCH).deb
$(HUMANITYICONTHEME): $(SPREZZ)/humanity-icon-theme/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf humanity-icon-theme-$(humanity-icon-theme_UPVER).tar.gz $(TARARGS) $@

.PHONY: ido
ido:$(IDO)_$(ARCH).deb
$(IDO): $(SPREZZ)/ido/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ido-$(ido_UPVER).tar.gz $(TARARGS) $@

.PHONY: indicator-applet
indicator-applet:$(INDICATORAPPLET)_$(ARCH).deb
$(INDICATORAPPLET): $(SPREZZ)/indicator-applet/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf indicator-applet-$(indicator-applet_UPVER).tar.gz $(TARARGS) $@

.PHONY: indicator-application
indicator-application:$(INDICATORAPPLICATION)_$(ARCH).deb
$(INDICATORAPPLICATION): $(SPREZZ)/indicator-application/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf indicator-application-$(indicator-application_UPVER).tar.gz $(TARARGS) $@

.PHONY: indicator-appmenu
indicator-appmenu:$(INDICATORAPPMENU)_$(ARCH).deb
$(INDICATORAPPMENU): $(SPREZZ)/indicator-appmenu/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf indicator-appmenu-$(indicator-appmenu_UPVER).tar.gz $(TARARGS) $@

.PHONY: indicator-datetime
indicator-datetime:$(INDICATORDATETIME)_$(ARCH).deb
$(INDICATORDATETIME): $(SPREZZ)/indicator-datetime/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf indicator-datetime-$(indicator-datetime_UPVER).tar.gz $(TARARGS) $@

.PHONY: indicator-me
indicator-me:$(INDICATORME)_$(ARCH).deb
$(INDICATORME): $(SPREZZ)/indicator-me/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf indicator-me-$(indicator-me_UPVER).tar.gz $(TARARGS) $@

.PHONY: indicator-messages
indicator-messages:$(INDICATORMESSAGES)_$(ARCH).deb
$(INDICATORMESSAGES): $(SPREZZ)/indicator-messages/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf indicator-messages-$(indicator-messages_UPVER).tar.gz $(TARARGS) $@

.PHONY: indicator-session
indicator-session:$(INDICATORSESSION)_$(ARCH).deb
$(INDICATORSESSION): $(SPREZZ)/indicator-session/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf indicator-session-$(indicator-session_UPVER).tar.gz $(TARARGS) $@

.PHONY: indicator-sound
indicator-sound:$(INDICATORSOUND)_$(ARCH).deb
$(INDICATORSOUND): $(SPREZZ)/indicator-sound/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf indicator-sound-$(indicator-sound_UPVER).tar.gz $(TARARGS) $@

.PHONY: libdbusmenu
libdbusmenu:$(LIBDBUSMENU)_$(ARCH).deb
$(LIBDBUSMENU): $(SPREZZ)/libdbusmenu/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libdbusmenu-$(libdbusmenu_UPVER).tar.gz $(TARARGS) $@

.PHONY: libindicate
libindicate:$(LIBINDICATE)_$(ARCH).deb
$(LIBINDICATE): $(SPREZZ)/libindicate/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libindicate-$(libindicate_UPVER).tar.gz $(TARARGS) $@

.PHONY: libindicator
libindicator:$(LIBINDICATOR)_$(ARCH).deb
$(LIBINDICATOR): $(SPREZZ)/libindicator/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libindicator-$(libindicator_UPVER).tar.gz $(TARARGS) $@

.PHONY: libunity
libunity:$(LIBUNITY)_$(ARCH).deb
$(LIBUNITY): $(SPREZZ)/libunity/debian/changelog
	bzr branch lp:libunity $@
	rm -rf $@/debian
	tar cJf libunity-$(libunity_UPVER).tar.xz $@ --exclude-vcs
	ln -sf libunity-$(libunity_UPVER).tar.xz \
		libunity_$(libunity_UPVER).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: libunity-misc
libunity-misc:$(LIBUNITYMISC)_$(ARCH).deb
$(LIBUNITYMISC): $(SPREZZ)/libunity-misc/debian/changelog
	bzr branch lp:libunity-misc $@
	rm -rf $@/debian
	tar cJf libunity-misc-$(libunity-misc_UPVER).tar.xz $@ --exclude-vcs
	ln -sf libunity-misc-$(libunity-misc_UPVER).tar.xz $(LIBUNITYMISC).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: libunity-webapps
libunity-webapps:$(LIBUNITYWEBAPPS)_$(ARCH).deb
$(LIBUNITYWEBAPPS): $(SPREZZ)/libunity-webapps/debian/changelog
	bzr branch lp:libunity-webapps $@
	rm -rf $@/debian
	tar cJf unity_webapps-$(libunity-webapps_UPVER).tar.xz $@ --exclude-vcs
	ln -sf unity_webapps-$(libunity-webapps_UPVER).tar.xz \
		$(LIBUNITYWEBAPPS).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: libvarpa
libvarpa:$(LIBVARPA)_$(ARCH).deb
$(LIBVARPA): $(SPREZZ)/libvarpa/debian/changelog
	bzr branch lp:libvarpa $@
	rm -rf $@/debian
	tar cJf libvarpa-$(libvarpa_UPVER).tar.xz $@ --exclude-vcs
	ln -sf libvarpa-$(libvarpa_UPVER).tar.xz libvarpa_$(libvarpa_UPVER).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: libzeitgeist
libzeitgeist:$(LIBZEITGEIST)_$(ARCH).deb
$(LIBZEITGEIST): $(SPREZZ)/libzeitgeist/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libzeitgeist-$(libzeitgeist_UPVER).tar.gz $(TARARGS) $@

.PHONY: zeitgeist
zeitgeist:$(ZEITGEIST)_$(ARCH).deb
$(ZEITGEIST): $(SPREZZ)/zeitgeist/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf zeitgeist-$(zeitgeist_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: marlin
marlin:$(MARLIN)_$(ARCH).deb
$(MARLIN): $(SPREZZ)/marlin/debian/changelog
	bzr branch lp:marlin $@
	rm -rf $@/debian
	tar cJf marlin-$(marlin_UPVER).tar.xz $@ --exclude-vcs
	ln -sf marlin-$(marlin_UPVER).tar.xz marlin_$(marlin_UPVER).orig.tar.xz
	cp -r $(<D) $@/

.PHONY: ntrack
ntrack:$(NTRACK)_$(ARCH).deb
$(NTRACK): $(SPREZZ)/ntrack/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ntrack-$(ntrack_UPVER).tar.gz $(TARARGS) $@

#.PHONY: unity
#unity:$(UNITY)_$(ARCH).deb
#$(UNITY): $(SPREZZ)/unity/debian/changelog
#	bzr branch lp:unity $@
#	rm -rf $@/debian
#	tar cJf unity-$(unity_UPVER).tar.xz $@ --exclude-vcs
#	ln -sf unity-$(unity_UPVER).tar.xz $(UNITY).orig.tar.xz
#	cp -r $(<D) $@/

.PHONY: unity
unity:$(UNITY)_$(ARCH).deb
$(UNITY): $(SPREZZ)/unity/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf unity-$(unity_UPVER).tar.gz $(TARARGS) $@

# We're building from bzr in Makefile
#.PHONY: nux
#nux:$(NUX)_$(ARCH).deb
#$(NUX): $(SPREZZ)/nux/debian/changelog
#	mkdir $@
#	cp -r $(<D) $@/
#	cd $@ && uscan --force-download --download-current-version
#	tar xzvf nux-$(nux_UPVER).tar.gz $(TARARGS) $@

.PHONY: usb-creator
usb-creator:$(USBCREATOR)_$(ARCH).deb
$(USBCREATOR): $(SPREZZ)/usb-creator/debian/changelog
	bzr branch lp:usb-creator $@
	rm -rf $@/debian
	tar cJf usb-creator-$(usb-creator_UPVER).tar.xz $@ --exclude-vcs
	ln -sf usb-creator-$(usb-creator_UPVER).tar.xz $(USBCREATOR).orig.tar.xz
	cp -r $(<D) $@/

