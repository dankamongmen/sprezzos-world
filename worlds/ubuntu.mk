.PHONY: dbus-test-runner
dbus-test-runner:$(DBUSTESTRUNNER)_$(ARCH).deb
$(DBUSTESTRUNNER): $(SPREZZ)/dbus-test-runner/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dbus-test-runner-$(dbus-test-runner_UPVER).tar.gz $(TARARGS) $@

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
