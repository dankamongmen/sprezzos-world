.PHONY: dbus-test-runner
dbus-test-runner:$(DBUSTESTRUNNER)_$(ARCH).deb
$(DBUSTESTRUNNER): $(SPREZZ)/dbus-test-runner/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dbus-test-runner-$(dbus-test-runner_UPVER).tar.gz $(TARARGS) $@

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

