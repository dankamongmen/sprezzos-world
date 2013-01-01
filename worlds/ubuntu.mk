.PHONY: dbus-test-runner
dbus-test-runner:$(DBUSTESTRUNNER)_$(ARCH).deb
$(DBUSTESTRUNNER): $(SPREZZ)/dbus-test-runner/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf dbus-test-runner-$(dbus-test-runner_UPVER).tar.gz $(TARARGS) $@
