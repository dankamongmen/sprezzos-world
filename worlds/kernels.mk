.PHONY: linux
linux:$(LINUX)_$(ARCH).deb
$(LINUX): $(SPREZZ)/linux/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf linux-$(linux_UPVER).tar.gz $(TARARGS) $@

