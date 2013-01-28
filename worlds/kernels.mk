.PHONY: linux
linux:$(LINUX)_$(ARCH).deb
$(LINUX): $(SPREZZ)/linux/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf linux-$(linux_UPVER).tar.xz $(TARARGS) $@
