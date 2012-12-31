.PHONY: libfm
libfm:$(LIBFM)_$(ARCH).deb
$(LIBFM): $(SPREZZ)/libfm/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libfm-$(libfm_UPVER).tar.gz $(TARARGS) $@
