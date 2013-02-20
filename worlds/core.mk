.PHONY: mhash
mhash:$(MHASH)_$(ARCH).deb
$(MHASH): $(SPREZZ)/mhash/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mhash-$(mhash_UPVER).tar.gz $(TARARGS) $@

