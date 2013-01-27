.PHONY: mozilla-noscript
mozilla-noscript:$(MOZILLANOSCRIPT)_$(ARCH).deb
$(MOZILLANOSCRIPT): $(SPREZZ)/mozilla-noscript/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	cd $@ && xpi-repack -p mozilla-noscript -u $(mozilla-noscript_UPVER) ../noscript-$(mozilla-noscript_UPVER).xpi
	tar xjvf mozilla-noscript_$(mozilla-noscript_UPVER).orig.tar.bz2 $(TARARGS) $@
