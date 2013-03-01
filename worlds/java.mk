.PHONY: rhino
rhino:$(RHINO)_$(ARCH).deb
$(RHINO): $(SPREZZ)/rhino/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf rhino_$(rhino_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: jansi
jansi:$(JANSI)_$(ARCH).deb
$(JANSI): $(SPREZZ)/jansi/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf jansi-$(jansi_UPVER).tar.gz $(TARARGS) $@

