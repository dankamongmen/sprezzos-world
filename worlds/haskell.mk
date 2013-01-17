.PHONY: ghc
ghc:$(GHC)_$(ARCH).deb
$(GHC): $(SPREZZ)/ghc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ghc-$(ghc_UPVER).tar.gz $(TARARGS) $@

.PHONY: haskell-edit-distance
haskell-edit-distance:$(HASKELLEDITDISTANCE)_$(ARCH).deb
$(HASKELLEDITDISTANCE): $(SPREZZ)/haskell-edit-distance/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf edit-distance-$(haskell-edit-distance_UPVER).tar.gz $(TARARGS) $@
