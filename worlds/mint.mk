.PHONY: mintsystem
mintsystem:$(MINTSYSTEM)_$(ARCH).deb
$(MINTSYSTEM): $(SPREZZ)/mintsystem/debian/changelog
	git clone git@github.com:Sprezzatech/mintsystem.git $@
	rm -rf $@/debian
	tar cJf mintsystem-$(mintsystem_UPVER).tar.xz $@ --exclude-vcs
	ln -sf mintsystem-$(mintsystem_UPVER).tar.xz mintsystem_$(mintsystem_UPVER).orig.tar.xz
	cp -r $(<D) $@/

#.PHONY: muffin
#muffin:$(MUFFIN)_$(ARCH).deb
#$(MUFFIN): $(SPREZZ)/muffin/debian/changelog
#	git clone git@github.com:dankamongmen/muffin.git $@
#	rm -rf $@/debian
#	tar cJf muffin-$(muffin_UPVER).tar.xz $@ --exclude-vcs
#	ln -sf muffin-$(muffin_UPVER).tar.xz muffin_$(muffin_UPVER).orig.tar.xz
#	cp -r $(<D) $@/

.PHONY: muffin
muffin:$(MUFFIN)_$(ARCH).deb
$(MUFFIN): $(SPREZZ)/muffin/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf muffin_$(muffin_UPVER).orig.tar.gz $(TARARGS) $@
	rm -rf $@/debian
	cp -r $(<D) $@/

.PHONY: cinnamon
cinnamon:$(CINNAMON)_$(ARCH).deb
$(CINNAMON): $(SPREZZ)/cinnamon/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf cinnamon_$(cinnamon_UPVER).orig.tar.gz $(TARARGS) $@
	rm -rf $@/debian
	cp -r $(<D) $@/

