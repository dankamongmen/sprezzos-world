.PHONY: mintsystem
mintsystem:$(MINTSYSTEM)_$(ARCH).deb
$(MINTSYSTEM): $(SPREZZ)/mintsystem/debian/changelog
	git clone git@github.com:Sprezzatech/mintsystem.git $@
	rm -rf $@/debian
	tar cJf mintsystem-$(mintsystem_UPVER).tar.xz $@ --exclude-vcs
	ln -sf mintsystem-$(mintsystem_UPVER).tar.xz mintsystem_$(mintsystem_UPVER).orig.tar.xz
	cp -r $(<D) $@/
