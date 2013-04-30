# Packages whose contents originated with Sprezzatech/SprezzOS itself.

.PHONY: grubtheme
sprezzos-grub2theme:$(SPREZZOSGRUB2THEME)_$(ARCH).deb
$(SPREZZOSGRUB2THEME): $(SPREZZ)/sprezzos-grub2theme/debian/changelog
	mkdir -p $@
	cp -r $(<D)/..* $@/
	tar cJf sprezzos-grub2theme-$(sprezzos-grub2theme_UPVER).tar.xz $@ --exclude-vcs --exclude=debian
	ln -s sprezzos-grub2theme-$(sprezzos-grub2theme_UPVER).tar.xz \
		sprezzos-grub2theme_$(sprezzos-grub2theme_UPVER).orig.tar.xz

.PHONY: charn
charn:$(CHARN)_$(ARCH).deb
$(CHARN): $(SPREZZ)/charn/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf charn_$(charn_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: raptorial
raptorial:$(RAPTORIAL)_$(ARCH).deb
$(RAPTORIAL): $(SPREZZ)/raptorial/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf raptorial_$(raptorial_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: libblossom
libblossom:$(LIBBLOSSOM)_$(ARCH).deb
$(LIBBLOSSOM): $(SPREZZ)/libblossom/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libblossom_$(libblossom_UPVER).orig.tar.gz $(TARARGS) $@

